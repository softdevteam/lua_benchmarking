--[[
Copyright (c) 2017 King's College London
created by the Software Development Team <http://soft-dev.org/>

The Universal Permissive License (UPL), Version 1.0

Subject to the condition set forth below, permission is hereby granted to any
person obtaining a copy of this software, associated documentation and/or data
(collectively the "Software"), free of charge and under any and all copyright
rights in the Software, and any and all patent rights owned or freely
licensable by each licensor hereunder covering either (i) the unmodified
Software as contributed to or provided by such licensor, or (ii) the Larger
Works (as defined below), to deal in both

(a) the Software, and
(b) any piece of software and/or hardware listed in the lrgrwrks.txt file if
one is included with the Software (each a "Larger Work" to which the Software
is contributed by such licensors),

without restriction, including without limitation the rights to copy, create
derivative works of, display, perform, and distribute the Software and make,
use, sell, offer for sale, import, export, have made, and have sold the
Software and the Larger Work(s), and to sublicense the foregoing rights on
either these or other terms.

This license is subject to the following condition: The above copyright notice
and either this complete permission notice or at a minimum a reference to the
UPL must be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
--]]

local ffi = require("ffi")
local jitstats

local f = io.open("/proc/self/cmdline", "rb")
local cmdline = f:read("*all")
f:close()

local krunoptstr = string.match(cmdline, "@KRUN@(.*)@KRUN@")
assert(krunoptstr, "Missing @KRUN@ option string on the command line")

options = {}
for optenty in string.gmatch(krunoptstr, "([^|]+)") do
    local key, val = string.match(optenty, "([^=]+)=(.*)")
    options[key] = val
end

assert(next(options), "Failed to match any options inside the @KRUN@ ")

local BM_benchmark = options.benchmark
local BM_iters = options.iters and tonumber(options.iters)
local BM_param = tonumber(options.param)
local BM_debug = tonumber(options.debug) > 0
local BM_instrument = tonumber(options.instrument) > 0
local BM_instdatadir = options.instdatadir
local BM_key = options.key
local BM_pexecidx = options.pexecidx

assert(BM_benchmark, "No benchmark specified")
assert(BM_iters, "No iteration count specified")

function add_luapackage_path(basepath, highpriority)
    assert(type(basepath) == "string" and #basepath > 0)
    
    if basepath[#basepath] == "/" then
        basepath = basepath:sub(#basepath - 1)
    end

    if highpriority then
        package.path = string.format("%s/?.lua;%s/?/init.lua;%s", basepath, basepath, package.path)
    else
        package.path = string.format("%s;%s/?.lua;%s/?/init.lua", package.path, basepath, basepath)
    end
end

function add_cpackage_path(basepath, highpriority)
    assert(type(basepath) == "string" and #basepath > 0)

    if basepath[#basepath] == "/" then
        basepath = basepath:sub(#basepath - 1)
    end

    if highpriority then
        package.cpath = string.format("%s/?.so;%s/?/?.so;%s", basepath, basepath, package.cpath)
    else
        package.cpath = string.format("%s;%s/?.so;%s/?/?.so", package.cpath, basepath, basepath)
    end
end

local slashidx = string.find(BM_benchmark, "/[^/]*$")
if slashidx then
    local benchdir = string.sub(BM_benchmark, 1, slashidx-1)
    add_luapackage_path(benchdir, true)
    add_cpackage_path(benchdir, true)
end

function emit_per_core_measurements(name, num_cores, tbl, tbl_len)
    io.stdout:write(string.format('"%s": [', name))

    for BM_core = 1, num_cores, 1 do
        io.stdout:write("[")
        for BM_i = 1, tbl_len, 1 do
            io.stdout:write(tbl[BM_core][BM_i])
            if BM_i < tbl_len then
                io.stdout:write(", ")
            end
        end
        io.stdout:write("]")
        if BM_core < num_cores then
            io.stdout:write(", ")
        end
    end
    io.stdout:write("]")
end

ffi.cdef[[
    void krun_init(void);
    void krun_done(void);
    void krun_measure(int);
    int krun_get_num_cores(void);
    double krun_get_wallclock(int);
    double krun_get_core_cycles_double(int, int);
    double krun_get_aperf_double(int, int);
    double krun_get_mperf_double(int, int);
]]
local libkruntime = ffi.load("kruntime")

local krun_init = libkruntime.krun_init
local krun_done = libkruntime.krun_done
local krun_measure = libkruntime.krun_measure
local krun_get_num_cores = libkruntime.krun_get_num_cores
local krun_get_wallclock = libkruntime.krun_get_wallclock
local krun_get_core_cycles_double = libkruntime.krun_get_core_cycles_double
local krun_get_aperf_double = libkruntime.krun_get_aperf_double
local krun_get_mperf_double = libkruntime.krun_get_mperf_double

if BM_instrument then
    jitlog = require("jitlog")
    jitlog.start()
    jitlog.addmarker("LOAD(START)")
end

dofile(BM_benchmark)

if BM_instrument then
    jitlog.addmarker("LOAD(END)")
end

krun_init()
local BM_num_cores = krun_get_num_cores()

-- Pre-allocate and fill results tables
local BM_wallclock_times = {}
for BM_i = 1, BM_iters, 1 do
    BM_wallclock_times[BM_i] = 0
end

local BM_cycle_counts = {}
for BM_core = 1, BM_num_cores, 1 do
    BM_cycle_counts[BM_core] = {}
    for BM_i = 1, BM_iters, 1 do
        BM_cycle_counts[BM_core][BM_i] = 0
    end
end

local BM_aperf_counts = {}
for BM_core = 1, BM_num_cores, 1 do
    BM_aperf_counts[BM_core] = {}
    for BM_i = 1, BM_iters, 1 do
        BM_aperf_counts[BM_core][BM_i] = 0
    end
end

local BM_mperf_counts = {}
for BM_core = 1, BM_num_cores, 1 do
    BM_mperf_counts[BM_core] = {}
    for BM_i = 1, BM_iters, 1 do
        BM_mperf_counts[BM_core][BM_i] = 0
    end
end

local stats_start, stats_stop

if BM_instrument then
    jitlog.addmarker("BENCH(START)")
end

-- Main loop
for BM_i = 1, BM_iters, 1 do
    if BM_debug then
        io.stderr:write(string.format("[iterations_runner.lua] iteration %d/%d\n", BM_i, BM_iters))
    end
    if BM_instrument then
        jitlog.addmarker("BEGIN")
    end

    -- Start timed section
    krun_measure(0);
    run_iter(BM_param)
    krun_measure(1);
    -- End timed section

    if BM_instrument then
        jitlog.addmarker("END")
    end

    -- Compute deltas
    BM_wallclock_times[BM_i] = krun_get_wallclock(1) - krun_get_wallclock(0);

    for BM_core = 1, BM_num_cores, 1 do
        BM_cycle_counts[BM_core][BM_i] =
            krun_get_core_cycles_double(1, BM_core - 1) -
            krun_get_core_cycles_double(0, BM_core - 1)
        BM_aperf_counts[BM_core][BM_i] =
             krun_get_aperf_double(1, BM_core - 1) -
             krun_get_aperf_double(0, BM_core - 1)
        BM_mperf_counts[BM_core][BM_i] =
            krun_get_mperf_double(1, BM_core - 1) -
            krun_get_mperf_double(0, BM_core - 1)
    end
end

if BM_instrument then
    jitlog.addmarker("BENCH(END)")
    jitlog.save(string.format("%s/%s_%d.jlog", BM_instdatadir, BM_key, BM_pexecidx):gsub(":", "_"))
end

krun_done()

io.stdout:write("{")

io.stdout:write('"wallclock_times": [')
for BM_i = 1, BM_iters, 1 do
    io.stdout:write(BM_wallclock_times[BM_i])
    if BM_i < BM_iters then
        io.stdout:write(", ")
    end
end
io.stdout:write("], ")

emit_per_core_measurements("core_cycle_counts", BM_num_cores, BM_cycle_counts, BM_iters)
io.stdout:write(", ")
emit_per_core_measurements("aperf_counts", BM_num_cores, BM_aperf_counts, BM_iters)
io.stdout:write(", ")
emit_per_core_measurements("mperf_counts", BM_num_cores, BM_mperf_counts, BM_iters)

io.stdout:write("}\n")
io.stdout:flush()
