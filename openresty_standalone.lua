local BM_instrument = os.getenv("JITLOG")
local BM_i
local BM_iters = os.getenv("BM_iters") and tonumber(os.getenv("BM_iters"))
local BM_benchmark = os.getenv("BM_benchmark") or "benchmarks/capnproto_encode/bench.lua"

local jitlog
if BM_instrument then
    jitlog = require("jitlog")
    jitlog.start()
end

local ffi = require"ffi"
local jit = require"jit"
local opt = require"jit.opt"
local format = string.format

local dbgout = io.open("debug.txt", "w")
io.debug = dbgout

-- @param addtostart Pass true to insert the search path at the start of the module search path list.
function add_luapackage_path(basepath, addtostart)
    assert(type(basepath) == "string" and #basepath > 0)
 
    if addtostart then
        package.path = string.format("%s/?.lua;%s/?/init.lua;%s", basepath, basepath, package.path)
    else
        package.path = string.format("%s;%s/?.lua;%s/?/init.lua", package.path, basepath, basepath)
    end
end
 
-- @param addtostart Pass true to insert the search path at the start of the module search path list.
function add_cpackage_path(basepath, addtostart)
    assert(type(basepath) == "string" and #basepath > 0)
 
    if addtostart then
        package.cpath = string.format("%s/?.so;%s/?/?.so;%s", basepath, basepath, package.cpath)
    else
        package.cpath = string.format("%s;%s/?.so;%s/?/?.so", package.cpath, basepath, basepath)
    end
end

add_luapackage_path("lualibs")
add_luapackage_path("builds/openresty")
--local jitp = require"jit.p"


if BM_instrument then
    jitlog = require("jitlog")
    jitlog.start()
    jitlog.addmarker("LOAD(START)")
end
 
print("Loading Resty")
-- Make sure were using resty's faster regex functions
local regex = require 'resty.core.regex'

print("Loading Benchmark")

dofile(BM_benchmark)

local run_iter = run_iter
local do_request, save_results
local BM_param = 1
BM_i = 1
BM_iters = BM_iters or 3

local function error_callback(err)
    local trace = debug.traceback(err, 1)
    io.stderr:write(trace, "\n")
    io.stderr:flush()
    os.exit(1)
end

local function request_wrapper()
    local ok, err = xpcall(do_request, error_callback)
    if not ok then
        os.exit(1)
    end
end

-- Main loop
function do_request()
    jit.prngstate(2654435761)
    math_randomseed(2654435761) 
    
    if BM_debug then
        io.stderr:write(format("iteration %d\n", BM_i))
    end
    io.debug:write(format("iteration %d\n", BM_i))
    
    if BM_instrument then
        jitlog.addmarker("BEGIN")
    end

    -- Start timed section
    run_iter(BM_param)
    -- End timed section

    if BM_instrument then
        jitlog.addmarker("END")
    end

    BM_i =  BM_i + 1
    if BM_i <= BM_iters then
        ngx.timer.at(0, request_wrapper)
    else
        local ok, err = xpcall(save_results, error_callback)
        os.exit(ok and 0 or 1)
    end
end

function save_results()
    io.stderr:write(format("Saving results iters=%d\n", BM_iters))

    if BM_instrument then
        jitlog.addmarker("BENCH(END)")
        jitlog.save("test.jlog")
    end
end

ngx.timer.at(0, request_wrapper)
