local jit, jit_util, jit_dump, jitstats
local match, gmatch = string.match, string.gmatch

local function add_package_path(basepath)
    package.path = string.format("%s/?/init.lua;%s/?.lua;%s/?/?.lua;%s", basepath, basepath, basepath, package.path)
end
add_package_path("lualibs")

local json = require("json_nojit")

local success
success, jit = pcall(require, "jit")

if not success or not pcall(jit.on) then
    jit = nil
else
    success, jit_util = pcall(require, "jit.util")

    if not success then
        jit_util = nil
    end
end

if not pcall(require, "jit.vmdef") then
    add_package_path("luajit_repo/src")
end

local function load_jitstats()
    local success
    success, jitstats = pcall(require, "jitstats")

    if not success then
      print("Warning: Failed to load jitstats")
      -- print the error message as well
      print(" ", jitstats)
      jitstats = nil
    else
      print("Loaded jitstats")
      require("table.new")
    end

    return success
end

local function table_filter(t, f)
    local result = {}
    for _, v in ipairs(t) do
        if not f(v) then
            table.insert(result, v)
        end
    end
    return result
end

local function readfile(path)
    local file, msg = io.open(path, "rb")
    if not file then
        return false, msg
    end

    local success, text = pcall(file.read, file, "*all")
    if not success then
        return false, text
    end

    pcall(file.close, file)
    return true, text
end

local function readbenchinfo()
    local success, text, info

    success, text = readfile("benchinfo.json")
    if not success then
        print("Warning: Failed to load benchinfo.json ")
        print("", text)
        return false
    end

    success, info = pcall(json.decode, text)
    if not success then
        print("Warning: Failed to parse benchinfo.json")
        print("", info)
        return false
    end
    benchinfo = info
    return true
end

local function jitfunc(f, n)
    assert(jit_util, "cannot prejit with no jit.util")
    n = n or 200
    local startbc = bit.band(jit_util.funcbc(f, 0), 0xff)
    for i = 1, n do
        f()
    end
    local stopbc = bit.band(jit_util.funcbc(f, 0), 0xff)
    -- Make sure the bc op shifts from FUNCF to JFUNCF
    assert(stopbc == startbc + 2, "failed to prejit method")
end

if jit then
    jit.off(jitfunc)
end

local start_ticks
local clock

local function startimer()
    start_ticks = clock()
    return
end

local function stoptimer()
    local stop_ticks = clock()
    return stop_ticks - start_ticks
end

local timeloaded, time = pcall(require, "time")
if timeloaded then
    clock = time.clock

    if jit_util then
        -- Force the timer functions to get JIT'ed so they don't go through the slow ffi path in the interpreter thats data driven
        jitfunc(startimer)
        jitfunc(stoptimer)
    end
else
    -- Fallback to os.clock which uses the libc 'clock' function. CLOCKS_PER_SEC is normally 1Mhz on Linux but on windows it was 1khz.
    clock = os.clock
end

local function loadbench(name)
    assert(not run_iter, "another benchmark is still loaded")
    -- Add the benchmark's directory to the module search path so it can correctly load any extra modules from there
    add_package_path("benchmarks/"..name)
    
    if loading_jitstats then
        jitstats.start()
    end
    dofile("benchmarks/"..name.."/bench.lua")

    if not run_iter then
        error("Missing benchmark function for '"..name.."'")
    end

    if loading_jitstats and jitstats.stats.starts > 0 then
        jitstats.print("Traces created while loading benchmark")
        jitstats.reset()
    end
end

function runbench(name, count, scaling)
    scaling = scaling or 1
    loadbench(name)

    local jstats, times
    local start, stop = {}, {}

    if jitstats then
        jitstats.start()
        jitstats.reset()
        times = table.new(count, 0)
        jstats = table.new(count, 0)
    else
       times = {}
    end
    io.write("Running "..name..": ")
    io.flush()

    local run_iter = _G.run_iter
    for i = 1, count do
        io.write(".")
        io.flush()
        if jitstats then
            jitstats.getsnapshot(start)
        end
        startimer()
        run_iter(scaling)
        local ticks = stoptimer()
        table.insert(times, ticks)
        if jitstats then
            jitstats.getsnapshot(stop)
            local iter_jstats = jitstats.diffsnapshots(start, stop)
            jstats[i] = iter_jstats
            if iter_jstats.starts ~= 0 then
                io.write(iter_jstats.starts)
            end
        end
    end
    -- Force a new line after our line of dots
    io.write("\n")
    io.flush()

    if jitstats then
        jitstats.stop()
    end

    --Clear the benchmark function so we know we're not running this benchmark again when we load another benchmark.
    _G.run_iter = nil
    return times, jstats
end

if jit then
    jit.off(runbench)
end

function permute(tab)
    local count = #tab
    math.randomseed(os.time())
    for i = count, 1, -1 do
        local j = math.random(i)
        local temp = tab[i]
        tab[i] = tab[j]
        tab[j] = temp
    end
end

function run_benchmark_list(benchmarks, count, options)
    assert(#benchmarks ~= 0, "Empty benchmark list")
    options = options or {}

    if not options.norandomize then
        permute(benchmarks)
    end

    local package_path = package.path
    
    for i, name in ipairs(benchmarks) do
        local times = runbench(name, count, options.scaling or scaling[name])
        local stats = calculate_stats(times)
        print(string.format("  Mean: %f +/- %f, min %f, max %f", stats.mean, stats.cinterval, stats.min, stats.max))
        if jitstats then
            jitstats.print()
        end
        -- Try to clean away the current benchmark and its data, so the behaviour of the GC is more predictable for the next benchmark.
        collectgarbage("collect")

        -- Restore the Lua module search path since we set it to the directory of the benchmark when we load it.
        package.path = package_path
    end
end

function calculate_stats(times, start)
    local stats = {}
    start = start or 1
    local count = #times - start-1

    local min, max, total = 10000, 0, 0
    for i = start, #times do
        local time = times[i]
        total = total + time
        min = math.min(time, min)
        max = math.max(time, max)
    end

    stats.min = min
    stats.max = max
    stats.mean = total / count

    local variance = 0
    for i = start, count do
        local diff = times[i] - stats.mean
        variance = variance + diff*diff
    end

    stats.stddev = math.sqrt(variance / count)
    -- 95% confidence interval
    stats.cinterval = 1.960 * stats.stddev/math.sqrt(count)
    return stats
end

-- Command line parsing system mostly copied from dynasm.lua in LuaJIT
local opt_map = {}
local g_opt = {}

-- Print error and exit with error status.
local function opterror(...)
    io.stderr:write("simplerunner.lua: ERROR: ", ...)
    io.stderr:write("\n")
    os.exit(1)
end

-- Get option parameter.
local function optparam(args, argtype)
    local argn = args.argn
    local p = args[argn]
    if not p or p:find("-") == 1 then
        opterror("missing parameter for option `", opt_current, "'.")
    end
    args.argn = argn + 1

    if argtype == "number" then
        local value = tonumber(p)
        if not value then
            opterror("expected number for parameter option `", opt_current, "'.")
        end
        p = value
    end

    return p
end

-- Short aliases for long options.
local opt_alias = {
    h = "help", ["?"] = "help",
    b = "bench", c = "count", s = "scaling",
    e = "exclude",
}

-- Print help text.
function opt_map.help()
    io.stdout:write[[

Usage: simplerunner [OPTION] [<benchmark>] [<count>]

  -h, --help           Display this help text.
  -b, --bench name     Name of benchmark to run.
  -c, --count num      Number of times to run the benchmarks.
  -s, --scaling num    Number of interations to run the benchmarks.
  -e, --exclude name   Exclude a benchmark from being run.

  --jitstats           Collect and print jit statisitcs from running the benchmarks.
  --jdump options      Run LuaJIT's jit.dump module with the specifed options.
  --benchloadjitstats  Also collect jitstats when loading a benchmark
  --nogc               Run the benchmarks with the GC disabled.
  --norandomize        Don't randomize the run order of the benchmarks.
]]
    os.exit(0)
end

-- Misc. options.
function opt_map.count(args) g_opt.count = optparam(args, "number") end
function opt_map.bench(args) table.insert(g_opt.benchmarks, optparam(args)) end
function opt_map.exclude(args) table.insert(g_opt.excludes, optparam(args)) end
function opt_map.scaling(args) g_opt.scaling = optparam(args, "number") end
function opt_map.jitstats() g_opt.jitstats = true end
function opt_map.benchloadjitstats() loading_jitstats = true end
function opt_map.nogc() g_opt.nogc = true end
function opt_map.norandomize() g_opt.norandomize = true end
function opt_map.jdump(args)
    local options = optparam(args)
    local outfile = options:find(",")

    if outfile then
        g_opt.jdump_output = options:sub(outfile + 1)
        g_opt.jdump = options:sub(1, outfile-1)
    else
        g_opt.jdump = options
    end
end

------------------------------------------------------------------------------

-- Parse single option.
local function parseopt(opt, args)
    opt_current = #opt == 1 and "-"..opt or "--"..opt
    local f = opt_map[opt] or opt_map[opt_alias[opt]]
    if not f then
        opterror("unrecognized option `", opt_current, "'. Try `--help'.\n")
    end
    f(args)
end

local hasffi = pcall(require, "ffi")

local benchfilters = {
    function(name)
        local info = benchinfo and benchinfo.info[name]
        
        if not hasffi and info and info.ffirequired then
            return true, "No ffi module"
        end
        return false
    end,
}

local benchlist

-- Parse arguments.
local function parseargs(args)
    -- Default options.
    g_opt.count = 30
    g_opt.benchmarks = {}
    g_opt.excludes = {}
    -- Process all option arguments.
    args.argn = 1
    repeat
        local a = args[args.argn]
        if not a then break end
        local lopt, opt = match(a, "^%-(%-?)(.+)")
        if not opt then break end
        args.argn = args.argn + 1
        if lopt == "" then
            -- Loop through short options.
            for o in gmatch(opt, ".") do parseopt(o, args) end
        else
            -- Long option.
            parseopt(opt, args)
        end
    until false

    if g_opt.jitstats then
        load_jitstats()
    end

    -- Check for a trailing list of benchmark names.
    local nargs = #args - args.argn + 1
    if (nargs > 0) then
        for i = args.argn, #args do
            table.insert(g_opt.benchmarks, args[i])
        end
    end

    if g_opt.nogc then
        print("Garbage collector disabled")
        -- Make sure the GC is not in the middle of phase before we switch it off
        collectgarbage("collect")
        collectgarbage("stop")
    end

    if g_opt.jdump then
        jit_dump = require("jit.dump")
        jit_dump.on(g_opt.jdump, g_opt.jdump_output)
    end

    local benchmarks

    if #g_opt.benchmarks > 0 then
        benchmarks = g_opt.benchmarks
    else
        benchmarks = benchlist
    end

    benchmarks = table_filter(benchmarks, 
        function(name) 
            for _, bench in ipairs(g_opt.excludes) do
                if bench == name then
                    return true
                end
            end
            return false
        end
    )
    
    benchmarks = table_filter(benchmarks, 
        function(name) 
            for _, filter in ipairs(benchfilters) do
                local filtered, reason = filter(name)
                if filtered then
                    print("Skipping ".. name, reason)
                    return true
                end
            end
            return false
        end
    )

    run_benchmark_list(benchmarks, g_opt.count, g_opt)
end

if not readbenchinfo() then
    scaling = {}
else
    scaling = benchinfo.scaling
    benchlist = {}
    for k, _ in pairs(benchinfo.scaling) do
        table.insert(benchlist, k)
    end
end

if arg[1] then
    parseargs(arg)
else
    run_benchmark_list(benchlist, 30)
end
