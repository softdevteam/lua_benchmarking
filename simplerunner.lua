local jit, jit_util, jit_dump, jitstats, json
local match, gmatch = string.match, string.gmatch
local benchinfo, scaling, oskind, subprocess

local function add_package_path(basepath)
    local ext
    if oskind == "Windows" then
        ext = "dll"
    else
        ext = "so"
    end

    package.path = string.format("%s/?/init.lua;%s/?.lua;%s/?/?.lua;%s", basepath, basepath, basepath, package.path)
    package.cpath = string.format("%s/?.%s;%s/?/?.%s;%s", basepath, ext, basepath, ext, package.cpath)
end

local devnull
if package.config:sub(1,1) == "\\" then
  -- Use nul on windows https://stackoverflow.com/questions/313111/is-there-a-dev-null-on-windows
  devnull = io.open("nul")
else
  devnull = io.open("/dev/null")
end

function io.write_devnull(...)
  return (devnull:write(...))
end

ffi = require("ffi")

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

    local result, err = file:read("*all")
    file:close()
    if not result then
        return false, err
    end
    return true, result
end

local function writefile(path, data)
    local file, msg = io.open(path, "w")
    if not file then
        return false, msg
    end

    local success, msg = file:write(data)
    file:close()
    if not success then
        return false, msg
    end
    return true
end

local function loadjson(path)
    local success, text, data

    success, text = readfile(path)
    if not success then
        return false, "Failed to load '"..path.."' "..text
    end

    success, data = pcall(json.decode, text)
    if not success then
        return false, "Failed to parse '"..path.."' "..data
    end
    return true, data
end

local runner = {
    jsonlib = "json_nojit"
}
local loaded = false
local startimer, stoptimer

function runner.init()
    if loaded then
        return true
    end
    loaded = true
    oskind = runner.detectos()
    add_package_path("lualibs")
    add_package_path("rocks/modules")
    runner.loadbenchinfo()
    
    success, jit = pcall(require, "jit")   
    local hasjit = success and pcall(jit.on)

    if not hasjit then
        startimer, stoptimer = runner.maketimer(os.clock)
        jit = nil
        return true
    end
    
    jit.off(runner.jitfunc)
    jit.off(runner.runbench)

    -- Raptor jit removed jit.util so only try to use if its available
    success, jit_util = pcall(require, "jit.util")

    if success then
        if not pcall(require, "jit.vmdef") then
            add_package_path("luajit_repo/src")
        end
    else
        jit_util = nil
    end

    local timeloaded, timelib = pcall(require, "time")
    -- jit.util is needed so we can check the bytecode has changed for the timer functions being pre JIT'ed.
    if timeloaded and jit_util then
        startimer, stoptimer = runner.maketimer(timelib.clock)
        -- Force the timer functions to get JIT'ed so they don't go through the slow
        -- ffi path in the interpreter thats data driven.
        runner.jitfunc(startimer)
        runner.jitfunc(stoptimer)
    else
        startimer, stoptimer = runner.maketimer(os.clock)
        -- os.clock can't be jit'ed so try to keep timer a bit more stable by 
        -- stopping the time randomly being inflated by trace creation in them.
        jit.off(startimer)
        jit.off(stoptimer)
    end
    
    return true
end

function runner.detectos()
    local success, ffi = pcall(require, "ffi")  
    if success then
        return ffi.os
    end

    -- Fallback to the file extension of binary Lua modules
    if string.find(package.cpath, "%.dll") then
        return "Windows"
    else
        return "Linux"
    end
end

function runner.maketimer(clock)
    local start_ticks

    local function startimer()
        start_ticks = clock()
        return
    end
    
    local function stoptimer()
        local stop_ticks = clock()
        return stop_ticks - start_ticks
    end
    
    return startimer, stoptimer
end

function runner.loadbenchinfo()
    if benchinfo then
        return
    end

    if not json then
        json = require(runner.jsonlib)
    end

    local success
    success, info = loadjson("benchinfo.json")
    
    if not success then
        error("Error while geting benchmark info: "..info)
    end

    runner.setbenchinfo(info)
end

function runner.setbenchinfo(info)
    assert(type(info.scaling) == "table", "benchinfo missing benchmark scaling values")
    assert(type(info.info) == "table", "benchinfo missing benchmark info table")

    benchinfo = info
    scaling = info.scaling
    benchlist = {}
    for k, _ in pairs(info.scaling) do
        table.insert(benchlist, k)
    end
end

function runner.readbenchinfo()
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
    return true, info
end

function runner.load_jitstats()
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

local jprof_mode, jprof_output
function runner.start_profiler()
    jit_profile.start(jprof_mode, jprof_output)
end

function runner.jitfunc(f, n)
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

function runner.loadbench(name)
    assert(not run_iter, "another benchmark is still loaded")
    -- Add the benchmark's directory to the module search path so it can correctly load any extra modules from there
    add_package_path("benchmarks/"..name, true)
    add_package_path("benchmarks/"..name.."/rocks/modules", true)
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

function runner.runbench(name, count, scaling)
    scaling = scaling or 1
    runner.loadbench(name)

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
    if jit_profile then
        runner.start_profiler()
    end
    
    io.write("Running "..name..": ")
    io.flush()

    -- Reset all the hot counters to their starting values right before we run the benchmark
    if jit then
        -- lj_dispatch_update triggers a hotcount reset when the JIT is turned off and back on again.
        jit.off()
        jit.on()
    end

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
    
    if jit_profile then
        jit_profile.stop()
    end

    if jitstats then
        jitstats.stop()
    end

    --Clear the benchmark function so we know we're not running this benchmark again when we load another benchmark.
    _G.run_iter = nil
    return times, jstats
end

local function permute(tab)
    local count = #tab
    math.randomseed(os.time())
    for i = count, 1, -1 do
        local j = math.random(i)
        local temp = tab[i]
        tab[i] = tab[j]
        tab[j] = temp
    end
end

function runner.run_benchmark_list(benchmarks, count, options)
    assert(#benchmarks ~= 0, "Empty benchmark list")
    options = options or {}

    if not options.norandomize then
        permute(benchmarks)
    end

    local package_path = package.path
    local package_cpath = package.cpath
    local results = {}

    for i, name in ipairs(benchmarks) do
        local scaling = options.scaling or scaling[name] or 1
        local stats

        if options.inprocess then
            local times, jstats = runner.runbench(name, count, scaling)
            stats = runner.calculate_stats(times)
            stats.times = times
            print("  " .. runner.fmtstats(stats))
            if jitstats then
                stats.jitstats = jstats
                jitstats.print()
            end
        else
           runner.runbench_outprocess(name, count, scaling, options)
        end

        if options.inprocess then
            -- Try to clean away the current benchmark and its data, so the behaviour of the GC is more predictable for the next benchmark.
            collectgarbage("collect")

            -- Restore the Lua module search path since we set it to the directory of the benchmark when we load it.
            package.path = package_path
            package.cpath = package_cpath
        end
    end
end

function runner.runbench_outprocess(name, count, scaling, options)
    local lc = require("luachild")
    local read, write = lc.pipe()

    local p = lc.spawn{
        command = arg[-1],
        args = {
            arg[0], "--childprocess", name, count, scaling, unpack(arg, 1) 
        },
    }
 
    local cmdret = p:wait()
    if cmdret ~= 0 then
        error("Out of process benchmark execution failed status = "..cmdret)
    end
end

function runner.subprocess_run(benchmark, count, scaling, parent_options)
    scaling = tonumber(scaling)
    count = tonumber(count)
    subprocess = true
    runner.processoptions(parent_options)

    local times, jstats = runner.runbench(benchmark, count, scaling)
    local stats = runner.calculate_stats(times)
    print("  " .. runner.fmtstats(stats))
    if jitstats then
        jitstats.print()
    end

    os.exit(0)
end

function runner.calculate_stats(times)
    local stats = {}
    local count = #times

    local min, max, total = 10000, 0, 0
    for i = 1, count do
        local time = times[i]
        total = total + time
        min = math.min(time, min)
        max = math.max(time, max)
    end

    stats.min = min
    stats.max = max
    stats.mean = total / count

    local variance = 0
    for i = 1, count do
        local diff = times[i] - stats.mean
        variance = variance + diff*diff
    end

    stats.stddev = math.sqrt(variance / count)
    -- 95% confidence interval
    stats.cinterval = 1.960 * stats.stddev/math.sqrt(count)
    return stats
end

function runner.fmtstats(stats)
    return string.format("Mean: %f +/- %f, min %f, max %f", stats.mean, stats.cinterval, stats.min, stats.max)
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

  -h, --help            Display this help text.
  -b, --bench name      Name of benchmark to run.
  -c, --count num       Number of times to run the benchmarks.
  -s, --scaling num     Number of interations to run the benchmarks.
  -e, --exclude name    Exclude a benchmark from being run.

  --jitstats           Collect and print jit statisitcs from running the benchmarks.
  --jdump options      Run LuaJIT's jit.dump module with the specifed options.
  --jprof options [, ourout file] Run the benchmaks under LuaJIT's sample based profiler
  --benchloadjitstats  Also collect jitstats when loading a benchmark
  --nogc               Run the benchmarks with the GC disabled.
  --norandomize        Don't randomize the run order of the benchmarks.
  --inprocess          Run benchmarks in the main process instead of executing them in a child process.
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
function opt_map.inprocess() g_opt.inprocess = true end
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

function opt_map.jprof(args)
    local options = optparam(args)
    local outfile = options:find(",")

    if outfile then
        g_opt.jprof_output = options:sub(outfile + 1)
        g_opt.jprof = options:sub(1, outfile-1)
    else
        g_opt.jprof = options
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

-- Parse arguments.
function runner.parse_commandline(args)
    -- Default options.
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

    -- Check for a trailing list of benchmark names.
    local nargs = #args - args.argn + 1
    if (nargs > 0) then
        for i = args.argn, #args do
            table.insert(g_opt.benchmarks, args[i])
        end
    end

    g_opt.count = g_opt.count or 30

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
    
    return benchmarks, g_opt
end

function runner.processoptions(options)
    -- Don't pointlessly run these options in the main process if we're running benchmarks in a child process
    if not subprocess and not options.inprocess then
        return
    end

    if options.jitstats then
        runner.load_jitstats()
    end

    if options.nogc then
        print("Garbage collector disabled")
        -- Make sure the GC is not in the middle of phase before we switch it off
        collectgarbage("collect")
        collectgarbage("stop")
    end

    if options.jdump then
        jit_dump = require("jit.dump")
        jit_dump.on(options.jdump, options.jdump_output)
    end
    
    if options.jprof then
        jit_profile = require("jit.p")
        jprof_mode = options.jprof
    end
end

local hasffi = pcall(require, "ffi")

runner.benchfilters = {
    function(name)
        local info = benchinfo.info[name]
        
        if not hasffi and info and info.ffirequired then
            return true, "No ffi module"
        end
        return false
    end,
}

local function check_filters(name) 
    for _, filter in ipairs(runner.benchfilters) do
        local filtered, reason = filter(name)
        if filtered then
            print("Skipping ".. name, reason)
            return true
        end
    end
    return false
end

function runner.filter_benchmarks(benchmarks)
    return table_filter(benchmarks, check_filters)
end

runner.init()

if arg[1] == "--childprocess" then
    local benchmark, count, scaling = unpack(arg, 2)
    local _, options = runner.parse_commandline({unpack(arg, 5)})
    runner.subprocess_run(benchmark, count, scaling, options)
else
    local benchmarks, options = runner.parse_commandline(arg)
    runner.processoptions(options)

    if not arg[1] then
        benchmarks = benchlist
    end
    benchmarks = runner.filter_benchmarks(benchmarks)
    runner.run_benchmark_list(benchmarks, options.count, options)
end
