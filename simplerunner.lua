package.path = package.path ..";lualibs/?/init.lua;lualibs/?.lua;lualibs/?/?.lua"
local json = require("json_nojit")
require("table.new")

if not pcall(require, "jit.vmdef") then
    package.path = package.path ..";luajit_repo/src/?/init.lua;luajit_repo/src/?.lua;luajit_repo/src/?/?.lua"
end

local success, jitstats = pcall(require, "jitstats")

if not success then
    print("Warning: Failed to load jitstats")
    jitstats = nil
end

local benchlist = {
    "binarytrees",
    "nbody",
    "fasta",
    "richards",
    "spectralnorm",
    "fannkuch_redux",
    "md5",
    "series",
    "luacheck_parser",
    "luacheck",
    "capnproto_encode",
    "capnproto_decode",
    "jsonlua_encode",
    "jsonlua_decode",
    "luafun",
}

function runbench_jitstats(name, count)
    if count == 1 then
        local start = os.clock()
        run_iter(count)
        print(name.." took", os.clock() - start)
        jitstats.print()
        jitstats.reset()
    else
        local stats = table.new(count, 0)
        local start, stop = {}, {}

        for i = 1, count do
            jitstats.getsnapshot(start)
            run_iter(1)
            jitstats.getsnapshot(stop)
            stats[i] = jitstats.diffsnapshots(start, stop)
        end

        jitstats.print()
        --print(json.encode(jitstats.getsnapshot()))
    end
end

function runbench(name, count)
    if jitstats then
        jitstats.start()
    end
    dofile("benchmarks/"..name.."/bench.lua")

    if not run_iter then
        error("Missing benchmark function for '"..name.."'")
    end

    count = count or 1
    
    if jitstats then 
        if jitstats.stats.starts > 0 then
            print("Traces created while loading benchmark")
            jitstats.print()
            jitstats.reset()
        end
        
        runbench_jitstats(name, count)
    else
        local start = os.clock()
        run_iter(count)
        print(name.." took", os.clock() - start)
    end

    --Clear the benchmark function so we know we're not running this benchmark again when we load another benchmark.
    run_iter = nil
end

function run_all_benchmarks()
    assert(#benchlist ~= 0)

    for i,name in ipairs(benchlist) do
        runbench(name)
    end
end

function run_single_benchmarks(benchname, count)
    assert(#benchlist ~= 0)

    benchname = benchname:lower()
    
    for i,name in ipairs(benchlist) do
        if name == benchname then
            runbench(name, count)
            return
        end
    end
    
    error("No benchmark named '"..benchname.."'")
end

if arg[1] then
    run_single_benchmarks(arg[1], arg[2])
else
    run_all_benchmarks()
end

