package.path = package.path ..";lualibs/?/init.lua;lualibs/?.lua;lualibs/?/?.lua"

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

function runbench(name, count)
    dofile("benchmarks/"..name.."/bench.lua")

    if not run_iter then
        error("Missing benchmark function for '"..name.."'")
    end

    local start = os.clock()
    run_iter(count or 1)
    print(name.." took", os.clock() - start)  

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

