package.path = package.path ..";lualibs/?/init.lua;lualibs/?.lua;lualibs/?/?.lua"

local json = require("json")
local fun = require 'fun'

local file = arg[1] or "luajit_results.json"

local luafile = io.open("luajit_results4.json", "rb")
local text = luafile:read("*all")
luafile:close()

local stats = json.decode(text)

if not stats.wallclock_times then
    error("no wallclock_times")
end

local filter = {
    NoJIT = true
}

local benches = {}
local benchlookup = {}

for name,times_lists in pairs(stats.wallclock_times) do

    local bench = name:match("^[^:]+")
    local VM = name:sub(#bench+1):match("[^:]+")
    
    local benchstatsm
    
    local vm_bench = {
        name = VM,
        times = times, 
    }
    
    if not benchlookup[bench] then
        local benchstats = {
            name = bench,
            vms = {
                [VM] = vm_bench 
            },
        }
        benchlookup[bench] = benchstats
        benches[#benches+1] = benchstats
    end 

    benchlookup[bench].vms[VM] = vm_bench 
    
    local avgs = {}
    vm_bench.avgs = avgs
    
    local alltimes = fun.iter({})
    

    for i,times in fun.iter(times_lists):filter(function(list) return #list > 0 end) do    
    
        local avg = fun.iter(times):sum()/#times
        avgs[#avgs + 1] = avg
        
        if not alltimes then
            alltimes = fun.iter(times)
        else
            alltimes = alltimes:chain(times)
        end
    end

    if not alltimes:is_null() then
        vm_bench.min = alltimes:min()
        vm_bench.max = alltimes:max()
        vm_bench.avg = fun.iter(avgs):sum()/#avgs
    end
end

table.sort(benches, function(b1, b2) return b1.name < b2.name end)

function print_slowdown(vm1, vm2)

    assert(vm1 and vm2)

    for i, bench in ipairs(benches) do
        local t1 = bench.vms[vm1] and bench.vms[vm1].avg
        local t2 = bench.vms[vm2] and bench.vms[vm2].avg

        if t1 and t2 then
            print(("%-18s %f, %f"):format(bench.name, t1-t2, t1/t2))
        end
    end
end

function getstats(vm, default)
    
    local stats = {}
    
    for i,bench in ipairs(benches) do
        local avg = bench.vms[vm] and bench.vms[vm].avg
        stats[#stats+1] = avg or default
    end
    
    return stats
end

function print_vmstats(vm)
    
    for i,bench in ipairs(benches) do
        local avg = bench.vms[vm] and bench.vms[vm].avg
        if avg then
            print(("%-18s %f"):format(bench.name, avg))
        end
    end
end

local n_pexecs, n_iters = 10, 1500

function calctime(bench_time, iters)
    return n_pexecs * (iters or n_iters) * bench_time
end

local nojit_stats = getstats("NoJIT", 0)
assert(#nojit_stats > 0)
local nojit = calctime(fun.iter(nojit_stats):sum(), 100)
local jit = calctime(fun.iter(getstats("Normal", 0)):sum())

local times = {
    GC64 = 0,
    DualNum = 0,
    Normal = 0,
    NoJIT = 0,
}

for name, _ in pairs(times) do
    local time = calctime(fun.iter(getstats(name, 0)):sum(), (name == "NoJIT" and 100) or nil)
    times[name] = time
    print(("%-18s %f"):format(name, time/ 60.0 / 60.0))
end

print(("%-18s %f"):format("Total", (times.GC64 + times.DualNum + times.Normal + times.NoJIT)  / 60.0 / 60.0))


--In [11]: secs * n_vms * n_benchs * n_pexecs * n_iters / 60.0 / 60.0 / 24.0

--print_slowdown("NoJIT", "Normal")

--print_vmstats("NoJIT")

