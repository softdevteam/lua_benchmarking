local fun = require 'fun'
local jitlog_capnp = require "jitlog_capnp"
local bor = bit.bor
local lshift = bit.lshift
-- Enable table.new function
require("table.new")

local luafile = io.open("benchdata/jitlog.bin", "rb")
local text = luafile:read("*all")
luafile:close()

local jitlog = jitlog_capnp.JITLog.parse(text)
local traces = jitlog.traces
local functions = jitlog.functions

local aborted, completed, roots

local stats = {
    top_aborted = false,
    top_stitched = false,
}

function init()
    for i, func in ipairs(functions) do
        func.starts = 0
        func.stops = 0
        func.aborts = 0
        func.sideexits = 0
        func.started_aborts = 0
        func.stitch_roots = 0
    end

    for i, trace in ipairs(traces) do
        trace.startloc = bor(lshift(trace.startfunc, 16), trace.startpc)

        local lastlua

        for i = #trace.funcs, 1, -1 do
            local func = functions[trace.funcs[i]]

            if not func.cfunc then
                lastlua = func
                break
            end
        end

        trace.lastlua = lastlua

        if lastlua then
            trace.stoploc = bor(lshift(lastlua.id, 16), trace.stoppc)
        end
    end

end

function run_iter(n)
    for n=1, n do
        init()

        local aborted = fun.iter(traces):filter(function(event) return event.abort end):totable()

        local completed = fun.iter(traces):filter(function(event) return not event.abort end):totable()

        assert((#aborted + #completed) == #traces)

        roots = fun.iter(completed):
                    filter(function(event) return event.parentid == 0 end):
                    totable()

        for i, trace in ipairs(completed) do
            local startfunc = functions[trace.startfunc]
            startfunc.starts = startfunc.starts + 1

            -- Function header root trace
            if trace.parentid == 0 and trace.startpc == 0 and not trace.stitched then
                assert(trace.exitnum == 0)
                startfunc.root_trace = trace
            end

            if trace.parentid == 0 and not trace.stitched then
                startfunc.sideexits = startfunc.sideexits + 1
            end

            if trace.stitched then
                startfunc.stitch_roots = startfunc.stitch_roots + 1
            end

            local stopfunc = functions[trace.stopfunc]
            stopfunc.stops = stopfunc.stops + 1
        end

        for i, trace in ipairs(aborted) do
            local startfunc = functions[trace.startfunc]
            startfunc.starts = startfunc.starts + 1
            startfunc.started_aborts = startfunc.started_aborts + 1

            local stopfunc = functions[trace.stopfunc]
            stopfunc.aborts = stopfunc.aborts + 1

            local lastlua = trace.lastlua
            if stopfunc ~= lastlua then
                lastlua.aborts = lastlua.aborts + 1
            end
        end

        local top_aborted = fun.iter(functions):max_by(function(f1, f2) return (f1.aborts > f2.aborts and f1) or f2 end)
        stats.top_aborted = top_aborted
        stats.top_aborted_list = fun.iter(aborted):filter(function(trace) return trace.lastlua == top_aborted end):totable()
        assert(top_aborted.aborts == #stats.top_aborted_list)

        stats.top_stitched = fun.iter(functions):max_by(function(f1, f2) return (f1.stitch_roots > f2.stitch_roots and f1) or f2 end)

        --Build aborted trace stop locations counts
        local aborted_locations = fun.iter(aborted):reduce(function(locs, trace)
            locs[trace.stoploc] = (locs[trace.stoploc] or 0) + 1
            return locs
        end, {})

        stats.aborted_locations = aborted_locations
        stats.most_aborted_location = fun.iter(aborted_locations):max_by(function(loc1, loc2)
            return (aborted_locations[loc1] > aborted_locations[loc2] and loc1) or loc2
        end)

        --Build trace side exit start location counts
        local sidexit_start_locations = fun.iter(completed):filter(function(trace) return trace.parentid ~= 0 end):reduce(function(locs, trace)
            locs[trace.startloc] = (locs[trace.startloc] or 0) + 1
            return locs
        end, {})

        stats.sidexit_start_locations = sidexit_start_locations

        local aborted_times = fun.iter(aborted):map(function(trace) return trace.endtime-trace.starttime end)
        stats.shortest_aborted = aborted_times:min()
        stats.longest_aborted = aborted_times:max()
    end
end