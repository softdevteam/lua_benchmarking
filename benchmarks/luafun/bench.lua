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

    -- Trace id remapping table
    local idmap = {}
    local tracecount = 1

    for i, trace in ipairs(traces) do
        -- Build start and stop location keys for all the traces.
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

        -- Map trace ids to a consistent increasing value based on their order in the log.
        local id = trace.id
        -- Use the total number of successful traces seen so far as the id for both aborted and successful traces.
        trace.id = tracecount

        if not trace.abort then
            idmap[id] = tracecount
            tracecount = tracecount + 1
        end
        if trace.parentid ~= 0 then
            assert(idmap[trace.parentid])
            trace.parentid = idmap[trace.parentid]
        end
    end
end

function run_iter(n)
    for n=1, n do
        init()

        local aborted = fun.iter(traces):filter(function(trace) return trace.abort end):totable()
        local completed = fun.iter(traces):filter(function(trace) return not trace.abort end):totable()
        assert((#aborted + #completed) == #traces)

        local function collect_roots(roots, trace)
            roots[trace.id] = trace
            return roots
        end
        local roots = fun.iter(completed):filter(function(trace) return trace.parentid == 0 end):reduce(collect_roots, {})
        assert(next(roots))

        for i, trace in ipairs(completed) do
            local startfunc = functions[trace.startfunc]
            startfunc.starts = startfunc.starts + 1

            -- Map side traces to their root trace
            if trace.parentid ~= 0 then
                if roots[trace.parentid] then
                    trace.rootid = trace.parentid
                else
                    trace.rootid = completed[trace.parentid].rootid
                    assert(trace.rootid, completed[trace.parentid].id)
                end
            end

            -- Function header root trace
            if trace.parentid == 0 and trace.startpc == 0 and not trace.stitched then
                assert(trace.exitnum == 0)
                startfunc.root_trace = trace
            end

            if trace.parentid ~= 0 and not trace.stitched then
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

        -- Build aborted trace stop location stats
        local function collect_abortlocs(locs, trace)
            locs[trace.stoploc] = (locs[trace.stoploc] or 0) + 1
            return locs
        end
        local aborted_locs = fun.iter(aborted):reduce(collect_abortlocs, {})
        stats.aborted_locations = aborted_locs
        stats.most_aborted_location = fun.iter(aborted_locs):max_by(
            function(loc1, loc2) return (aborted_locs[loc1] > aborted_locs[loc2] and loc1) or loc2 end
        )
        assert(aborted_locs[stats.most_aborted_location] > 0)

        -- Build side trace start location stats
        function collect_sidestartlocs(locs, trace)
            locs[trace.startloc] = (locs[trace.startloc] or 0) + 1
            return locs
        end
        local sidetrace_locs = fun.iter(completed):filter(function(trace) return trace.parentid ~= 0 end):reduce(collect_sidestartlocs, {})
        stats.sidetrace_locations = sidetrace_locs
        stats.top_sidetrace_location = fun.iter(sidetrace_locs):max_by(
            function(loc1, loc2) return (sidetrace_locs[loc1] > sidetrace_locs[loc2] and loc1) or loc2 end
        )
        assert(sidetrace_locs[stats.top_sidetrace_location] > 0)

        -- Build trace abort reason error code stats
        local function collect_aborterrors(counts, trace)
            counts[trace.aborterror] = counts[trace.aborterror] + 1
            return counts
        end
        local aborted_reasons = fun.iter(aborted):reduce(collect_aborterrors, fun.zeros():take(#jitlog.abortreasons):totable())
        stats.aborted_reasons = aborted_reasons
        stats.top_aborted_reason = fun.range(1, #jitlog.abortreasons):max_by(
            function(err1, err2) return (aborted_reasons[err1] > aborted_reasons[err2] and err1) or err2 end
        )
        assert(aborted_reasons[stats.top_aborted_reason] > 0)

        local aborted_times = fun.iter(aborted):map(function(trace) return trace.endtime-trace.starttime end)
        stats.shortest_aborted = aborted_times:min()
        stats.longest_aborted = aborted_times:max()
        assert(stats.longest_aborted > stats.shortest_aborted)
    end
end