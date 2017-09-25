local jit = require("jit")
local bc = require("jit.bc")
local vmdef = require("jit.vmdef")
local jutil = jit.util or require("jit.util")
local funcinfo, funcbc = jutil.funcinfo, jutil.funcbc
local band = bit.band
require("table.clear")
require("table.new")

local bcnames, bcname_lookup = {}, {} 
local bccount = 0
for i = 0, #vmdef.bcnames-1, 6 do
    local name = string.sub(vmdef.bcnames, i, i+6):match "^%s*(.-)%s*$"
    bcnames[bccount] = name
    bcname_lookup[name] = bccount
    bccount = bccount + 1
end

local traceerr_lookup = {}
for i, msg in pairs(vmdef.traceerr) do
    traceerr_lookup[msg] = i
end

local traceerror_info = {
  {name = "RECERR",  msg = "error thrown or hook called during recording"},
  {name = "TRACEUV", msg = "trace too short"},
  {name = "TRACEOV", msg = "trace too long"},
  {name = "STACKOV", msg = "trace too deep"},
  {name = "SNAPOV",  msg = "too many snapshots"},
  {name = "BLACKL",  msg = "blacklisted", displaymsg = "Function or loop blacklisted"},
  {name = "RETRY",   msg = "retry recording"},
  {name = "NYIBC",   msg = "NYI: bytecode %d", displaymsg = "NYI: bytecode"},
  
  -- Recording loop ops.
  {name = "LLEAVE",  msg = "leaving loop in root trace"},
  {name = "LINNER",  msg = "inner loop in root trace"},
  {name = "LUNROLL", msg = "loop unroll limit reached"},
  
  -- Recording calls/returns.
  {name = "BADTYPE", msg = "bad argument type"},
  {name = "CJITOFF", msg = "JIT compilation disabled for function"},
  {name = "CUNROLL", msg = "call unroll limit reached"},
  {name = "DOWNREC", msg = "down-recursion, restarting"},
  {name = "NYIFFU",  msg = "NYI: unsupported variant of FastFunc %s", displaymsg = "NYI: unsupported variant of FastFunc"},
  {name = "NYIRETL", msg = "NYI: return to lower frame"},
  
  -- Recording indexed load/store.
  {name = "STORENN", msg = "store with nil or NaN key"},
  {name = "NOMM",    msg = "missing metamethod"},
  {name = "IDXLOOP", msg = "looping index lookup"},
  {name = "NYITMIX", msg = "NYI: mixed sparse/dense table"},
  
  -- Recording C data operations. */
  {name = "NOCACHE", msg = "symbol not in cache"},
  {name = "NYICONV", msg = "NYI: unsupported C type conversion"},
  {name = "NYICALL", msg = "NYI: unsupported C function type"},
  
  -- Optimizations.
  {name = "GFAIL",   msg = "guard would always fail"},
  {name = "PHIOV",   msg = "too many PHIs"},
  {name = "TYPEINS", msg = "persistent type instability"},
  
  -- Assembler.
  {name = "MCODEAL", msg = "failed to allocate mcode memory"},
  {name = "MCODEOV", msg = "machine code too long"},
  {name = "MCODELM", msg = "hit mcode limit (retrying)"},
  {name = "SPILLOV", msg = "too many spill slots"},
  {name = "BADRA",   msg = "inconsistent register allocation"},
  {name = "NYIIR",   msg = "NYI: cannot assemble IR instruction %d"},
  {name = "NYIPHI",  msg = "NYI: PHI shuffling too complex"},
  {name = "NYICOAL", msg = "NYI: register coalescing too complex"},
}

local traceerr = {}
local traceerr_fromid = table.new(#vmdef.traceerr, 0)
local traceerr_displaymsg = {}

for i, evt in ipairs(traceerror_info) do
    local id = traceerr_lookup[evt.msg]
    if id then
        traceerr[evt.name] = id
        traceerr_fromid[id] = evt.name
    end
    traceerr_displaymsg[evt.name] = evt.displaymsg or evt.msg
end

local function printf(fmt, ...)
    print(string.format(fmt, ...))
end

local debugprint = function () end

local function fmtfunc(func, pc)
    local fi = funcinfo(func, pc)
    if fi.loc then
        return fi.loc
    elseif fi.ffid then
        return vmdef.ffnames[fi.ffid]
    elseif fi.addr then
        return string.format("C:%x", fi.addr)
    else
        return "(?)"
    end
end

-- Format trace error message.
local function fmterr(err, info)
    if type(err) == "number" then
        if type(info) == "function" then
          info = fmtfunc(info) 
        end
        err = string.format(vmdef.traceerr[err], info)
    end
    return err
end

local jitstats = {
    isrunning = false,
    debug = false,
    stats = {}
}

local stats = jitstats.stats

local vmevent = {}

local curr_parentid, curr_stitched
-- Trace started
function vmevent.start(tr, func, pc, parentid, exitnum)
    curr_parentid = parentid
    curr_stitched = parentid and parentid > 0 and exitnum == -1

    debugprint("START(%d): parent = %d", tr, parentid or -1)
    
    stats.starts = stats.starts + 1
    if curr_stitched then
        stats.stitch_starts = stats.stitch_starts + 1
    elseif parentid then
        stats.side_starts = stats.side_starts + 1
    end 
end

--Trace Stopped
function vmevent.stop(tr)
    local info = jutil.traceinfo(tr)
     
    debugprint("STOP(%d): link = %s", tr, info.linktype)
    
    stats.completed = stats.completed + 1
    
    if curr_stitched then
        stats.stitch_completed = stats.stitch_completed + 1
    elseif curr_parentid then
        stats.side_completed = stats.side_completed + 1
    end

    stats.linktypes[info.linktype] = (stats.linktypes[info.linktype] or 0) + 1
end

--Trace Aborted
function vmevent.abort(tr, func, pc, code, errinfo)
    stats.aborts = stats.aborts + 1
    
    local reason = ""
    if jitstats.debug then
        reason = fmterr(code, errinfo)
        reason = reason:gsub("bytecode (%d+)", function(c)
            c = tonumber(c)
            assert(bcnames[c], c)
            return "bytecode "..bcnames[c]
        end)
    end

    if curr_stitched then
        stats.stitch_aborts = stats.stitch_aborts + 1
    elseif curr_parentid then
        debugprint("ABORT(%d): parent = %d, %s", tr, curr_parentid, reason)
        stats.side_aborts = stats.side_aborts + 1
    else
        debugprint("ABORT(%d): %s", tr, reason)
    end

    -- We sometimes don't get a bc for the stop location
    local bc = funcbc(func, pc)
    local opcode = bc and band(bc, 0xff)
    
    if code == traceerr.LLEAVE then
        debugprint("LLEAVE: %d/%s", opcode, bcnames[opcode])
    elseif code == traceerr.LUNROLL then
        
    elseif code == traceerr.NYIBC then
        bcname = bcnames[errinfo]
        stats.nyi_bc[bcname] = (stats.nyi_bc[bcname] or 0) + 1
    end

    -- Use internal enum name of the trace abort error if possible
    local tracerror = traceerr_fromid[code] or vmdef.traceerr[code]
    stats.abort_counts[tracerror] = (stats.abort_counts[tracerror] or 0) + 1
end

function vmevent.flush()
    debugprint("Full trace flush")
    stats.traceflush = stats.traceflush + 1
end

function vmevent.texit(id, exitno)
    debugprint("EXIT(%d): exit = %d", id, exitno)
    stats.exits = stats.exits + 1
end

local function vmeventcb(what, ...)
    local cb = vmevent[what]
    if cb then 
        cb(...)
    end
end

function jitstats.start()
    if jitstats.isrunning then
        return false
    end

    jit.attach(vmeventcb, "trace")
    jit.attach(vmevent.texit, "texit")
    jitstats.isrunning = true
    return true
end

function jitstats.stop()
    if not jitstats.isrunning then
        return false
    end

    jit.attach(vmeventcb)
    jit.attach(vmevent.texit)
    jitstats.isrunning = false
    return true
end

local function initorclear(tab, key)
    local t = tab[key]
    if t then
        table.clear(t)
    else
        tab[key] = {}
    end
end

local function reset_subtables(statstbl)
    initorclear(statstbl, "abort_counts")
    initorclear(statstbl, "nyi_bc")
    initorclear(statstbl, "linktypes")
end

function jitstats.reset(statstbl)
    statstbl = statstbl or stats
    statstbl.starts = 0
    statstbl.aborts = 0
    statstbl.completed = 0
    statstbl.stitch_starts = 0
    statstbl.stitch_aborts = 0
    statstbl.stitch_completed = 0
    statstbl.side_starts = 0
    statstbl.side_aborts = 0  
    statstbl.side_completed = 0
    statstbl.exits = 0
    statstbl.traceflush = 0
    --Clear or initlize 
    reset_subtables(statstbl)
end

-- Note no attempt is made to handle circular references
local function table_copy(src, dst)
    dst = dst or {}
    for k,v in pairs(src) do
        if type(v) == "table" then
            dst[k] = table_copy(v, dst[k])
        else
            dst[k] = v
        end
    end
    return dst
end

--Optionally pass in a table to write the snapshot to instead of creating a new table
function jitstats.getsnapshot(dst)
    if dst then
        reset_subtables(dst)
    end
    return table_copy(stats, dst)
end

local function diffstatbl(old, new, result, key)
    assert(old[key], "missing old sub table")
    assert(new[key], "missing new sub table")
    assert(result[key], "missing result sub table")
    old = old[key]
    new = new[key]
    result = result[key]

    for k, v in pairs(new) do
        local change = v - (old[k] or 0)
        if change ~= 0 then
            result[k] = change
        end
    end
    return result
end

-- Get the diff between two jit stat snapshots
function jitstats.diffsnapshots(snap1, snap2)
    local result = {
        abort_counts = {},
        nyi_bc = {},
        linktypes = {},
    }

    for k, v in pairs(snap2) do   
        if type(v) == "number" then
            result[k] = v - (snap1[k] or 0)
        end
    end

    diffstatbl(snap1, snap2, result, "abort_counts")
    diffstatbl(snap1, snap2, result, "nyi_bc")
    diffstatbl(snap1, snap2, result, "linktypes")

    return result
end

-- Checks if the current stats have changed compared to the snapshot passed in
function jitstats.stats_changed(snapshot) 
    return snapshot.starts ~= stats.starts or snapshot.exits ~= stats.exits or 
           snapshot.traceflush ~= stats.traceflush
end

function jitstats.setdebugprint(enable)
    jitstats.debug = enable
    debugprint = (enable and printf) or function() end
end

local function valpercent(total, subval)
    return string.format("%d(%.0f%%)", subval, subval / total * 100)
end

function jitstats.print_simple(msg)
    local running = jitstats.stop()
    printf("%sStarted %d, Aborted %d, Exits %d", msg or "", stats.starts, stats.aborts, stats.exits)
    if running then
        jitstats.start()
    end
end

local function table_keys(t)
    local result = {}
    for k,_ in pairs(t) do
        table.insert(result, k)
    end
    return result
end

local function statstbl_sortkeys(t, sorter)
    local keys = table_keys(t) 
    table.sort(keys, function(k1, k2) return t[k1] > t[k2] end)
    return keys
end

local function statstbl_concat(tbl)
    local strings = {}
    local count = 0
    for k, v in pairs(tbl) do
        count = count + 1
        strings[count] = k.." = "..v
    end
    
    return table.concat(strings, ", ")
end

local function buildtemplate(template, values)
  return (string.gsub(template, "{{([^\n]-)}}", function(key)
    if values[key] == nil then
        error("missing value for template key '"..key.."'")
    end
    return values[key]
  end))
end

function jitstats.print(statstbl, msg)
    local running = jitstats.stop()

    if not statstbl then
        statstbl = stats
        msg = "Trace Stats:"
    elseif type(statstbl) == "string" then
        assert(msg == nil)
        msg = statstbl
        statstbl = stats
    else
        assert(type(statstbl) == "table", type(statstbl))
        msg = "Trace Stats:"
    end

    printf(msg)

    if statstbl.traceflush > 0 then
        printf("  Full trace flush(all traces) %d", statstbl.traceflush)
    end
   
    local values = {
        side_starts = valpercent(statstbl.starts, statstbl.side_starts),
        stitch_starts = valpercent(statstbl.starts, statstbl.stitch_starts),
        aborts = valpercent(statstbl.starts, statstbl.aborts),
        side_aborts = valpercent(statstbl.starts, statstbl.side_aborts),
        stitch_aborts = valpercent(statstbl.starts, statstbl.stitch_aborts),
        linktypes = statstbl_concat(statstbl.linktypes),
    }
    setmetatable(values, {__index = statstbl})

    print(buildtemplate([[
  Started {{starts}}, side {{side_starts}}, stitch {{stitch_starts}}
  Completed Link types: {{linktypes}}
  Aborted {{aborts}}, side {{side_aborts}}, stitch {{stitch_aborts}}]], values))

    local sortedkeys = statstbl_sortkeys(statstbl.abort_counts)

    for _, name in ipairs(sortedkeys) do
        local msg = traceerr_displaymsg[name] or name
        printf("    %d %s", statstbl.abort_counts[name], msg)
    end

    -- Print NYI bytecode abort counts
    if next(statstbl.nyi_bc) then
        printf("  NYI BC Aborts")
        sortedkeys = statstbl_sortkeys(statstbl.nyi_bc)
        for _, bc in ipairs(sortedkeys) do
            printf("    %d %s", statstbl.nyi_bc[bc], bc)
        end
    end
    
    printf(" Uncompiled exits taken %d", statstbl.exits)
    
    if running then
        jitstats.start()
    end
end

jitstats.reset()
return jitstats
