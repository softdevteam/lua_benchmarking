local bc = require("jit.bc")
local vmdef = require("jit.vmdef")
local jutil = jit.util or require"jit.util"
local funcinfo, funcbc = jutil.funcinfo, jutil.funcbc
local bcnames = {}

for i = 0, #vmdef.bcnames-1, 6 do
    bcnames[i] = string.sub(vmdef.bcnames, i, i+6):match "^%s*(.-)%s*$"
end

-- Utilities -------------------------------------------------------------------

local function printf(fmt, ...)

    if select("#", ...) == 0 then
        --print(fmt)
    else
       -- print(string.format(fmt, ...))
    end
end

-- Stolen from dump.lua
local function fmtfunc(func, pc)
  local fi = funcinfo(func, pc)
  if fi.loc then
    return fi.loc
  elseif fi.ffid then
    return vmdef.ffnames[fi.ffid]
  elseif fi.addr then
    return ("C:%x"):format(fi.addr)
  else
    return "(?)"
  end
end

-- Format trace error message.  Stolen from dump.lua
local function fmterr(err, info)
    if type(err) == "number" then
        if type(info) == "function" then 
            info = fmtfunc(info) 
        end
        err = vmdef.traceerr[err]:format(info)
    end
    return err
end


-- Tracing ---------------------------------------------------------------------

local traces = {}
local func_lookup = {}
local func_list = {}
local func_count = 0

local function getfuncid(func)
    local info = func_lookup[func]
    
    if info then
        return info.id
    end
    
    func_count = func_count + 1
    local id = func_count

    local info = funcinfo(func)

    if info.linedefined then
        printf("New Lua func: %d %s@%d", id, info.source, info.linedefined)
        info = {
            id = id,
            chunk = info.source,
            line = info.linedefined,
        }
    else
        info = {
            id = id,
            cfunc = true,
            ffid = info.ffid,
            addr = addr,
            name = info.ffid and vmdef.ffnames[info.ffid]
        }
    end
    
    func_list[#func_list+1] = info
    func_lookup[func] = info
    return id
end

local last_func, last_pc

function reset_state()
    last_pc = nil
    last_func = nil
end

local vmevent = {}

-- Trace started
function vmevent.start(tr, func, pc, parentid, exitnum)   
    reset_state()

    local startid = getfuncid(func)
    last_func = func
    local stitched = parentid and parentid > 0 and exitnum == -1

    printf("START(%d): parent = %d", tr, parentid or -1)
    
    local m, ot, op1, op2, prev = jutil.traceir(tr, 0x8000)

    local t = { 
        id = tr,
        startfunc = startid,
        startpc = pc,
        parentid = parentid,
        exitnum = exitnum,
        stitched = stitched,
        bytecode = {},
        funcs = {startid},
        starttime = 0,
    }
    traces[#traces+1] = t
    t.starttime = os.clock()
end

--Trace Stopped
function vmevent.stop(tr)
    local endtime = os.clock()
    local info = jutil.traceinfo(tr)
    
    local t = traces[#traces]
    t.endtime = endtime
    t.link = info.linktype
    
    printf("STOP(%d): link = ", tr, info.linktype)
    
    if t.funcs[1] then
        t.stopfunc = t.funcs[#t.funcs]
        --t.stop = t.bytecode[#t.bytecode].info
    else
        t.stopfunc = -1
    end
end

--Trace Aborted
function vmevent.abort(tr, func, pc, code, errinfo)
    local endtime = os.clock()
    local t = traces[#traces]

    local reason = fmterr(code, errinfo)
    reason = reason:gsub("bytecode (%d+)", function(c)
    c = tonumber(c) * 6
    return "bytecode "..vmdef.bcnames:sub(c, c+6):gsub(" ", "")
    end)
    
    printf("ABORT(%d): %s", tr, reason)
    
    t.endtime = endtime
    t.abort = true
    t.aborterror = code+1
    
    if type(errinfo) == "number" then
        t.abortinfo = errinfo
    end

    if func then
        -- t.lastlua getfuncid(func)
    end
    
    if t.funcs[1] then
        t.stopfunc = t.funcs[#t.funcs]
        --t.stop = t.bytecode[#t.bytecode].info        
    else
        t.stopfunc = -1
    end
       
    t.stoppc = pc
end

local function vmeventcb(what, ...)
    local cb = vmevent[what]
    if cb then 
        cb(...)
    end
end

local funcid

local function record_bytecode(tr, func, pc, depth)
    local t = traces[#traces]
    assert(t.id == tr)

    if func ~= last_func then
        last_func = func
        
        funcid = getfuncid(func)
        local func_slot = #t.funcs + 1
        t.funcs[func_slot] = funcid
        -- Mark a function change in the pc index stream
        t.bytecode[#t.bytecode+1] = last_pc
    end

   --t.bytecode[#t.bytecode+1] = pc
   
   last_pc = pc
end

local jitlog = {
    isrunning = false,
}

function jitlog.start()
    jit.attach(vmeventcb, "trace")
    jit.attach(record_bytecode, "record")
    jitlog.isrunning = true
end

function jitlog.stop()

    if not jitlog.isrunning then
        return
    end

    jit.attach(vmeventcb)
    jit.attach(record_bytecode)
end

function jitlog.save(path)
    print "saving jitlog"
    jitlog.stop()
    
    local file = io.open(path or "jitlog.json", "w")
    local data

    if true then
        local jitlog_capnp = require "jitlog_capnp"
        data = jitlog_capnp.JITLog.serialize({functions = func_list, traces = traces, abortreasons = vmdef.traceerr, bcnames = bcnames})
    else
        local json = require "json"
        data = json.encode({functions = func_list, traces = traces})
    end

    file:write(data)
    file:close()
end

return jitlog
