local function printf(...)
    print(string.format(...))
end

local function pc2proto(reader, pc)
  for i, pt in ipairs(reader.protos) do
    local line = pt:get_pcline(pc)
    if line then
      return pt, line
    end
  end
  return nil, nil
end

local function printsnaps(reader, t)
  for i=0, t.nsnap-1 do
    local pc = t:get_snappc(i)
    local pt, line = pc2proto(reader, pc)
    printf("  Snap(%d): %s: %d", i, pt and pt.chunk or "?", line or -1)
  end
end

local function getparentid(t)
  return (t.parentid ~= 0) and (", parent "..t.parentid) or ""
end

local mixin = {
  mixins = { "iter_annotate" },
  actions = {},
}
  
local actions = mixin.actions

function actions:stringmarker(msg, marker)
  if marker.label == "BEGIN" then
    printf("------------------ Iteration %d --------------------------", self.itern)
  end
end

function actions:trace(msg, trace)
  if self.mstart and self.mend then
    printf("Trace(%d%s): outside iter %d", trace.id, trace.aborted and ", ABORT" or "",  self.mstart.n, trace:get_startlocation())
  end

  if not trace.abortcode then
    printf("Trace(%d%s): start %s stop %s, link %s", trace.id, getparentid(trace), trace:get_startlocation(), trace:get_stoplocation(), trace.link)
  else
    printf("TraceAbort(%d%s): start: %s, stop: %s, reaason: %s ", trace.id, getparentid(trace), trace:get_startlocation(), 
            trace:get_stoplocation(), trace.abortreason)
  end
  if self.dumpsnapshots then
    printsnaps(self, trace)
  end
end

function actions:protobl(msg, blinfo)
  printf("ProtoBlacklisted: %s, bcindex: %d", blinfo.proto:get_location(), blinfo.bcindex)
end

function actions:alltraceflush(msg, flush)
end

function actions:protoloaded(msg,  address, proto)
  printf("ProtoLoaded: %s", proto:get_location())
end

return mixin
