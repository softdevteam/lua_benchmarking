local band, lshift, rshift = bit.band, bit.lshift, bit.rshift
local json
local format = string.format
require"table.new"

local hasjson, json = pcall(require, "json")

if not hasjson then
  json = nil
end

local function printf(...)
    print(string.format(...))
end

local action = {
  mixins = {
    "iter_annotate"
  },
  config = {
  }
}

-- borrowed from https://github.com/moteus/lua-path
local function splitext(P)
  local s1,s2 = string.match(P,"(.-[^\\/.])(%.[^\\/.]*)$")
  if s1 then return s1,s2 end
  return P, ''
end

local function splitpath(P)
  return string.match(P,"^(.-)[\\/]?([^\\/]*)$")
end

function action.logopened(reader, logpath)
  reader.ktimers = reader.ktimers or {}
  local dir, fname = splitpath(logpath)
  fname = splitext(fname):gsub("_", "__") .. ".json"

end

local hotsize = 64

local function getslot(address)
  return  band(rshift(address, 2), hotsize-1)
end

function action.logparsed(jlog)
  local slots = {}
  for i=0, 64 do
    slots[i] = {
      pts = {},
      traces = {},
      starting_locs = {},
      startloc_count = 0,
    }
  end
  for i, pt in pairs(jlog.protos) do
    table.insert(slots[pt.hotslot].pts, pt)
  end
  for _, tracelist in pairs({jlog.aborts, jlog.traces}) do
    for i, trace in ipairs(tracelist) do
      local pt = trace.startpt
      if trace.parentid == 0 then
        local bcaddr = pt.address + trace.startpc
        local slot = getslot(bcaddr)
        slot = slots[slot]
        slot.pts[pt] = (slot.pts[pt] or 0) + 1
        if not slot.starting_locs[bcaddr] then
          slot.starting_locs[bcaddr] = 1
          slot.startloc_count = slot.startloc_count + 1
        else
          slot.starting_locs[bcaddr] = slot.starting_locs[bcaddr] + 1
        end
        table.insert(slot.traces, trace)
      end
    end
  end
  
  for i=0, 64 do
    local slot = slots[i]
    table.sort(slot.traces, function(t1, t2) return t1.eventid < t2.eventid end)
    if slot.startloc_count ~= 0 then
      printf("Slot %d: startlocs= %d", i, slots[i].startloc_count)
      if slots[i].startloc_count > 1 then
        for _, trace in ipairs(slot.traces) do
          local startpt, stoppt = trace.startpt, trace.stoppt
          if trace.abortcode then
            printf("  AbortedTrace(%d): reason %s, start= %s\n   stop= %s", trace.id, trace.abortreason, startpt:get_location(), stoppt:get_location())
          else
            printf("  Trace(%d): start= %s\n   stop= %s", trace.id, startpt:get_location(), stoppt:get_location())
          end
        end
       end
    end
  end
end

return action
