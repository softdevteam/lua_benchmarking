local mixin = {
  start_marker = "BEGIN",
  mixins = {"markerstats"},
  actions = {}
}

function mixin:init()
  self.itern = 0
end

mixin.statfields = {
  "traces",
  "aborts",
  "flushes",
  "protos",
  "ptbl",
  "exits",
  "gccount",
  "gcstatecount",
}

local tscfreq = 3700000000.0

local function getdiff(b, e)
  local t = {}
  for _, k in ipairs(mixin.statfields) do
    local val = e[k]
    assert(val)
    if type(val) == "number" then
      t[k] = val - b[k]
    else
      assert(false, k)
    end
  end
  t.time = tonumber(e.time - b.time) / tscfreq
  t.eventid = b.eventid
  t.eventcount = e.eventid - b.eventid
  t.tstart = b.time
  t.tend = e.time
  t.gcstate = b.gcstate
  return t
end

local actions = mixin.actions

local start_marker, end_marker = "BEGIN", "END"

function actions:stringmarker(msg, marker)
  if marker.label == start_marker then
    self.mstart = marker
    self.mend = nil
    marker.n = self.itern
    marker.stralloc = 0
    marker.memalloc = 0
    self.itern = self.itern + 1
  elseif marker.label == end_marker then
    local stats = getdiff(self.mstart, marker)
    self.mstart.stats = stats
    marker.stats = stats
    self.mend = marker
  end
end

function actions:trace(msg, trace)
  trace.itern = self.itern
end

function actions:protobl(msg, blinfo)
  blinfo.itern = self.itern
end

function actions:alltraceflush(msg, flush)
  flush.itern = self.itern
end

function actions:protoloaded(msg, address, proto)
  proto.itern = self.itern
end

function actions:perf_timers(msg)
  local timerdef = self.enums.timers
  local times_length = msg:get_times_length()
  local ids = msg.ids_length ~= 0 and msg:get_ids()
  local timers = {}

  local tscfreq = self.tscfreq or 3700000000.0
  local times = msg:get_times()
  for i=0, times_length-1 do
    local key
    if ids then
      key = timerdef[ids[i]]
    else
      key = timerdef[i]
    end
    timers[key] = tonumber(times[i])/tscfreq
  end
  assert(not self.mstart.timers)
  self.mstart.timers = timers
end

function actions:perf_counters(msg)
  local counterdef = self.enums.counters
  local counts_length = msg:get_counts_length()
  local ids = msg.ids_length ~= 0 and msg:get_ids()

  local counters = {}
  local counts = msg:get_counts()
  for i=0, counts_length-1 do
    local key
    if ids then
      key = counterdef[ids[i]]
    else
      key = counterdef[i]
    end
    counters[key] = counts[i]
  end
  assert(not self.mstart.counters)
  self.mstart.counters = counters
end

function actions:section(msg, id, isstart, length)
  if not length or not self.mstart then
    return
  end

  local marker = self.mstart
  if not marker.section_times then
    marker.section_times = {}
    marker.section_counts = {}
  end
  
  local name = self.section_names[id]
  marker.section_times[name] = (marker.section_times[name] or 0ull) + length
  marker.section_counts[name] = (marker.section_counts[name] or 0) + 1
end

function actions:gcstate(msg)
  local marker = self.mstart
  if not marker then
    return
  end
  local strnum = (self.last_strnum and self.last_strnum-msg.strnum) or msg.strnum
  self.last_strnum = msg.strnum
  if strnum > 0 then
    marker.stralloc = marker.stralloc + strnum
  end

  local totalmem = (self.last_totalmem and self.last_totalmem-msg.totalmem) or msg.totalmem
  self.last_totalmem = msg.totalmem
  if totalmem > 0 then
    marker.memalloc = marker.memalloc + totalmem
  end
end

function actions:gcstats(msg, gcstats)
  local marker = self.mstart
  if not marker then
    return
  end
  assert(not marker.gcstats, "more one gcstats found for an iteration")
  marker.gcstats = gcstats
end


return mixin
