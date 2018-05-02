local names = {
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
  for _, k in ipairs(names) do
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

local mixin = {
  mixins = {"markerstats"},
  init = function(self)
    self.itern = 0
  end,
  actions = {
    stringmarker = function(self, msg, marker)
      if marker.label == "BEGIN" then
        self.mstart = marker
        self.mend = nil
        marker.n = self.itern
        self.itern = self.itern + 1
      elseif marker.label == "END" then
        local stats = getdiff(self.mstart, marker)
        self.mstart.stats = stats
        marker.stats = stats
        self.mend = marker
      end
    end,
    trace = function(self, msg, trace)
      trace.itern = self.itern
    end,
    protobl = function(self, msg, blinfo)
      blinfo.itern = self.itern
    end,
    alltraceflush = function(self, msg, flush)
      flush.itern = self.itern
    end,
    protoloaded = function(self, msg,  address, proto)
      proto.itern = self.itern
    end
  }
}

return mixin
