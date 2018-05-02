local mixin = {
  filtered = {}
}
local filtered = mixin.filtered

mixin.actions = {
  stringmarker = function(self, msg, marker)
    if filtered[marker.label] then
      return
    end
  
    -- Record the start intervals for all the lists in the marker
    marker.id = #self.markers
    marker.traces = #self.traces
    marker.aborts = #self.aborts
    marker.flushes = #self.flushes
    marker.protos = #self.protos
    marker.ptbl = #self.proto_blacklist
    marker.exits = self.exits
    marker.gcstate = self.gcstate
    marker.gccount = self.gccount
    marker.gcstatecount = self.gcstatecount
  end
}

return mixin
