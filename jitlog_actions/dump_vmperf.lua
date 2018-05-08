local mixin = {
  actions = {}
}

local function printf(...)
    print(string.format(...))
end


function mixin.actions.perf_timers(self, msg)
  local timerdef = self.enums.timers
  local times_length = msg:get_times_length()
  local ids = msg.ids_length ~= 0 and msg:get_ids()
  
  printf("Perf Timers:")
  local tscfreq = self.tscfreq or 3700000000.0
  local times = msg:get_times()
  for i=0, times_length-1 do
    local key
    if ids then
      key = timerdef[ids[i]]
    else
      key = timerdef[i]
    end
    printf("  %s: %f", key, tonumber(times[i])/tscfreq)
  end
end

function mixin.actions.perf_counters(self, msg)
  local counterdef = self.enums.counters
  local counts_length = msg:get_counts_length()
  local ids = msg.ids_length ~= 0 and msg:get_ids()
  
  printf("Perf Counts:")
  local counts = msg:get_counts()
  for i=0, counts_length-1 do
    local key
    if ids then
      key = counterdef[ids[i]]
    else
      key = counterdef[i]
    end
    printf("  %s: %d", key, counts[i])
  end
end

return mixin
