local mixin = {
  actions = {}
}

local function printf(...)
    print(string.format(...))
end

function mixin.init(self)
  self.section_counts = {}
  self.section_times = {}
  self.section_max = {}
end

function mixin.actions.section(self, msg, section)
  if section.length == 0 then
    return
  end
  local name = self.section_names[section.id]
  
  self.section_times[name] = (self.section_times[name] or 0ull) + section.length
  self.section_max[name] = math.max((self.section_max[name] or 0), section.length)
  self.section_counts[name] = (self.section_counts[name] or 0) + 1
end

local action = {
  mixins = { mixin}
}

function action.logopened(reader, logpath)
end

function action.logparsed(jlog)
  local tscfreq = jlog.tscfreq or 3700000000.0
  local times = jlog.timers or jlog.section_times
  local gctime = tonumber(times.gc_step + times.gc_step_jit)/tscfreq
  printf("Total GC Time: %f", gctime)
  for name, time in pairs(times ) do
    printf("  %s(%d): %f", name, jlog.section_counts[name], tonumber(time)/tscfreq)
  end

  io.stdin:read()
  return
end

return action