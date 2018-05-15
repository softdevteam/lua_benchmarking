local json
local format = string.format
require"table.new"

local hasjson, json = pcall(require, "json")

if not hasjson then
  json = nil
end

local action = {
  mixins = {
    "iter_annotate"
  },
  config = {
    skip_timers = false,
    skip_counters = true,
    ignore = {
      trace_exit = true,
      race_exit_restore = true,
    },
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
  reader.instr_path = dir .. "/" .. fname
end

local function allzero(list)
  for i, v in ipairs(list) do
    if v ~= 0 then
      return false, i
    end
  end
  return true
end

local function changes_aftern(list, n)
  local prev = list[n]
  for i=n, #list do
    if list[n] ~= prev then
      return true
    end
    prev = list[i]
  end
  return false
end

local function add_ifchanges(t, label, list)
  if allzero(list) or not changes_aftern(list, 6) then
    return
  end
  local entry = {label = label, data = list}
  table.insert(t, entry)
  return entry
end

local function add_ifnonzero(t, label, list)
  if allzero(list) then
    return
  end
  local entry = {label = label, data = list}
  table.insert(t, entry)
  return entry
end

local firstlist = true

local function write_timelist(file, name, values, last)
  if allzero(values) then
    return false
  end
  if not firstlist then
    firstlist = false
    result:write(',\n')
  end
  result:write('  { "label" : "', name, '", "data" : [', table.concat(values, ", "))
  result:write(']}')
  return true
end

function action.logparsed(jlog)
  local config = action.config
  local traces = table.new(jlog.itern, 0)
  local aborts = table.new(jlog.itern, 0)
  local exits = table.new(jlog.itern, 0)
  local memalloc = table.new(jlog.itern, 0)
  local stralloc = table.new(jlog.itern, 0)
  local timers, counters
  
  if jlog.timers and not config.skip_timers then
    timers = {}
    for k, v in pairs(jlog.timers) do
      timers[k] = table.new(jlog.itern, 0)
    end
  end

  if jlog.counters and not config.skip_counters then
    counters = {}
    for k, v in pairs(jlog.counters) do
      counters[k] = table.new(jlog.itern, 0)
    end
  end

  local mstart
  for i, marker in ipairs(jlog.markers) do
    if marker.label == "BEGIN" then
      local stats = marker.stats
      local n = marker.n+1
      traces[n] = stats.traces
      aborts[n] = stats.aborts
      exits[n] = stats.exits
      memalloc[n] = marker.memalloc  or 0
      stralloc[n] = marker.stralloc  or 0

      if timers and marker.timers then
        for k, list in pairs(timers) do
          list[n] = marker.timers[k] or 0
        end
      end
      if counters and marker.counters then
        for k, list in pairs(counters) do
          list[n] = marker.counters[k] or 0
        end
      end
    end
  end

  local path = os.getenv("instr_results") or jlog.instr_path
  local result = io.open(path, "w")

  local timer_lists = {}
  if json then
    -- TODO optionally load existing json and append to timers
    --timer_lists = data.timers
  end
  add_ifnonzero(timer_lists, "traces", traces)
  add_ifnonzero(timer_lists, "aborts", aborts)
  add_ifnonzero(timer_lists, "exits", exits)
  add_ifnonzero(timer_lists, "memalloc",   memalloc)
  add_ifnonzero(timer_lists, "stralloc",   stralloc)


  if timers then
    for label, list in pairs(timers) do
      if not config.ignore[label] then
        add_ifnonzero(timer_lists, label, list)
      end
    end
  end
  if counters then
    for k, list in pairs(counters) do
      local label = k.."_count"
      if not config.ignore[label] then
        add_ifnonzero(timer_lists, label, list)
      end
    end
  end
  
  if jlog.ktimers then
    for label, list in pairs(jlog.ktimers) do
      if type(list) == "function" then
        -- Call the defered list generator
        list = list(jlog)
      end
      if not config.ignore[label] then
        add_ifchanges(timer_lists, label, list)
      end
    end
  end
  
  table.sort(timer_lists, function(a,b) return a.label < b.label end)
  
  if json then
    local data = {timers = timer_lists}
    result:write(json.encode(data))
  else
    result:write('{\n  "timers" : [\n')
    for label, list in pairs(timer_lists) do
      write_timelist(file, label, list)
    end
    result:write(']}\n')
  end
  result:flush()
  result:close()
end


return action
