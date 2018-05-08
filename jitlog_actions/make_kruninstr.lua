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

local function add_ifnonzero(t, label, list)
  if allzero(list) then
    return
  end
  local entry = {label = label, data = list}
  table.insert(t, entry)
  return entry
end

function action.logparsed(jlog)
  local traces = table.new(jlog.itern, 0)
  local aborts = table.new(jlog.itern, 0)
  local exits = table.new(jlog.itern, 0)
  local timers, counters
  
  if jlog.timers then
    timers = {}
    for k, v in pairs(jlog.timers) do
      timers[k] = table.new(jlog.itern, 0)
    end
  end

  if jlog.counters then
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
      
      if marker.timers then
        for k, list in pairs(timers) do
          list[n] = marker.timers[k] or 0
        end
      end
      if marker.counters then
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

  if timers then
    for k, list in pairs(timers) do
      add_ifnonzero(timer_lists, k, list)
    end
  end
  if counters then
    for k, list in pairs(counters) do
      add_ifnonzero(timer_lists, k.."_count", list)
    end
  end
  
  if jlog.ktimers then
    for label, list in pairs(jlog.ktimers) do
      if type(list) == "function" then
        -- Call the defered list generator
        list = list(jlog)
      end
      add_ifnonzero(timer_lists, label, list)
    end
  end
  
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
