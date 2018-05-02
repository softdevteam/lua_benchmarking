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

function action.logparsed(jlog)
  local traces = table.new(jlog.itern, 0)
  local aborts = table.new(jlog.itern, 0)
  local exits = table.new(jlog.itern, 0)

  local mstart
  for i, marker in ipairs(jlog.markers) do
    if marker.label == "BEGIN" then
      local stats = marker.stats
      local n = marker.n+1
      traces[n] = stats.traces
      aborts[n] = stats.aborts
      exits[n] = stats.exits
    end
  end

  local path = os.getenv("instr_results") or jlog.instr_path
  local result = io.open(path, "w")

  if json then
    local data = {timers = {}}
    local timers = data.timers
    table.insert(timers, {label = "traces", data = traces})
    table.insert(timers, {label = "aborts", data = aborts})
    table.insert(timers, {label = "exits", data = aborts})
    result:write(json.encode(data))
  else
    result:write('{\n  "timers" : [\n')
    result:write('  { "label" : "traces", "data" : [', table.concat(traces, ", "), ']},\n')
    result:write('  { "label" : "aborts", "data" : [', table.concat(aborts, ", "), ']},\n')
    result:write('  { "label" : "exits", "data" : [', table.concat(exits, ", "), ']}\n')
    result:write(']}\n')
  end
  result:flush()
  result:close()
end

return action
