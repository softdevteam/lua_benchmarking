local format = string.format
local readerlib
local debugger_attached = decoda_output ~= nil

local logpath = arg[1]
local action_names = {unpack(arg, 2, #arg)}

function add_luapackage_path(basepath, addtostart)
    assert(type(basepath) == "string" and #basepath > 0)
 
    if addtostart then
        package.path = string.format("%s/?.lua;%s/?/init.lua;%s", basepath, basepath, package.path)
    else
        package.path = string.format("%s;%s/?.lua;%s/?/init.lua", package.path, basepath, basepath)
    end
end
add_luapackage_path("lualibs", true)

local success
success, readerlib = pcall(require, "jitlog.reader")

if not success then
  add_luapackage_path("builds/normal", true)
  readerlib = require("jitlog.reader")
end

local actions = {}
local mixins = {}
local loaded_mixins = {}

local function loadmodule(name)
  local success, module = pcall(require, "jitlog_actions."..name)
  if not success then
    if string.find(module, "error loading") then
      print(module)
      os.exit(1)
    else
      -- Try the plain name it might be custom user module 
      module = require(name)
    end
  end
  return module
end

local function load_mixin(name, nested)
  assert(loaded_mixins[name] ~= false, "NYI circular mixins")
  if loaded_mixins[name] then
    return
  end
  loaded_mixins[name] = false
  local module = loadmodule(name)
  assert(module.actions or module.aftermsg)
  loaded_mixins[name] = module
  if module.mixins then
    for _, mixin_name in ipairs(module.mixins) do
      local mixin = load_mixin(mixin_name, true)
      if mixin then
        table.insert(mixins, mixin)
      end
    end
  end
  
  return module
end

for i, name in ipairs(action_names) do
  if string.find(name, "-") ~= 1 then
    -- Try load the name as built-in module first
    local module = loadmodule(name)
    
    local isaction
    if module.action_init then
      module.action_init(arg, i+1)
      isaction = true
    end

    if module.mixins then
      for _, mixin in ipairs(module.mixins) do
        if type(mixin) == "string" then
          if not loaded_mixins[name] then
            mixin = load_mixin(mixin)
          else
            -- already loaded
            mixin = nil
          end
        end
        if mixin then
          table.insert(mixins, mixin)
        end
      end
    end
    if not isaction and type(module.actions) == "table" then
      table.insert(mixins, module)
      loaded_mixins[name] = module
    else
      table.insert(actions, module)
    end
  end
end

local logfile, msg = io.open(logpath, "rb")
if not logfile then
  error("Error while opening jitlog '"..msg.."'")
end
local logbuffer = logfile:read("*all")
logfile:close()

local reader = readerlib.makereader(mixins)

for i, action in ipairs(actions) do
  if action.logopened then
    action.logopened(reader, logpath)
  end
end

local start = os.clock()
local success, msg
if debugger_attached then
  success = true
  reader:parse_buffer(logbuffer, #logbuffer)
else
  success, msg = pcall(reader.parse_buffer, reader, logbuffer, #logbuffer)
end
local stop = os.clock()
print("Log parsing took", stop - start)

if not success then
  print("Log Parser Error:", msg)
  if #reader.markers > 0 then
    print("Last Marker:", reader.markers[#reader.markers].label)
  end
  os.exit(1)
end

print("Traces:", #reader.traces)
print("Aborts:", #reader.aborts)
print("Flushes:", #reader.flushes)
print("Blacklisted:", #reader.proto_blacklist)
print("Events:", reader.eventid)

for i, action in ipairs(actions) do
  if action.logparsed then
    action.logparsed(reader)
  end
end

