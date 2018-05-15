local action = {
    mixins = {},
}

local function printf(...)
    print(string.format(...))
end

function action.action_init(args, argstart)
    print("TODO: action_init")
end

local function makestats_func(otype, stat)
  return function(jlog)
    local t = table.new(jlog.itern, 0)
    for _, marker in ipairs(jlog.markers) do
      if marker.label == "BEGIN" then
        t[marker.n+1] = marker.gcstats and marker.gcstats[otype][stat] or 0
      end
    end
    return t
  end
end

local fields = {
  "acount",
  "fcount",
  "atotal",
  "ftotal",
}

local objtype_stats = {
"string",
"upvalue",
"thread",
"proto",
"function",
"cdata",
"table",
"table_hash",
"table_array",
}

function action.logopened(reader, jlogpath)
    reader.ktimers = reader.ktimers or {}
    
    for _, otype in ipairs(objtype_stats) do
      for _, field in ipairs(fields) do
        reader.ktimers[otype.."_"..field] = makestats_func(otype,  field)
      end
    end
    
   -- reader.ktimers.tarray_atotal = makestats_func("table_array", "atotal")
    --reader.ktimers.thash_atotal = makestats_func("table_hash", "atotal")
    
    --reader.ktimers.tarray_acount = makestats_func("table_array", "acount")
    --reader.ktimers.thash_acount = makestats_func("table_hash", "acount")
end

function action.logparsed(jlog)
    print("TODO: logparsed")
end

return action
