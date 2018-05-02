local action = {}

local function printf(...)
    print(string.format(...))
end

function action.logparsed(jlog)
  for i, ptbl in ipairs(jlog.proto_blacklist) do
    if ptbl.bcindex == 0 then
      printf("ProtoBL #%d: %s", i, ptbl.proto:get_location())
    else
      printf("ProtoBL(LOOP) #%d: %s, LoopLine: %d", i, ptbl.proto:get_location(), ptbl.proto:get_linenumber(ptbl.bcindex))
    end
    for i, abort in ipairs(jlog.aborts) do
      if abort.startpt == ptbl.proto and abort.parentid == 0 then
        printf("  AbortedTrace(%d): reason= '%s',  stop= %s:%d", abort.id, abort.abortreason, abort.stoppt.chunk, abort.stoppt:get_linenumber(abort.stoppc))
      end
    end 
  end
end

return action
