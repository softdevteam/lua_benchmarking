local action = {
    mixins = {},
}

local function printf(...)
    print(string.format(...))
end

function action.action_init(args, argstart)
    print("TODO: action_init")
end

function action.logopened(reader, jlogpath)
    print("TODO: logopened")
end

function action.logparsed(jlog)
    print("TODO: logparsed")
end

return action
