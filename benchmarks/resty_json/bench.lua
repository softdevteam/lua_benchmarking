local ljson_decoder = require"json_decoder"
local instance = ljson_decoder.new()

local luafile = io.open("benchdata/simpledata.json", "rb")
local text = luafile:read("*all")
luafile:close()

function run_iter(count)
    for n=1, count do
        local result, err = instance:decode(text)
        assert(result and not err)
    end
end
