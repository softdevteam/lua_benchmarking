local json = require "json"

local luafile = io.open("benchdata/simpledata.json", "rb")
local text = luafile:read("*all")
luafile:close()

function run_iter(n)
    for n=1, n do
        json.decode(text)
    end
end