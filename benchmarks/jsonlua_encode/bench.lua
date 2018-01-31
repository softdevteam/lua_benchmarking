local json = require "json"
local list = {}

local bit = _G.bit or _G.bit32

for i=1, 1000 do
    local data = {
        i0 = 32,
        i1 = 16,
        i2 = 127,
        b0 = true,
        b1 = true,
        i3 = 65536,
        e0 = "enum3",
        s0 = {
            f0 = 3.14,
            f1 = 3.14159265358979,
        },
        l0 = {0, i, -i, 127 },
        t0 = "hello",
        e1 = "enum7",
    }

    -- Make the bool fields vary a bit to trigger side traces like real world data.
    if bit.band(i, 3) == 0 then
        data.b0 = false
    end

    if bit.band(i, 15) == 0 then
        data.b1 = false
    end

    list[i] = data
end

local t1list = { list = list }

function run_iter(n)
    for n=1, n do
        local json = json.encode(t1list)
        assert(#json > 0)
    end
end