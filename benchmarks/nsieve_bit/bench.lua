-- "I hereby put all Lua/LuaJIT tests and benchmarks that I wrote under the public domain." Mike Pall
-- https://github.com/LuaJIT/LuaJIT-test-cleanup
local bit = require("bit")
local band, bxor, rshift, rol = bit.band, bit.bxor, bit.rshift, bit.rol
require("table.new")
require("table.clear")

local function nsieve(p, m)
  local count = 0
  for i=0,rshift(m, 5) do p[i] = -1 end
  for i=2,m do
    if band(rshift(p[rshift(i, 5)], i), 1) ~= 0 then
      count = count + 1
      for j=i+i,m,i do
        local jx = rshift(j, 5)
        p[jx] = band(p[jx], rol(-2, j))
      end
    end
  end
  return count
end

local primes

function run_iter(N)
  if N < 2 then N = 2 end
  local size = (2^N)*10000
  -- The GC is not able to handle us recreating this table each time. It screws with the GC heap 
  -- size trigger values until we hit 2gb and crash
  if not primes then
    primes = table.new(size, 0)
  end
  table.clear(primes)
  for i=0,2 do
    local m = (2^(N-i))*10000
    nsieve(primes, m)
  end
end
