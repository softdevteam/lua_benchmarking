-- "I hereby put all Lua/LuaJIT tests and benchmarks that I wrote under the public domain." Mike Pall
-- https://github.com/LuaJIT/LuaJIT-test-cleanup
require"table.new"
require("table.clear")

local function nsieve(p, m)
  for i=2,m do p[i] = true end
  local count = 0
  for i=2,m do
    if p[i] then
      for k=i+i,m,i do p[k] = false end
      count = count + 1
    end
  end
  return count
end

local primes

function run_iter(N)
  if N < 2 then N = 2 end
  local size = (2^N)*10000
  -- The GC is not able to handle us recreating this table each time. it screws with the GC heap size trigger values 
  -- until we hit 2gb and crash
  if not primes then
    primes = table.new(size, 0)
  end
  table.clear(primes)
  for i=0,2 do
    local m = (2^(N-i))*10000
    nsieve(primes, m)
  end
end
