-- "I hereby put all Lua/LuaJIT tests and benchmarks that I wrote under the public domain." Mike Pall
-- https://github.com/LuaJIT/LuaJIT-test-cleanup
local function Ack(m, n)
  if m == 0 then return n+1 end
  if n == 0 then return Ack(m-1, 1) end
  return Ack(m-1, (Ack(m, n-1))) -- The parentheses are deliberate.
end

function run_iter(N)
    local result = Ack(3,N)
    return result
end
