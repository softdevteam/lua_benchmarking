-- "I hereby put all Lua/LuaJIT tests and benchmarks that I wrote under the public domain." Mike Pall
-- https://github.com/LuaJIT/LuaJIT-test-cleanup
local sqrt, sin, cos = math.sqrt, math.sin, math.cos

function run_iter(n)
  local a1, a2, a3, a4, a5, a6, a7, a8, a9, alt = 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
  
  for k=1,n do
    local k2, sk, ck = k*k, sin(k), cos(k)
    local k3 = k2*k
    a1 = a1 + (2/3)^k
    a2 = a2 + 1/sqrt(k)
    a3 = a3 + 1/(k2+k)
    a4 = a4 + 1/(k3*sk*sk)
    a5 = a5 + 1/(k3*ck*ck)
    a6 = a6 + 1/k
    a7 = a7 + 1/k2
    a8 = a8 + alt/k
    a9 = a9 + alt/(k+k-1)
    alt = -alt
  end
  
  assert(a1 ~= 0)
  assert(a2 ~= 0)
  assert(a3 ~= 0)
  assert(a4 ~= 0)
  assert(a5 ~= 0)
  assert(a6 ~= 0)
  assert(a7 ~= 0)
  assert(a8 ~= 0)
  assert(a9 ~= 0)
  return
end
