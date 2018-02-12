------------------------------------------------------------------------------
-- Lua SciMark (2010-03-15).
--
-- A literal translation of SciMark 2.0a, written in Java and C.
-- Credits go to the original authors Roldan Pozo and Bruce Miller.
-- See: http://math.nist.gov/scimark2/
------------------------------------------------------------------------------
local bit = require("bit")
local band, sar = bit.band, bit.arshift

local darray = ffi.typeof("double[?]")
local iarray = ffi.typeof("int[?]")

local lib = {
  RANDOM_SEED = 101009, -- Must be odd.
  darray = darray,
  iarray = iarray,
}

-- LJ2 has bit operations and zero-based arrays (internally).
local Rm, Rj, Ri = {}, 0, 0
for i=0,16 do Rm[i] = 0 end
function lib.rand_init(seed)
  Rj, Ri = 16, 11
  for i=16,0,-1 do
    seed = band(seed*9069, 0x7fffffff)
    Rm[i] = seed
  end
end

local function rand()
  local i = band(Ri+1, sar(Ri-16, 31))
  local j = band(Rj+1, sar(Rj-16, 31))
  Ri, Rj = i, j
  local k = band(Rm[i] - Rm[j], 0x7fffffff)
  Rm[j] = k
  return k * (1.0/2147483647.0)
end
lib.rand = rand

function lib.random_vector(n)
  local v = darray(n+1)
  for x=1,n do v[x] = rand() end
  return v
end

function lib.random_matrix(m, n)
  local a = {}
  for y=1,m do
    local v = darray(n+1)
    a[y] = v
    for x=1,n do v[x] = rand() end
  end
  return a
end

function lib.matrix_alloc(m, n)
  local a = {}
  for y=1,m do 
    a[y] = darray(n+1) 
  end
  return a
end

function lib.matrix_copy(dst, src, m, n)
  for y=1,m do
    local vd, vs = dst[y], src[y]
    for x=1,n do 
      vd[x] = vs[x] 
    end
  end
end

return lib
