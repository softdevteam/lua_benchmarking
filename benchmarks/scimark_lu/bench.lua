------------------------------------------------------------------------------
-- Lua SciMark (2010-03-15).
--
-- A literal translation of SciMark 2.0a, written in Java and C.
-- Credits go to the original authors Roldan Pozo and Bruce Miller.
-- See: http://math.nist.gov/scimark2/
------------------------------------------------------------------------------
local scimarklib = require"scimarklib_ffi"
local iarray, matrix_copy = scimarklib.iarray, scimarklib.matrix_copy
local abs = math.abs
local scimark_size = 100

-----------------------------------------------------------------------------
-- LU: Dense Matrix Factorization.
------------------------------------------------------------------------------

local function lu_factor(a, pivot, m, n)
  local min_m_n = m < n and m or n
  for j=1,min_m_n do
    local jp, t = j, abs(a[j][j])
    for i=j+1,m do
      local ab = abs(a[i][j])
      if ab > t then
        jp = i
        t = ab
      end
    end
    pivot[j] = jp
    if a[jp][j] == 0 then error("zero pivot") end
    if jp ~= j then a[j], a[jp] = a[jp], a[j] end
    if j < m then
      local recp = 1.0 / a[j][j]
      for k=j+1,m do
        local v = a[k]
        v[j] = v[j] * recp
      end
    end
    if j < min_m_n then
      for i=j+1,m do
        local vi, vj = a[i], a[j]
        local eij = vi[j]
        for k=j+1,n do 
          vi[k] = vi[k] - eij * vj[k] 
        end
      end
    end
  end
end


local function LU(n)
  scimarklib.rand_init(scimarklib.RANDOM_SEED)
  local mat = scimarklib.random_matrix(n, n)
  local tmp = scimarklib.matrix_alloc(n, n)
  local pivot = iarray(n+1)
  return function(cycles)
    for i=1,cycles do
      matrix_copy(tmp, mat, n, n)
      lu_factor(tmp, pivot, n, n)
    end
    return 2.0/3.0*n*n*n*cycles
  end
end

local func = LU(scimark_size)

function run_iter(n)
  return (func(n))
end
