------------------------------------------------------------------------------
-- Lua SciMark (2010-03-15).
--
-- A literal translation of SciMark 2.0a, written in Java and C.
-- Credits go to the original authors Roldan Pozo and Bruce Miller.
-- See: http://math.nist.gov/scimark2/
------------------------------------------------------------------------------
local scimarklib = require"scimarklib_ffi"
local scimark_size = 100

local function sor_run(mat, m, n, cycles, omega)
  local om4, om1 = omega*0.25, 1.0-omega
  m = m - 1
  n = n - 1
  for i=1,cycles do
    for y=2,m do
      local v, vp, vn = mat[y], mat[y-1], mat[y+1]
      for x=2,n do
        v[x] = om4*((vp[x]+vn[x])+(v[x-1]+v[x+1])) + om1*v[x]
      end
    end
  end
end

local function SOR(n)
  scimarklib.rand_init(scimarklib.RANDOM_SEED)
  local mat = scimarklib.random_matrix(n, n)
  return function(cycles)
    sor_run(mat, n, n, cycles, 1.25)
    return (n-1)*(n-1)*cycles*6
  end
end

local func = SOR(scimark_size)

function run_iter(n)
  return (func(n))
end
