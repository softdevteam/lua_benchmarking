------------------------------------------------------------------------------
-- Lua SciMark (2010-03-15).
--
-- A literal translation of SciMark 2.0a, written in Java and C.
-- Credits go to the original authors Roldan Pozo and Bruce Miller.
-- See: http://math.nist.gov/scimark2/
------------------------------------------------------------------------------
local scimarklib = require"scimarklib_ffi"
local iarray, darray = scimarklib.iarray, scimarklib.darray
local scimark_size1, scimark_size2 = 1000, 5000
local floor = math.floor

------------------------------------------------------------------------------
-- Sparse Matrix Multiplication.
------------------------------------------------------------------------------
local function sparse_mult(n, cycles, vy, val, row, col, vx)
  for p=1,cycles do
    for r=1,n do
      local sum = 0
      for i=row[r],row[r+1]-1 do 
        sum = sum + vx[col[i]] * val[i] 
      end
      vy[r] = sum
    end
  end
end

local function SPARSE(n, nz)
  scimarklib.rand_init(scimarklib.RANDOM_SEED)
  local nr = floor(nz/n)
  local anz = nr*n
  local vx = scimarklib.random_vector(n)
  local val = scimarklib.random_vector(anz)
  local vy, col, row = darray(n+1), iarray(nz+1), iarray(n+2)
  row[1] = 1
  for r=1,n do
    local step = floor(r/nr)
    if step < 1 then step = 1 end
    local rr = row[r]
    row[r+1] = rr+nr
    for i=0,nr-1 do 
      col[rr+i] = 1+i*step 
    end
  end
  return function(cycles)
    sparse_mult(n, cycles, vy, val, row, col, vx)
    return anz*cycles*2
  end
end


local func = SPARSE(scimark_size1, scimark_size2)

function run_iter(n)
  return (func(n))
end
