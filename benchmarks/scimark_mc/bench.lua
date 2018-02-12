------------------------------------------------------------------------------
-- Lua SciMark (2010-03-15).
--
-- A literal translation of SciMark 2.0a, written in Java and C.
-- Credits go to the original authors Roldan Pozo and Bruce Miller.
-- See: http://math.nist.gov/scimark2/
------------------------------------------------------------------------------
local scimarklib = require"scimarklib_ffi"

local function mc_integrate(cycles)
  local under_curve = 0
  local rand = scimarklib.rand
  for i=1,cycles do
    local x = rand()
    local y = rand()
    if x*x + y*y <= 1.0 then under_curve = under_curve + 1 end
  end
  return (under_curve/cycles) * 4
end

function MC(cycles)
  local res = mc_integrate(cycles)
  assert(math.sqrt(cycles)*math.abs(res-math.pi) < 5.0, "bad MC result")
  return cycles * 4 -- Way off, but same as SciMark in C/Java.
end
 
scimarklib.rand_init(scimarklib.RANDOM_SEED)
 
function run_iter(n)
  return (MC(n))
end
