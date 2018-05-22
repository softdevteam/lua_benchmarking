local array_generator = require("array_generator")
local hsort = require"heapsort"
require("table.new")

local function cmp(a, b)
  return a > b
end

local list

function run_iter(n)
  if not list then
    list = table.new(n, 0)
  end

  array_generator.fill_random_int(list, n)
  local result = hsort(list, cmp, n)
  assert(result[1] <= result[2])
  assert(result[n-2] <= result[n-1])

  array_generator.fill_sorted(list, n)
  result = hsort(list, cmp, n)
  assert(result[1] <= result[2])
  assert(result[n-2] <= result[n-1])

  array_generator.fill_reverse_sorted(list, n)
  result = hsort(list, cmp, n)
  assert(result[1] <= result[2])
  assert(result[n-2] <= result[n-1])

  return
end
