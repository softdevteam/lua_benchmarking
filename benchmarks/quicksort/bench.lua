local array_generator = require("array_generator")
local qsort = require"qsort"
require("table.new")

local function cmp(a, b)
  return a < b
end

function run_iter(n)
  if not list then
    list = table.new(n, 0)
  end

  array_generator.fill_random_int(list, n)
  qsort(list, cmp, n)
  assert(list[1] <= list[2])
  assert(list[n-2] <= list[n-1])

  array_generator.fill_sorted(list, n)
  qsort(list, cmp, n)
  assert(list[1] <= list[2])
  assert(list[n-2] <= list[n-1])

  array_generator.fill_reverse_sorted(list, n)
  qsort(list, cmp, n)
  assert(list[1] <= list[2])
  assert(list[n-2] <= list[n-1])

  return
end
