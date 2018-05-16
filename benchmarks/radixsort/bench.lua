-- LSD radix sort implementation based on code from http://www.osix.net/modules/article/?id=704
local array_generator = require("array_generator")
local band, rshift = bit.band, bit.rshift

local mask = 0xff
local counts_size = 256
local counts = ffi.new("int32_t[?]", counts_size)

local function count_mask(list, n, shift)
  ffi.fill(counts, counts_size * 4, 0)
  for i = 0, n-1 do
    local index = band(rshift(list[i], shift), mask)
    counts[index] = counts[index] + 1
  end
end

local prefix_sums = ffi.new("int32_t[?]", counts_size, {0})

local function radixsort(list, size)
  local temp = ffi.new("int64_t[?]", size)
  local shift = 0

  for n = 0, 7 do
    count_mask(list, size, shift)

    prefix_sums[0] = 0
    for i = 1, counts_size-1 do
      prefix_sums[i] = prefix_sums[i-1] + counts[i-1]
    end

    for i = 0, size-1 do
      local bin = band(rshift(list[i], shift), mask)
      local index = prefix_sums[bin]
      prefix_sums[bin] = index + 1
      temp[index] = list[i]
    end

    ffi.copy(list, temp, size * 8)
    shift = shift + 8
  end
end

local list

function run_iter(n)
  if not list then
    list = ffi.new("int64_t[?]", n)
  end
  array_generator.fill_random_int(list, n)
  radixsort(list, n)
  assert(list[0] <= list[1])
  assert(list[n-2] <= list[n-1])
  
  array_generator.fill_sorted(list, n)
  radixsort(list, n)
  assert(list[0] <= list[1])
  assert(list[n-2] <= list[n-1])

  array_generator.fill_reverse_sorted(list, n)
  radixsort(list, n)
  assert(list[0] <= list[1])
  assert(list[n-2] <= list[n-1])

  return
end
