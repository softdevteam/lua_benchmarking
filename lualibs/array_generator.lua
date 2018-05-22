local random = math.random
local lib = {
  randomseed = 2654435761,
}

local function check_args(list, size, start, stop)
  if type(list) == "table" then
    size = size or #list
    start = start or 1
    stop = stop or size
  else
    assert(type(list) == "cdata")
    assert(size > 0, "bad list size")
    start = start or 0
    stop = stop or size-1
  end
  assert(size ~= 0)
  return start, stop, size
end

function lib.fill_sorted(list, size)
  local start, stop
  start, stop, size = check_args(list, size)
  for i = start, stop do
    list[i] = i
  end
end

function lib.fill_reverse_sorted(list, size)
  local start, stop
  start, stop, size = check_args(list, size)
  for i = start, stop do
    list[i] = size-i
  end
end

function lib.fill_random(list, size)
  local start, stop
  start, stop, size = check_args(list, size)
  math.randomseed(lib.randomseed)
  for i = start, stop do
    list[i] = random()
  end
end

function lib.fill_random_int(list, size)
  local start, stop
  start, stop, size = check_args(list, size)
  math.randomseed(lib.randomseed)
  for i = start, stop do
    list[i] = random(0xFFFFFFF)
  end
end

function lib.fill_random_int64(list, size)
  local start, stop
  start, stop, size = check_args(list, size)
  math.randomseed(lib.randomseed)
  for i = start, stop do
    list[i] = random(281474976710655)
  end
end

function lib.assert_sorted(list, size, start, stop)
  start, stop, size = check_args(list, size, start, stop)
  for i = start, stop-1 do
    if list[i] > list[i+1] then
      error(string.format("List not sorted at index %d = [-1]: %s, [i]: %s, [i+1]%s", i, list[i-1], list[i], list[i+1]))
    end
  end
end
return lib
