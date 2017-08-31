-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
-- modified by Sokolov yura

--collectgarbage("setstepmul", 0) -- sometimes it helps much. For this benchmark ~ 10%

MIN_DEPTH = 4
MAX_DEPTH = 12
EXPECT_CKSUM = -10914

local function BottomUpTree(item, depth)
  if depth > 0 then
    local i = item + item
    depth = depth - 1
    local left, right = BottomUpTree(i-1, depth), BottomUpTree(i, depth)
    return { item, left, right }
  else
    return { item } -- Faster for LuaJIT: return { item, false }
  end
end

local function ItemCheck(tree)
  if #tree == 3 then -- Faster for LuaJIT: if tree[2] then
    return tree[1] + ItemCheck(tree[2]) - ItemCheck(tree[3])
  else
    return tree[1]
  end
end

local function inner_iter(mindepth, maxdepth)
    local check = 0

	do
	  local stretchdepth = maxdepth + 1
	  local stretchtree = BottomUpTree(0, stretchdepth)
	  check = check +ItemCheck(stretchtree)
	end

	local longlivedtree = BottomUpTree(0, maxdepth)

	for depth=mindepth,maxdepth,2 do
	  local iterations = 2 ^ (maxdepth - depth + mindepth)
	  for i=1,iterations do
	    check = check + ItemCheck(BottomUpTree(1, depth)) +
		    ItemCheck(BottomUpTree(-1, depth))
	  end
	end

	check = check + ItemCheck(longlivedtree)

    if check ~= EXPECT_CKSUM then
        puts("bad checksum: " .. checksum .. " vs " .. EXPECT_CKSUM)
        os.exit(1)
    end
end

function run_iter(n)
    local i
    for i=1,n do
        inner_iter(MIN_DEPTH, MAX_DEPTH)
    end
end
