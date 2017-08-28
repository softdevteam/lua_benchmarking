-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall
table.unpack = table.unpack or unpack

local SCALE = 10000
local EXPECT_CKSUM = 9611973
local checksum = 0

local INITIAL_STATE = 42
local Last = INITIAL_STATE
local function random(max)
  local y = (Last * 3877 + 29573) % 139968
  Last = y
  return (max * y) / 139968
end

function wrap_write(...)
  local str = table.concat({...})
  local i
  for i = 1, #str do
    checksum = checksum + string.byte(str, i)
  end
end

local function make_repeat_fasta(s, n)
  local sub = string.sub
  local p, sn, s2 = 1, #s, s..s
  for i=60,n,60 do
    wrap_write(sub(s2, p, p + 59), "\n")
    sub(s2, p, p + 59)
    p = p + 60; if p > sn then p = p - sn end
  end
  local tail = n % 60
  if tail > 0 then wrap_write(sub(s2, p, p + tail-1), "\n") end
end

local function make_random_fasta(bs, n)
  load([=[
    local char, unpack, n, random = string.char, table.unpack, ...
    local buf, p = {}, 1
    for i=60,n,60 do
      for j=p,p+59 do ]=]..bs..[=[ end
      buf[p+60] = 10; p = p + 61
      if p >= 2048 then
          wrap_write(char(unpack(buf, 1, p-1)));
          p = 1
      end
    end
    local tail = n % 60
    if tail > 0 then
      for j=p,p+tail-1 do ]=]..bs..[=[ end
      p = p + tail; buf[p] = 10; p = p + 1
    end
    wrap_write(char(unpack(buf, 1, p-1)))
    char(unpack(buf, 1, p-1))
  ]=], desc)(n, random)
end

local function bisect(c, p, lo, hi)
  local n = hi - lo
  if n == 0 then return "buf[j] = "..c[hi].."\n" end
  local mid = math.floor(n / 2)
  return "if r < "..p[lo+mid].." then\n"..bisect(c, p, lo, lo+mid)..
         "else\n"..bisect(c, p, lo+mid+1, hi).."end\n"
end

local function make_bisect(tab)
  local c, p, sum = {}, {}, 0
  for i,row in ipairs(tab) do
    c[i] = string.byte(row[1])
    sum = sum + row[2]
    p[i] = sum
  end
  return "local r = random(1)\n"..bisect(c, p, 1, #tab)
end

local alu =
  "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"..
  "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"..
  "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"..
  "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"..
  "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"..
  "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"..
  "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

local iub = make_bisect{
  { "a", 0.27 },
  { "c", 0.12 },
  { "g", 0.12 },
  { "t", 0.27 },
  { "B", 0.02 },
  { "D", 0.02 },
  { "H", 0.02 },
  { "K", 0.02 },
  { "M", 0.02 },
  { "N", 0.02 },
  { "R", 0.02 },
  { "S", 0.02 },
  { "V", 0.02 },
  { "W", 0.02 },
  { "Y", 0.02 },
}

local homosapiens = make_bisect{
  { "a", 0.3029549426680 },
  { "c", 0.1979883004921 },
  { "g", 0.1975473066391 },
  { "t", 0.3015094502008 },
}

function run_iter(N)
  for i=0,N-1 do  -- inclusive upper bound
      make_repeat_fasta(alu, SCALE*2)
      make_random_fasta(iub, SCALE*3)
      make_random_fasta(homosapiens, SCALE*5)

      if checksum ~= EXPECT_CKSUM then
        print("bad checksum:", EXPECT_CKSUM, "vs", checksum)
        os.exit(1)
      end

      Last = INITIAL_STATE
      checksum = 0;
  end

end
