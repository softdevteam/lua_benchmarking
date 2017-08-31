-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
-- contributed by Mike Pall


local SPECTRAL_N = 1000
local EXPECT_CKSUM = 1.2742241481294835914184204739285632967948913574218750

local function A(i, j)
  local ij = i+j-1
  return 1.0 / (ij * (ij-1) * 0.5 + i)
end

local function Av(x, y, N)
  for i=1,N do
    local a = 0
    for j=1,N do a = a + x[j] * A(i, j) end
    y[i] = a
  end
end

local function Atv(x, y, N)
  for i=1,N do
    local a = 0
    for j=1,N do a = a + x[j] * A(j, i) end
    y[i] = a
  end
end

local function AtAv(x, y, t, N)
  Av(x, t, N)
  Atv(t, y, N)
end

local function inner_iter(N)
  local u, v, t = {}, {}, {}
  for i=1,N do u[i] = 1 end

  for i=1,10 do AtAv(u, v, t, N) AtAv(v, u, t, N) end

  local vBv, vv = 0, 0
  for i=1,N do
    local ui, vi = u[i], v[i]
    vBv = vBv + ui*vi
    vv = vv + vi*vi
  end
  return math.sqrt(vBv / vv)
end

function run_iter(n)
  local i
  for i=1,n  do
    local checksum = inner_iter(SPECTRAL_N);
    if checksum ~= EXPECT_CKSUM then
      io.write("bad checksum: " .. checksum .. " vs " .. EXPECT_CKSUM)
      os.exit(1)
    end
  end
end
