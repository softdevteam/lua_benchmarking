
local function kfrequency(seq, freq, k, frame)
  local sub = string.sub
  local k1 = k - 1
  for i=frame,#seq-k1,k do
    local c = sub(seq, i, i+k1)
    freq[c] = (freq[c] or 0) + 1
  end
end

local function count(seq, frag)
  local k = #frag
  local freq = {}
  for frame=1,k do kfrequency(seq, freq, k, frame) end
  return freq[frag] or 0 
end

local function frequency(seq, k)
  local freq = {}
  for frame=1,k do kfrequency(seq, freq, k, frame) end
  local sfreq, sn, sum = {}, 1, 0
  for c,v in pairs(freq) do sfreq[sn] = c; sn = sn + 1; sum = sum + v end
  table.sort(sfreq, function(a, b)
    local fa, fb = freq[a], freq[b]
    return fa == fb and a > b or fa > fb
  end)
  local ret = 0
  for _,c in ipairs(sfreq) do
    ret = ret + ((freq[c]*100)/sum)
  end
  return ret
end

local function readseq()
  local sub = string.sub
  local lines, ln = {}, 0
  for line in io.lines("benchdata/fasta_output.txt") do
    local c = sub(line, 1, 1)
    if c == ">" then
      break
    elseif c ~= ";" then
      ln = ln + 1
      lines[ln] = line
    end
  end
  return string.upper(table.concat(lines, "", 1, ln))
end

local seq = readseq()

local function run()
    frequency(seq, 1)
    frequency(seq, 2)
    assert(count(seq, "GGT") == 2950)
    assert(count(seq, "GGTA") == 895)
    assert(count(seq, "GGTATT") == 90)
    assert(count(seq, "GGTATTTTAATT") == 2)
    assert(count(seq, "GGTATTTTAATTTATAGT") == 2)
end

function run_iter(n)
    for i=1,n do
       run() 
    end
end
