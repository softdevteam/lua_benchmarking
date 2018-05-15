local moonscript = require "moonscript"
local parse = require("moonscript.parse")
local compile = require("moonscript.compile")

if pcall(require, "jit.opt") then
  require("jit.opt").start("maxmcode=8000", "maxtrace=5000")
end

local source_paths = {
  "base.moon",
  "compile.moon",
  "data.moon",
  "dump.moon",
  "errors.moon",
  "init.moon",
  "line_tables.moon",
  "parse.moon",
  "transform.moon",
  "types.moon",
  "util.moon",
  "version.moon",
  "cmd/args.moon",
  "cmd/coverage.moon",
  "cmd/lint.moon",
  "cmd/moonc.moon",
  "cmd/watchers.moon",
  "compile/statement.moon",
  "compile/value.moon",
  "parse/env.moon",
  "parse/literals.moon",
  "parse/util.moon",
  "transform/accumulator.moon",
  "transform/class.moon",
  "transform/comprehension.moon",
  "transform/destructure.moon",
  "transform/names.moon",
  "transform/statement.moon",
  "transform/statements.moon",
  "transform/transformer.moon",
  "transform/value.moon",
}

local sources = {}

for i,path in ipairs(source_paths) do
    local luafile = io.open("benchdata/moonscript/"..path, "rb")
    local content = luafile:read("*all")
    table.insert(sources, content)
    luafile:close()
end

local function compile_text(text)
  local tree, err = parse.string(text)
  assert(tree, err)

  local code, posmap_or_err, err_pos = compile.tree(tree)
  if not (code) then
    error(compile.format_error(posmap_or_err, err_pos, text))
  end
  assert(#code > 0)
  return
end

function run_iter(count)
  for name, text in ipairs(sources) do
    compile_text(text)
  end
end
