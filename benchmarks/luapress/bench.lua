local luapress = require('luapress')
local util = require('luapress.util')
local template = require('luapress.template')

if pcall(require, "jit.opt") then
  require("jit.opt").start("maxmcode=8000", "maxtrace=5000")
end

local old_open = io.open

function io.open(path, mode)
  print(string.format([[  "%s",]], path))
  return old_open(path, mode)
end

if true then

function util.write_html(destination, object, templates)
    -- Write the file
    --assert(false)
   -- print('\t' .. object.title)
    local output = template:process(templates.header, templates[object.template], templates.footer)
    assert(#output ~= 0)
end

function util.copy_dir(directory, destination)
  assert(#directory > 0 and #destination > 0)
  return
end

function util.copy_file(source, destination)
  assert(#source > 0 and #destination > 0)
  return
end

end

-- Load the local config file into a global variable.
config = require('site.config')

config.base = "benchmarks/luapress/site"
config.build_dir = "build"
config.root = "benchmarks/luapress/site"
config.cache = false

-- Get the default config, apply them to config
local default_config = require('luapress.default_config')
for key, value in pairs(default_config) do
    if config[key] == nil then
        config[key] = value
    end
end

local source_paths = {
  "site/templates/pointless_v10/archive.mustache",
  "site/templates/pointless_v10/footer.lhtml",
  "site/templates/pointless_v10/header.lhtml",
  "site/templates/pointless_v10/page.mustache",
  "site/templates/pointless_v10/post.mustache",
  "site/templates/pointless_v10/rss.mustache",
  "site/posts/2017.md",
  "site/posts/Agentless Everywhere.md",
  "site/posts/Benchmarking in Virtual Machines.md",
  "site/posts/Democracy is a Farce.md",
  "site/posts/El Capitan.md",
  "site/posts/Introducing mdoc.md",
  "site/posts/Learning by Working.md",
  "site/posts/Learning New Things.md",
  "site/posts/Learning to Chef.md",
  "site/posts/Lua vs Node vs LuaNginx.md",
  "site/posts/Luapress v3.2.md",
  "site/posts/My PRISM Fallout.md",
  "site/posts/pngquant vs pngcrush vs optipng vs pngnq.md",
  "site/posts/Querying 50k Gameservers in 100 lines of Python.md",
  "site/posts/selected.js.md",
  "site/posts/Starting a Business for $250.md",
  "site/posts/Switching to Windows Phone.md",
  "site/posts/Welcome to Pointless Ramblings 2.md",
  "site/posts/What Mailchimp Considers an Email.md",
  "site/posts/Why Always Docker.md",
  "site/posts/Why Always Electron.md",
  "site/posts/Work Life Balance.md",
  "site/pages/Copyright.md",
  "site/pages/Info.md",
  "site/pages/iPhone to Lumia.md",
  "site/pages/Work.md",
}

local sources = {}
local lookup = {}

for i,path in ipairs(source_paths) do
    path = "benchmarks/luapress/"..path
    local luafile = io.open("benchmarks/luapress/"..path, "rb")
    local content = luafile:read("*all")
    table.insert(sources, content)
    lookup[path] = i
    luafile:close()
end


function run_iter(count)
    luapress.make_build()

    -- Build the blog
    local status, err = luapress.build()
    assert(not err, err)
end
