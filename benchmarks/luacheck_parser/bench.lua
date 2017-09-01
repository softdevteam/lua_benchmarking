local luacheck = require "luacheck"

local source_paths = {
    "main.lua",
    "config.lua",
    "version.lua",
    "editor/commands.lua",
    "editor/print.lua",
    "editor/outline.lua",
    "editor/markers.lua",
    "editor/editor.lua",
    "editor/filetree.lua",
    "editor/toolbar.lua",
    "editor/commandbar.lua",
    "editor/keymap.lua",
    "editor/menu_search.lua",
    "editor/style.lua",
    "editor/settings.lua",
    "editor/package.lua",
    "editor/findreplace.lua",
    "editor/singleinstance.lua",
    "editor/proto.lua",
    "editor/output.lua",
    "editor/autocomplete.lua",
    "editor/debugger.lua",
    "editor/menu_edit.lua",
    "editor/menu_project.lua",
    "editor/markup.lua",
    "editor/shellbox.lua",
    "editor/menu_help.lua",
    "editor/iofilters.lua",
    "editor/gui.lua",
    "editor/ids.lua",
    "editor/menu_file.lua",
    "editor/menu_view.lua",
    "editor/inspect.lua",
    "util.lua",
}

local sources = {}

for i,path in ipairs(source_paths) do
    local luafile = io.open("benchdata/ZeroBraneSrc/"..path, "rb")
    local content = luafile:read("*all")
    table.insert(sources, content)
    luafile:close()   
end

local parser = require "luacheck.parser"   
    
function run_iter(count)
    for i=1,count do  
        local asts = {}
        
        for _, filebuf in ipairs(sources) do
            table.insert(asts, {parser.parse(filebuf)})
        end
    end
end
