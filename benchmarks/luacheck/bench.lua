local luacheck = require "luacheck" 

local luafile = io.open("benchmarks/luacheck/ZeroBraneStudio.lua", "rb")
local content = luafile:read("*all")
luafile:close()

function run_iter(count)
    for i=1,count do
        local report = luacheck.get_report(content)

        local options = {
            cache = false,
        }
        local processed_report = luacheck.process_reports({report}, options)
        assert(processed_report.warnings > 0)
    end
end
