lua_modules_path = "/modules"
lib_modules_path = "/modules"
rocks_subdir = "/packages"

variables = {}

if os_getenv then
    local path = os_getenv("LUAVM_BASEDIR")

    variables.LUA_BINDIR = path
    variables.LUA_INCDIR = path.."/include"
    variables.LUA_LIBDIR = path
end

if platform == "windows" or (platforms and platforms["windows"]) then
    variables.LUALIB = "lua51.lib"
else
    variables.LUALIB = "lua51.so"
end

