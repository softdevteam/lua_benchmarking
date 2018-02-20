@echo off

setlocal enableextensions enabledelayedexpansion

set BenchRoot=%~dp0

if not exist "luajit_repo" (
    git clone https://github.com/softdevteam/LuaJIT luajit_repo
)

if not defined INCLUDE (
  echo No MSVC env vars set attempting to find vcvars.bat using system installed vswhere.exe
  set vswhere="%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
  
  if exist !vswhere! (
    for /f "usebackq tokens=*" %%i in (`!vswhere! -requires Microsoft.VisualStudio.ComponentGroup.NativeDesktop.Core -property installationPath`) do (
      if exist "%%i" set vsdir=%%i
    )

    if exist "!vsdir!\VC\Auxiliary\Build\vcvars64.bat" (
      set "VSCMD_START_DIR=%BenchRoot%"
      echo Running vcvars64 found in !vsdir! with vswhere.exe
      call "!vsdir!\VC\Auxiliary\Build\vcvars64.bat"
    )
  )
)

if not defined INCLUDE (
  echo No VC compiler found
  EXIT /B 1
)

echo --------- Building LuaJIT --------------------

cd "%BenchRoot%luajit_repo\src"

del *.pdb *.lib *.dll *.exp *.exe
call msvcbuild.bat
call :copy_luajit "normal"

del *.pdb *.lib *.dll *.exp *.exe
call msvcbuild.bat gc64
call :copy_luajit "gc64"

cd ..\..

set LUAROCKS_CONFIG=%BenchRoot%rocks_config.lua
set LUAVM_BASEDIR=%BenchRoot%builds\normal
luarocks install --tree rocks https://raw.githubusercontent.com/fsfod/luachild/master/luachild-0.1-1.rockspec

EXIT

:copy_luajit
if exist "luajit.exe" (
  mkdir ..\..\builds\%~1
  xcopy luajit.exe ..\..\builds\%~1 /y
  xcopy lua51.dll ..\..\builds\%~1 /y
  xcopy lua51.pdb ..\..\builds\%~1 /y
  xcopy lua51.lib ..\..\builds\%~1 /y  
  xcopy jit\*.lua ..\..\builds\%~1\jit /y /i

  mkdir ..\..\builds\%~1\include
  xcopy luaconf.h ..\..\builds\%~1\include /y
  xcopy lua.h ..\..\builds\%~1\include /y
  xcopy luajit.h ..\..\builds\%~1\include /y
  xcopy lualib.h ..\..\builds\%~1\include /y
  xcopy lauxlib.h ..\..\builds\%~1\include /y
) ELSE (
  @echo LuaJIT %~1 build failed
  EXIT /B 1
)
EXIT /B 0
