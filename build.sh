#!/bin/bash

#Build the various interesting flavours of LuaJIT
if [ $# -gt 1 ]; then
  echo "Usage: build.sh [<LuaJIT source dir>]" 2>&1
  exit 1
fi

if [ $# -eq 1 ]; then
  ljsrc=$1/src
  echo "Using ${ljsrc} as LuaJIT source path"
else
  echo "Defaulting to ./luajit_repo/src as LuaJIT source path"
  ljsrc=luajit_repo/src

  if [ ! -d "luajit_repo" ]; then
    git clone https://github.com/softdevteam/LuaJIT luajit_repo
  fi

  if [ ! -d "raptorjit_repo" ]; then
    git clone https://github.com/raptorjit/raptorjit raptorjit_repo
  fi

  if [ -f "commitlock.txt" ]; then
    source commitlock.txt
    echo "Current luajit commit hash: ${luajit_commitid}"
    echo "Current raptorjit commit hash: ${raptorjit_commitid}"
    (cd luajit_repo && git checkout ${luajit_commitid})
    (cd raptorjit_repo && git checkout ${raptorjit_commitid})
  fi
fi

ljbins=${ljbins:=builds}

mkdir -p ${ljbins}
# Make directory path relative so we don't have to worry about what path we have to pass to make.
ljbins="$( cd "$ljbins" && pwd )"

function copy_binaries() {
  mkdir -p ${ljbins}/$1/jit/
  cp ${ljsrc}/luajit ${ljbins}/$1/luajit
  cp ${ljsrc}/libluajit.so ${ljbins}/$1/libluajit.so
  cp ${ljsrc}/jit/*.lua ${ljbins}/$1/jit/
  
  mkdir -p ${ljbins}/$1/include/
  cp ${ljsrc}/luaconf.h ${ljbins}/$1/include
  cp ${ljsrc}/lua.h ${ljbins}/$1/include
  cp ${ljsrc}/luajit.h ${ljbins}/$1/include
  cp ${ljsrc}/lualib.h ${ljbins}/$1/include
  cp ${ljsrc}/lauxlib.h ${ljbins}/$1/include
}

BASE_XCFLAGS=${BASE_XCFLAGS:=""}

#Unmodified build with 32 bit sized GC object pointers. Object allocation limited to the lower 4gb virtual address space
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS="${BASE_XCFLAGS}"
copy_binaries "normal"

#Build with JIT removed
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS="${BASE_XCFLAGS} -DLUAJIT_DISABLE_JIT"
copy_binaries "nojit"

##GC64 64 bit sized GC object pointer
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS="${BASE_XCFLAGS} -DLUAJIT_ENABLE_GC64"
copy_binaries "gc64"

## Build with dual number mode enabled
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS="${BASE_XCFLAGS} -DLUAJIT_NUMMODE=2"
copy_binaries "dualnum"

if [ -d "raptorjit_repo" ]; then
  make -C ./raptorjit_repo clean
  make -C ./raptorjit_repo -j HOST_LUA=${ljbins}/normal/luajit
  mkdir -p ${ljbins}/raptorjit/jit/
  cp ./raptorjit_repo/src/raptorjit ${ljbins}/raptorjit/luajit
  cp ./raptorjit_repo/src/libraptorjit.so ${ljbins}/raptorjit/libluajit.so
  cp ./raptorjit_repo/src/jit/*.lua ${ljbins}/raptorjit/jit/
fi

## Optional 32 bit build
if [ ${BUILD_LJ32} ]; then
  make -C ${ljsrc} clean
  make -C ${ljsrc} -j CC="gcc -m32" XCFLAGS="${BASE_XCFLAGS}"
  copy_binaries "luajit32"
fi

if [ ${BUILDALL} ]; then
  if [ ! -d "lua-vermelha" ]; then
    git clone  --recursive https://github.com/Leonardo2718/lua-vermelha
  fi

  mkdir -p ${ljbins}/lua-vermelha

  # Make directory path relative so we don't have to worry about what path we have to pass to make.
  ljbins="$( cd "$ljbins" && pwd )"

  cd lua-vermelha/omr/
  make -f run_configure.mk SPEC=linux_x86-64 OMRGLUE=./example/glue
  cd ..
  make -j
  cd ..
  cp lua-vermelha/luav ${ljbins}/lua-vermelha/luav
fi

# We don't have access to string functions in the config file so pass the base dir for paths
export LUAVM_BASEDIR=$(pwd)/builds/normal
export LUAROCKS_CONFIG=$(pwd)/rocks_config.lua

luarocks --tree=rocks install https://raw.githubusercontent.com/fsfod/luachild/master/luachild-0.1-1.rockspec

echo "--------------Running benchmark build scripts-------------------------"

for subdir in benchmarks/*; do
  if [ -f "$subdir/build.sh" ]; then
    (cd "$subdir" && ./build.sh)
  fi
done
