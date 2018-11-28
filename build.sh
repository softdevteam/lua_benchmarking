#!/bin/bash

# Make sure we bail out if any of our commands fail, like the commit lock
set -e

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

if [ -d "raptorjit_repo" ]; then
  make -C ./raptorjit_repo clean
  make -C ./raptorjit_repo -j HOST_LUA=${ljbins}/normal/luajit
  mkdir -p ${ljbins}/raptorjit/jit/
  cp ./raptorjit_repo/src/raptorjit ${ljbins}/raptorjit/luajit
  cp ./raptorjit_repo/src/libraptorjit.so ${ljbins}/raptorjit/libluajit.so
  cp ./raptorjit_repo/src/jit/*.lua ${ljbins}/raptorjit/jit/
fi

