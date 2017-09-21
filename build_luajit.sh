#!/bin/sh
#Build the various intresting flavors of LuaJIT


if [ $# -gt 1 ]; then
    echo "Usage: build_luajit.sh [<source dir>]" 2>&1
    exit 1
fi

if [ $# -eq 1 ]; then
    ljsrc=$1/src
    echo "Using ${ljsrc} as LuaJIT source path"
else
    echo "Defaulting to ./luajit_repo/src as LuaJIT source path"
    ljsrc=luajit_repo/src

    if [ ! -d "luajit_repo" ]; then 
      git clone -b v2.1 https://github.com/LuaJIT/LuaJIT luajit_repo
    fi

    if [ ! -d "raptorjit_repo" ]; then 
      git clone https://github.com/raptorjit/raptorjit raptorjit_repo
    fi
    fi
fi

ljbins=${ljbins:=builds}

mkdir -p ${ljbins}
# Make directory path relative so we don't have to worry about what path we have to pass to make.
ljbins="$( cd "$ljbins" && pwd )"

#Unmodified build with 32 bit sized gc object pointers. Object allocataion limited to the lower 4gb virtual address space
make -C ${ljsrc} clean
make -C ${ljsrc} -j
cp ${ljsrc}/luajit ${ljbins}/luajit

#Build with JIT removed
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS=-DLUAJIT_DISABLE_JIT
cp ${ljsrc}/luajit ${ljbins}/luajit_nojit

#GC64 64 bit sized gc object pointer
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS=-DLUAJIT_ENABLE_GC64
cp ${ljsrc}/luajit ${ljbins}/luajit_gc64

# Build with dual number mode enabled
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS=-DLUAJIT_NUMMODE=2
cp ${ljsrc}/luajit ${ljbins}/luajit_dualnum

if [ -d "raptorjit_repo" ]; then
  make -C ./raptorjit_repo clean
  make -C ./raptorjit_repo -j HOST_LUA=${ljbins}/luajit
  cp ./raptorjit_repo/src/raptorjit ${ljbins}/raptorjit
fi

#32 bit build
#make -C ${ljsrc} clean
#make -C ${ljsrc} -j CC="gcc -m32"
#cp ${ljsrc}/luajit ${ljbins}/luajit32
