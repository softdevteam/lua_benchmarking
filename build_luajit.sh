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
fi

ljbins=${ljbins:=builds}

mkdir -p ${ljbins}

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

#32 bit build
#make -C ${ljsrc} clean
#make -C ${ljsrc} -j CC="gcc -m32"
#cp ${ljsrc}/luajit ${ljbins}/luajit32
