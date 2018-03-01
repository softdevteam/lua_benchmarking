#!/bin/bash

mkdir -p builds/openresty/src

buildsdir=$(cd "builds" && pwd)

(cd luajit_repo && git worktree add ../builds/openresty/src origin/openresty/merge)

(cd builds/openresty/src/ && make clean && make -j XCFLAGS="-msse4.2 -DLUAJIT_ENABLE_LUA52COMPAT")

function copy_binaries() {
  mkdir -p ${buildsdir}/$1/jit/
  cp ${ljsrc}/luajit ${buildsdir}/$1/luajit
  cp ${ljsrc}/libluajit.so ${buildsdir}/$1/libluajit.so
  
  mkdir ${buildsdir}/$1/lib
  ln -s ${buildsdir}/$1/libluajit.so ${buildsdir}/$1/lib/libluajit-5.1.so
  ln -s ${buildsdir}/$1/libluajit.so ${buildsdir}/$1/libluajit-5.1.so
  ln -s ${buildsdir}/$1/libluajit.so ${buildsdir}/$1/lib/libluajit-5.1.so.2
  
  cp ${ljsrc}/jit/*.lua ${buildsdir}/$1/jit/
  
  mkdir -p ${buildsdir}/$1/include/
  cp ${ljsrc}/luaconf.h ${buildsdir}/$1/include
  cp ${ljsrc}/lua.h ${buildsdir}/$1/include
  cp ${ljsrc}/luajit.h ${buildsdir}/$1/include
  cp ${ljsrc}/lualib.h ${buildsdir}/$1/include
  cp ${ljsrc}/lauxlib.h ${buildsdir}/$1/include
  mkdir -p ${buildsdir}/$1/include/luajit-2.1
  cp ${buildsdir}/$1/include/*.h ${buildsdir}/$1/include/luajit-2.1
}

ljsrc=builds/openresty/src/src
copy_binaries "openresty"

mkdir build && cd build

curl https://www.openssl.org/source/${OPENSSL}.tar.gz -O
curl https://openresty.org/download/openresty-1.13.6.1.tar.gz -O

mkdir -p openresty && tar -zxf openresty-1.13.6.1.tar.gz -C openresty --strip-components=1
mkdir -p openssl && tar -zxf ${OPENSSL}.tar.gz -C openssl --strip-components=1
(cd openssl && patch -p1 < ../openresty/patches/openssl-1.0.2h-sess_set_get_cb_yield.patch)

(cd openresty && ./configure -j3 --with-openssl=../openssl --sbin-path=nginx --prefix=${buildsdir}/openresty --with-luajit=${buildsdir}/openresty)
(cd openresty && make -j3 && make install)



