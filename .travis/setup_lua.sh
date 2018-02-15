#! /bin/bash

# A script for setting up environment for travis-ci testing.
# Sets up Lua and Luarocks.
# LUA must be "lua5.1", "lua5.2" or "luajit".
# luajit2.0 - master v2.0
# luajit2.1 - master v2.1

set -eufo pipefail

source .travis/platform.sh

LR_HOME_DIR=$TRAVIS_BUILD_DIR/install/luarocks

mkdir $HOME/.lua

cd $TRAVIS_BUILD_DIR

lua -v

LUAROCKS_BASE=luarocks-$LUAROCKS

curl --location http://luarocks.org/releases/$LUAROCKS_BASE.tar.gz | tar xz

cd $LUAROCKS_BASE
./configure --prefix="$LR_HOME_DIR"
make build && make install
ln -s $LR_HOME_DIR/bin/luarocks $HOME/.lua/luarocks

cd $TRAVIS_BUILD_DIR

luarocks --version

rm -rf $LUAROCKS_BASE
