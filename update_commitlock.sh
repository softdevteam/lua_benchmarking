#!/bin/bash

if [ -f "commitlock.txt" ]; then 
  source commitlock.txt
  echo "Current LuaJIT commit hash: ${luajit_commitid}"
  echo "Current RaptorJIT commit hash: ${raptorjit_commitid}"
fi

raptorjitid=$(cd raptorjit_repo && git rev-parse --verify HEAD)
luajitid=$(cd luajit_repo && git rev-parse --verify HEAD)

if [ ${luajitid} != ${luajit_commitid:="?"} ]; then
  echo "  LuaJIT commit hash changed to ${luajitid}"
fi

if [ ${raptorjitid} != ${raptorjit_commitid:="?"} ]; then
  echo "  RaptorJIT commit hash changed to ${raptorjitid}"
fi

commitids=(
  "luajit_commitid=\"${luajitid}\""
  "raptorjit_commitid=\"${raptorjitid}\""
)

printf "%s\n" "${commitids[@]}" > commitlock.txt
