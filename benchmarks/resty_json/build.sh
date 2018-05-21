#!/bin/bash

git clone https://github.com/cloudflare/lua-resty-json repo
cd repo
git checkout 547c34da77ab96bcf60a829243179b90b7f30459
make
cp libljson.so ..
cp json_decoder.lua ..
