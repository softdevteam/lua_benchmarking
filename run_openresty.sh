#!/bin/bash

currdir=$(pwd)
scriptdir="$( cd "$(dirname "${BASH_SOURCE[0]}")" && pwd )"

export startup_script=${currdir}/openresty_standalone.lua
openresty/nginx/nginx -c ${scriptdir}/nginx.conf
