#!/bin/bash

function clean_results() {
  (shopt -s nullglob; rm -rf $1*{log,manifest,bz2,envlogs,env,instr_data})
}

clean_results "luajit"
clean_results "quicktest"
clean_results "bencher"