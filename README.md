# Benchmarking Lua

This repository collects together a number of open-source Lua benchmarks,
suitable for quick or rigorous benchmarking. Contributions are welcome!


## Adding new benchmarks

New benchmarks should be put in the `benchmarks/` repository with an appropriate
name. The "main" file should be called `bench.lua`, which must contain a
function `run_iter` which takes a single parameter `n` which is the number of
times the benchmark should be run in a `for` loop (or equivalent) to make up a
single "in-process iteration". The reason for this is that many benchmarks run
too quickly for reliable measurements to be taken. However, the number of times
a benchmark should be repeated to run "long enough" is machine dependent.
`run_iter` is thus easily customisable for different situations. Roughly
speaking, a single in-process iteration should run for around 1 second (with a
minimum acceptable of 0.1s).


## Benchmarking using Krun

Those who wish to use [Krun](http://soft-dev.org/src/krun/) or
[warmup_stats](http://soft-dev.org/src/warmup_stats/) should investigate the
various `.krun` files in this repository. `quicktest.krun` is useful to
determine whether the benchmark suite and benchmarking setup are free
from errors, but does not run benchmarks long enough to give useful statistics.
`luajit.krun` runs a lengthy benchmarking suite that may take 3-4 days to
complete but gives high quality results.
