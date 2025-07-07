# Conclusion
Using Unmanaged is a bit slower??

--- 

## Use ArrayListUnmanaged on VM
❯ just benchmark-compare ./zlox-alm ./zlox-alu
Benchmarking: binary_trees
Benchmark 1: old
  Time (mean ± σ):      2.157 s ±  0.008 s    [User: 2.030 s, System: 0.121 s]
  Range (min … max):    2.144 s …  2.170 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      2.173 s ±  0.006 s    [User: 2.047 s, System: 0.120 s]
  Range (min … max):    2.164 s …  2.181 s    10 runs

Summary
  old ran
    1.01 ± 0.00 times faster than new

Benchmarking: equality
Benchmark 1: old
  Time (mean ± σ):      6.109 s ±  0.003 s    [User: 6.075 s, System: 0.033 s]
  Range (min … max):    6.106 s …  6.113 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      6.095 s ±  0.005 s    [User: 6.060 s, System: 0.033 s]
  Range (min … max):    6.090 s …  6.103 s    10 runs

Summary
  new ran
    1.00 ± 0.00 times faster than old

Benchmarking: fib
Benchmark 1: old
  Time (mean ± σ):      1.830 s ±  0.005 s    [User: 1.817 s, System: 0.010 s]
  Range (min … max):    1.824 s …  1.843 s    10 runs

  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet system without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.

Benchmark 2: new
  Time (mean ± σ):      1.869 s ±  0.002 s    [User: 1.858 s, System: 0.011 s]
  Range (min … max):    1.867 s …  1.872 s    10 runs

Summary
  old ran
    1.02 ± 0.00 times faster than new

Benchmarking: instantiation
Benchmark 1: old
  Time (mean ± σ):     619.4 ms ±   1.7 ms    [User: 614.3 ms, System: 4.6 ms]
  Range (min … max):   616.0 ms … 621.8 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     619.0 ms ±   0.6 ms    [User: 613.3 ms, System: 5.2 ms]
  Range (min … max):   618.0 ms … 619.8 ms    10 runs

Summary
  new ran
    1.00 ± 0.00 times faster than old

Benchmarking: invocation
Benchmark 1: old
  Time (mean ± σ):     499.2 ms ±   0.3 ms    [User: 494.5 ms, System: 4.3 ms]
  Range (min … max):   498.7 ms … 499.5 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     499.1 ms ±   0.7 ms    [User: 494.2 ms, System: 4.3 ms]
  Range (min … max):   498.3 ms … 500.9 ms    10 runs

Summary
  new ran
    1.00 ± 0.00 times faster than old

Benchmarking: method_call
Benchmark 1: old
  Time (mean ± σ):     331.7 ms ±   0.5 ms    [User: 327.8 ms, System: 3.5 ms]
  Range (min … max):   330.9 ms … 332.3 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     337.7 ms ±   4.6 ms    [User: 332.9 ms, System: 3.7 ms]
  Range (min … max):   333.6 ms … 346.1 ms    10 runs

Summary
  old ran
    1.02 ± 0.01 times faster than new

Benchmarking: properties
Benchmark 1: old
  Time (mean ± σ):     705.2 ms ±   1.4 ms    [User: 703.1 ms, System: 2.0 ms]
  Range (min … max):   703.5 ms … 708.1 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     703.4 ms ±   1.0 ms    [User: 701.9 ms, System: 1.6 ms]
  Range (min … max):   702.3 ms … 705.4 ms    10 runs

Summary
  new ran
    1.00 ± 0.00 times faster than old

Benchmarking: string_equality
Benchmark 1: old
  Time (mean ± σ):      2.403 s ±  0.001 s    [User: 2.400 s, System: 0.003 s]
  Range (min … max):    2.402 s …  2.406 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      2.452 s ±  0.009 s    [User: 2.449 s, System: 0.003 s]
  Range (min … max):    2.448 s …  2.479 s    10 runs

  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet system without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.

Summary
  old ran
    1.02 ± 0.00 times faster than new

Benchmarking: trees
Benchmark 1: old
  Time (mean ± σ):      3.846 s ±  0.010 s    [User: 3.831 s, System: 0.012 s]
  Range (min … max):    3.837 s …  3.871 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      3.847 s ±  0.003 s    [User: 3.815 s, System: 0.030 s]
  Range (min … max):    3.843 s …  3.854 s    10 runs

Summary
  old ran
    1.00 ± 0.00 times faster than new

Benchmarking: zoo
Benchmark 1: old
  Time (mean ± σ):     536.8 ms ±   1.4 ms    [User: 531.2 ms, System: 5.0 ms]
  Range (min … max):   535.4 ms … 540.3 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     537.1 ms ±   0.3 ms    [User: 531.9 ms, System: 4.7 ms]
  Range (min … max):   536.7 ms … 537.6 ms    10 runs

Summary
  old ran
    1.00 ± 0.00 times faster than new

Benchmarking: zoo_batch
Benchmark 1: old
  Time (mean ± σ):      9.995 s ±  0.002 s    [User: 9.941 s, System: 0.052 s]
  Range (min … max):    9.992 s …  9.997 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      9.995 s ±  0.001 s    [User: 9.939 s, System: 0.053 s]
  Range (min … max):    9.993 s …  9.996 s    10 runs

Summary
  new ran
    1.00 ± 0.00 times faster than old

-----

## Then use ArrayListUnmanaged on Table

❯ just benchmark-compare ./zlox-alm ./zlox-alu
Benchmarking: binary_trees
Benchmark 1: old
  Time (mean ± σ):      2.151 s ±  0.007 s    [User: 2.028 s, System: 0.117 s]
  Range (min … max):    2.141 s …  2.168 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      2.346 s ±  0.013 s    [User: 2.226 s, System: 0.113 s]
  Range (min … max):    2.330 s …  2.377 s    10 runs

Summary
  old ran
    1.09 ± 0.01 times faster than new

Benchmarking: equality
Benchmark 1: old
  Time (mean ± σ):      6.111 s ±  0.004 s    [User: 6.075 s, System: 0.034 s]
  Range (min … max):    6.106 s …  6.116 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      6.120 s ±  0.034 s    [User: 6.084 s, System: 0.034 s]
  Range (min … max):    6.088 s …  6.171 s    10 runs

Summary
  old ran
    1.00 ± 0.01 times faster than new

Benchmarking: fib
Benchmark 1: old
  Time (mean ± σ):      1.832 s ±  0.010 s    [User: 1.820 s, System: 0.012 s]
  Range (min … max):    1.828 s …  1.860 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      1.826 s ±  0.002 s    [User: 1.814 s, System: 0.012 s]
  Range (min … max):    1.824 s …  1.830 s    10 runs

Summary
  new ran
    1.00 ± 0.01 times faster than old

Benchmarking: instantiation
Benchmark 1: old
  Time (mean ± σ):     619.7 ms ±   0.6 ms    [User: 614.1 ms, System: 5.1 ms]
  Range (min … max):   618.6 ms … 620.7 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     622.4 ms ±   0.7 ms    [User: 616.7 ms, System: 5.2 ms]
  Range (min … max):   621.5 ms … 623.8 ms    10 runs

Summary
  old ran
    1.00 ± 0.00 times faster than new

Benchmarking: invocation
Benchmark 1: old
  Time (mean ± σ):     499.5 ms ±   0.4 ms    [User: 494.6 ms, System: 4.4 ms]
  Range (min … max):   498.9 ms … 500.1 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     497.7 ms ±   0.6 ms    [User: 492.7 ms, System: 4.5 ms]
  Range (min … max):   496.6 ms … 498.6 ms    10 runs

Summary
  new ran
    1.00 ± 0.00 times faster than old

Benchmarking: method_call
Benchmark 1: old
  Time (mean ± σ):     331.6 ms ±   0.8 ms    [User: 327.9 ms, System: 3.3 ms]
  Range (min … max):   330.7 ms … 333.0 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     331.2 ms ±   0.5 ms    [User: 327.2 ms, System: 3.7 ms]
  Range (min … max):   330.6 ms … 332.3 ms    10 runs

Summary
  new ran
    1.00 ± 0.00 times faster than old

Benchmarking: properties
Benchmark 1: old
  Time (mean ± σ):     708.9 ms ±   0.9 ms    [User: 702.5 ms, System: 5.7 ms]
  Range (min … max):   708.2 ms … 711.2 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     706.9 ms ±   4.4 ms    [User: 700.6 ms, System: 5.6 ms]
  Range (min … max):   694.5 ms … 709.6 ms    10 runs

  Warning: Statistical outliers were detected. Consider re-running this benchmark on a quiet system without any interferences from other programs. It might help to use the '--warmup' or '--prepare' options.

Summary
  new ran
    1.00 ± 0.01 times faster than old

Benchmarking: string_equality
Benchmark 1: old
  Time (mean ± σ):      2.416 s ±  0.001 s    [User: 2.400 s, System: 0.014 s]
  Range (min … max):    2.414 s …  2.418 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      2.456 s ±  0.005 s    [User: 2.442 s, System: 0.013 s]
  Range (min … max):    2.446 s …  2.467 s    10 runs

Summary
  old ran
    1.02 ± 0.00 times faster than new

Benchmarking: trees
Benchmark 1: old
  Time (mean ± σ):      3.867 s ±  0.036 s    [User: 3.850 s, System: 0.014 s]
  Range (min … max):    3.841 s …  3.930 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      4.021 s ±  0.014 s    [User: 4.007 s, System: 0.012 s]
  Range (min … max):    4.008 s …  4.054 s    10 runs

Summary
  old ran
    1.04 ± 0.01 times faster than new

Benchmarking: zoo
Benchmark 1: old
  Time (mean ± σ):     546.8 ms ±   5.2 ms    [User: 542.6 ms, System: 2.7 ms]
  Range (min … max):   535.5 ms … 553.7 ms    10 runs

Benchmark 2: new
  Time (mean ± σ):     538.9 ms ±   2.7 ms    [User: 536.2 ms, System: 2.1 ms]
  Range (min … max):   534.4 ms … 543.1 ms    10 runs

Summary
  new ran
    1.01 ± 0.01 times faster than old

Benchmarking: zoo_batch
Benchmark 1: old
  Time (mean ± σ):      9.995 s ±  0.002 s    [User: 9.985 s, System: 0.007 s]
  Range (min … max):    9.993 s …  9.998 s    10 runs

Benchmark 2: new
  Time (mean ± σ):      9.980 s ±  0.037 s    [User: 9.908 s, System: 0.047 s]
  Range (min … max):    9.876 s …  9.997 s    10 runs

Summary
  new ran
    1.00 ± 0.00 times faster than old
