rundebug PATH="":
    zig build run {{ if PATH == "" { "" } else { "--" } }} {{ PATH }}

run PATH="":
    zig build -Doptimize=ReleaseFast
    ./zig-out/bin/zlox {{ PATH }}

test:
    zig build test --summary new

watch-run:
    watchexec -r -e zig -- zig build run

watch-test:
    watchexec -r -e zig -- zig build test --summary new
