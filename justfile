rundebug PATH="":
    zig build run {{ if PATH == "" { "" } else { "--" } }} {{ PATH }}

run PATH="":
    zig build -Doptimize=ReleaseSafe
    ./zig-out/bin/zlox {{ PATH }}
