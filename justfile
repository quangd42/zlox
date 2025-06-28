rundebug PATH="":
    zig build run {{ if PATH == "" { "" } else { "--" } }} {{ PATH }}

run PATH="":
    zig build -Doptimize=ReleaseFast
    ./zig-out/bin/zlox {{ PATH }}

build:
    zig build

test:
    zig build test --summary new

# export TEST_FILES to shell such as
# fish: set -x TEST_FILES (fd -t f -e lox . test/)
# test/.fdignore will filter out unwanted tests

test-all: build
    sudo zig run test/test.zig -- zig-out/bin/zlox $TEST_FILES

watch-run:
    watchexec -r -e zig -- zig build run

watch-test:
    watchexec -r -e zig -- zig build test --summary new
