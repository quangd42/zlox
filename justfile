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
# fish: (fd --glob "*.lox" test/call  \
#     | grep -v test/benchmark \
#     | grep -v test/scanning \
#     | grep -v test/expressions \
#     | grep -v test/for/closure_in_body.lox)

test-all: build
    sudo zig run test/test.zig -- zig-out/bin/zlox $TEST_FILES

watch-run:
    watchexec -r -e zig -- zig build run

watch-test:
    watchexec -r -e zig -- zig build test --summary new
