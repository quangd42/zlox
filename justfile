# reference https://github.com/jwmerrill/zig-lox/blob/main/Makefile
# test/.fdignore will filter out unwanted tests

export TEST_FILES := `fd -t f -e lox . test/`
export BENCHMARK_FILES := `fd -t f -e lox . test/benchmark`

run-db PATH="":
    zig build run {{ if PATH == "" { "" } else { "--" } }} {{ PATH }}

run PATH="": release
    ./zig-out/bin/zlox {{ PATH }}

release:
    zig build -Doptimize=ReleaseSafe

test:
    zig build test --summary new

test-all: release
    # sudo is needed for now because somehow when there is a large amount of TEST_FILES
    # I got error: unable to create compilation: AccessDenied
    zig run util/test.zig -- zig-out/bin/zlox $TEST_FILES

benchmark: release
    zig run util/benchmark.zig -- zig-out/bin/zlox $BENCHMARK_FILES

watch-run:
    watchexec -r -e zig -- zig build run

watch-test:
    watchexec -r -e zig -- zig build test --summary new
