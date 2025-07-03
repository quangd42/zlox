run-db PATH="":
    zig build run {{ if PATH == "" { "" } else { "--" } }} {{ PATH }}

run PATH="": release
    ./zig-out/bin/zlox {{ PATH }}

release:
    zig build -Doptimize=ReleaseSafe

test:
    zig build test --summary new

# unclear why setting TEST_FILES as just variable doesn't work
# workaround: export TEST_FILES to shell such as
# fish: set -x TEST_FILES (fd -t f -e lox . test/)
# test/.fdignore will filter out unwanted tests

test-all: release
    # sudo is needed for now because zig-cache does have permission
    sudo zig run util/test.zig -- zig-out/bin/zlox $TEST_FILES

watch-run:
    watchexec -r -e zig -- zig build run

watch-test:
    watchexec -r -e zig -- zig build test --summary new
