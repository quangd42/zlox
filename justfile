# reference https://github.com/jwmerrill/zig-lox/blob/main/Makefile
# test/.fdignore will filter out unwanted tests

run-db PATH="":
    zig build run {{ if PATH == "" { "" } else { "--" } }} {{ PATH }}

run PATH="": release
    ./zig-out/bin/zlox {{ PATH }}

release:
    zig build -Doptimize=ReleaseFast -Dnan-boxing

test:
    zig build test --summary new

# sudo is needed for now because somehow when there is a large amount of TEST_FILES
# I got error: unable to create compilation: AccessDenied

test-all: release
    sudo fd -t f -e lox . test/ -X zig run util/test.zig -- zig-out/bin/zlox

benchmark bench_file: release
    hyperfine --warmup 3 './zig-out/bin/zlox {{ bench_file }}'

# Compare two interpreter versions across all benchmarks e.g. benchmark-compare zlox-alm zlox-alu

# binaries to compare are expected to be in the main folder
benchmark-compare old_interpreter new_interpreter:
    mkdir -p benchmark-results/{{ old_interpreter }}_v_{{ new_interpreter }}
    fd -t f -e lox . test/benchmark/ -j 1 -x hyperfine \
            --warmup 3 \
            --command-name "old" "./{{ old_interpreter }} {}" \
            --command-name "new" "./{{ new_interpreter }} {}" \
            --export-markdown benchmark-results/{{ old_interpreter }}_v_{{ new_interpreter }}/{/.}.md
