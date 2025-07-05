# reference https://github.com/jwmerrill/zig-lox/blob/main/Makefile
# test/.fdignore will filter out unwanted tests
# Directory containing benchmark scripts

BENCH_DIR := "test/benchmark"
export TEST_FILES := `fd -t f -e lox . test/`

run-db PATH="":
    zig build run {{ if PATH == "" { "" } else { "--" } }} {{ PATH }}

run PATH="": release
    ./zig-out/bin/zlox {{ PATH }}

release:
    zig build -Doptimize=ReleaseSafe -Dnan-boxing

test:
    zig build test --summary new

test-all: release
    # sudo is needed for now because somehow when there is a large amount of TEST_FILES
    # I got error: unable to create compilation: AccessDenied
    sudo zig run util/test.zig -- zig-out/bin/zlox $TEST_FILES

benchmark bench_file: release
    hyperfine --warmup 3 './zig-out/bin/zlox {{ bench_file }}'

# Compare two interpreter versions across all benchmarks
benchmark-compare old_interpreter new_interpreter:
    #!/usr/bin/env bash
    benchmark_files=({{ BENCH_DIR }}/*.lox)

    # Run comparison for each file individually
    for file in "${benchmark_files[@]}"; do
        filename=$(basename "$file" .lox)
        echo "Benchmarking: $filename"

        hyperfine \
            --warmup 3 \
            --min-runs 10 \
            --command-name "old" "{{ old_interpreter }} $file" \
            --command-name "new" "{{ new_interpreter }} $file"

        echo ""
    done
