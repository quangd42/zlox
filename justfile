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
    zig build -Doptimize=ReleaseSafe

test:
    zig build test --summary new

test-all: release
    # sudo is needed for now because somehow when there is a large amount of TEST_FILES
    # I got error: unable to create compilation: AccessDenied
    sudo zig run util/test.zig -- zig-out/bin/zlox $TEST_FILES

benchmark bench_file:
    hyperfine --warmup 3 \
                --parameter-list bench_file {{ bench_file }} \
                './zlox-multiarray-before {bench_file}' \
                './zlox-multiarray-after {bench_file}' \
                --export-markdown benchmark_results.md

# Compare two interpreter versions across all benchmarks
benchmark-compare old_interpreter new_interpreter:
    #!/usr/bin/env bash
    # Create results directory
    mkdir -p benchmark-results
    timestamp=$(date +%Y%m%d-%H%M%S)

    # Get all .lox files
    benchmark_files=({{ BENCH_DIR }}/*.lox)

    if [ ${#benchmark_files[@]} -eq 0 ]; then
        echo "No .lox files found in {{ BENCH_DIR }}"
        exit 1
    fi

    echo "Results will be saved to benchmark-results/"
    echo ""

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
