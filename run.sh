#!/bin/bash

__help_string="
Usage:
  $(basename $0) -tru native node64 browser32  # runs tests natively, in node (wasm64), in browser (wasm32)
  $(basename $0) -t native                     # builds tests for native
  $(basename $0) -bru                          # runs benchmarks native, wasm32, wasm64, and js
  $(basename $0) -bru native js                # runs benchmarks native and js
  $(basename $0) -bru native wasm32            # runs benchmarks native and wasm32

Options:
  -? -h --help         Print this usage information.
  -r --run             Run the compiled output.
  -u --user_settings   Use user_settings.sh to setup variables.
  -t --test            Test. Accepts positional args [native node browser].
  -b --bench           Benchmark Accepts positional args [native wasm js].
  -d --debug           Compile with debug flags.
  -o --optimize        (This is done by default for wasm targets).
"

RUN=0
TEST=0
DEBUG=0
OPTIMIZE=0
BENCHMARK=0
USER_SETTINGS=0

# A POSIX variable
OPTIND=1         # Reset in case getopts has been used previously in the shell.

while getopts "h?rtdobu" opt; do
  case "$opt" in
    h|\?)
      echo "$__help_string"
      exit 0
      ;;
    r) RUN=1 ;;
    t) TEST=1 ;;
    d) DEBUG=1 ;;
    o) OPTIMIZE=1 ;;
    b) BENCHMARK=1 ;;
    u) USER_SETTINGS=1 ;;
    -)
      case "${OPTARG}" in
        help)
          echo "$__help_string"
          exit 0
          ;;
        run           ) RUN=1 ;;
        test          ) TEST=1 ;;
        debug         ) DEBUG=1 ;;
        bench         ) BENCHMARK=1 ;;
        optimize      ) OPTIMIZE=1 ;;
        user_settings ) USER_SETTINGS=1 ;;
        *)
          echo "Invalid option: --$OPTARG"
          exit 1
          ;;
      esac
      ;;
  esac
done

shift $((OPTIND-1))

if [ $TEST -eq 1 ] && [ $BENCHMARK -eq 1 ]; then
  echo "Invalid: We currently do not support running -b and -t together."
  exit 1
fi

# Function to prepend a string to each line of input
with_prefix() {
    prefix="$1"
    while IFS= read -r line; do
        echo "$prefix $line"
    done
}

# Pad a string with spaces to the right
pad_right() {
    local string="$1"
    local length="$2"
    printf "%-${length}s" "$string"
}

PREFIX_NATIVE=$(pad_right "NATIVE" 10)
PREFIX_WASM32=$(pad_right "WASM32" 10)
PREFIX_WASM64=$(pad_right "WASM64" 10)
PREFIX_NODE32=$(pad_right "NODE32" 10)
PREFIX_NODE64=$(pad_right "NODE64" 10)
PREFIX_BROWSER32=$(pad_right "BROWSER32" 10)
PREFIX_BROWSER64=$(pad_right "BROWSER64" 10)
PREFIX_JS=$(pad_right "JS" 10)

FILE=""
NAME=""
if [ $TEST -eq 1 ]; then
  export FILE="tests/test.nim"
  export NAME="test"
elif [ $BENCHMARK -eq 1 ]; then
  export FILE="benchmark/benchmark.nim"
  export NAME="benchmark"
fi

native_built=0
wasm32_built=0
wasm64_built=0

build_native() {
  if [ $TEST -eq 1 ]; then
    native_built=1
    opt_str="-"
    if [ $USER_SETTINGS -eq 1 ]; then opt_str+="u"; fi
    if [ $OPTIMIZE -eq 1 ]; then opt_str+="o"; fi
    if [ $DEBUG -eq 1 ]; then opt_str+="d"; fi
    if [[ opt_str = "-" ]]; then opt_str=""; fi
    (./scripts/build.sh -f "${FILE}" -n "${NAME}" -t native "${opt_str}")
  elif [ $BENCHMARK -eq 1 ]; then
    native_built=1
    opt_str="-o"
    if [ $USER_SETTINGS -eq 1 ]; then opt_str+="u"; fi
    if [ $DEBUG -eq 1 ]; then opt_str+="d"; fi
    (./scripts/build.sh -f "${FILE}" -n "${NAME}" -t native "${opt_str}")
  else
    echo "TODO"
  fi
}

build_wasm32() {
  if [ $TEST -eq 1 ] || [ $BENCHMARK -eq 1 ]; then
    wasm32_built=1
    opt_str="-o"
    if [ $USER_SETTINGS -eq 1 ]; then opt_str+="u"; fi
    if [ $DEBUG -eq 1 ]; then opt_str+="d"; fi
    (./scripts/build.sh -f "${FILE}" -n "${NAME}" -t wasm32 "${opt_str}")
  else
    echo "TODO"
  fi
}

build_wasm64() {
  if [ $TEST -eq 1 ] || [ $BENCHMARK -eq 1 ]; then
    wasm64_built=1
    opt_str="-o"
    if [ $USER_SETTINGS -eq 1 ]; then opt_str+="u"; fi
    if [ $DEBUG -eq 1 ]; then opt_str+="d"; fi
    (./scripts/build.sh -f "${FILE}" -n "${NAME}" -t wasm64 "${opt_str}")
  else
    echo "TODO"
  fi
}

positional_args=("$@")

if [ $BENCHMARK -eq 1 ]; then
  
  run_native=0
  run_wasm32=0
  run_wasm64=0
  run_js=0

  if [ $RUN -eq 1 ]; then
    # delete previous partial reports
    node --experimental-default-type=module benchmark/cleanup.js
  fi

  # default to benchmarking native, wasm, and js
  if [ ${#positional_args[@]} -eq 0 ]; then
    positional_args=("native" "wasm32" "js")
  fi

  # Build in parallel
  for arg in "${positional_args[@]}"
  do
    case "$arg" in
      native)
        if [ $native_built -eq 0 ]; then
          build_native | with_prefix "$PREFIX_NATIVE" &
        fi
        run_native=1
        ;;
      wasm32)
        if [ $wasm32_built -eq 0 ]; then 
          build_wasm32 | with_prefix "$PREFIX_WASM32" &
        fi
        run_wasm32=1
        ;;
      wasm64)
        if [ $wasm64_built -eq 0 ]; then 
          build_wasm64 | with_prefix "$PREFIX_WASM64" &
        fi
        run_wasm64=1
        ;;
      js)
        run_js=1
        ;;
      *)
        echo "Unrecognized arg: ${arg}"
        echo "When option -b is passed 'native', 'wasm32', 'wasm64', and 'js' are supported"
        exit 1
        ;;
    esac
  done

  # Wait for the builds to complete
  wait

  if [ $RUN -eq 1 ]; then
    # Run the benchmarks in parallel
    if [ $run_native -eq 1 ]; then
      "./dist/${NAME}_native" | with_prefix "$PREFIX_NATIVE" &
    fi
    if [ $run_wasm32 -eq 1 ]; then
      node \
      --experimental-default-type=module \
      benchmark/node_glue.js \
      "./dist/${NAME}_wasm32.wasm" | with_prefix "$PREFIX_WASM32" &
    fi
    if [ $run_wasm64 -eq 1 ]; then
      node \
      --experimental-default-type=module \
      --experimental-wasm-memory64 \
      benchmark/node_glue.js \
      "./dist/${NAME}_wasm64.wasm" | with_prefix "$PREFIX_WASM64" &
    fi
    if [ $run_js -eq 1 ]; then
      node --experimental-default-type=module benchmark/benchmark.js | with_prefix "$PREFIX_JS" &
    fi
    # Wait for the benchmarks to run
    wait
    # Create a report from the individual results
    node --experimental-default-type=module benchmark/report.js
    exit 0
  fi

else

  run_native=0
  run_node32=0
  run_node64=0
  run_browser32=0
  run_browser64=0

  # default to benchmarking native, wasm, and js
  if [ ${#positional_args[@]} -eq 0 ]; then
    positional_args=("native" "node32")
  fi

  # Build in parallel
  for arg in "${positional_args[@]}"
  do
    case "$arg" in
      native)
        if [ $native_built -eq 0 ]; then
          build_native | with_prefix "$PREFIX_NATIVE" &
        fi
        run_native=1
        ;;
      node32)
        if [ $wasm32_built -eq 0 ]; then
          build_wasm32 | with_prefix "$PREFIX_WASM32" &
        fi
        run_node32=1
        ;;
      browser32)
        if [ $wasm32_built -eq 0 ]; then
          build_wasm32 | with_prefix "$PREFIX_WASM32" &
        fi
        run_browser32=1
        ;;
      node64)
        if [ $wasm64_built -eq 0 ]; then
          build_wasm64 | with_prefix "$PREFIX_WASM64" &
        fi
        run_node64=1
        ;;
      browser64)
        if [ $wasm64_built -eq 0 ]; then
          build_wasm64 | with_prefix "$PREFIX_WASM64" &
        fi
        run_browser64=1
        ;;
      *)
        echo "Unrecognized arg: ${arg}"
        echo "'native', 'node32', 'node64', 'browser32', and 'browser64' are supported"
        exit 1
        ;;
    esac
  done

  # Wait for the builds to complete
  wait

  if [ $RUN -eq 1 ]; then
    if [ $run_native -eq 1 ]; then
      echo "== Running native ==========================="
      "./dist/${NAME}_native"
    fi
    if [ $run_node32 -eq 1 ]; then
      echo "== Running wasm32 in node ==================="
      node --experimental-default-type=module src/node_glue.js "./dist/${NAME}_wasm32.wasm"
    fi
    if [ $run_browser32 -eq 1 ]; then
      echo "== Running wasm32 in browser ================"
      # pass the wasm path to the webpage
      export VITE_WASM_PATH="./dist/${NAME}_wasm32.wasm"
      npm run start
    fi
    if [ $run_node64 -eq 1 ]; then
      echo "== Running wasm64 in node ==================="
      node --experimental-default-type=module src/node_glue.js "./dist/${NAME}_wasm64.wasm"
    fi
    if [ $run_browser64 -eq 1 ]; then
      echo "== Running wasm64 in browser ================"
      # pass the wasm path to the webpage
      export VITE_WASM_PATH="./dist/${NAME}_wasm64.wasm"
      npm run start
    fi
    exit 0
  fi

fi