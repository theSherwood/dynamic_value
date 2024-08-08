/**
 * TODO
 * -[ ] support wasm64
 */

import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";
import { instantiate_wasm, run_nim_main } from "../src/setup_wasm.js";
import fs from "node:fs";

const VERBOSE = 0;
const LITTLE_ENDIAN = true;
const OUTPUT_PATH = "./benchmark/results_wasm32.csv";
const WASM_PATH = process.argv[2];

let csv_rows = [];

function get_wasi() {
  let wasi;
  let args = [];
  let env = [];
  let fds = [
    new OpenFile(new File([])), // stdin
    ConsoleStdout.lineBuffered((msg) => console.log(msg)),
    ConsoleStdout.lineBuffered((msg) => console.warn(msg)),
  ];
  wasi = new WASI(args, env, fds);
  if (VERBOSE) console.log("wasi", wasi.wasiImport);
  return wasi;
}

function nim_str_to_js_str(data_buffer_addr, len, ctx) {
  let { HEAPU8, decoder } = ctx;
  let thing = new Uint8Array(HEAPU8.buffer, data_buffer_addr, len);
  return decoder.decode(thing);
}

function get_env_imports() {
  let memory = new WebAssembly.Memory({
    initial: 10000,
    maximum: 10000,
  });
  const HEAPU8 = new Uint8Array(memory.buffer);
  const DATA = new DataView(memory.buffer);
  const decoder = new TextDecoder();
  const ctx = {
    HEAPU8,
    DATA,
    decoder,
  };
  const env_imports = {
    memory,
    // functions
    get_time: () => Math.round(performance.now() * 1000),
    write_row_string: (str_addr, len) => {
      let text = nim_str_to_js_str(str_addr, len, ctx);
      csv_rows.push(text);
    },
  };
  return env_imports;
}

async function run_wasm() {
  try {
    /* setup the wasm module instance */
    let wasm = fs.readFileSync(WASM_PATH);
    let wasm_data = await instantiate_wasm(wasm, {
      streaming: false,
      env_imports: get_env_imports(),
      wasi: get_wasi(),
    });
    /* run the wasm */
    return run_nim_main(wasm_data);
  } catch (e) {
    console.error(e);
    process.exitCode = 1;
  }
}

if (WASM_PATH) {
  run_wasm().then(() => {
    fs.writeFileSync(OUTPUT_PATH, csv_rows.join("\n"));
  });
} else {
  console.error(new Error("An argument must be supplied with the path to a wasm file to run."));
  process.exitCode = 1;
}
