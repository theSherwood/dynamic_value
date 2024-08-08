import { instantiate_wasm, run_nim_main } from "./setup_wasm.js";
import fs from "node:fs"

const WASM_PATH = process.argv[2]
console.log("WASM_PATH", WASM_PATH)

async function run_wasm() {
  try {
    /* setup the wasm module instance */
    let wasm = fs.readFileSync(WASM_PATH);
    let wasm_data = await instantiate_wasm(wasm, { streaming: false })
    /* run the wasm */
    return run_nim_main(wasm_data)
  } catch (e) {
    console.error(e);
    process.exitCode = 1;
  }
}

if (WASM_PATH) {
  run_wasm();
} else {
  console.error(new Error("An argument must be supplied with the path to a wasm file to run."))
  process.exitCode = 1
}

