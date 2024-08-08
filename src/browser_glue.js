import { instantiate_wasm, run_nim_main } from "./setup_wasm.js";

// Pickup the wasm path from vite
const WASM_PATH = import.meta.env.VITE_WASM_PATH
console.log("WASM_PATH", WASM_PATH)

async function run_wasm() {
  try {
    /* setup the wasm module instance */
    let wasm = fetch(WASM_PATH)
    let wasm_data = await instantiate_wasm(wasm, { streaming: true })
    /* run the wasm */
    return run_nim_main(wasm_data)
  } catch (e) {
    console.error(e);
  }
}

if (WASM_PATH) {
  run_wasm();
} else {
  console.error(new Error("An argument must be supplied with the path to a wasm file to run."))
}

