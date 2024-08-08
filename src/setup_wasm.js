import { WASI, File, OpenFile, ConsoleStdout } from "@bjorn3/browser_wasi_shim";

const VERBOSE = 0;

function get_default_wasi() {
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

function get_default_env_imports() {
  let memory = new WebAssembly.Memory({
    initial: 10000,
    maximum: 10000,
  });
  const env_imports = {
    memory,
    // functions
    get_time: () => Date.now() * 1000,
  };
  return env_imports;
}

export async function instantiate_wasm(wasm, opts = {}) {
  if (VERBOSE) console.log("import.meta", import.meta);

  let { streaming, env_imports, wasi } = opts;

  /* setup imports */
  let imports;
  if (!env_imports) env_imports = get_default_env_imports();
  if (!wasi) wasi = get_default_wasi();
  {
    imports = {
      env: env_imports,
      wasi_snapshot_preview1: wasi.wasiImport,
    };
  }

  /* setup the wasm module instance */
  let wasm_module_instance;
  {
    if (streaming) {
      let compiled = await WebAssembly.compileStreaming(wasm);
      wasm_module_instance = await WebAssembly.instantiate(compiled, imports);
    } else {
      let wrapper = await WebAssembly.instantiate(wasm, imports);
      wasm_module_instance = wrapper.instance;
    }
  }

  /*
      Fix - We do this to add a memory export (even though we are importing
      memory). Wasi requires a memory export for fd_write. However, this may
      break all sorts of other stuff.
    */
  let new_exports;
  let { exports } = wasm_module_instance;
  if (VERBOSE) console.log("exports", exports);
  let fake_instance = { exports: { ...exports, memory: imports.env.memory } };
  wasi.initialize(fake_instance);
  new_exports = fake_instance.exports;

  return {
    wasi,
    raw_instance: wasm_module_instance,
    instance: fake_instance,
    imports,
    exports: new_exports,
  };
}

export function run_nim_main(wasm_data) {
  return wasm_data.exports.NimMain();
}
