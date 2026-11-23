// Load a nano-ui-sdl-web program: the wasm module scripts/build-web.sh
// links, the JSFFI glue GHC's post-link.mjs generates for it, and the WASI
// shim, all served beside this file. The page's canvas is passed to the
// runtime in nano-ui-sdl-web.js before the program starts.
import { WASI, OpenFile, File, ConsoleStdout } from "./browser_wasi_shim/index.js";
import ghcWasmJsffi from "./ghc_wasm_jsffi.js";

export async function start(canvas, wasmUrl) {
  globalThis.nanoUiSdlWeb.setup(canvas);
  const fds = [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered((msg) => console.log(msg)),
    ConsoleStdout.lineBuffered((msg) => console.warn(msg)),
  ];
  const wasi = new WASI([], [], fds);
  // The glue reads the instance's exports, which exist only once it is
  // instantiated; it is handed the object they are copied into.
  const exports = {};
  const { instance } = await WebAssembly.instantiateStreaming(fetch(wasmUrl), {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghcWasmJsffi(exports),
  });
  Object.assign(exports, instance.exports);
  wasi.initialize(instance);
  instance.exports.hs_init(0, 0);
  await instance.exports.nanoMain();
}
