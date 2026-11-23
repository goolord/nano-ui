// Load a nano-ui-sdl-web program: the wasm module scripts/build-web.sh
// links, the JSFFI glue GHC's post-link.mjs generates for it, and the WASI
// shim, all served beside this file. The page's canvas is passed to the
// runtime in nano-ui-sdl-web.js before the program starts.
import { WASI, OpenFile, File, ConsoleStdout } from "./browser_wasi_shim/index.js";
import ghcWasmJsffi from "./ghc_wasm_jsffi.js";

// rtsFlags are GHC runtime options, read through GHCRTS as a reactor module
// takes no command line: "-T" keeps the statistics askSdlWebDebug's runtime
// rows show. env is more of the program's environment, as { NAME: "value" }:
// what the page has to tell the program before it starts, which the program
// reads with System.Environment.lookupEnv.
export async function start(canvas, wasmUrl, { rtsFlags = "", env = {} } = {}) {
  globalThis.nanoUiSdlWeb.setup(canvas);
  const fds = [
    new OpenFile(new File([])),
    ConsoleStdout.lineBuffered((msg) => console.log(msg)),
    ConsoleStdout.lineBuffered((msg) => console.warn(msg)),
  ];
  const environment = Object.entries(env).map(([name, value]) => `${name}=${value}`);
  if (rtsFlags) environment.push(`GHCRTS=${rtsFlags}`);
  const wasi = new WASI([], environment, fds);
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
