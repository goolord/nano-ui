// The page's side of nano-ui-sdl-web: the canvas, its WebGL2 renderer, the
// event queue the Haskell session reads, and the text input element.
// NanoUI.SdlWeb.Internal.Host lists the calls; NanoUI.SdlWeb.Internal.Input
// documents the event records. Load this before the wasm module and call
// nanoUiSdlWeb.setup(canvas).

const EVENT_WORDS = 8; // eventRecordSize / 4
const COMMAND_WORDS = 8; // commandRecordSize / 4

const EV_RESIZE = 1;
const EV_MOTION = 2;
const EV_BUTTON = 3;
const EV_LEAVE = 4;
const EV_SCROLL = 5;
const EV_KEY = 6;
const EV_CHAR = 7;
const EV_COMPOSITION = 8;
const EV_COMPOSITION_CHAR = 9;
const EV_FOCUS_LOST = 10;
const EV_REDRAW = 11;

// Named keys, numbered as decodeKey in NanoUI.SdlWeb.Internal.Input reads
// them: -1 to -21 in the order of Key from KeyBackspace to KeyMenu.
const NAMED_KEYS = {
  Backspace: -1, Delete: -2, Enter: -3, NumpadEnter: -3, Escape: -4, Tab: -5,
  ArrowLeft: -6, ArrowRight: -7, ArrowUp: -8, ArrowDown: -9, Home: -10, End: -11,
  PageUp: -12, PageDown: -13, Insert: -14, Space: -15, PrintScreen: -16, Pause: -17,
  CapsLock: -18, NumLock: -19, ScrollLock: -20, ContextMenu: -21,
};

// Keys that type a character on a US layout, by KeyboardEvent.code: the
// key's character when the layout's own cannot be read (a chord changed it).
const US_KEYS = {
  Backquote: "`", Minus: "-", Equal: "=", BracketLeft: "[", BracketRight: "]",
  Backslash: "\\", Semicolon: ";", Quote: "'", Comma: ",", Period: ".", Slash: "/",
  IntlBackslash: "\\", NumpadDivide: "/", NumpadMultiply: "*", NumpadSubtract: "-",
  NumpadAdd: "+", NumpadEqual: "=",
};

const MODIFIER_CODES = new Set([
  "ShiftLeft", "ShiftRight", "ControlLeft", "ControlRight", "AltLeft", "AltRight",
  "MetaLeft", "MetaRight", "OSLeft", "OSRight",
]);

const VERTEX_SHADER = `#version 300 es
in vec2 aPos;
in vec4 aColor;
in vec2 aUv;
uniform vec2 uScale;
out vec4 vColor;
out vec2 vUv;
void main() {
  gl_Position = vec4(aPos.x * uScale.x - 1.0, 1.0 - aPos.y * uScale.y, 0.0, 1.0);
  vColor = aColor;
  vUv = aUv;
}`;

const FRAGMENT_SHADER = `#version 300 es
precision mediump float;
in vec4 vColor;
in vec2 vUv;
uniform sampler2D uTex;
out vec4 outColor;
void main() {
  outColor = texture(uTex, vUv) * vColor;
}`;

class NanoUiSdlWeb {
  constructor() {
    this.memory = null;
    this.canvas = null;
    this.gl = null;
    this.queue = [];
    this.waiter = null;
    this.waitTimer = null;
    this.wakePending = false;
    this.settleTimer = null;
    this.clipboard = null;
    this.focused = false;
    this.composing = false;
    this.textInputOn = false;
    this.scale = 1;
    this.backing = [1, 1];
    this.textures = [];
    this.textureSizes = [];
    this.encoder = new TextEncoder();
    this.decoder = new TextDecoder();
    this.localFonts = null;
    this.fontList = new Uint8Array(0);
    this.fontBytes = new Uint8Array(0);
  }

  // ---------------------------------------------------------------- setup

  setup(canvas) {
    this.canvas = canvas;
    canvas.tabIndex = -1;
    canvas.style.touchAction = "none";
    canvas.style.outline = "none";
    this.input = this.makeTextInput();
    this.initGl();
    canvas.addEventListener("webglcontextlost", (e) => e.preventDefault());
    canvas.addEventListener("webglcontextrestored", () => {
      this.initGl();
      this.push(EV_REDRAW);
    });
    this.listenPointer(canvas);
    this.listenKeys(this.input);
    this.pinCssSize(canvas);
    // Only a change of the CSS box is a resize: the session sets the
    // drawing buffer from it, and must not hear back from its own change.
    let box = [canvas.clientWidth, canvas.clientHeight];
    new ResizeObserver(() => {
      const now = [canvas.clientWidth, canvas.clientHeight];
      if (now[0] === box[0] && now[1] === box[1]) return;
      box = now;
      this.push(EV_RESIZE);
    }).observe(canvas);
    this.watchPixelRatio();
    const scheme = window.matchMedia("(prefers-color-scheme: dark)");
    scheme.addEventListener("change", () => this.push(EV_RESIZE));
    this.focusInput();
  }

  // The session sizes the drawing buffer from the canvas's CSS box, times
  // devicePixelRatio. A canvas the page gives no CSS width or height takes
  // its box from the drawing buffer, so each resize would grow the next
  // (doubling at a ratio of 2). An axis whose box follows the buffer is
  // fixed at the size it has now.
  pinCssSize(canvas) {
    const [w, h] = [canvas.width, canvas.height];
    const [cw, ch] = [canvas.clientWidth, canvas.clientHeight];
    canvas.width = w + 16;
    canvas.height = h + 16;
    const followsW = canvas.clientWidth !== cw;
    const followsH = canvas.clientHeight !== ch;
    canvas.width = w;
    canvas.height = h;
    if (followsW) canvas.style.width = `${cw}px`;
    if (followsH) canvas.style.height = `${ch}px`;
  }

  // The element keys and text go through. A textarea receives typed text,
  // input method compositions and pastes; it sits over the caret while text
  // input is on, so the input method's window opens there.
  makeTextInput() {
    const ta = document.createElement("textarea");
    ta.setAttribute("autocomplete", "off");
    ta.setAttribute("autocorrect", "off");
    ta.setAttribute("autocapitalize", "off");
    ta.setAttribute("spellcheck", "false");
    ta.setAttribute("aria-hidden", "true");
    Object.assign(ta.style, {
      position: "fixed", left: "0px", top: "0px", width: "1px", height: "1px",
      opacity: "0", border: "0", padding: "0", margin: "0", resize: "none",
      overflow: "hidden", whiteSpace: "pre", pointerEvents: "none", zIndex: "-1",
    });
    ta.inputMode = "none";
    document.body.appendChild(ta);
    ta.addEventListener("focus", () => { this.focused = true; this.push(EV_RESIZE); });
    ta.addEventListener("blur", () => { this.focused = false; this.push(EV_FOCUS_LOST); });
    return ta;
  }

  focusInput() {
    if (document.activeElement !== this.input) this.input.focus({ preventScroll: true });
  }

  initGl() {
    const gl = this.canvas.getContext("webgl2", {
      alpha: false, antialias: false, depth: false, stencil: false,
      premultipliedAlpha: false, preserveDrawingBuffer: true,
    });
    if (!gl) throw new Error("nano-ui-sdl-web: WebGL2 is not available");
    this.gl = gl;
    const program = gl.createProgram();
    for (const [type, src] of [[gl.VERTEX_SHADER, VERTEX_SHADER], [gl.FRAGMENT_SHADER, FRAGMENT_SHADER]]) {
      const shader = gl.createShader(type);
      gl.shaderSource(shader, src);
      gl.compileShader(shader);
      if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(shader));
      gl.attachShader(program, shader);
    }
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(program));
    gl.useProgram(program);
    this.uScale = gl.getUniformLocation(program, "uScale");
    gl.uniform1i(gl.getUniformLocation(program, "uTex"), 0);
    this.vertexBuffer = gl.createBuffer();
    this.indexBuffer = gl.createBuffer();
    const vao = gl.createVertexArray();
    gl.bindVertexArray(vao);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);
    // A vertex is 32 bytes: position, colour (0 to 1) and UV, as floats.
    // The stride is the core's vertexSize (NanoUI.Internal.Draw.Types) and
    // NANO_UI_VERTEX_BYTES in nano-ui-ttf's nano_ui_ttf_shared.h; JavaScript
    // cannot read either, so nano-ui-sdl-web-test checks only that those two
    // agree. Change all three together.
    const attrs = [["aPos", 2, 0], ["aColor", 4, 8], ["aUv", 2, 24]];
    for (const [name, size, offset] of attrs) {
      const loc = gl.getAttribLocation(program, name);
      gl.enableVertexAttribArray(loc);
      gl.vertexAttribPointer(loc, size, gl.FLOAT, false, 32, offset);
    }
    // SDL_BLENDMODE_BLEND: colour by source alpha, alpha accumulating.
    gl.enable(gl.BLEND);
    gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA, gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
    gl.activeTexture(gl.TEXTURE0);
    this.white = this.makeTexture(1, 1, new Uint8Array([255, 255, 255, 255]));
    this.textures = [];
    this.textureSizes = [];
  }

  makeTexture(w, h, pixels) {
    const gl = this.gl;
    const tex = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, tex);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA8, w, h, 0, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    // Linear filtering, as the SDL backend's atlases use.
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    return tex;
  }

  // A change of devicePixelRatio (browser zoom, a move to another display)
  // matches no media query for the old ratio; watch the new one each time.
  watchPixelRatio() {
    const query = window.matchMedia(`(resolution: ${window.devicePixelRatio}dppx)`);
    query.addEventListener("change", () => {
      this.push(EV_RESIZE);
      this.watchPixelRatio();
    }, { once: true });
  }

  // ---------------------------------------------------------------- events

  push(kind, a = 0, b = 0, c = 0, mods = 0, x = 0, y = 0, z = 0) {
    this.queue.push([kind, a, b, c, mods, x, y, z]);
    this.settle();
  }

  // Answer a waiting session from a task of its own, after the event's
  // default actions: a paste a Ctrl+V key starts is then already in hand
  // when the session reads the clipboard for that key.
  settle() {
    if (!this.waiter || this.settleTimer !== null) return;
    this.settleTimer = setTimeout(() => {
      this.settleTimer = null;
      this.release();
    }, 0);
  }

  release() {
    const resolve = this.waiter;
    this.waiter = null;
    if (this.waitTimer !== null) {
      clearTimeout(this.waitTimer);
      this.waitTimer = null;
    }
    if (resolve) resolve();
  }

  static mods(e) {
    let m = 0;
    if (e.shiftKey) m |= 1;
    if (e.ctrlKey) m |= 2;
    if (e.altKey) m |= 4;
    if (e.metaKey) m |= 8;
    if (e.getModifierState && e.getModifierState("NumLock")) m |= 16;
    return m;
  }

  pointerPos(e) {
    const r = this.canvas.getBoundingClientRect();
    return [e.clientX - r.left, e.clientY - r.top];
  }

  listenPointer(canvas) {
    canvas.addEventListener("pointermove", (e) => {
      const [x, y] = this.pointerPos(e);
      this.push(EV_MOTION, 0, 0, 0, NanoUiSdlWeb.mods(e), x, y);
    });
    canvas.addEventListener("pointerdown", (e) => {
      e.preventDefault();
      this.focusInput();
      // Capture keeps a drag's motion and release coming from outside.
      canvas.setPointerCapture(e.pointerId);
      const [x, y] = this.pointerPos(e);
      this.push(EV_MOTION, 0, 0, 0, NanoUiSdlWeb.mods(e), x, y);
      this.push(EV_BUTTON, e.button, 1, 0, NanoUiSdlWeb.mods(e));
    });
    canvas.addEventListener("pointerup", (e) => {
      const [x, y] = this.pointerPos(e);
      this.push(EV_MOTION, 0, 0, 0, NanoUiSdlWeb.mods(e), x, y);
      this.push(EV_BUTTON, e.button, 0, 0, NanoUiSdlWeb.mods(e));
    });
    canvas.addEventListener("pointerleave", () => this.push(EV_LEAVE));
    canvas.addEventListener("contextmenu", (e) => e.preventDefault());
    canvas.addEventListener("wheel", (e) => {
      e.preventDefault();
      // Notches, as SDL reports the wheel: a line is a third of one, and a
      // pixel delta a hundredth.
      const unit = e.deltaMode === 1 ? 1 / 3 : e.deltaMode === 2 ? 3 : 1 / 100;
      this.push(EV_SCROLL, 0, 0, 0, NanoUiSdlWeb.mods(e), e.deltaX * unit, e.deltaY * unit);
    }, { passive: false });
  }

  // The key code decodeKey reads: positive for a key that types a
  // character, negative for a named key, 0 for a modifier.
  static keyCode(e) {
    if (MODIFIER_CODES.has(e.code)) return 0;
    if (e.code in NAMED_KEYS) return NAMED_KEYS[e.code];
    const f = /^F(\d+)$/.exec(e.key);
    if (f) return -(100 + Number(f[1]));
    const pad = /^Numpad(\d)$/.exec(e.code);
    if (pad) return -(300 + pad[1].charCodeAt(0));
    if (e.code === "NumpadDecimal") return -(300 + 46);
    // The layout's character, when no chord changed it.
    if ([...e.key].length === 1 && !e.ctrlKey && !e.altKey && !e.metaKey) {
      return e.key.toLowerCase().codePointAt(0);
    }
    const letter = /^Key([A-Z])$/.exec(e.code);
    if (letter) return letter[1].toLowerCase().charCodeAt(0);
    const digit = /^Digit(\d)$/.exec(e.code);
    if (digit) return digit[1].charCodeAt(0);
    if (e.code in US_KEYS) return US_KEYS[e.code].charCodeAt(0);
    return null;
  }

  listenKeys(ta) {
    const key = (e, down) => {
      if (e.isComposing || e.keyCode === 229) return;
      const code = NanoUiSdlWeb.keyCode(e);
      if (code === null) return;
      this.push(EV_KEY, code, down ? 1 : 0, 0, NanoUiSdlWeb.mods(e));
      // Let typing reach the textarea (Space too: it is a named key that
      // also types, as SDL reports it), and Ctrl/Cmd+C, X and V raise their
      // clipboard events; everything else is the view's.
      const chord = e.ctrlKey || e.metaKey;
      const clip = chord && ["KeyC", "KeyX", "KeyV"].includes(e.code);
      const types = e.code === "Space" && !chord && !e.altKey;
      if (down && ((code < 0 && !types) || (chord && !clip))) e.preventDefault();
    };
    ta.addEventListener("keydown", (e) => key(e, true));
    ta.addEventListener("keyup", (e) => key(e, false));
    ta.addEventListener("input", (e) => {
      if (this.composing) return;
      if (e.inputType === "insertText" || e.inputType === "insertReplacementText") {
        this.pushText(e.data || "");
      }
      ta.value = "";
    });
    ta.addEventListener("compositionstart", () => { this.composing = true; });
    ta.addEventListener("compositionupdate", (e) => this.pushComposition(e.data || ""));
    ta.addEventListener("compositionend", (e) => {
      this.composing = false;
      this.pushComposition("");
      this.pushText(e.data || "");
      ta.value = "";
    });
    ta.addEventListener("paste", (e) => {
      e.preventDefault();
      this.clipboard = e.clipboardData ? e.clipboardData.getData("text/plain") : this.clipboard;
    });
    ta.addEventListener("copy", (e) => e.preventDefault());
    ta.addEventListener("cut", (e) => e.preventDefault());
  }

  pushText(text) {
    for (const ch of text) this.push(EV_CHAR, ch.codePointAt(0));
  }

  pushComposition(text) {
    const chars = [...text];
    this.push(EV_COMPOSITION, chars.length, chars.length, 0);
    for (const ch of chars) this.push(EV_COMPOSITION_CHAR, ch.codePointAt(0));
  }

  // ---------------------------------------------------------------- host calls

  attach(memory) {
    this.memory = memory;
  }

  bytes(ptr, len) {
    return new Uint8Array(this.memory.buffer, ptr, len);
  }

  text(ptr, len) {
    return this.decoder.decode(this.bytes(ptr, len).slice());
  }

  queryCanvas(ptr) {
    const out = new Float32Array(this.memory.buffer, ptr, 5);
    out[0] = this.canvas.clientWidth;
    out[1] = this.canvas.clientHeight;
    out[2] = window.devicePixelRatio || 1;
    out[3] = this.focused && document.hasFocus() ? 1 : 0;
    const dark = window.matchMedia("(prefers-color-scheme: dark)").matches;
    const light = window.matchMedia("(prefers-color-scheme: light)").matches;
    out[4] = dark ? 2 : light ? 1 : 0;
  }

  takeEvents(ptr, n) {
    const count = Math.min(n, this.queue.length);
    if (count === 0) return 0;
    const ints = new Int32Array(this.memory.buffer, ptr, count * EVENT_WORDS);
    const floats = new Float32Array(this.memory.buffer, ptr, count * EVENT_WORDS);
    for (let i = 0; i < count; i++) {
      const ev = this.queue[i];
      const o = i * EVENT_WORDS;
      for (let k = 0; k < 5; k++) ints[o + k] = ev[k];
      floats[o + 5] = ev[5];
      floats[o + 6] = ev[6];
      floats[o + 7] = ev[7];
    }
    this.queue.splice(0, count);
    return count;
  }

  waitEvents(ms) {
    if (this.queue.length > 0 || this.wakePending) {
      this.wakePending = false;
      return Promise.resolve();
    }
    return new Promise((resolve) => {
      this.waiter = () => {
        this.wakePending = false;
        resolve();
      };
      if (ms >= 0) this.waitTimer = setTimeout(() => this.release(), ms);
    });
  }

  wake() {
    this.wakePending = true;
    if (this.waiter) this.settle();
  }

  resizeBacking(w, h) {
    this.canvas.width = w;
    this.canvas.height = h;
    this.backing = [w, h];
  }

  uploadTexture(slot, w, h, ptr, rowPixels, x, y, rw, rh) {
    const gl = this.gl;
    const size = this.textureSizes[slot];
    if (!this.textures[slot] || !size || size[0] !== w || size[1] !== h) {
      if (this.textures[slot]) gl.deleteTexture(this.textures[slot]);
      this.textures[slot] = this.makeTexture(w, h, null);
      this.textureSizes[slot] = [w, h];
    }
    gl.bindTexture(gl.TEXTURE_2D, this.textures[slot]);
    gl.pixelStorei(gl.UNPACK_ROW_LENGTH, rowPixels);
    gl.pixelStorei(gl.UNPACK_SKIP_PIXELS, x);
    gl.pixelStorei(gl.UNPACK_SKIP_ROWS, y);
    const source = this.bytes(ptr, rowPixels * 4 * (y + rh));
    gl.texSubImage2D(gl.TEXTURE_2D, 0, x, y, rw, rh, gl.RGBA, gl.UNSIGNED_BYTE, source);
    gl.pixelStorei(gl.UNPACK_ROW_LENGTH, 0);
    gl.pixelStorei(gl.UNPACK_SKIP_PIXELS, 0);
    gl.pixelStorei(gl.UNPACK_SKIP_ROWS, 0);
  }

  beginFrame(clear, r, g, b, a, scale, w, h) {
    const gl = this.gl;
    this.scale = scale;
    this.backing = [w, h];
    gl.viewport(0, 0, w, h);
    gl.uniform2f(this.uScale, (2 * scale) / w, (2 * scale) / h);
    if (clear) {
      gl.disable(gl.SCISSOR_TEST);
      gl.clearColor(r, g, b, a);
      gl.clear(gl.COLOR_BUFFER_BIT);
    }
  }

  draw(vertices, vertexCount, indices, indexCount, commands, commandCount) {
    const gl = this.gl;
    gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, this.bytes(vertices, vertexCount * 32), gl.STREAM_DRAW);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, this.bytes(indices, indexCount * 4), gl.STREAM_DRAW);
    const cmds = new Int32Array(this.memory.buffer, commands, commandCount * COMMAND_WORDS);
    const height = this.backing[1];
    for (let i = 0; i < commandCount; i++) {
      const o = i * COMMAND_WORDS;
      const slot = cmds[o];
      gl.bindTexture(gl.TEXTURE_2D, (slot >= 0 && this.textures[slot]) || this.white);
      if (cmds[o + 3]) {
        gl.enable(gl.SCISSOR_TEST);
        // GL's scissor counts rows from the bottom.
        gl.scissor(cmds[o + 4], height - cmds[o + 5] - cmds[o + 7], cmds[o + 6], cmds[o + 7]);
      } else {
        gl.disable(gl.SCISSOR_TEST);
      }
      gl.drawElements(gl.TRIANGLES, cmds[o + 2], gl.UNSIGNED_INT, cmds[o + 1] * 4);
    }
    gl.disable(gl.SCISSOR_TEST);
  }

  setCursor(ptr, len) {
    this.canvas.style.cursor = this.text(ptr, len);
  }

  textInput(purpose, x, y, w, h) {
    const ta = this.input;
    this.textInputOn = purpose >= 0;
    ta.inputMode = purpose < 0 ? "none" : purpose === 2 ? "decimal" : "text";
    const r = this.canvas.getBoundingClientRect();
    Object.assign(ta.style, {
      left: `${r.left + x}px`, top: `${r.top + y}px`,
      width: `${Math.max(1, w)}px`, height: `${Math.max(1, h)}px`,
    });
  }

  clipboardRead(ptr, n) {
    if (this.clipboard === null) return -1;
    const encoded = this.encoder.encode(this.clipboard);
    if (n > 0) this.bytes(ptr, Math.min(n, encoded.length)).set(encoded.subarray(0, n));
    return encoded.length;
  }

  clipboardWrite(ptr, len) {
    const text = this.text(ptr, len);
    this.clipboard = text;
    if (navigator.clipboard) navigator.clipboard.writeText(text).catch(() => {});
  }

  setTitle(ptr, len) {
    document.title = this.text(ptr, len);
  }

  // Copy up to n bytes of encoded text to ptr; answer its full length.
  writeText(text, ptr, n) {
    const encoded = this.encoder.encode(text);
    if (n > 0) this.bytes(ptr, Math.min(n, encoded.length)).set(encoded.subarray(0, n));
    return encoded.length;
  }

  // The WebGL renderer's name: the unmasked one where the browser gives it.
  rendererInfo(ptr, n) {
    const gl = this.gl;
    let name = gl.getParameter(gl.RENDERER);
    const info = gl.getExtension("WEBGL_debug_renderer_info");
    if (info) name = gl.getParameter(info.UNMASKED_RENDERER_WEBGL) || name;
    return this.writeText(`WebGL2, ${name}`, ptr, n);
  }

  // ---------------------------------------------------------------- fonts

  // The installed fonts, through the Local Font Access API. The browser asks
  // the user the first time, and only during a click or key press, which a
  // view's press still counts as. Answers the length of the sorted family
  // names, one a line, for takeFontList; -1 without the API, -2 declined.
  async findFonts() {
    if (typeof window.queryLocalFonts !== "function") return -1;
    try {
      this.localFonts = await window.queryLocalFonts();
    } catch (e) {
      console.warn("nano-ui-sdl-web: installed fonts not available:", e);
      return -2;
    }
    const families = [...new Set(this.localFonts.map((f) => f.family))].sort();
    this.fontList = this.encoder.encode(families.join("\n"));
    return this.fontList.length;
  }

  takeFontList(ptr) {
    this.bytes(ptr, this.fontList.length).set(this.fontList);
    this.fontList = new Uint8Array(0);
  }

  // A font file's bytes: an installed family's regular face (kind 0), or a
  // URL (kind 1). Answers their length for takeFontBytes, or -1.
  async loadFont(kind, ptr, len) {
    const name = this.text(ptr, len);
    try {
      let blob;
      if (kind === 0) {
        if (!this.localFonts) {
          if (typeof window.queryLocalFonts !== "function") return -1;
          this.localFonts = await window.queryLocalFonts();
        }
        const faces = this.localFonts.filter((f) => f.family === name);
        const face = faces.find((f) => /^(regular|normal|book|roman)$/i.test(f.style)) || faces[0];
        if (!face) return -1;
        blob = await face.blob();
      } else {
        const response = await fetch(name);
        if (!response.ok) return -1;
        blob = await response.blob();
      }
      this.fontBytes = new Uint8Array(await blob.arrayBuffer());
      return this.fontBytes.length;
    } catch (e) {
      console.warn(`nano-ui-sdl-web: could not load the font ${name}:`, e);
      return -1;
    }
  }

  takeFontBytes(ptr) {
    this.bytes(ptr, this.fontBytes.length).set(this.fontBytes);
    this.fontBytes = new Uint8Array(0);
  }
}

globalThis.nanoUiSdlWeb = new NanoUiSdlWeb();
