// The page's side of nano-ui-sdl-web: the canvas, its WebGL2 renderer, the
// event queue the Haskell session reads, and the text input element.
// NanoUI.SdlWeb.Internal.Host lists the calls; NanoUI.SdlWeb.Internal.Input
// documents the event records. Load this before the wasm module and call
// nanoUiSdlWeb.setup(canvas).

const EVENT_WORDS = 8; // eventRecordSize / 4

// A timed wait this short paces an animation (two frames at 60 Hz), and
// ends on one of the display's frames. FRAME_SLACK_MS is how far before its
// time such a wait may end: the session measures a frame from its own start,
// a little after the display's, so the frame it is waiting for comes a
// little early.
const FRAME_WAIT_MS = 34;
const FRAME_SLACK_MS = 4;
const COMMAND_WORDS = 12; // commandRecordSize / 4

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
uniform vec2 uOrigin;
out vec4 vColor;
out vec2 vUv;
void main() {
  vec2 p = aPos - uOrigin;
  gl_Position = vec4(p.x * uScale.x - 1.0, 1.0 - p.y * uScale.y, 0.0, 1.0);
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
    this.waitFrame = null;
    this.shown = true;
    this.held = false;
    this.wakePending = false;
    this.settleTimer = null;
    this.clipboard = null;
    this.focused = false;
    this.colorScheme = null;
    this.composing = false;
    this.textInputOn = false;
    this.scale = 1;
    this.backing = [1, 1];
    this.curSize = [1, 1];
    this.originX = 0;
    this.originY = 0;
    this.uScaleX = 1;
    this.uScaleY = 1;
    this.textures = [];
    this.textureSizes = [];
    this.shaders = new Map();
    this.targets = [];
    this.uniformData = new Uint8Array(0);
    this.encoder = new TextEncoder();
    this.decoder = new TextDecoder();
    this.localFonts = null;
    this.installedFamilies = [];
    this.serviceFamilies = [];
    this.fontService = 0;
    this.fontsVersion = 0;
    this.discovering = false;
    this.fontList = new Uint8Array(0);
    this.fontBytes = new Uint8Array(0);
    this.picked = new Uint8Array(0);
  }

  // ---------------------------------------------------------------- setup

  setup(canvas) {
    this.canvas = canvas;
    canvas.tabIndex = -1;
    // Every touch goes to the program unless the page's inline style says
    // otherwise: "pan-y" on a canvas in a scrolling page lets a vertical
    // swipe scroll the page (see pointercancel in listenPointer).
    if (!canvas.style.touchAction) canvas.style.touchAction = "none";
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
    // A canvas scrolled out of the viewport shows nothing, so its timed
    // waits are held (waitEvents) and one frame is drawn when it is back.
    new IntersectionObserver((entries) => {
      this.shown = entries[entries.length - 1].isIntersecting;
      if (this.shown && this.held) this.release();
    }).observe(canvas);
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
    const vs = gl.createShader(gl.VERTEX_SHADER);
    gl.shaderSource(vs, VERTEX_SHADER);
    gl.compileShader(vs);
    if (!gl.getShaderParameter(vs, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(vs));
    const fs = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fs, FRAGMENT_SHADER);
    gl.compileShader(fs);
    if (!gl.getShaderParameter(fs, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(fs));
    gl.attachShader(program, vs);
    gl.attachShader(program, fs);
    // Pin attribute locations 0/1/2 so custom fragment shader programs share the same VAO.
    gl.bindAttribLocation(program, 0, "aPos");
    gl.bindAttribLocation(program, 1, "aColor");
    gl.bindAttribLocation(program, 2, "aUv");
    gl.linkProgram(program);
    gl.deleteShader(vs);
    gl.deleteShader(fs);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(program));
    this.defaultProgram = program;
    gl.useProgram(program);
    this.uScale = gl.getUniformLocation(program, "uScale");
    this.uOrigin = gl.getUniformLocation(program, "uOrigin");
    gl.uniform1i(gl.getUniformLocation(program, "uTex"), 0);
    this.vertexBuffer = gl.createBuffer();
    this.indexBuffer = gl.createBuffer();
    this.shaderUbo = gl.createBuffer();
    const vao = gl.createVertexArray();
    this.vao = vao;
    gl.bindVertexArray(vao);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);
    // A vertex is 32 bytes: position, colour (0 to 1) and UV, as floats.
    // The stride is the core's vertexSize (NanoUI.Internal.Draw.Types) and
    // NANO_UI_VERTEX_BYTES in nano-ui-ttf's nano_ui_ttf_shared.h; JavaScript
    // cannot read either, so nano-ui-sdl-web-test checks only that those two
    // agree. Change all three together.
    const attrs = [[0, 2, 0], [1, 4, 8], [2, 2, 24]];
    for (const [loc, size, offset] of attrs) {
      gl.enableVertexAttribArray(loc);
      gl.vertexAttribPointer(loc, size, gl.FLOAT, false, 32, offset);
    }
    this.effectVbo = gl.createBuffer();
    this.effectVao = gl.createVertexArray();
    gl.bindVertexArray(this.effectVao);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.effectVbo);
    for (const [loc, size, offset] of attrs) {
      gl.enableVertexAttribArray(loc);
      gl.vertexAttribPointer(loc, size, gl.FLOAT, false, 32, offset);
    }
    gl.bindVertexArray(this.vao);
    // SDL_BLENDMODE_BLEND: colour by source alpha, alpha accumulating.
    gl.enable(gl.BLEND);
    gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA, gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
    gl.activeTexture(gl.TEXTURE0);
    this.white = this.makeTexture(1, 1, new Uint8Array([255, 255, 255, 255]));
    this.textures = [];
    this.textureSizes = [];
    this.shaders = new Map();
    this.targets = [];
    this.uniformData = new Uint8Array(0);
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
    this.held = false;
    if (this.waitTimer !== null) {
      clearTimeout(this.waitTimer);
      this.waitTimer = null;
    }
    if (this.waitFrame !== null) {
      cancelAnimationFrame(this.waitFrame);
      this.waitFrame = null;
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
    // The browser took the pointer for a gesture of its own, as when a
    // touch on a canvas whose touch-action allows panning starts to scroll
    // the page. No pointerup follows. The button is released here, far from
    // the canvas, so the widget that was pressed gets no click.
    canvas.addEventListener("pointercancel", (e) => {
      this.push(EV_MOTION, 0, 0, 0, NanoUiSdlWeb.mods(e), -1e5, -1e5);
      this.push(EV_BUTTON, Math.max(e.button, 0), 0, 0, NanoUiSdlWeb.mods(e));
      this.push(EV_LEAVE);
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

  setColorScheme(scheme) {
    this.colorScheme = scheme === "dark" || scheme === "light" ? scheme : null;
    if (this.canvas) this.push(EV_RESIZE);
  }

  queryCanvas(ptr) {
    const out = new Float32Array(this.memory.buffer, ptr, 5);
    out[0] = this.canvas.clientWidth;
    out[1] = this.canvas.clientHeight;
    out[2] = window.devicePixelRatio || 1;
    out[3] = this.focused && document.hasFocus() ? 1 : 0;
    if (this.colorScheme === "dark") {
      out[4] = 2;
    } else if (this.colorScheme === "light") {
      out[4] = 1;
    } else {
      const dark = window.matchMedia("(prefers-color-scheme: dark)").matches;
      const light = window.matchMedia("(prefers-color-scheme: light)").matches;
      out[4] = dark ? 2 : light ? 1 : 0;
    }
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
      if (ms < 0) return;
      // Out of view there is no frame to show, so a timed wait holds for an
      // event or for the canvas to come back.
      if (!this.shown) {
        this.held = true;
        return;
      }
      // A longer wait is a timer. A frame's wait ends on the display's
      // first frame at or after its time: a timer's steps are coarser than
      // a frame (15.6 ms on Windows), which drops every other one, and the
      // display's frames stop while the tab is hidden.
      if (ms > FRAME_WAIT_MS) {
        this.waitTimer = setTimeout(() => this.release(), ms);
        return;
      }
      const due = performance.now() + ms - FRAME_SLACK_MS;
      const onFrame = () => {
        this.waitFrame = null;
        if (performance.now() >= due) this.release();
        else this.waitFrame = requestAnimationFrame(onFrame);
      };
      this.waitFrame = requestAnimationFrame(onFrame);
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

  registerShader(slot, srcPtr, srcLen) {
    const gl = this.gl;
    const fragSrc = this.text(srcPtr, srcLen);
    const vs = gl.createShader(gl.VERTEX_SHADER);
    gl.shaderSource(vs, VERTEX_SHADER);
    gl.compileShader(vs);
    if (!gl.getShaderParameter(vs, gl.COMPILE_STATUS)) {
      console.error("nano-ui-sdl-web: vertex shader compile failed:", gl.getShaderInfoLog(vs));
      gl.deleteShader(vs);
      return 0;
    }
    const fs = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fs, fragSrc);
    gl.compileShader(fs);
    if (!gl.getShaderParameter(fs, gl.COMPILE_STATUS)) {
      console.error("nano-ui-sdl-web: fragment shader compile failed:", gl.getShaderInfoLog(fs));
      gl.deleteShader(vs);
      gl.deleteShader(fs);
      return 0;
    }
    const program = gl.createProgram();
    gl.attachShader(program, vs);
    gl.attachShader(program, fs);
    gl.bindAttribLocation(program, 0, "aPos");
    gl.bindAttribLocation(program, 1, "aColor");
    gl.bindAttribLocation(program, 2, "aUv");
    gl.linkProgram(program);
    gl.deleteShader(vs);
    gl.deleteShader(fs);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      console.error("nano-ui-sdl-web: shader link failed:", gl.getProgramInfoLog(program));
      gl.deleteProgram(program);
      return 0;
    }
    const blk = gl.getUniformBlockIndex(program, "NanoShaderBlock");
    if (blk !== gl.INVALID_INDEX) {
      gl.uniformBlockBinding(program, blk, 0);
    }
    const uScale = gl.getUniformLocation(program, "uScale");
    const uOrigin = gl.getUniformLocation(program, "uOrigin");
    const uInput = gl.getUniformLocation(program, "uInput");
    this.shaders.set(slot, { program, uScale, uOrigin, uInput });
    return 1;
  }

  uploadUniforms(ptr, len) {
    if (len <= 0) {
      this.uniformData = new Uint8Array(0);
    } else {
      this.uniformData = this.bytes(ptr, len).slice();
    }
  }

  acquireTarget(minW, minH, outCapsPtr) {
    const gl = this.gl;
    const needW = minW > 0 ? ((minW + 63) & ~63) : 64;
    const needH = minH > 0 ? ((minH + 63) & ~63) : 64;
    let best = -1;
    for (let i = 0; i < this.targets.length; i++) {
      const s = this.targets[i];
      if (!s.inUse && s.capW >= needW && s.capH >= needH && s.capW <= needW * 2 && s.capH <= needH * 2) {
        best = i;
        break;
      }
    }
    if (best < 0) {
      const tex = this.makeTexture(needW, needH, null);
      const fbo = gl.createFramebuffer();
      gl.bindFramebuffer(gl.FRAMEBUFFER, fbo);
      gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, tex, 0);
      const status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
      gl.bindFramebuffer(gl.FRAMEBUFFER, null);
      if (status !== gl.FRAMEBUFFER_COMPLETE) {
        gl.deleteFramebuffer(fbo);
        gl.deleteTexture(tex);
        return -1;
      }
      best = this.targets.length;
      this.targets.push({ fbo, tex, capW: needW, capH: needH, inUse: false });
    }
    const slot = this.targets[best];
    slot.inUse = true;
    const out = new Int32Array(this.memory.buffer, outCapsPtr, 2);
    out[0] = slot.capW;
    out[1] = slot.capH;
    return best;
  }

  releaseTarget(slot) {
    if (slot >= 0 && slot < this.targets.length) {
      this.targets[slot].inUse = false;
    }
  }

  bindTarget(slot, clear, ox, oy) {
    const gl = this.gl;
    if (slot >= 0 && slot < this.targets.length) {
      const s = this.targets[slot];
      gl.bindFramebuffer(gl.FRAMEBUFFER, s.fbo);
      this.curSize = [s.capW, s.capH];
    } else {
      gl.bindFramebuffer(gl.FRAMEBUFFER, null);
      this.curSize = [this.backing[0], this.backing[1]];
    }
    this.originX = ox;
    this.originY = oy;
    this.uScaleX = (2 * this.scale) / this.curSize[0];
    this.uScaleY = (2 * this.scale) / this.curSize[1];
    gl.viewport(0, 0, this.curSize[0], this.curSize[1]);
    gl.disable(gl.SCISSOR_TEST);
    if (clear) {
      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
    }
    gl.useProgram(this.defaultProgram);
    gl.uniform2f(this.uScale, this.uScaleX, this.uScaleY);
    gl.uniform2f(this.uOrigin, this.originX, this.originY);
  }

  compositeEffect(srcSlot, shaderSlot, blendCopy, rectUvPtr, scissorPtr, uPtr, uLen) {
    const gl = this.gl;
    if (srcSlot < 0 || srcSlot >= this.targets.length) return;
    const entry = this.shaders.get(shaderSlot);
    if (!entry) return;
    const rv = new Float32Array(this.memory.buffer, rectUvPtr, 8);
    const sc = new Int32Array(this.memory.buffer, scissorPtr, 5);
    const [dx, dy, dw, dh, u0, v0, u1, v1] = rv;
    const quad = new Float32Array([
      dx,      dy,      1, 1, 1, 1, u0, v0,
      dx + dw, dy,      1, 1, 1, 1, u1, v0,
      dx + dw, dy + dh, 1, 1, 1, 1, u1, v1,
      dx,      dy,      1, 1, 1, 1, u0, v0,
      dx + dw, dy + dh, 1, 1, 1, 1, u1, v1,
      dx,      dy + dh, 1, 1, 1, 1, u0, v1,
    ]);
    gl.useProgram(entry.program);
    if (entry.uScale !== null) gl.uniform2f(entry.uScale, 2 / this.curSize[0], 2 / this.curSize[1]);
    if (entry.uOrigin !== null) gl.uniform2f(entry.uOrigin, 0, 0);
    if (entry.uInput !== null) gl.uniform1i(entry.uInput, 0);
    if (uLen > 0) {
      gl.bindBuffer(gl.UNIFORM_BUFFER, this.shaderUbo);
      gl.bufferData(gl.UNIFORM_BUFFER, this.bytes(uPtr, uLen), gl.STREAM_DRAW);
      gl.bindBufferBase(gl.UNIFORM_BUFFER, 0, this.shaderUbo);
    }
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, this.targets[srcSlot].tex);
    // Always blend straight-alpha output into premultiplied offscreen targets
    // (cleared to 0,0,0,0) and onto the backbuffer.
    void blendCopy;
    gl.enable(gl.BLEND);
    gl.blendFuncSeparate(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA, gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
    if (sc[0]) {
      gl.enable(gl.SCISSOR_TEST);
      gl.scissor(sc[1], this.curSize[1] - sc[2] - sc[4], sc[3], sc[4]);
    } else {
      gl.disable(gl.SCISSOR_TEST);
    }
    gl.bindVertexArray(this.effectVao);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.effectVbo);
    gl.bufferData(gl.ARRAY_BUFFER, quad, gl.STREAM_DRAW);
    gl.drawArrays(gl.TRIANGLES, 0, 6);

    gl.bindVertexArray(this.vao);
    gl.enable(gl.BLEND);
    gl.disable(gl.SCISSOR_TEST);
    gl.useProgram(this.defaultProgram);
    gl.uniform2f(this.uScale, this.uScaleX, this.uScaleY);
    gl.uniform2f(this.uOrigin, this.originX, this.originY);
  }

  beginFrame(clear, r, g, b, a, scale, w, h) {
    const gl = this.gl;
    this.scale = scale;
    this.backing = [w, h];
    this.curSize = [w, h];
    this.originX = 0;
    this.originY = 0;
    this.uScaleX = (2 * scale) / w;
    this.uScaleY = (2 * scale) / h;
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, w, h);
    gl.useProgram(this.defaultProgram);
    gl.uniform2f(this.uScale, this.uScaleX, this.uScaleY);
    gl.uniform2f(this.uOrigin, 0, 0);
    if (clear) {
      gl.disable(gl.SCISSOR_TEST);
      gl.clearColor(r, g, b, a);
      gl.clear(gl.COLOR_BUFFER_BIT);
    }
  }

  draw(vertices, vertexCount, indices, indexCount, commands, commandCount) {
    const gl = this.gl;
    gl.bindVertexArray(this.vao);
    gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, this.bytes(vertices, vertexCount * 32), gl.STREAM_DRAW);
    gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, this.indexBuffer);
    gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, this.bytes(indices, indexCount * 4), gl.STREAM_DRAW);
    const cmds = new Int32Array(this.memory.buffer, commands, commandCount * COMMAND_WORDS);
    const height = this.curSize[1];
    let activeShader = 0;
    for (let i = 0; i < commandCount; i++) {
      const o = i * COMMAND_WORDS;
      const slot = cmds[o];
      const shaderSlot = cmds[o + 8];
      if (shaderSlot > 0) {
        const entry = this.shaders.get(shaderSlot);
        if (!entry) continue;
        if (activeShader !== shaderSlot) {
          gl.useProgram(entry.program);
          if (entry.uScale !== null) gl.uniform2f(entry.uScale, this.uScaleX, this.uScaleY);
          if (entry.uOrigin !== null) gl.uniform2f(entry.uOrigin, this.originX, this.originY);
          if (entry.uInput !== null) gl.uniform1i(entry.uInput, 0);
          activeShader = shaderSlot;
        }
        const uOff = cmds[o + 9];
        const uLen = cmds[o + 10];
        if (uLen > 0 && uOff + uLen <= this.uniformData.length) {
          gl.bindBuffer(gl.UNIFORM_BUFFER, this.shaderUbo);
          gl.bufferData(gl.UNIFORM_BUFFER, this.uniformData.subarray(uOff, uOff + uLen), gl.STREAM_DRAW);
          gl.bindBufferBase(gl.UNIFORM_BUFFER, 0, this.shaderUbo);
        }
      } else if (activeShader !== 0) {
        gl.useProgram(this.defaultProgram);
        gl.uniform2f(this.uScale, this.uScaleX, this.uScaleY);
        gl.uniform2f(this.uOrigin, this.originX, this.originY);
        activeShader = 0;
      }
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
    if (activeShader !== 0) {
      gl.useProgram(this.defaultProgram);
      gl.uniform2f(this.uScale, this.uScaleX, this.uScaleY);
      gl.uniform2f(this.uOrigin, this.originX, this.originY);
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

  // Font families come from two places. Installed ones, through the Local
  // Font Access API (Chromium-based browsers): once the user has allowed it
  // they are read at once; while the browser has not asked yet, the request
  // waits for the next press on the page, the only moment a browser shows
  // the question. And a font service's catalogue (1: Fontsource), in every
  // browser. Each arrival bumps fontsVersion and wakes the session, which
  // reads the merged list with fontFamilies.
  startFontDiscovery(service) {
    if (this.discovering) return;
    this.discovering = true;
    this.fontService = service;
    if (service === 1) {
      fetch("https://api.fontsource.org/v1/fonts")
        .then((r) => (r.ok ? r.json() : []))
        .then((fonts) => {
          this.serviceFamilies = fonts.map((f) => f.family);
          this.fontsChanged();
        })
        .catch((e) => console.warn("nano-ui-sdl-web: the font service's list is not available:", e));
    }
    if (typeof window.queryLocalFonts !== "function" || !navigator.permissions) return;
    navigator.permissions.query({ name: "local-fonts" }).then((status) => {
      if (status.state === "granted") {
        this.readInstalled();
      } else if (status.state === "prompt") {
        // The next press asks; its own effect on the page goes on as usual.
        const ask = () => {
          this.canvas.removeEventListener("pointerdown", ask, true);
          this.input.removeEventListener("keydown", ask, true);
          this.readInstalled();
        };
        this.canvas.addEventListener("pointerdown", ask, true);
        this.input.addEventListener("keydown", ask, true);
      }
      status.addEventListener("change", () => { if (status.state === "granted") this.readInstalled(); });
    }).catch(() => {});
  }

  readInstalled() {
    return window.queryLocalFonts().then((fonts) => {
      this.localFonts = fonts;
      this.installedFamilies = [...new Set(fonts.map((f) => f.family))];
      this.fontsChanged();
    }).catch((e) => console.warn("nano-ui-sdl-web: installed fonts not available:", e));
  }

  fontsChanged() {
    this.fontsVersion++;
    this.wake();
  }

  fontsVersionNow() {
    return this.fontsVersion;
  }

  // The families known so far, installed first, sorted and without repeats,
  // a line each.
  fontFamilies(ptr, n) {
    const seen = new Set();
    const all = [];
    for (const f of [...this.installedFamilies].sort().concat([...this.serviceFamilies].sort())) {
      if (!seen.has(f)) { seen.add(f); all.push(f); }
    }
    return this.writeText(all.join("\n"), ptr, n);
  }

  // A family name as the desktop's font search compares them.
  static fontKey(name) {
    return name.toLowerCase().replace(/[\s_-]/g, "");
  }

  // The installed fonts, through the Local Font Access API, asking the user
  // if the browser has not yet. Answers the length of the sorted family
  // names, one a line, for takeFontList; -1 without the API, -2 declined.
  async findFonts() {
    if (typeof window.queryLocalFonts !== "function") return -1;
    try {
      this.localFonts = await window.queryLocalFonts();
    } catch (e) {
      console.warn("nano-ui-sdl-web: installed fonts not available:", e);
      return -2;
    }
    this.installedFamilies = [...new Set(this.localFonts.map((f) => f.family))];
    const families = [...this.installedFamilies].sort();
    this.fontList = this.encoder.encode(families.join("\n"));
    return this.fontList.length;
  }

  takeFontList(ptr) {
    this.bytes(ptr, this.fontList.length).set(this.fontList);
    this.fontList = new Uint8Array(0);
  }

  // An installed family's regular face, else its first, or null. Reads the
  // installed fonts first when the user has allowed it but they have not
  // been read yet; never asks.
  async installedFace(name) {
    if (!this.localFonts && typeof window.queryLocalFonts === "function" && navigator.permissions) {
      try {
        const status = await navigator.permissions.query({ name: "local-fonts" });
        if (status.state === "granted") await this.readInstalled();
      } catch (e) { /* no permission query: nothing installed to read */ }
    }
    if (!this.localFonts) return null;
    const key = NanoUiSdlWeb.fontKey(name);
    const faces = this.localFonts.filter((f) => NanoUiSdlWeb.fontKey(f.family) === key);
    return faces.find((f) => /^(regular|normal|book|roman)$/i.test(f.style)) || faces[0] || null;
  }

  // A family's regular face from the font service, as TrueType, or null.
  async serviceFace(name) {
    if (this.fontService !== 1) return null;
    const id = name.toLowerCase().trim().replace(/[\s_]+/g, "-");
    const r = await fetch(`https://api.fontsource.org/v1/fonts/${encodeURIComponent(id)}`);
    if (!r.ok) return null;
    const font = await r.json();
    const weight = font.variants["400"] ? "400" : Object.keys(font.variants)[0];
    const style = font.variants[weight].normal ? "normal" : Object.keys(font.variants[weight])[0];
    const subsets = font.variants[weight][style];
    const subset = subsets[font.defSubset] || subsets.latin || Object.values(subsets)[0];
    const file = await fetch(subset.url.ttf);
    return file.ok ? file.blob() : null;
  }

  // A font file's bytes: an installed family's regular face (kind 0), a URL
  // (kind 1), or a family by name (kind 2): installed if the user has
  // allowed it, else from the font service. Answers their length for
  // takeFontBytes, or -1.
  async loadFont(kind, ptr, len) {
    const name = this.text(ptr, len);
    try {
      let blob = null;
      if (kind === 0) {
        if (!this.localFonts && typeof window.queryLocalFonts === "function") {
          this.localFonts = await window.queryLocalFonts();
        }
        blob = await this.installedFace(name).then((face) => (face ? face.blob() : null));
      } else if (kind === 1) {
        const response = await fetch(name);
        if (response.ok) blob = await response.blob();
      } else {
        const face = await this.installedFace(name);
        blob = face ? await face.blob() : await this.serviceFace(name);
      }
      if (!blob) return -1;
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

  // ---------------------------------------------------------------- files

  // Filters as Haskell sends them, a line each of a name, a tab and a
  // semicolon-separated extension list, as names with ".ext" lists. "*"
  // stands for any file, so a filter holding it lists none.
  static parseFilters(text) {
    return text.split("\n").filter((line) => line.includes("\t")).map((line) => {
      const [name, pattern] = line.split("\t");
      const exts = pattern.split(";").map((e) => e.trim()).filter((e) => e);
      return { name, extensions: exts.includes("*") ? [] : exts.map((e) => `.${e.replace(/^\./, "")}`) };
    });
  }

  // Keep bytes for takePicked; answer their length.
  setPicked(bytes) {
    this.picked = bytes;
    return bytes.length;
  }

  takePicked(ptr) {
    this.bytes(ptr, this.picked.length).set(this.picked);
    this.picked = new Uint8Array(0);
  }

  // The file chooser: files (mode 0) or every file in a folder (mode 1).
  // The chosen files are read and packed, each a little-endian 32-bit name
  // length, the UTF-8 name, a 32-bit length and the bytes. -1 cancelled,
  // -2 failed.
  async pickFiles(mode, many, ptr, len) {
    const filters = NanoUiSdlWeb.parseFilters(this.text(ptr, len));
    const input = document.createElement("input");
    input.type = "file";
    input.multiple = many !== 0;
    if (mode === 1) {
      input.webkitdirectory = true;
    } else {
      const exts = filters.flatMap((f) => f.extensions);
      if (exts.length && !filters.some((f) => f.extensions.length === 0)) input.accept = exts.join(",");
    }
    input.style.display = "none";
    document.body.appendChild(input);
    try {
      const files = await new Promise((resolve) => {
        input.addEventListener("change", () => resolve([...input.files]), { once: true });
        input.addEventListener("cancel", () => resolve(null), { once: true });
        input.click();
      });
      if (!files) return -1;
      const parts = [];
      for (const file of files) {
        parts.push(this.encoder.encode(file.webkitRelativePath || file.name));
        parts.push(new Uint8Array(await file.arrayBuffer()));
      }
      const out = new Uint8Array(parts.reduce((n, p) => n + 4 + p.length, 0));
      const view = new DataView(out.buffer);
      let o = 0;
      for (const p of parts) {
        view.setUint32(o, p.length, true);
        out.set(p, o + 4);
        o += 4 + p.length;
      }
      return this.setPicked(out);
    } catch (e) {
      console.warn("nano-ui-sdl-web: the file chooser failed:", e);
      return -2;
    } finally {
      input.remove();
    }
  }

  // Save bytes: through the browser's save dialog where it has one, else as
  // a download under the suggested name. Answers the length of the name it
  // was saved under, for takePicked; -1 cancelled, -2 failed.
  async saveFile(namePtr, nameLen, filtersPtr, filtersLen, dataPtr, dataLen) {
    const name = this.text(namePtr, nameLen);
    const filters = NanoUiSdlWeb.parseFilters(this.text(filtersPtr, filtersLen));
    // Copied now: the module's memory may move while the dialog is open.
    const data = this.bytes(dataPtr, dataLen).slice();
    try {
      if (typeof window.showSaveFilePicker === "function") {
        const types = filters.filter((f) => f.extensions.length)
          .map((f) => ({ description: f.name, accept: { "application/octet-stream": f.extensions } }));
        let handle;
        try {
          handle = await window.showSaveFilePicker({ suggestedName: name, types: types.length ? types : undefined });
        } catch (e) {
          if (e.name === "AbortError") return -1;
          throw e;
        }
        const writable = await handle.createWritable();
        await writable.write(data);
        await writable.close();
        return this.setPicked(this.encoder.encode(handle.name));
      }
      const url = URL.createObjectURL(new Blob([data]));
      const link = document.createElement("a");
      link.href = url;
      link.download = name;
      document.body.appendChild(link);
      link.click();
      link.remove();
      setTimeout(() => URL.revokeObjectURL(url), 60000);
      return this.setPicked(this.encoder.encode(name));
    } catch (e) {
      console.warn("nano-ui-sdl-web: saving failed:", e);
      return -2;
    }
  }
}

globalThis.nanoUiSdlWeb = new NanoUiSdlWeb();
