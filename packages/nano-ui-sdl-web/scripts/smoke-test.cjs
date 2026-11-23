// Load a directory build-web.sh made in headless Chromium and check the
// program runs: the page sets its title from Haskell, the canvas gets a
// frame with text on it, a click at the given point changes the frame, and
// nothing is logged as an error.
//
//   node packages/nano-ui-sdl-web/scripts/smoke-test.cjs <dir> <x> <y> [screenshot.png]
//
// The point is in CSS pixels of a 1280x800 page; pick one on a control
// whose press redraws something. The screenshot, if named, is taken at the
// end whether the checks pass or not. Needs the playwright package (ghc-wasm-meta
// installs it under NODE_PATH). SMOKE_CHROME names a Chrome or Chromium
// binary to use instead of Playwright's own.
"use strict";

const http = require("http");
const fs = require("fs");
const path = require("path");
const { chromium } = require("playwright");

const [dir, x, y, shot] = process.argv.slice(2);
if (!dir || x === undefined || y === undefined) {
  console.error("usage: smoke-test.cjs <dir> <x> <y> [screenshot.png]");
  process.exit(2);
}

const types = {
  ".html": "text/html", ".js": "text/javascript", ".mjs": "text/javascript",
  ".wasm": "application/wasm", ".css": "text/css", ".png": "image/png",
};

function serve(root) {
  const server = http.createServer((req, res) => {
    const url = decodeURIComponent(new URL(req.url, "http://localhost").pathname);
    let file = path.join(root, url);
    if (!file.startsWith(path.resolve(root))) { res.writeHead(403); res.end(); return; }
    if (fs.existsSync(file) && fs.statSync(file).isDirectory()) file = path.join(file, "index.html");
    fs.readFile(file, (err, data) => {
      if (err) { res.writeHead(404); res.end(); return; }
      res.writeHead(200, { "Content-Type": types[path.extname(file)] || "application/octet-stream" });
      res.end(data);
    });
  });
  return new Promise((resolve) => server.listen(0, "127.0.0.1", () => resolve(server)));
}

// A summary of the canvas's pixels: how many distinct colours a coarse grid
// of samples has, and a hash of those samples.
async function canvasSummary(page) {
  return page.evaluate(() => {
    const c = document.querySelector("canvas");
    const probe = document.createElement("canvas");
    probe.width = c.width;
    probe.height = c.height;
    const ctx = probe.getContext("2d");
    ctx.drawImage(c, 0, 0);
    const data = ctx.getImageData(0, 0, c.width, c.height).data;
    const colours = new Set();
    let hash = 0;
    for (let i = 0; i < data.length; i += 4 * 7) {
      const v = (data[i] << 16) | (data[i + 1] << 8) | data[i + 2];
      colours.add(v);
      hash = (hash * 31 + v) | 0;
    }
    return { colours: colours.size, hash };
  });
}

async function waitFor(what, ms, check) {
  const deadline = Date.now() + ms;
  for (;;) {
    const result = await check();
    if (result) return result;
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((r) => setTimeout(r, 200));
  }
}

(async () => {
  const server = await serve(path.resolve(dir));
  const url = `http://127.0.0.1:${server.address().port}/`;
  const browser = await chromium.launch({
    executablePath: process.env.SMOKE_CHROME || undefined,
    // WebGL2 on the CPU, for machines with no GPU.
    args: ["--use-angle=swiftshader", "--enable-unsafe-swiftshader", "--ignore-gpu-blocklist"],
  });
  const errors = [];
  const page = await browser.newPage({ viewport: { width: 1280, height: 800 }, deviceScaleFactor: 1 });
  try {
    page.on("pageerror", (e) => errors.push(`page error: ${e.message}`));
    page.on("console", (m) => {
      if (m.type() === "error") errors.push(`console error: ${m.text()}`);
      else console.log(`[page] ${m.text()}`);
    });
    page.on("requestfailed", (r) => errors.push(`request failed: ${r.url()}`));
    await page.goto(url);
    const title = await waitFor("the program to set the title", 60000, async () => {
      const t = await page.title();
      return t !== "nano-ui" && t ? t : (errors.length ? Promise.reject(new Error(errors.join("\n"))) : null);
    });
    console.log(`title: ${title}`);
    const first = await waitFor("a frame with text", 30000, async () => {
      const s = await canvasSummary(page);
      return s.colours >= 8 ? s : null;
    });
    console.log(`first frame: ${first.colours} colours sampled`);
    // Hover first and let the frame settle, so only the press can change it.
    await page.mouse.move(Number(x), Number(y));
    let hovered = await canvasSummary(page);
    await waitFor("the hovered frame to settle", 10000, async () => {
      await new Promise((r) => setTimeout(r, 500));
      const s = await canvasSummary(page);
      const same = s.hash === hovered.hash;
      hovered = s;
      return same;
    });
    await page.mouse.down();
    await page.mouse.up();
    const after = await waitFor("the click to redraw the frame", 10000, async () => {
      await new Promise((r) => setTimeout(r, 300));
      const s = await canvasSummary(page);
      return s.hash !== hovered.hash ? s : null;
    });
    console.log(`after the click at ${x},${y}: ${after.colours} colours sampled`);
    if (errors.length) throw new Error(errors.join("\n"));
    console.log(`ok: ${url}`);
  } finally {
    if (shot) await page.screenshot({ path: shot }).catch(() => {});
    await browser.close();
    server.close();
  }
})().catch((e) => {
  console.error(`FAIL: ${e.message}`);
  process.exit(1);
});
