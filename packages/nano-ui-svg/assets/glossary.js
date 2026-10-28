const widgetMasks = {};
const widgetCompare = {};
let globalMask = 0;
let globalCompare = false;
let heroMask = 0;
let stateMode = 0;
let stateStyleMask = 0;
let typoMask = 0;

function unpackSvg(bankId, mask) {
  const b = SVG_BANKS[bankId];
  if (!b || !b.c[mask]) return '';
  return b.c[mask].map(i => b.l[i]).join('><');
}

function buildHaskellCode(expr, mask, isOverlay) {
  const mods = [];
  if (mask & 32) mods.push('rounded 999');
  if (mask & 16) mods.push('accentColor rose');
  if (mask & 8) mods.push(isOverlay ? 'panelStyle probePnl . popupStyle probePop . windowStyle probeWin' : 'panelStyle probePnl');
  if (mask & 4) mods.push('inputStyle probeInp');
  if (mask & 2) mods.push('buttonStyle probeBtn');
  const lines = [];
  let indent = '';
  if (mask & 1) { lines.push('themed defaultLightTheme $'); indent += '  '; }
  if (mods.length > 0) { lines.push(indent + 'styled (' + mods.join(' . ') + ') $'); indent += '  '; }
  if (mask & 64) { lines.push(indent + 'disabledWhen True $'); indent += '  '; }
  lines.push(indent + expr);
  return lines.join('\n');
}

function describeMaskShort(mask) {
  if (mask === 0) return 'defaultTheme (0 modifiers)';
  const names = [];
  if (mask & 1) names.push('Light');
  if (mask & 2) names.push('buttonStyle');
  if (mask & 4) names.push('inputStyle');
  if (mask & 8) names.push('panelStyle');
  if (mask & 16) names.push('accentColor');
  if (mask & 32) names.push('rounded');
  if (mask & 64) names.push('disabledWhen');
  return 'Active (' + names.length + '): ' + names.join(' + ');
}

/* Status labels have a fixed width so toggling a mask cannot reflow the card;
   the full text stays available as a tooltip. */
function setLabel(el, text) {
  el.textContent = text;
  el.title = text;
}

function renderWidgetNotes(wid, previewBit) {
  const meta = WIDGET_META[wid];
  const mask = widgetMasks[wid] || 0;
  const box = document.getElementById('notes-' + wid);
  if (!meta || !box) return;
  const activeNotes = meta.notes.filter(n => (mask & (1 << n.bit)) !== 0 || n.bit === previewBit);
  if (activeNotes.length === 0) {
    box.innerHTML = '<div class="ins-empty">Select a style above to see what it changes.</div>';
    return;
  }
  box.innerHTML = activeNotes.map(n => {
    const isOn = (mask & (1 << n.bit)) !== 0;
    const prefix = isOn ? '' : '<em>(Preview)</em> ';
    const detail = n.detail.replace(/`([^`]+)`/g, '<code>$1</code>');
    return '<div class="ins-item">' + prefix + '<strong><code>' + n.label + '</code></strong> <span class="chip-tag impact-' + n.impact + '">' + n.badge + '</span>: ' + detail + '</div>';
  }).join('');
}

function previewWidgetNote(wid, bit) { renderWidgetNotes(wid, bit); }
function clearWidgetPreview(wid) { renderWidgetNotes(wid, -1); }

function setWidgetMask(wid, mask) {
  widgetMasks[wid] = mask;
  const svgEl = document.getElementById('svg-' + wid);
  if (svgEl) {
    svgEl.innerHTML = unpackSvg(wid, mask);
    svgEl.classList.toggle('light-theme', (mask & 1) !== 0);
  }
  const statusEl = document.getElementById('status-' + wid);
  if (statusEl) setLabel(statusEl, describeMaskShort(mask));
  const meta = WIDGET_META[wid];
  const codeEl = document.getElementById('code-' + wid);
  if (meta && codeEl) codeEl.textContent = buildHaskellCode(meta.expr, mask, meta.isOverlay);
  document.querySelectorAll('.style-chip[data-wid="' + wid + '"]').forEach(btn => {
    const b = parseInt(btn.getAttribute('data-bit'), 10);
    btn.classList.toggle('active', (mask & (1 << b)) !== 0);
  });
  renderWidgetNotes(wid, -1);
}

function toggleWidgetBit(wid, bit) {
  const cur = widgetMasks[wid] || 0;
  setWidgetMask(wid, cur ^ (1 << bit));
}

function toggleWidgetCompare(wid, forceVal) {
  const next = typeof forceVal === 'boolean' ? forceVal : !widgetCompare[wid];
  widgetCompare[wid] = next;
  const baseWell = document.getElementById('base-well-' + wid);
  const baseSvg = document.getElementById('base-svg-' + wid);
  const btn = document.getElementById('cmp-btn-' + wid);
  if (baseWell && baseSvg) {
    if (next && !baseSvg.innerHTML) {
      baseSvg.innerHTML = unpackSvg(wid, 0).replaceAll('wb-' + wid + '-', 'wb-' + wid + '-base-');
    }
    baseWell.classList.toggle('hidden-compare', !next);
  }
  if (btn) btn.classList.toggle('active', next);
}

function toggleGlobalCompare() {
  globalCompare = !globalCompare;
  const gbtn = document.getElementById('global-cmp-btn');
  if (gbtn) gbtn.classList.toggle('active', globalCompare);
  Object.keys(WIDGET_META).forEach(wid => toggleWidgetCompare(wid, globalCompare));
}

function syncMasterButtons() {
  document.querySelectorAll('#master-toggles .style-chip').forEach(btn => {
    const b = parseInt(btn.getAttribute('data-gbit'), 10);
    btn.classList.toggle('active', (globalMask & (1 << b)) !== 0);
  });
}

function setAllWidgetsMask(mask) {
  globalMask = mask;
  syncMasterButtons();
  Object.keys(WIDGET_META).forEach(wid => setWidgetMask(wid, mask));
}

function toggleGlobalBit(bit) {
  globalMask ^= (1 << bit);
  syncMasterButtons();
  const turnOn = (globalMask & (1 << bit)) !== 0;
  Object.keys(WIDGET_META).forEach(wid => {
    const cur = widgetMasks[wid] || 0;
    const next = turnOn ? (cur | (1 << bit)) : (cur & ~(1 << bit));
    setWidgetMask(wid, next);
  });
}

function setHeroMask(mask) {
  heroMask = mask;
  const well = document.getElementById('hero-svg-well');
  if (well) {
    well.innerHTML = unpackSvg('hero', mask);
    well.classList.toggle('light-theme', (mask & 1) !== 0);
  }
  const st = document.getElementById('hero-status');
  if (st) setLabel(st, 'Composite · ' + describeMaskShort(mask));
  const cd = document.getElementById('hero-code-preview');
  if (cd) cd.textContent = buildHaskellCode('compositeDashboardView', mask, false);
  document.querySelectorAll('#hero-toggles .style-chip').forEach(btn => {
    const b = parseInt(btn.getAttribute('data-hbit'), 10);
    btn.classList.toggle('active', (mask & (1 << b)) !== 0);
  });
}
function toggleHeroBit(bit) { setHeroMask(heroMask ^ (1 << bit)); }

const STATE_NAMES = [
  ['Resting', 'Default fill and border from the widget surface.'],
  ['Hovered', 'Pointer over the Action Button: fill becomes styleHoverBg.'],
  ['Pressed', 'Left button held on the Action Button: fill becomes styleActiveBg.'],
  ['Focused', 'Keyboard focus on the Action Button: a 2px themeFocusRing is drawn outside it.'],
  ['Focused input', 'Clicked textInput: border becomes themeAccent, fill becomes styleActiveBg, caret shown.'],
  ['Open select', 'Clicked select: the closed box uses themeButton, the open menu uses themePopup.']
];

function updateStateWorkbench() {
  const idx = stateMode * 32 + stateStyleMask;
  const well = document.getElementById('state-svg-well');
  if (well) well.innerHTML = unpackSvg('state', idx);
  const info = STATE_NAMES[stateMode];
  const sNames = [];
  if (stateStyleMask & 1) sNames.push('buttonStyle');
  if (stateStyleMask & 2) sNames.push('inputStyle');
  if (stateStyleMask & 4) sNames.push('popupStyle');
  if (stateStyleMask & 8) sNames.push('accentColor');
  if (stateStyleMask & 16) sNames.push('disabledWhen True');
  const lbl = document.getElementById('state-active-label');
  if (lbl) setLabel(lbl, 'State: ' + info[0] + ' · ' + (sNames.length ? sNames.join(' + ') : 'defaultTheme'));
  const codeEl = document.getElementById('state-code-preview');
  if (codeEl) codeEl.textContent = '-- ' + info[0] + (sNames.length ? ' with ' + sNames.join(', ') : ' under defaultTheme');
  document.querySelectorAll('#state-mode-pills .state-pill').forEach(btn => {
    btn.classList.toggle('active', parseInt(btn.getAttribute('data-state'), 10) === stateMode);
  });
  document.querySelectorAll('#state-style-toggles .style-chip').forEach(btn => {
    const b = parseInt(btn.getAttribute('data-sbit'), 10);
    btn.classList.toggle('active', (stateStyleMask & (1 << b)) !== 0);
  });
  const ins = document.getElementById('state-inspector');
  if (ins) {
    let msg = '<div class="ins-item"><strong>' + info[0] + ':</strong> ' + info[1] + '</div>';
    if ((stateStyleMask & 16) && stateMode > 0) {
      msg += '<div class="ins-item"><span class="chip-tag impact-surprise">Suppressed</span> disabledWhen blocks hover, press, focus and menus in this subtree.</div>';
    }
    if (stateMode === 5 && (stateStyleMask & 4)) {
      msg += '<div class="ins-item"><span class="chip-tag impact-primary">popupStyle</span> The open menu uses themePopup; the closed select uses themeButton.</div>';
    }
    ins.innerHTML = msg;
  }
}
function setStateMode(m) { stateMode = m; updateStateWorkbench(); }
function toggleStateStyle(b) { stateStyleMask ^= (1 << b); updateStateWorkbench(); }
function resetStateWorkbench() { stateMode = 0; stateStyleMask = 0; updateStateWorkbench(); }

function setTypoMask(mask) {
  typoMask = mask;
  const well = document.getElementById('typo-svg-well');
  if (well) well.innerHTML = unpackSvg('typo', mask);
  const parts = [];
  if (mask & 1) parts.push('fontBold');
  if (mask & 2) parts.push('fontItalic');
  if (mask & 4) parts.push('fontUnderline');
  if (mask & 8) parts.push('fontStrike');
  if (mask & 16) parts.push('fontTone Danger');
  if (mask & 32) parts.push('fontSize 16');
  const lbl = document.getElementById('typo-active-label');
  if (lbl) setLabel(lbl, parts.length ? 'Active (' + parts.length + '): ' + parts.join(' . ') : 'No modifiers');
  const cd = document.getElementById('typo-code-preview');
  if (cd) cd.textContent = 'let m = ' + (parts.length ? parts.join(' . ') : 'id');
  document.querySelectorAll('#typo-toggles .style-chip').forEach(btn => {
    const b = parseInt(btn.getAttribute('data-tbit'), 10);
    btn.classList.toggle('active', (mask & (1 << b)) !== 0);
  });
}
function toggleTypoBit(b) { setTypoMask(typoMask ^ (1 << b)); }

function setTrapView(tid, mode, btn) {
  const wrap = document.getElementById('trap-vis-' + tid);
  const badBox = document.getElementById('trap-bad-' + tid);
  const goodBox = document.getElementById('trap-good-' + tid);
  if (!wrap || !badBox || !goodBox) return;
  btn.parentElement.querySelectorAll('.mini-btn').forEach(b => b.classList.remove('active'));
  btn.classList.add('active');
  if (mode === 'bad') {
    wrap.classList.add('single-view');
    badBox.classList.remove('hidden-compare');
    goodBox.classList.add('hidden-compare');
  } else if (mode === 'good') {
    wrap.classList.add('single-view');
    badBox.classList.add('hidden-compare');
    goodBox.classList.remove('hidden-compare');
  } else {
    wrap.classList.remove('single-view');
    badBox.classList.remove('hidden-compare');
    goodBox.classList.remove('hidden-compare');
  }
}

function filterGlossary(cat, btn) {
  document.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));
  btn.classList.add('active');
  document.querySelectorAll('.filter-item').forEach(el => {
    if (cat === 'all') {
      el.classList.remove('hidden-by-filter');
    } else {
      const cats = (el.getAttribute('data-cat') || '').split(/\s+/);
      if (cats.includes(cat)) {
        el.classList.remove('hidden-by-filter');
      } else {
        el.classList.add('hidden-by-filter');
      }
    }
  });
}

Object.keys(WIDGET_META).forEach(wid => renderWidgetNotes(wid, -1));
updateStateWorkbench();
