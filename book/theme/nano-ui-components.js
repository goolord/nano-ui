// nano-ui interactive Web Components for mdBook

(function() {
  function unpackSvg(bankId, mask) {
    const banks = window.NANO_UI_BANKS || {};
    const b = banks[bankId];
    if (!b || !b.c || !b.c[mask]) return '';
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
    lines.push(indent + (expr || 'widget'));
    return lines.join('\n');
  }

  const BITS = [
    { bit: 0, mask: 1, label: 'Light Theme' },
    { bit: 1, mask: 2, label: 'buttonStyle' },
    { bit: 2, mask: 4, label: 'inputStyle' },
    { bit: 3, mask: 8, label: 'panelStyle' },
    { bit: 4, mask: 16, label: 'accentColor' },
    { bit: 5, mask: 32, label: 'rounded' },
    { bit: 6, mask: 64, label: 'disabledWhen' }
  ];

  // -------------------------------------------------------------------------
  // 1. <nano-ui-workbench widget="...">
  // -------------------------------------------------------------------------
  class NanoUiWorkbench extends HTMLElement {
    connectedCallback() {
      this.widgetId = this.getAttribute('widget') || 'button';
      this.mask = parseInt(this.getAttribute('default-mask') || '0', 10);
      this.compare = false;
      this.render();
    }

    render() {
      const meta = (window.NANO_UI_WIDGET_META || {})[this.widgetId] || { expr: this.widgetId, isOverlay: false, notes: [] };
      const title = this.getAttribute('title') || (this.widgetId.charAt(0).toUpperCase() + this.widgetId.slice(1).replace(/-/g, ' '));

      this.innerHTML = `
        <div class="nano-wb-header">
          <div class="nano-wb-title">
            <span>${title}</span>
            <span class="nano-wb-badge">Interactive Specimen</span>
          </div>
          <div class="nano-chip-group">
            <button type="button" class="nano-chip chip-compare" id="btn-compare">Toggle Compare Baseline</button>
            <button type="button" class="nano-chip" id="btn-reset">Reset</button>
          </div>
        </div>

        <div class="nano-wb-controls">
          <div class="nano-chip-group">
            <span class="nano-chip-group-label">Modifiers:</span>
            ${BITS.map(b => `
              <button type="button" class="nano-chip ${this.mask & b.mask ? 'active' : ''}" data-mask="${b.mask}">
                ${b.label}
              </button>
            `).join('')}
          </div>
        </div>

        <div class="nano-well-container">
          <div class="${this.compare ? 'nano-well-split' : ''}" id="svg-container">
            <div class="nano-svg-pane">
              ${this.compare ? '<span class="nano-pane-label">Baseline (defaultTheme)</span>' : ''}
              ${this.compare ? `<div class="svg-well">${unpackSvg(this.widgetId, 0)}</div>` : ''}
            </div>
            <div class="nano-svg-pane">
              ${this.compare ? '<span class="nano-pane-label">Active Configuration</span>' : ''}
              <div class="svg-well" id="active-well">${unpackSvg(this.widgetId, this.mask)}</div>
            </div>
          </div>
        </div>

        <div class="nano-code-bar">
          <div class="nano-code-snippet" id="code-snippet"><code>${escapeHtml(buildHaskellCode(meta.expr, this.mask, meta.isOverlay))}</code></div>
          <button type="button" class="nano-copy-btn" id="btn-copy">Copy Code</button>
        </div>

        <div class="nano-notes-box" id="notes-box">
          ${this.renderNotes(meta)}
        </div>
      `;

      this.attachListeners(meta);
    }

    renderNotes(meta) {
      if (!meta || !meta.notes) return '';
      const activeNotes = meta.notes.filter(n => (this.mask & (1 << n.bit)) !== 0);
      if (activeNotes.length === 0) {
        return '<div style="opacity:0.6;font-style:italic">Select one or more styling modifiers above to inspect how nano-ui routes them to this widget.</div>';
      }
      return activeNotes.map(n => `
        <div class="nano-note-item">
          <span class="nano-note-badge ${n.impact}">${n.badge || n.impact}</span>
          <strong>${n.label}:</strong>
          <span>${n.detail}</span>
        </div>
      `).join('');
    }

    attachListeners(meta) {
      const chips = this.querySelectorAll('.nano-wb-controls .nano-chip');
      chips.forEach(chip => {
        chip.addEventListener('click', () => {
          const m = parseInt(chip.dataset.mask, 10);
          this.mask ^= m;
          chip.classList.toggle('active', !!(this.mask & m));
          this.updateActive(meta);
        });
      });

      const btnCompare = this.querySelector('#btn-compare');
      if (btnCompare) {
        btnCompare.addEventListener('click', () => {
          this.compare = !this.compare;
          this.render();
        });
      }

      const btnReset = this.querySelector('#btn-reset');
      if (btnReset) {
        btnReset.addEventListener('click', () => {
          this.mask = 0;
          this.compare = false;
          this.render();
        });
      }

      const btnCopy = this.querySelector('#btn-copy');
      if (btnCopy) {
        btnCopy.addEventListener('click', () => {
          const code = buildHaskellCode(meta.expr, this.mask, meta.isOverlay);
          navigator.clipboard.writeText(code).then(() => {
            btnCopy.textContent = 'Copied!';
            setTimeout(() => { btnCopy.textContent = 'Copy Code'; }, 1500);
          });
        });
      }
    }

    updateActive(meta) {
      const activeWell = this.querySelector('#active-well');
      if (activeWell) {
        activeWell.innerHTML = unpackSvg(this.widgetId, this.mask);
      }
      const codeSnippet = this.querySelector('#code-snippet');
      if (codeSnippet) {
        codeSnippet.innerHTML = `<code>${escapeHtml(buildHaskellCode(meta.expr, this.mask, meta.isOverlay))}</code>`;
      }
      const notesBox = this.querySelector('#notes-box');
      if (notesBox) {
        notesBox.innerHTML = this.renderNotes(meta);
      }
    }
  }

  // -------------------------------------------------------------------------
  // 2. <nano-ui-hero>
  // -------------------------------------------------------------------------
  class NanoUiHero extends HTMLElement {
    connectedCallback() {
      this.mask = 0;
      this.render();
    }

    render() {
      this.innerHTML = `
        <div class="nano-wb-header">
          <div class="nano-wb-title">
            <span>Surface Routing Overview & Semantic Tones</span>
            <span class="nano-wb-badge">Theme Surface Matrix</span>
          </div>
          <div class="nano-chip-group">
            <button type="button" class="nano-chip" id="hero-all">All Surfaces</button>
            <button type="button" class="nano-chip" id="hero-reset">Reset</button>
          </div>
        </div>

        <div class="nano-wb-controls">
          <div class="nano-chip-group">
            <span class="nano-chip-group-label">Toggle Surfaces:</span>
            ${BITS.map(b => `
              <button type="button" class="nano-chip ${this.mask & b.mask ? 'active' : ''}" data-mask="${b.mask}">
                ${b.label}
              </button>
            `).join('')}
          </div>
        </div>

        <div class="nano-well-container">
          <div class="svg-well" id="hero-well">${unpackSvg('hero', this.mask)}</div>
        </div>
      `;

      this.querySelectorAll('.nano-wb-controls .nano-chip').forEach(c => {
        c.addEventListener('click', () => {
          const m = parseInt(c.dataset.mask, 10);
          this.mask ^= m;
          c.classList.toggle('active', !!(this.mask & m));
          this.querySelector('#hero-well').innerHTML = unpackSvg('hero', this.mask);
        });
      });

      this.querySelector('#hero-all').addEventListener('click', () => {
        this.mask = 30; // 2 | 4 | 8 | 16
        this.render();
      });

      this.querySelector('#hero-reset').addEventListener('click', () => {
        this.mask = 0;
        this.render();
      });
    }
  }

  // -------------------------------------------------------------------------
  // 3. <nano-ui-state-lab>
  // -------------------------------------------------------------------------
  class NanoUiStateLab extends HTMLElement {
    connectedCallback() {
      this.mask = 0;
      this.render();
    }

    render() {
      this.innerHTML = `
        <div class="nano-wb-header">
          <div class="nano-wb-title">
            <span>Interaction State Strip (Default -> Hover -> Pressed -> Disabled)</span>
            <span class="nano-wb-badge">4 States</span>
          </div>
          <div class="nano-chip-group">
            <button type="button" class="nano-chip ${this.mask & 1 ? 'active' : ''}" id="btn-light">Light Theme</button>
          </div>
        </div>

        <div class="nano-well-container">
          <div class="svg-well" id="state-well">${unpackSvg('state', this.mask)}</div>
        </div>
      `;

      const btn = this.querySelector('#btn-light');
      btn.addEventListener('click', () => {
        this.mask ^= 1;
        btn.classList.toggle('active', !!(this.mask & 1));
        this.querySelector('#state-well').innerHTML = unpackSvg('state', this.mask);
      });
    }
  }

  // -------------------------------------------------------------------------
  // 4. <nano-ui-typo-lab>
  // -------------------------------------------------------------------------
  class NanoUiTypoLab extends HTMLElement {
    connectedCallback() {
      this.mask = 0;
      this.render();
    }

    render() {
      this.innerHTML = `
        <div class="nano-wb-header">
          <div class="nano-wb-title">
            <span>Typography Modifiers & Hierarchy</span>
            <span class="nano-wb-badge">Font Options</span>
          </div>
          <div class="nano-chip-group">
            <button type="button" class="nano-chip ${this.mask & 1 ? 'active' : ''}" id="btn-typo-light">Light Theme</button>
          </div>
        </div>

        <div class="nano-well-container">
          <div class="svg-well" id="typo-well">${unpackSvg('typo', this.mask)}</div>
        </div>
      `;

      const btn = this.querySelector('#btn-typo-light');
      btn.addEventListener('click', () => {
        this.mask ^= 1;
        btn.classList.toggle('active', !!(this.mask & 1));
        this.querySelector('#typo-well').innerHTML = unpackSvg('typo', this.mask);
      });
    }
  }

  // -------------------------------------------------------------------------
  // 5. <nano-ui-trap id="...">
  // -------------------------------------------------------------------------
  class NanoUiTrap extends HTMLElement {
    connectedCallback() {
      let rawId = this.getAttribute('id') || 'trap-01';
      if (!rawId.startsWith('trap-')) {
        rawId = 'trap-' + (rawId.length === 1 ? '0' + rawId : rawId);
      }
      this.trapId = rawId;
      this.mode = 'split'; // 'bad', 'good', or 'split'
      this.render();
    }

    render() {
      const traps = window.NANO_UI_TRAPS || {};
      const trap = traps[this.trapId];
      if (!trap) {
        this.innerHTML = `<div style="padding:16px;color:#f87171">Trap specimen <code>${this.trapId}</code> not found in library.</div>`;
        return;
      }

      this.innerHTML = `
        <div class="nano-trap-header">
          <div class="nano-trap-meta">
            <span class="nano-trap-num">${trap.numLabel || this.trapId.toUpperCase()}</span>
            <div class="nano-chip-group">
              <button type="button" class="nano-chip ${this.mode === 'bad' ? 'active' : ''}" data-mode="bad">Show Trap</button>
              <button type="button" class="nano-chip ${this.mode === 'good' ? 'active' : ''}" data-mode="good">Show Fix</button>
              <button type="button" class="nano-chip ${this.mode === 'split' ? 'active' : ''}" data-mode="split">Side-by-Side</button>
            </div>
          </div>
          <h3 class="nano-trap-title">${trap.title}</h3>
          ${trap.where ? `<code class="nano-trap-where">${trap.where}</code>` : ''}
        </div>

        <div class="nano-trap-blocks">
          ${(trap.blocks || []).map((b, i) => `
            <div class="nano-trap-block ${i === 1 ? 'nano-trap-fix-block' : ''}">${b}</div>
          `).join('')}
        </div>

        <div class="nano-trap-code-grid">
          <div class="nano-code-card">
            <div class="nano-code-card-header bad">Gotcha / Trap Pattern</div>
            <pre><code>${escapeHtml(trap.bad.code)}</code></pre>
          </div>
          <div class="nano-code-card">
            <div class="nano-code-card-header good">Recommended Fix</div>
            <pre><code>${escapeHtml(trap.good.code)}</code></pre>
          </div>
        </div>

        <div class="nano-well-container">
          <div class="${this.mode === 'split' ? 'nano-well-split' : ''}">
            ${this.mode !== 'good' ? `
              <div class="nano-svg-pane">
                <span class="nano-pane-label" style="color:#f87171">${trap.bad.svgLabel || 'Trap Output'}</span>
                <div class="svg-well">${trap.bad.svg}</div>
              </div>
            ` : ''}
            ${this.mode !== 'bad' ? `
              <div class="nano-svg-pane">
                <span class="nano-pane-label" style="color:#34d399">${trap.good.svgLabel || 'Fix Output'}</span>
                <div class="svg-well">${trap.good.svg}</div>
              </div>
            ` : ''}
          </div>
        </div>
      `;

      this.querySelectorAll('.nano-trap-meta .nano-chip').forEach(btn => {
        btn.addEventListener('click', () => {
          this.mode = btn.dataset.mode;
          this.render();
        });
      });
    }
  }

  function escapeHtml(str) {
    if (!str) return '';
    return str
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  // Register Web Components
  customElements.define('nano-ui-workbench', NanoUiWorkbench);
  customElements.define('nano-ui-hero', NanoUiHero);
  customElements.define('nano-ui-state-lab', NanoUiStateLab);
  customElements.define('nano-ui-typo-lab', NanoUiTypoLab);
  customElements.define('nano-ui-trap', NanoUiTrap);

  console.log('nano-ui Web Components initialized');
})();
