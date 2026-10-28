# Interactive Workbenches

Explore how every styling modifier (`buttonStyle`, `inputStyle`, `panelStyle`, `accentColor`, `rounded`, `disabledWhen`) and theme mode affects each widget family in `nano-ui`. For what each widget *does* rather than how it looks, see the [Widget Gallery](../widgets/index.md).

Use the toggle chips on each workbench to toggle modifiers on and off, click **Toggle Compare Baseline** to see a side-by-side diff against the unstyled widget, and copy the generated Haskell code directly into your project.

---

## 1. Buttons

Standard push buttons, quiet buttons, and styled actions.

<nano-ui-workbench widget="button" title="Push Buttons"></nano-ui-workbench>

---

## 2. Toggle Buttons

Two-state toggle buttons with persistent active styling.

<nano-ui-workbench widget="toggle-button" title="Toggle Buttons"></nano-ui-workbench>

---

## 3. Checkboxes

Checkboxes route through three distinct surfaces: the box well (`themeInput`), check indicator (`themeAccent`), and label text.

<nano-ui-workbench widget="checkbox" title="Checkboxes"></nano-ui-workbench>

---

## 4. Radio Buttons

Single-choice radio groups with circular indicator wells.

<nano-ui-workbench widget="radio" title="Radio Buttons"></nano-ui-workbench>

---

## 5. Text Inputs

Single-line text fields with active caret, selection highlighting, and focus rings.

<nano-ui-workbench widget="text-input" title="Text Fields"></nano-ui-workbench>

---

## 6. Search & Numeric Inputs

Debounced search fields and bounded numeric steppers.

<nano-ui-workbench widget="search-numeric" title="Search & Numeric Fields"></nano-ui-workbench>

---

## 7. Select vs ComboBox

Dropdown triggers: `select` uses `themeButton` for trigger styling, whereas `comboBox` uses `themeInput`.

<nano-ui-workbench widget="select-vs-combo" title="Dropdown Select & ComboBox"></nano-ui-workbench>

---

## 8. Multi-line Text Areas

Scrollable multi-line text editing with document-level line sharing.

<nano-ui-workbench widget="textarea" title="Multi-line Text Area"></nano-ui-workbench>

---

## 9. Sliders

Continuous and discrete sliders with track and thumb geometry.

<nano-ui-workbench widget="slider" title="Sliders"></nano-ui-workbench>

---

## 10. Custom Controls (Switches, Knobs, Spinners)

Custom vector canvas controls from `NanoUI.Widgets.Custom`.

<nano-ui-workbench widget="custom-controls" title="Custom Vector Controls"></nano-ui-workbench>

---

## 11. Panels, Cards & Callouts

Structural containers: `panelStyle` routes background, border, and semantic tone styling.

<nano-ui-workbench widget="panel-card-callout" title="Panels, Cards & Callouts"></nano-ui-workbench>

---

## 12. Bounded Scroll Areas

Scrollable wells and custom styled scrollbars.

<nano-ui-workbench widget="scroll-bounded" title="Scroll Areas"></nano-ui-workbench>

---

## 13. Tabs

Tab headers in Underline, Segmented, and Pill styles.

<nano-ui-workbench widget="tabs-styles" title="Tab Bars"></nano-ui-workbench>

---

## 14. Tables & Trees

Colonnade data tables with sort arrows and hierarchical tree views.

<nano-ui-workbench widget="table-tree" title="Tables & Tree Views"></nano-ui-workbench>

---

## 15. Overlays (Floating Windows & Menus)

Floating modal windows, popup menus, and tooltips.

<nano-ui-workbench widget="overlays" title="Overlays & Floating Windows"></nano-ui-workbench>
