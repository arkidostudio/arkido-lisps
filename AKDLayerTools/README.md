# AKDLayerTools

Three layer utilities in one LISP — a small, coordinated toolkit that covers the everyday layer moves in AutoCAD without leaving the keyboard.

| Command | What it does |
|---|---|
| **ERC** | **Set Current Layer** — dialog of *preset* layers, sets the pick current, then re-fires the last drawing command so you keep drawing. |
| **ERS** | **Select By Layer** — dialog of *all drawing* layers. Grabs everything on the chosen layer. Supports whole-drawing selection, user-drawn crossing window, filtering an existing selection (double-click), and an Append toggle. |
| **ERT** | **Move To Layer** — dialog of *all drawing* layers. Moves the objects you pick after OK onto the chosen layer. |

All three remember your last-picked layer across the session and pre-select it next time.

**Platform:** AutoCAD for Mac (also runs on Windows AutoCAD).

## Install

**Quickest:** drag `AKDLayerTools.lsp` from Finder / Explorer onto the AutoCAD drawing window — loads for the current session.

**Persistent (recommended):**
1. Type `APPLOAD` and press Enter.
2. Load `AKDLayerTools.lsp`.
3. Add it to the **Startup Suite** so it auto-loads every session.

Then type `ERC`, `ERS`, or `ERT` to launch.

Loading a single file gives you all three commands. No `.dcl` or config files — the dialogs are generated at runtime and cleaned up on close.

## ERC — Set Current Layer

Preset workflow. Edit the preset list at the top of the LISP file:

```lisp
(setq *ERC_Layers*
  '(
    "A-WALL"
    "A-DOOR"
    "A-WINDOW"
    "S-BEAM"
    "X-DIMS"
    "P-HATCH"
    "Z-TITLE"
  )
)
```

Flow:
1. Mid-`LINE` (or any of `PLINE / ARC / CIRCLE / RECTANG / POLYGON / MTEXT / TEXT / HATCH`), realise you're on the wrong layer.
2. Type `ERC`.
3. Double-click a preset layer, or select + OK.
4. Layer is set current **and** the command you were running is re-launched — click the next point and keep going.

Anything else falls back to launching `LINE`.

## ERS — Select By Layer

Dialog lists every layer in the drawing (alphabetically). Buttons:

| Button | Behavior |
|---|---|
| **All (Drawing)** *(default / Enter)* | Grabs every entity on the chosen layer, whole drawing. |
| **By Selection** | Prompts for two corners → crossing window filtered to that layer. |
| **Cancel** | Nothing. |
| **Append: OFF / ON** *(toggle)* | Merges the new layer-selection with objects you already had grip-selected. State remembered across runs. |

**Double-click a layer** with objects already grip-selected → filters that selection down to just entities on the chosen layer. If nothing was pre-selected, double-click falls through to *All*.

## ERT — Move To Layer

Simplest of the three. Flow:
1. Type `ERT`.
2. Double-click a layer, or select + OK.
3. `Select objects:` prompt — pick the entities you want moved.
4. Every picked entity's layer is rewritten. Regen. Grip selection cleared.

Prints the count when done.

## Notes

- All three dialogs remember their last-picked layer via session globals (`*erc:last-layer*`, `*ers:last-layer*`, `*ert:last-layer*`). Persist until AutoCAD closes.
- ERS also remembers the Append toggle state (`*ers:last-append*`).
- Only ERC uses a preset list — ERS and ERT always pull from the current drawing's layer table.
