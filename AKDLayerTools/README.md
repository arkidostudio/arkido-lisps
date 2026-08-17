# AKDLayerTools

A coordinated toolkit of layer utilities for AutoCAD — everything you actually do with layers, on short two-to-four-letter shortcuts. One `.lsp` file, no config or `.dcl` files needed.

**Platform:** AutoCAD for Mac (also runs on Windows AutoCAD).

## Commands

| Shortcut | Name | What it does |
|---|---|---|
| **ER1** | Set Current Layer | Dialog of **preset** layers → sets picked one current, then re-fires the last drawing command so you keep drawing. |
| **ERS** | Select By Layer | Dialog of **all** drawing layers → grabs every entity on that layer. Whole drawing / by-selection / append-mode / filter existing grip selection. |
| **ERT** | Move To Layer | Dialog of all layers → pick objects → they move onto that layer. |
| **ERD** | Isolate Objects (toggle) | Pick objects → everything else hides (regardless of layer). Run again to unisolate. Wraps native `ISOLATEOBJECTS` / `UNISOLATEOBJECTS`. |
| **ERF** | Layer Off (pick loop) | Pick object → its layer turns off. Keeps prompting. Enter exits. Skips current layer. |
| **ERA** | Restore All | One-key recovery: turns on all layers, thaws all layers, unisolates. Wipes ERD memory. |
| **ERAF** | All Off But Current | Turns off every layer except your current one. |
| **ERL** | Lock Layer (pick loop) | Pick object → its layer locks. Type `A` at the prompt to lock every layer except current. Enter exits. |
| **ERU** | Unlock Layer (pick loop) | Pick object → its layer unlocks. Type `A` to unlock all. Enter exits. |
| **ERSC** | Shortcuts | Prints the command list to the command line. |

## Install

**Quickest:** drag `AKDLayerTools.lsp` from Finder / Explorer onto the AutoCAD drawing window — loads for the current session.

**Persistent (recommended):**
1. Type `APPLOAD` and press Enter.
2. Load `AKDLayerTools.lsp`.
3. Add it to the **Startup Suite** so it auto-loads every session.

Loading one file gives you all ten commands.

## ER1 — preset list

Edit the preset list at the top of `AKDLayerTools.lsp`:

```lisp
(setq *ER1_Layers*
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

`ER1` re-fires these draw commands after setting the layer: `LINE`, `PLINE`, `ARC`, `CIRCLE`, `RECTANG`, `POLYGON`, `MTEXT`, `TEXT`, `HATCH`. Anything else falls back to `LINE`.

## ERS — buttons

| Button | Behavior |
|---|---|
| **All (Drawing)** *(default / Enter)* | Every entity on the chosen layer, whole drawing. |
| **By Selection** | Prompts for two corners → crossing window filtered to that layer. |
| **Cancel** | Nothing. |
| **Append: OFF / ON** *(toggle)* | Merges the new selection with objects already grip-selected. State remembered across runs. |

**Double-click a layer** with objects already grip-selected → filters that selection to just entities on the chosen layer. If nothing was pre-selected, double-click behaves like *All*.

## Typical recovery flow

Layer state gone weird? Just type **`ERA`** — turns everything on, thaws all layers, unisolates.

## Notes

- ER1, ERS, ERT remember their last-picked layer via session globals; pre-selected in the list next time.
- ERD stores its "isolated" state in `*erd:isolated*`. ERA also clears it as a safety net.
- ERL and ERU refuse to touch the current layer (AutoCAD blocks it anyway — the tool just tells you cleanly).
- All dialogs are generated at runtime into a temp file and cleaned up on close. No `.dcl` maintenance.
