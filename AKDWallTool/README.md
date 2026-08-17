# AKDWallTool

A wall-linework cleanup toolkit for AutoCAD, combining two commands into one LISP:

| Command | What it does |
|---|---|
| `TW`  | **Junction Scissor** — cleans X, T, and L intersections in a picked window. Splits or trims lines exactly at each true intersection point and removes tiny leftover stubs. |
| `FW`  | **Fix Walls** — caps open wall ends inside a picked window (parallel line pairs that don't meet at the corner). |
| `FIXWALLS` | Alias of `FW`. |

**Platform:** AutoCAD for Mac (also runs on Windows AutoCAD).

## Install

**Quickest:** drag `AKDWallTool.lsp` from Finder / Explorer onto the AutoCAD drawing window — loads for the current session.

**Persistent (recommended):**
1. Type `APPLOAD` and press Enter.
2. Load `AKDWallTool.lsp`.
3. Add it to the **Startup Suite** so it auto-loads every session.

Then type `TW` or `FW` to launch.

## Typical workflow

Wall linework almost always needs both passes:

1. `TW` — pick two corners around the messy junctions → intersections are cleaned.
2. `FW` — pick two corners around remaining open ends → they get capped.

Both commands operate on **LINE entities** only. Layer, color, linetype, and lineweight are preserved on all generated segments.

## `TW` — Junction Scissor

For every pair of LINEs inside the window, computes the true math intersection point and classifies:

- **X junction** — both lines cross through each other's interior → both get **split** at the point.
- **T junction** — one interior, one endpoint → the interior line gets split.
- **L junction** — endpoints of both lines meet → both get **trimmed** back to the point (removes overshoots).

Any resulting sub-segment shorter than `*tw:stub-tol*` is deleted.

**Tuning knob** (top of file):
```lisp
(setq *tw:stub-tol* 1.0)   ; delete leftover segments shorter than this
```

## `FW` / `FIXWALLS` — Fix Walls

Detects open wall ends (parallel line pairs whose endpoints don't connect) inside the window and draws a cap line closing them.

**Tuning knobs:**
```lisp
(setq *fw:stub-tol*  1.0)    ; ignore near-touching endpoints
(setq *fw:cap-tol*   500.0)  ; max cap width
(setq *fw:axial-tol* 10.0)   ; how "parallel" two lines must be
```

## Notes

- Only works on `LINE` entities (not `LWPOLYLINE`, `PLINE`, `ARC`).
- All tolerances are in **drawing units** — if you draw in mm, `500` = 500 mm. Change the constants at the top if you draw in a different unit or scale.
- If a junction still looks wrong after `TW` + `FW`, it's usually because the involved entities aren't `LINE`s. Explode them first.

## Origin

Merged from the working `WallScissor.lsp` (TW) and `FixWalls.lsp` (FW) originals. No name collisions — TW uses `tw:` helpers, FW uses `fw:` helpers, so they coexist cleanly in a single file.
