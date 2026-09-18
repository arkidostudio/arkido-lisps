# AKDColumn — session handoff

## What it is
AutoCAD LISP for AutoCAD Mac. Places rectangular columns in plan, cuts and end-wall-repairs any walls that pass through the footprint, live-drops an axis cross inside the column, and tags the column so `AKDProjections`' `WE`/`SECT` picks it up in elevation.

Single file: `AKDColumn.lsp` in `/Users/razzan/Documents/Claude Projects/LISPs/AKDColumn/`. Not yet committed. Repo is `arkido-lisps`, branch `main`, folder-per-tool.

## Current commands
- `AC` — Add Column. Prompt loops with keywords `Width`/`Depth`/`Base`. Click to place at cursor per current base anchor. Draws outer polyline, cuts crossing wall LINEs, caps ends, hatches solid, then runs the live axis-cross ghost — click to lock position. All entities grouped as `AKCOL<n>`; two AWALL POINT tags emitted for projection.
- `CB` — Change Base anchor. Opens the interactive picker (see below).
- `CCW` — Change Column Width. Base point → click reference distance (= current width) → type/click new distance. Wraps native `STRETCH` on a thin crossing window at the ref edge. After the stretch, finds the column polyline by handle, reads the new bbox, deletes the two old AWALL POINT tags in the old bbox, and re-emits fresh ones — so `AKDProjections` reflects the new width.

## Config globals
```
*col-w*            300.0   ; default width
*col-d*            300.0   ; default depth
*col-base*         "C"     ; TL TC TR ML C MR BL BC BR
*col-axis-off*     100.0   ; axis inset from each column edge
*col-layer*        "S-COLUMN"
*col-layer-color*  3       ; layer color; outer poly is BYLAYER so this drives it
*col-hatch-color*  8       ; solid grey hatch, explicit
*col-axis-color*   1       ; red axis cross, explicit
```

## Key design decisions (do NOT undo without asking)
- **`command-s` everywhere.** Regular `command` on Mac AutoCAD can trigger the error handler and leave the engine in a bad state, silently breaking follow-up `entmake`/`ssget`. Every `command` call in this file is `command-s`. From persistent memory: this is a confirmed Mac gotcha.
- **All column entities on ONE layer (`S-COLUMN`).** Outer polyline is BYLAYER (no `(62 . _)` group), so the layer color drives it. Hatch gets explicit color 8 via `CECOLOR` set before `-HATCH`. Axis lines get explicit color 1 via `(cons 62 1)`.
- **Column is a group (`AKCOL<n>`), not a block.** Mirrors the AKDDoorWin decision — widths vary continuously; per-size blocks would defeat reuse and non-uniform scale would distort.
- **AWALL projection tags = two POINT entities per column,** one for each in-plane axis. Piggybacks on the existing `AKDProjections` `WE`/`SECT` reader, which already scans A-WALL-DATA POINTs by AWALL xdata. Height/base come from `*WW_Height*` / `*WW_BaseElev*` if set, else defaults (2700/0).
- **Wall cutter is LINE-only.** WW.lsp draws LINE walls, so LINE-only covers the primary case. LWPOLYLINE walls (AKDWallTool?) are not supported — extend `akc:cut-walls` if needed.
- **Wall cutter has a layer allowlist.** `akc:cuttable-lyr` skips `A-DOOR*`, `A-WIN*`, `A-COLUMN*`, `S-COLUMN*`, `A-WALL-DATA`, `X-TAGS*`. Without this, door/window frame LINEs whose endpoints fall inside the column bbox were being deleted or truncated.
- **Axis cross is FULL width × FULL height,** intersection follows the cursor clamped to `*col-axis-off*` inside all four edges. Ghost via `grread` (event 5 tracks, event 3 confirms, Esc cancels → center).
- **Base picker is interactive.** `akc:pick-base` draws a mini rectangle at `VIEWCTR` with 9 anchor markers. Hover enlarges the target 2.5× and switches it to yellow + hollow crosshair; current anchor stays cyan, others red. Click computes the nearest anchor from the click position (not just the last hover — so a click without moving still works).
- **CCW rebuilds AWALL tags after STRETCH.** STRETCH won't touch the AWALL POINT (it's mid-column on an off layer). So we snapshot the polyline bbox + handle before stretch, run stretch, then re-read the polyline via `handent`, find the old tags in the old bbox, delete them, emit fresh tags for the new bbox.

## Key helpers
- `akc:mkpline` (LWPOLYLINE, closed, BYLAYER) — one call per column outline; also used implicitly wherever we need a rect on `*col-layer*`.
- `akc:mkline` — takes an optional color override; nil = BYLAYER. Cap lines pass nil, axis lines pass `*col-axis-color*`.
- `akc:cut-walls` — scans `_X` LINE selection, filters by `akc:cuttable-lyr`, computes intersections with each of the 4 column edges, splits/trims/deletes as appropriate, returns a per-edge list of hit points.
- `akc:draw-caps` — sorts hits along each edge (X for top/bottom, Y for left/right), pairs adjacent pts, draws cap LINE between each pair (BYLAYER on `S-COLUMN`). Adjacent-pair pairing assumes each wall contributes exactly 2 parallel hits per edge — holds for straight walls, breaks for weird configurations.
- `akc:pick-axis` — grread ghost, clamps cross center to `*col-axis-off*` inside the column, returns the clicked position.
- `akc:pick-base` / `akc:draw-base-picker` — interactive base anchor picker with hover feedback.
- `akc:tag-awall` — emits an AWALL POINT on `A-WALL-DATA` (which the ensure-lyr call makes OFF) with `regapp "AWALL"` + xdata `1000 "AWALL"`, `1040 thk h base`, `1011 p1 p2`. Identical schema to WW.lsp's `ww:tag-awall`.
- `akc:find-col-at` / `akc:pl-bbox` / `akc:awalls-in` / `akc:awall-nums` — CCW-only helpers for the tag-rebuild step.

## Layer / group cheat sheet
- `S-COLUMN` — every column entity: outer poly, hatch, axis lines, cap LINEs.
- `A-WALL-DATA` — AWALL POINT tags (turned OFF so users don't see them; same layer WW.lsp uses).
- Each column is one group named `AKCOL<n>` (unique via `akc:uniqname`).

## Open items / known issues
- **EW (from AKDDoorWin) does not erase columns.** EW is scoped to `AWIN`/`ADOOR` xdata; columns carry `AWALL` xdata. If user clicks near a column with EW, EW picks up the nearest door/window and erases *that*, hence "EW doesn't remove the columns; just the door is getting removed." **Not a bug in AKDColumn** — it's a feature-gap. Options if asked: (a) add `c:EC` here that deletes the column group and optionally rejoins the split walls (rejoin is nontrivial — see notes below), or (b) extend AKDDoorWin's EW to also handle AWALL tags with a `T:COL` prefix and route to a column-erase path.
- **Erase + wall rejoin.** No stored record of which walls were split by which column, and no undo trail besides AutoCAD's own. Simple `c:EC` = delete group + cap lines; user re-draws wall stubs manually or hits Ctrl+Z. Full rejoin would need column xdata storing `(wall-handle, near-pt, far-pt)` tuples for each cut wall.
- **`c:AC` name clash with AKDDoorWin's `c:AC`** (curtain wall). Whichever file loads LAST wins. If user was seeing weird behavior after loading both, that's why. Options: rename to `AKC`, or accept the last-loaded-wins policy and load AKDColumn last.
- **LWPOLYLINE walls not cut.** LINE only. Extend `akc:cut-walls` to also process LWPOLYLINE segments (see AKDDoorWin's `hole:segs` / `hole:splitseg` for the pattern).
- **Cap pairing assumes clean geometry.** Two hits per column edge from one wall = pair. Four hits (two walls crossing the same edge) will still pair (0,1)(2,3) — works if walls are separated, wrong if their hits interleave.
- **Base-picker rectangle uses `VIEWCTR` / `VIEWSIZE`.** If those return degenerate values on some Mac viewport state, the picker will show a zero-size box. Not seen in practice; noted for debugging.
- **Wall-cut diagnostic line prints intersection count.** `\nColumn placed (AKCOL7). Wall cuts: 4 intersections.` — use it to distinguish "no LINEs found" (=0) from "cuts computed but something else failed" (>0).

## Uncommitted state
`git status` on the repo: `AKDColumn/` is untracked (new folder). No commits yet for this tool. Do not push without the user asking.

## What a fresh session should do first
1. Read this file.
2. Skim `AKDColumn.lsp` top-to-bottom — it's ~500 lines and self-contained (no `load` dependencies on other AKD files).
3. Read persistent memory (`MEMORY.md`) — user preferences (lean, tight prompts, no over-engineering, no emojis) apply here too.
4. When the user reports a bug, ask them to paste the diagnostic count line from AC's success message before touching wall-cutter code.
