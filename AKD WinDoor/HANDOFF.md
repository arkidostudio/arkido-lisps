# AKDDoorWin — session handoff

## What it is
AutoCAD LISP for AutoCAD Mac. Plan-view doors, windows, curtain walls, corner windows, pocket doors — cuts the wall opening and places the element in one command. Also resizes (`CW`), erases-with-wall-repair (`EW`), and repairs empty holes (`RH`).

Single file: `AKDDoorWin.lsp` in `/Users/razzan/Documents/Claude Projects/LISPs/AKDDoorHole/`. Repo `arkido-lisps` at github.com/arkidostudio/arkido-lisps, branch `main`. Last pushed commit `8b55eaa` (revert of CW short-circuit). **Pocket door work + latest polyline/dim-text/draworder changes are uncommitted.**

Companion tool: `WW.lsp` (root of repo) draws double-line walls. Compatible with all AKD hole-cutting commands.

## AKD WallTool integration (2026-09-17)
- Provider/hook section near the end of `AKDDoorWin.lsp` (`akd:wt-*`). WallTool API: `wt:api-opening-status`, `wt:api-openings-changed`.
- WallTool walls: `hole:do` (AD/AW/ACW), `ew:do-one` (EDW/VX), `CW`, `RH` delegate linework to WallTool. Ordinary walls keep the legacy code.
- `EW` renamed `EDW`, `WR` renamed `WRN` (WallTool owns EW / WWR).
- Integration tests: `AKD WallTool/test/integration_tests.py` (loads this file).
- Corner windows unsupported: XData lacks arm geometry (see README).

## Current commands (see README.md for user-facing table)

Placement (loops, accepts typed width, cuts hole first):
- `HH` hole only
- `AD` door — types via `A`/`D`/`S`/`Pocket`/`Number`
- `AW` window
- `ACW` curtain wall — `S`→Spacing mode then `S`=set value; `D`→Divisions mode then `D`=set value

2-click direct-draw (no hole cutting):
- `ADD` `AWW` `ACWW`

Corners:
- `AXW` corner window — pick end → corner → end; `Divisions` sets per-arm panels; `H` runs `HHX` then exits
- `HHX` corner hole only

Edit:
- `CW` change width (walls repair via `cw:move-wall-end` moving every matching endpoint; short-circuit was tried and reverted — grabs multiple stubs on corners but the user's typical geometry works)
- `EW` erase placed door/window + repair wall
- `RH` repair empty hole — pick 2 cap lines, merges 4 wall stubs back

Bookkeeping: `WC` `WR` `DC` `DR` `DWT` `LT` `LC`.

## Recent session changes (uncommitted)

### Pocket door (new door type, "P")
- User's spec: 1550 system = 650 clear opening. Standard formula: **total = 2×opening + 250**.
- Keyword change: renamed `Panels` → `Number` to free `P` for `Pocket`.
- Flow: `AD` → `Pocket` → `650` → click wall. Numeric input in Pocket mode sets `*cfg-pd-opening*`; total is auto-derived via `hd:getw` returning `2*opening + 250`.
- Layout (in `akd:pd-parts`, all dims in mm):
  - Along wall from `p1`: `greyWall`+`gypsum` at x=[0, greyLen]; `jambPost` at x=[0, 50] (back zone, overlaps greyWall in x but not in y); `leaf` at x=[jamb, jamb+greyLen] (lip = jamb-length past greyWall); `endStud` at x=[greyLen-50, greyLen]; `midStud` centered between jambPost right and endStud left; opening = [greyLen, totalW-jamb]; `strikeFrame` at x=[totalW-jamb, totalW].
  - Across wall (fy=1 puts greyWall on +perp side): pocket components (75+40+25+10=150) are **centered** in wall thickness. `off = (fd - 150) / 2`, `yGF = fdH - off`. This fixed the "sits 25 above wall" bug when `*cfg-door-fd*` ≠ 150.
- Grey wall = SOLID entity (color 9) + polyline outline; outline is brought to front via `command "_.DRAWORDER"` so the black outline sits above the color 9 fill.
- StrikeFrame = 8-vertex C-shape polyline with 18mm groove opening toward the pocket, spanning full wall thickness (yGyB → yGF).
- Wall boundary lines only in the opening (between greyWall right and strikeFrame left), NOT the whole length.
- Ghost preview (`_ghost-pocket`) shows greyWall + leaf + gypsum + strikeFrame + wall lines — omits jamb/studs so the strikeFrame groove clearly indicates the opening side.
- `CW` refuses to resize pocket doors (custom geometry — user just deletes and redraws). `_tagdoor` uses type code 4 for pocket.
- Config vars at top of file: `*cfg-pd-opening*` `*cfg-pd-jamb*` `*cfg-pd-jamb-h*` `*cfg-pd-grey-t*` `*cfg-pd-leaf-t*` `*cfg-pd-stud-t*` `*cfg-pd-gyp-t*` `*cfg-pd-stud-w*` `*cfg-pd-lipext*` `*cfg-pd-groove*` `*cfg-pd-grey*` (grey outline layer/color).

### All members as polylines
- New helper `_mkpline-open` (LWPOLYLINE with `70 . 0`) for lines that shouldn't be closed rectangles.
- `_pd-x` X-marks now use `_mkpline-open` (2-vertex polylines).
- Pocket wall lines in the opening use `_mkpline-open`.

### Dimension text on Defpoints, red
- New config: `*cfg-dim-text* '("Defpoints" . 1)`.
- The centered width-readout text (`_mktext ... (rtos width 2 0) ...`) in `akd:place-window` and `akd:place-door` now uses `*cfg-dim-text*` (was `*cfg-lbl-text*` yellow on X-TAGS).
- AXW corner window doesn't stamp a middle text; no change there.

### Prompt reformat + numeric shortcut fix
- All placement prompts now follow `[Kind | Type: X | Width: N | Placement: FromWall | Gap: N] Click wall or [...]:` — see `hole:prompt2`.
- Numeric-at-prompt shortcut (`AD` `800`) was broken because `entsel` on Mac AutoCAD doesn't reliably return typed strings. Switched `hole:loop` to `getpoint` (which accepts typed input reliably); entity detection now uses `ssget inp '((0 . "LINE,LWPOLYLINE")))` at the click point. Trade-off: you now click ON the line (osnap grabs it). If nothing hits, prints "No LINE or polyline at that point. Use snap."

### AXW cleanup
- Removed Center/Inner/Outer reference options — always Center.
- Flow changed to end → corner → end (was corner → end → end).
- Added `H` keyword that runs `HHX` then exits (user re-runs AXW to place).

### RH (Repair Hole) command
- User picks two cap lines. Reuses `ew:merge` / `ew:rejoin` machinery. `rh:pair-b` picks collinear partner endpoint based on off-axis distance. Earlier tried a window-select variant that filtered LINEs by `rh:is-cap` — user preferred the 2-click version and reverted.

### Small fixes
- `(exit)` in `_win-renum` / `_door-renum` swapped for `cond` guards (was throwing errors on empty selection).
- Dead `kmap` loop in `_win-renum` removed; prefix list now derived from `keys` (supports arbitrary types beyond `W`/`CW`).
- Command renames: `AC` → `ACW`, `ACC` → `ACWW`.

## Key design decisions (do NOT undo without asking)
- **Groups, not blocks.** Widths vary continuously — per-width blocks defeat reuse, non-uniform scale distorts frames.
- **Closed polylines** via `_mkpline` (DXF 70=1). `_mkpline-open` (70=0) exists for X-marks and diagonal/track lines only.
- **`or` returns T on AutoCAD Mac when given `nil <list>`.** All `(setq info (or …))` patterns are `(if a a b)` — do not "simplify" back to `or`.
- **Getpoint (not entsel)** on Mac AutoCAD for main input, so typed numbers work reliably.
- **`ssget "_X"` with layer/xdata filters is fine on Mac.**
- **`cw:move-wall-end` moves ALL matching endpoints** (not first-only). The short-circuit fix was reverted because it broke normal CW repairs. Grabbing/slanting on wall corners is a known limit — user's typical geometry works.
- **HHX trims one side** via `hhx:trim-half` — do not replace with `hole:do` + stub cleanup.
- **Door shortcuts**: `A`=Single, `D`=Double, `S`=Sliding, `Pocket`, `Number` (was `Panels`). User's chosen letters.
- **Pocket door total formula**: `2×opening + 250` (fixed). Do not "generalize" without asking.
- **User workflow** (from persistent memory): solo dev, tight prompts, minimal code, no emojis, no trailing summaries, no over-engineering.

## Xdata schema
Each placed door/window carries xdata under app `AWIN` (windows/curtain/corner) or `ADOOR` (doors):
- 1040 = width (float)
- 1070 = door subtype (1=Single, 2=Double, 3=Sliding, 4=Pocket) OR window divisions
- 1071 = door divisions (for sliding) OR window side (±1)
- 1042 = door label side
- 1041 = label batch number
- 1011 = midpoint (3D point)
- 1013 = wall-direction unit vector (3D point)
- 1000 "G:<gname>" = group name
- 1000 "T:<type>" = window sub-type ("W" or "CW")
- 1000 "L:<label>" = current label number (e.g. "W3", "D1")

Label entities carry `AWINLBL` / `ADOORLBL` with the group name for auto-cleanup during renumber.

## Known issues / follow-ups
- **CW pocket door**: refuses to resize. Would need a custom pd-resize path if user asks. Delete + redraw is fine.
- **CW wall slant** on corners where multiple wall stubs share a vertex: current `cw:move-wall-end` grabs both. Fix would be a layer + direction filter, not a first-hit break.
- **AXW hole cutting**: not integrated (previous attempt reverted). `H` in AXW runs HHX separately; user restarts AXW to place.
- **Ghost preview for CW resize**: none currently. User types number and it just resizes.
- **Right jamb groove direction**: verify `(+ (- totalW jamb) gr)` puts the notch opening toward the pocket (left) not away. If wrong, flip the `+` to `-` in those vertices.
- **DRAWORDER command**: added `command "_.DRAWORDER" ent "" "_F"` inside a command echo=0 UNDO block. If it prints stray text on Mac, wrap in a suppression or use `entdel`/`entmake` to reinsert instead.

## Test recipe
On a fresh drawing with `WW.lsp` walls (thickness 150):
1. `AD` → `Pocket` → `650` → click a wall segment ≥1550 wide.
   - Expect: 1550-wide system, grey wall filled solid grey with black outline, leaf tucked behind jambPost, strikeFrame with groove at right, dimension "1550" in red on Defpoints in the middle.
2. `AD` → `Single` → `900` → click wall. Expect single door, "900" dimension in red on Defpoints.
3. `AW` → `Divisions` `4` → click. Expect 4-panel window.
4. `ACW` → `S` → `S` → enter spacing → click. Expect curtain wall.
5. `AXW` → click end → corner → end. Expect corner window (no hole cut).
6. `AXW` → `H` → HHX runs. Then `AXW` again → place the window over the hole.
7. `RH` → pick 2 cap lines on any hole. Expect wall stubs merged, cap lines deleted.
8. `CW` on any non-pocket door/window. Expect resize with wall repair.
9. `EW` → select tagged doors/windows. Expect delete + wall repair.

## Repo housekeeping
- Other dirty files in root repo (`../README.md`, `../WW.lsp`, `../AKDAxisTool/`, etc.) are NOT part of this session's work. Leave them alone unless user asks.
- When committing, stage only `AKDDoorHole/AKDDoorWin.lsp` and `AKDDoorHole/README.md`.
- Commit format the user has been using:
  ```
  AKDDoorWin: <short summary>

  <optional detail bullets>

  Co-Authored-By: Claude Opus 4.7 <noreply@anthropic.com>
  ```
