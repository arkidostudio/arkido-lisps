# AKDDW Session Handoff

**Project:** Arkido Doors & Windows — single AutoCAD LISP for Mac
**Path:** `/Users/razzan/Documents/Claude Projects/CADLisps/AddWindow/`
**Files:**
- `AKDDW.lsp` — the tool (all commands, load with `APPLOAD`)
- `AKDVent.lsp` — unrelated reference (table style inspiration)

## Commands
| Cmd | Purpose |
|---|---|
| `AW` | Add window. Prompt keyword `D` = set divisions (mullions). Picks 2 points, auto-labels if enabled. |
| `AD` | Add door. Keywords `S`/`D`/`G` = Single/Double/sliGing (immediate switch); `G` also prompts panel count; `P` sets panels only. Ghost preview lets mouse flip hinge/swing side. |
| `WC` / `DC` | Print width tally to command line. |
| `WR` / `DR` | Renumber. Prompts selection (Enter=all). Uses width-desc ranking, then type/div. |
| `LT` | Toggle labels on/off. Shows `Labels: ON, Continuous` etc. Keywords `On`/`oFf`. |
| `LC` | Numbering mode. Keywords `Continuous`/`New`. New increments batch counter so future items start at W1/D1 without touching existing labels. |
| `DWT` | Doors & Windows schedule table at picked point. Prompts selection (Enter=all). |

## Config block (top of file — user-editable)
Layer/color pairs per element type; layers auto-create. Current values:
- Windows on `A-WINDOW`, doors on `A-DOOR` (yellow frames/panels, red glass/arc/wall lines)
- Labels on `X-TAGS & SYMBOLS` (red shape, yellow text)
- Dimensions: window frame 50×100, door frame 50×100, sliding frame 50×(35×panels), panel thickness 35, wall-line offset 75, label hex Ø500, label circle Ø450, text height 150

## Key implementation details
- **Xdata tags:** `AWIN` on window main frame with (1040 width) (1070 div) (1071 side) (1041 batch) (1011 mid) (1013 axis-dir) (1000 "G:<gname>") (1000 "L:<label>"). `ADOOR` similar plus (1070 type: 1=S 2=D 3=G) (1071 div/panels) (1042 side).
- **Grouping:** each door/window is an anonymous-named ACAD group (`AWIN1`, `ADOOR1`, ...). Labels are separate groups (`AWINLBL1`, ...).
- **Batching:** `*label-batch*` int, incremented on `LC>N`. Renum auto-called from AW/AD filters by current batch; manual WR/DR ignore batch.
- **Live preview:** grread loop draws ghost via `_ghost` / `_ghost-dbl` / `_ghost-slide`; mouse position in wall-local coords sets `fx` (along-wall flip) and `fy` (perp flip). Esc / ctrl-C wrapped in vl-catch-all-apply so ghost is redrawn away on cancel.
- **Sliding pattern:** `n=4` uses bi-parting layout (`___----- -----___`), other n stack in equal slots with 25mm overlap at each meeting point.

## Recent bug fixes worth knowing
- **AutoLISP `or` returns t/nil, not the value.** All `(or x fallback)` patterns must be `(if x x fallback)` in this file. Two sites in `_collect-doors` / `_collect-windows`.
- DXF code order: `(cons 62 color)` must appear at AcDbEntity level, before subclass markers.
- Door xdata `side` uses code 1042 (real) — 1072 is not a valid xdata group code.
- `_uniqname` must seed `n` before the while loop (fresh drawing has no ACAD_GROUP dict).
- All string-arg helpers coerce non-strings via `vl-princ-to-string` defensively.

## Session state / known good behavior
- `AW` and `AD` create geometry, group it, tag it, auto-run renum → label appears with side chosen by ghost.
- `LC>N` starts fresh numbering; `LC>C` returns to global sequence.
- `DWT` builds a two-section table (DOORS above WINDOWS), rows are per unique (width, type[, div]) with count column, labels match what renum would assign globally.
- Layers `A-WINDOW`, `A-DOOR`, `X-TAGS & SYMBOLS` created on first use.

## Open items (nothing broken, just future asks)
- Table currently derives labels via global ranking (mirrors renum). If the user later wants literal in-drawing labels, the xdata read path is stubbed (`_extract-lblnum`, `_tag-lblnum`) but unused.
- Sliding-door flip preview only toggles `fy`; `fx` is symmetric so pinned to 1.

Reload with `(load "AKDDW.lsp")` in a fresh session and everything is available.
