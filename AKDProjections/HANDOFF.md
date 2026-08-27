# AKDProjections — Session Handoff

## Purpose
Generate elevations and sections from tagged plan geometry drawn by other AKD tools.
Reads `AWALL` xdata (walls + columns) and `AWIN`/`ADOOR` xdata (openings) laid down
by AKDWall's `WW`, AKDColumn's `AC`, and AKDDoorWin's `AW`/`AD`.

## Commands
- `DE`  — single-opening elevation. Pick a tagged window/door, ghost flips side, click to place.
- `WE`  — view elevation. Two view-line points → arrow lock → placement ghost → click.
- `SECT` — section. Frame corner 1 → corner 2 → cut line 2 pts → arrow lock → placement.
- `QQ`  — repeat last WE or SECT.

## SECT flow details (this session's focus)
1. **Frame pick** (two points). Frame is drawn as a temp HIDDEN cyan polyline on layer `0`.
   Scope for background walls + far-side openings. Auto-erased at end or on cancel via `*error*`.
2. **Cut line** (two points). Cut-line scan is NOT filtered by frame — the cut segment already
   scopes hits (see `_dwe-sect-scan`, `AKDProjections.lsp:748`).
3. **Arrow lock** (grread loop). Arrow tip = drawing side. Click locks `lock-side`.
4. **Placement ghost** (grread loop). Rubber-band rectangle constrained perpendicular to view
   line. Cursor position sets both drawing side (recomputed each mousemove) and offset.
   Click captures `(place-side . bl)`. Arrow lock is currently informational — placement wins.

## Painter's order (SECT render)
Background walls first → far-side openings → cut walls last. Cut walls' SOLID hatches
naturally obstruct anything behind them. Occlusion is done by draw-order only.

## Persistent output
- Section line (c1→c2) drawn on `*dwe-lyr-sect*` after render (record of scope).
- Frame temp entity deleted.

## Layer map (top of file, edit RHS to remap)
```
*dwe-lyr-hatch*  X-HATCH   8   ; cut wall body, slab fills
*dwe-lyr-elv1*   ELV-1     7   ; cut wall + sill/head outlines
*dwe-lyr-elv2*   ELV-2     3   ; bg wall outlines, opening frames
*dwe-lyr-elv3*   ELV-3     4   ; mullions, panel divisions
*dwe-lyr-elv5*   ELV-5     5   ; slab outlines, ground line
*dwe-lyr-sect*   X-TAGS & SYMBOLS  1  ; persistent section line
*dwe-lyr-frame*  0         4   ; temp frame (HIDDEN linetype)
*dwe-lyr-anno*   X-ANNO    2   ; DE dimension text
```
Component-to-layer aliases follow (`*dwe-lyr-cut-fill*`, `-cut-out*`, `-bg-wall*`, `-win*`,
`-glass*`, `-door*`, `-panel*`, `-slab-fill*`, `-slab-out*`). All entities drawn ByLayer
(no DXF 62 forced) — layer color/linetype/lineweight wins.

## Session tunables (`if null` guards preserve session state)
- `*dwe-win-h*` 1500 / `*dwe-win-sill*` 900 / `*dwe-door-h*` 2100
- `*dwe-offset*` 3000  (initial ghost offset, then follows cursor)
- `*dwe-gnd-ext*` 500  (ground line context beyond plan)
- `*dwe-wall-h*` 2700 / `*dwe-slab-t*` 150
- `*dwe-far-tol*` 5000  (unused now — kept for compatibility)
- `*dwe-face-tol*` 300  (WE only: slop past wall thk/2 for view-line match — always refreshed)

## What works
- SECT top/bottom (horizontal cut line, placement above/below) — user confirmed "perfect".
- Frame persists visually through picks, auto-erased on end/cancel.
- Cut walls have polyline outlines around SOLID fills (sill/head strips included).
- Slabs flush with outermost cut/bg wall extents, closed polyline outlines.
- Far-side openings filtered to those fitting within a bg wall's along-range (no floating rects).
- All openings/frames/walls are single closed LWPOLYLINEs.
- Placement can cross view line; drawing follows cursor side.
- Column tags (AKDColumn `AC`) processed as walls — same AWALL schema.

## Known / user-flagged issues
- **Left/right cuts (vertical view line) render rotated 90°** — drawing xh follows sect-dir
  so a vertical cut produces a rotated elevation. User rejected the "world-upright" fix
  ("no why upright. dont do that"). Root cause of complaint may just be arrow direction
  interpretation; needs another round of clarification.
- **Occasional "removed walls"** — no code path deletes walls. Possible causes:
  - `wdot > 0.85` in `_dwe-sect-bg-walls` skips walls > ~30° off cut direction.
  - Column vertical-axis tag is perpendicular to horizontal cut → skipped as bg wall by
    design (would be edge-on). Only horizontal-axis tag shows for that column.
  - Frame too small → bg walls/openings outside frame skipped.
- Arrow ↔ view-side ↔ placement-side ↔ mirror semantics went through many revisions
  in this session. Current state: arrow lock is preview-only; placement recomputes side
  from cursor. No content mirroring. User seemed satisfied at that point.

## Files
- [AKDProjections.lsp](AKDProjections.lsp)
- [elevation-section-tool-spec.md](elevation-section-tool-spec.md) — original spec (may be stale)

## Related tools (schemas consumed)
- [AKDWall/WW.lsp](../AKDWall/WW.lsp) — `WW`, tags `AWALL` POINT markers on layer `A-WALL-DATA`.
- [AKDColumn/AKDColumn.lsp](../AKDColumn/AKDColumn.lsp) — `AC`, drops two `AWALL` tags per column.
- [AKDDoorHole/](../AKDDoorHole/) — `AW`/`AD` for `AWIN`/`ADOOR` xdata.

## Quick pick-up
Load `AKDProjections.lsp`. `WW` a rectangle, `AW`/`AD` a couple of openings, `SECT`,
frame the plan, cut across, click arrow up, click placement above. Should render.
