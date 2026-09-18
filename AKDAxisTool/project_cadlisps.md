---
name: project-cadlisps
description: State of the CADLisps AutoLISP toolchain — Draw Axis (AX) and Axis-to-Wall (XWW) — as of session handoff.
metadata: 
  node_type: memory
  type: project
  originSessionId: f77bc70a-74b8-4442-a065-4e28c6ffbd36
  modified: 2026-08-18T04:42:05.099Z
---

AutoLISP tools for a modular CAD wall-drawing workflow. Files live in `/Users/razzan/Documents/Claude Projects/CADLisps/`.

**Why:** User is building this iteratively as a suite where each tool builds on prior conventions (AXIS layer semantics established by AX are consumed by XWW).
**How to apply:** When extending, keep the AXIS/WALL layer conventions and BYLAYER color/linetype defaults consistent across new tools.

## DrawAxis.lsp — command `AX`
- Line tool that draws on layer `AXIS` (created if missing) with color red (1), linetype `PHANTOM2`, color/linetype `BYLAYER` on entity, `CELTSCALE` 500.
- Loads PHANTOM2 from `acad.lin` if not present; enforces AXIS layer properties every run (even if pre-existing).
- Restores previous CLAYER/CECOLOR/CELTYPE/CELTSCALE afterward.

## AxisToWall.lsp — command `XWW`
Converts preselected AXIS lines into walls on layer `WALL` (color 7, Continuous). Thickness stored in `*ax-wall-thk*` (default 150); at prompt press `T` to change. Wraps operations in one `UNDO` group. Prints `Junctions detected: L=? T=? X=?` at end.

Junction handling (all classification is by endpoint/interior relationships between axes; walls are found by geometry, not stored ename, so multiple splits on the same wall work):
- **L-corner** (endpoint↔endpoint): compute interior/exterior wall pairs; each pair meets at their extended intersection. Uses `ax-move-nearest-endpoint` (direct entity trim/extend) instead of AutoCAD's `FILLET` — fillet was unreliable at outer corners needing extension.
- **T-junction** (endpoint↔interior): split crossbar's near-side wall between the stem's two wall intersections (removes middle); trim stem walls to those intersections.
- **X-cross** (interior↔interior): split each of the 4 walls between its two intersections with the perpendicular pair — clean open plus.
- **End caps**: any axis endpoint not touching another axis gets a perpendicular cap line on WALL layer.

Key implementation notes:
- Axis record: `(ent p1 p2 wall+ wall-)` — but wall enames are seed values only. All operations look up the current segment via `ax-wall-at-point` (searches WALL-layer lines by parallelism + perpendicular offset + segment containment). This is what made multi-junction grids work.
- `ax-split-line` deletes the middle piece by shrinking the original entity and `entmakex`'ing a new one for the far side.
- `ax-compute-ip` computes intersection of two offset axis lines algebraically (independent of current wall geometry).
- Local var named `t` will error with "incorrect object to bind: T" — AutoLISP reserves T. Use `tt` instead.
- Requires `TRIMMODE=1` (set/restored). Sets `FILLETRAD=0`, `CMDECHO=0` during run.
- Endpoint-matching tolerance is `1e-6` via `ax-pteq`; strict-inside check uses cross product with `1e-6 * len(axis)` tolerance.

## Known unhandled case
- 3-way endpoint junctions (two collinear axes end-to-end + perpendicular stem endpoint at same point) — user's workaround: draw the crossbar as one continuous axis. A `T-virtual` implementation was written and then reverted per user request; can be reintroduced if needed (adds collinear-mate detection to route the L into a virtual-crossbar T).

## Next likely additions
User has been building a room-planning toolkit. Anticipated next tools may follow the same pattern (preselect axes, generate WALL geometry). Keep helper functions (`ax-*` prefix) reusable across files if new tools are added — or move shared helpers into a common lib if the collection grows.
