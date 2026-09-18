# AKDColumn V2 and WallTool

Load `AKD WallTool/WallTool.lsp` first, then `AKDColumn V2/AKDColumn.lsp` last. Both define `c:EW`; AKDColumn's route needs to stay active. Load WinDoor before AKDColumn when its erase helpers are needed. `AC` also conflicts with WinDoor's curtain-wall command, so the last loaded definition wins.

## Behavior

- `AC` rectangular columns plan all affected centered WallTool masters in the current drawing space. Original masters are removed, surviving spans are created on `wt:cfg`'s axis layer, and `wt:rebuild` regenerates faces and free-end caps on its configured wall layer. No `S-COLUMN` wall caps or `AKCOL` cut-history XData are written.
- Rectangular support includes T, L, and X junctions, reversed master directions, one-ended branches, wholly covered arms, independent crossings, and split collinear spans. A placement with ambiguous master ownership, overlapping masters, incompatible split spans, a zero-length remnant, or an overlapping column is refused before mutation.
- A column outline handle keys one `AKCOL2` XRecord in the drawing's `AKCOL_MASTERS` dictionary. It stores original master endpoints and thickness plus surviving stub handles, endpoints, and thickness. These are drawing-persistent records, not session registry entries or derived faces. `entmakex`, `dictadd`, `dictsearch`, `dictremove`, and `handent` are documented for AutoCAD for Mac. The XRecord is dictionary-owned and removed with the column.
- `AC` circular columns are allowed away from WallTool walls. A circle touching or crossing a WallTool master is refused. WallTool generates square free-end caps, so it cannot produce an exact circular fit.
- `EC` and column selection in `EW` check every recorded stub by handle, geometry, and reconstructed thickness. They restore every original master only if no intervening column, replacement stub, new wall in the gap, or conflicting junction is found. Missing or ambiguous stubs leave the column in place. Wall selection in `EW` passes WallTool's original selection items, including click points, to `wt:ew-erase`; its PickFirst handling, ownership decisions, deduplication, and local rebuild remain authoritative. Mixed selections are preflighted before one undo transaction. WinDoor objects continue through their named erase helpers when loaded.
- Each column's two `AWALL` projection POINTs are in its own `AKCOL*` group. Erase removes only that group's members.
- `CCW` is disabled until resizing can update masters, caps, column geometry, and its own projection tags in one transaction. The previous broad `STRETCH` selection could move a wall gap and remove neighboring tags.
- Legacy columns with `AKCOL` cut-history XData have a guarded restore route for independent non-WallTool LINEs. Earlier `AKCOLW` columns retain the geometric opposite-stub route. Legacy WallTool cuts that cannot be proven safe are refused.

## Checks

Run from `AKDColumn V2`:

```sh
PYTHONDONTWRITEBYTECODE=1 python3 test/test_integration.py
```

The check loads the actual WallTool and AKDColumn LISP sources through WallTool's small AutoLISP interpreter. It covers T/L/X junctions, reversed and short arms, master cuts, caps, rebuild, `EW` wall and column routes, mixed selection, Undo simulation, session registry reset, repeat removal, tag ownership, and ambiguous geometry. Native AutoCAD command behavior, dictionary persistence after save/reopen, group persistence, hatch behavior, and Undo/Redo still require a drawing test in AutoCAD for Mac.
