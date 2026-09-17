# AKD WallTool — session handoff (2026-09-17)

## Current state

Branch `walltool-integration` holds the integrated line (centerline masters, TX, WWD, WWE, WWR, axis healing). The old A/B split below is history.

AKD WinDoor integration (2026-09-17): openings layer in section 10 (`wt:openings`, `wt:op-assoc`, `wt:opening-voids`), transaction close `wt:pend-end` (deletes openings of removed spans), WWD moves openings, WWR ignores registered jambs/gaps (`wt:wr-open-faces`). `WR` renamed `WWR`. Tests: `test/run_tests.py` (434) and `test/integration_tests.py` (55, loads AKD WinDoor). See README "Openings".

## Package

```
WallTool.lsp   WallTool.txt   WallTool.dcl   README.md   CHANGELOG.md   HANDOFF.md
test/alisp.py  test/run_tests.py  test/tx_only.py
```

- `test/results.svg` and `__pycache__` are generated (ignored by `.gitignore`); `.DS_Store` is ignored at repo root.
- `test/tx_only.py` is new: runs only the TX/WWD/WWE part of the suite for fast iteration.
- Loads as: `AKD WallTool 0.1.0 loaded: AX, ZXW, WW, XW, EW, WWF, WWD, WWE, TW, TX.`

## Commands in the working tree (A)

| Cmd | Section | Summary |
|---|---|---|
| AX | 6 | Axis lines on X-AXIS |
| ZXW | 7 | Grid + bubbles |
| WW | 14 | Walls: `[Width/posiTion/Rectangle/Settings]`, `[Width/posiTion/Undo/Close]`, posiTion Q/W/E |
| XW | 15 | Lines → walls |
| EW | 16 | Erase one span, rebuild neighbours |
| WWF | 17 | Wall from wall (WWO alias) |
| TW | 18 | Local connection repair for AKD walls |
| **TX** | 19 | **New.** Junction cleanup ("supertrim") for ordinary LINEs on any layer, no AKD data. Preselection or window. Plan-then-apply: merge collinear pieces/gaps → find parallel face pairs (double-line walls) → wall L/T → single-line L/T → apply. X-AXIS masters skipped. |
| **WWD** | 20 | **New.** Wall to distance: pick face to move, pick reference face, enter clear distance (default 1200, remembered). Parallel walls only. AKD walls move via master + rebuild; ordinary double-line walls are translated then cleaned with the TX solver. |
| **WWE** | 21 | **New.** Wall extend: pick wall end, pick target face. Extend only (never shortens or moves the target). Handles free ends, L and T ends (goes through the old host as a cross), angled walls, AKD and ordinary walls; refuses mixed AKD/ordinary junctions, obstructions, ambiguity, collinear continuation. |

New config keys (`WallTool.txt`, section `[REPAIR]`): `TX_CONNECT_DISTANCE=150`, `TX_WALL_MAX=600`.
New tolerances (section 19): `*wt:tx-tol-col*` 0.5, `*wt:tx-tol-par*` 1e-4, `*wt:tx-min-sin*` 0.02.

## Architecture (unchanged in A)

- X-AXIS master spans = logical wall network; A-WALL is derived and regenerated.
- Pipeline: operation → master network → `wt:normalize-masters` → `wt:rebuild` → solver (sections 8–10) → A-WALL.
- Identification only in section 11 (`wt:wall-from-master`, `wt:ew-resolve`, `wt:face-owners`, …).
- Every edit records created/erased/modified in `*wt:pending*`; `wt:seg-undo` reverts. One AutoCAD undo group per command (`wt:begin`/`wt:end`).
- No XData, no VL/VLA/VLAX, plain AutoLISP + DCL, Mac + Windows.
- TX/WWD/WWE add a second, geometry-first path for ordinary (non-AKD) LINE walls.

## Status

- Harness (A): **310/310 pass** (`python3 test/run_tests.py`, ~2–3 min). Of these, 77 TX, 29 WWD, 51 WWE checks.
- Static checks not re-run on A this session (last clean run was on v0.1.0 / branch B).
- Manual AutoCAD acceptance of TX/WWD/WWE: unknown to this session — ask the user.
- README and CHANGELOG in A do **not** document TX, WWD, WWE (they are the v0.1.0 text).

## Harness notes

- `test/alisp.py` is an AutoLISP subset interpreter with a fake drawing DB (entmake/entget/entmod/entdel/ssget), stubs for getpoint/getcorner/entsel/getkword/getdist/getreal/ssnamex, and `command-s` clearing PickFirst like AutoCAD.
- Tests run the real `WallTool.lsp`. Wall geometry is checked against independently sampled outlines; incremental edits against a full solve; Undo against exact snapshots.
- Never name a LISP local after a builtin (`rem`, `last`, …): AutoLISP is Lisp-1 and it breaks callees dynamically. `ssnamex` takes an index, not an ename.

## Branch B details (for the integration decision)

- `wt:placement-to-centerline`, `wt:centerlines`: LEFT/CENTER/RIGHT are creation alignments; stored masters are always centerlines; offset ends slide to the centerline they land on; a single host end follows the corner and is passed to `wt:rebuild`.
- `wt:recon` recognises CENTER only; legacy eccentric masters migrate only via WWR.
- WWR (section 19 there): band audit re-centers masters from faces, keeps registered masters whose faces are gone, rebuilds missing masters only when both ends are proven (junction or cap), joins broken collinear pieces, normalizes, rebuilds.
- WW prompts `[Width/Alignment/Rectangle/Undo/Close/Settings]`, `A` submenu `Alignment [Left/Center/Right]` (Q/W/E aliases), chain entries validated against current master ends (fixes corner join after Undo).
- Manual AutoCAD testing of branch B: pending.

## Known limitations (A)

See README "Known limitations" (general + TW). Additionally, from the new commands' messages:
- WWD: parallel walls only.
- WWE: no connected collinear extension; mixed AKD/ordinary junctions refused; never shortens (use EW).
- TX: ordinary LINEs only; masters on X-AXIS are skipped.

## Next steps

1. Decide A vs B integration (see top). Commit A on its own branch before editing anything.
2. Update README/CHANGELOG for TX, WWD, WWE (and WWR/centerline if merged).
3. Re-run static checks (undefined `wt:` calls, duplicate defuns, builtin shadowing, no VL/XData).
4. Manual Mac acceptance, then merge to `main` and tag (e.g. `walltool-v0.2.0`) only when asked.
