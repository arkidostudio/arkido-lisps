# AKD WallTool — Changelog

## Unreleased (walltool-integration)

- **Centerline masters** — every stored X-AXIS master is the wall's geometric centerline. LEFT / CENTER / RIGHT are creation alignments for WW, Rectangle and XW; offset ends meet the centerline they land on, so drawn walls look the same as before.
- **WR** — wall repair: audits masters in a window, centers off-centre and legacy eccentric masters from their faces, rebuilds missing masters only when both ends are proven, joins broken collinear master pieces, normalizes and rebuilds. One undo step, idempotent.
- Reconstruction recognises centered masters only; older eccentric drawings migrate through WR.
- **TX** — junction cleanup for ordinary LINEs (single lines and double-line walls): collinear merge, L, T, X, caps in a window. AKD wall geometry is protected.
- **WWD** — wall to distance: moves one wall so a picked face is at a clear distance from a reference face (AKD and ordinary walls).
- **WWE** — wall extend: extends one wall end to a picked target wall, including L- and T-connected ends.
- **TW** — also repairs ordinary double-line walls in the window.
- Older off-centre walls are refused by WWD / WWE ("Run WR first") and left alone by TX / TW. WR ignores other walls' caps inside a wall's band when auditing.

## v0.1.0 — Stage 1: 2D Wall Core

First stable development baseline.

- **AX / ZXW** — axis lines on X-AXIS; orthogonal grid with numbered/lettered bubbles on X-GRID.
- **WW** — intelligent walls: width presets/custom, Center/Left/Right position, Rectangle, Close, logical Undo, face picking to connect to existing walls.
- **Wall topology** — L, T, X, collinear and multi-way junctions at any angle with mixed thickness and position; masters normalized into independent spans at topology nodes.
- **XW** — convert LINEs into walls as one network.
- **EW** — erase one logical wall span (by master or face pick) and regenerate its neighbours.
- **WWF** — wall from wall: parallel wall at a clear face-to-face distance from a clicked face, with automatic junctions and caps (`WWO` kept as alias).
- **TW** — local wall repair: rebuild damaged linework, connect small gaps, trim small overshoots inside a window.
- **Configuration** — `WallTool.txt` (KEY=VALUE) with built-in fallbacks.
- **Platform** — plain AutoLISP + DCL for AutoCAD for Mac and Windows; no COM/ActiveX/VLA/VLAX.
- Walls are ordinary editable AutoCAD LINEs; no XData or custom objects.
