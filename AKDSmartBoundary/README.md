# SmartBoundary

An AutoCAD LISP that behaves like `BOUNDARY` (`BO`) but bridges small gaps — door and window openings — automatically before tracing the boundary. Works on AutoCAD for Mac (pure AutoLISP, no ActiveX).

## Commands

- **`SB`** — auto mode. Pick an internal point, SB detects openings and bridges them, then runs `BOUNDARY`.
  - `Gap` option — change the max opening width bridged.
  - `Layer` option — set the layer new boundaries are placed on (created if missing). Enter `.` to reset to current layer.
- **`SBM`** — manual mode. Click pairs of points across each opening, pick the internal point, SB bridges and runs `BOUNDARY`. Also respects the output layer set via `SB`'s `Layer` option, or `(setq *sb-out-layer* "MYLAYER")`.

## Install

1. Save `SmartBoundary.lsp` somewhere permanent.
2. In AutoCAD: `APPLOAD` → browse to `SmartBoundary.lsp` → **Load**.
3. To auto-load every session: in the APPLOAD dialog, click **Contents** under *Startup Suite* and add the file.

To reload during development:

```
(load "/path/to/SmartBoundary.lsp")
```

## How auto detection works

Three detectors run and combine their bridges:

1. **Free endpoints** — pairs open curve endpoints (LINE / ARC / open POLYLINE) that are within `*sb-gap*` and roughly collinear with the wall direction. Handles single-line walls with breaks.
2. **Closed-polyline notches** — scans vertices of closed polylines for pairs whose entering segments are perpendicular to the direct line between them. Handles walls drawn as one closed polyline with inward jogs at doors.
3. **Cap pairs** — finds pairs of short parallel lines (wall-thickness caps) facing each other across a gap. Bridges their endpoints. Handles typical double-line walls with capped ends at each opening.

Temp bridges are drawn on layer `SB_TEMP` in red, `BOUNDARY` runs, then the temp lines are erased.

## Tunables (top of `SmartBoundary.lsp`)

| Variable        | Default | Meaning |
|-----------------|---------|---------|
| `*sb-gap*`      | `1000`  | Max opening width to bridge (drawing units). |
| `*sb-fuzz*`     | `0.5`   | Endpoints within this distance are treated as connected. |
| `*sb-align*`    | `0.85`  | Min `|cos θ|` between bridge direction and wall tangent (free-endpoint detector). Lower = more permissive. |
| `*sb-cap-max*`  | `300`   | Max length considered a wall-thickness cap. |
| `*sb-keep*`     | `nil`   | Set `T` to leave temp bridges visible for debugging. |
| `*sb-out-layer*`| `nil`   | Layer for created boundaries. `nil` = current layer. |

Change from the command line:

```
(setq *sb-gap* 1500)
(setq *sb-cap-max* 200)
```

## Manual mode (`SBM`)

Falls back to hand-picked bridges when auto detection can't handle the geometry:

1. Type `SBM`.
2. Click two points to define each bridge (use OSNAP endpoint / intersection). Enter to finish.
3. Click the internal point.
4. SB draws the bridges, runs `BOUNDARY`, cleans up.

## Notes / limitations

- Assumes drawing units are millimetres for the default gap and cap sizes — adjust for other units.
- SPLINE and ELLIPSE walls are not handled (Mac AutoCAD lacks the `vlax-curve-*` functions used to get their endpoints).
- Blocks and XREFs are ignored — explode them first if the walls live inside them.
- `SB_TEMP` layer is created automatically and used only for temp bridges. Don't put real geometry on it — SB erases everything on that layer at the start of each run.

## License

MIT.
