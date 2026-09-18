# Arkido LISPs

AutoCAD LISP tools developed for optimizing Architectural and Drafting workflows.

Each tool lives in its own folder with its own README (install + usage).

## Tools

| Folder | Command | What it does |
|---|---|---|
| [AKDDoorHole](./AKDDoorHole) | `AD` `AW` `ADD` `AWW` `HH` `CW` `EW` | Plan-view door/window tool for two-line walls. `AD`/`AW` cut the opening and place the door/window in one shot; `CW` resizes with wall repair; `EW` deletes with wall repair (multi-select). |
| [AKDPickArea](./AKDPickArea) | `AA` | Scaled area takeoff for windows, doors, and rooms. Picks closed polylines, totals with unit/scale conversion, drops a labeled MTEXT (e.g. `W3 / 1.44 SQM`). |
| [AKDHolePunch](./AKDHolePunch) | `HH` | Cuts door/window openings through two-parallel-line walls (LINE or LWPOLYLINE). `Center` puts the hole at the clicked segment midpoint; `FromWall` insets by `G` from the nearer end. Type a number at the prompt to set `Width` directly. Loops until Esc. |
| [AKDSmartBoundary](./AKDSmartBoundary) | `SB` | `BOUNDARY` that auto-bridges door/window gaps before tracing. Three detectors (free endpoints, closed-polyline notches, cap pairs) drop temp bridges, run `BOUNDARY`, then clean up. Options: `Walls` (layer filter), `Gap` (max bridged width), `Layer` (output layer). |
| [AKDSmartBoundary+Label](./AKDSmartBoundary+Label) | `SB` | `AKDSmartBoundary` plus an `H` toggle: each new boundary is auto-hatched using the current default pattern with a random-color, 25%-opacity fill on the output layer, sent to back. |
| [AKDVent](./AKDVent) | `VE` | Auto-generates a Ventilation Schedule table from room MTEXTs. Calculates required opening area at 10% of room area (Malaysian UBBL). Pairs with **AKDHatchToLabel**. |
| [AKDHatchToLabel](./AKDHatchToLabel) | `HATX` | Turns a room hatch into a formatted room label (name + area). Duplicate rooms auto-number. Settings-file driven. |
| [AKDWallTool](./AKDWallTool) | `TW`, `FW` | Wall-linework cleanup. `TW` scissors X/T/L junctions, `FW` caps open wall ends. |
| [AKDLayerTools](./AKDLayerTools) | `ER1` `ERS` `ERT` `ERD` `ERF` `ERA` `ERAF` `ERL` `ERU` `ERSC` | Full layer toolkit: set current, select by layer, move to layer, isolate objects (toggle), layer off / lock / unlock (pick loops), restore-all recovery, shortcut list. |

More tools will be added here over time.

## Loading a LISP in AutoCAD

**Quickest — drag & drop:** drag the `.lsp` file from Finder / Explorer onto the AutoCAD drawing window. It loads for the current session only.

**Persistent — APPLOAD:**
1. Type `APPLOAD` and press Enter.
2. Browse to the `.lsp` file inside the tool's folder and load it.
3. Add it to the **Startup Suite** in the same dialog to auto-load every session.

Then type the command shown in the table above.

## License

MIT — see [LICENSE](./LICENSE) if included, otherwise use freely with attribution.
