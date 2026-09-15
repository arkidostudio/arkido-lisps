# AKDDoorWin

Current version: **2.3.4**

Plan-view door and window tool that cuts the opening through a two-parallel-line wall and places the door/window inside it in one shot. Straight doors, windows, and curtain walls are reusable blocks keyed by type and size. Includes corner windows/holes, resize, erase, and hole-repair.

## Commands

### Placement (cuts hole + places object; loops until Esc/Enter)

| Command | What it does |
|---|---|
| `HH` | Cut a hole only (no door/window). Options: `Center`/`FromWall`/`poinT`/`Width`/`Gap`. In `poinT`, move along the wall to make the click the first jamb, center, or opposite jamb, then click to confirm. |
| `AD` | Cuts hole and places a door. Options: `A`=Single leaf, `D`=Double, `S`=Sliding, `Pocket`, `Number`, plus width/placement/gap. |
| `AW` | Cuts hole and places a window. Options: `Divisions`, `S`=toggle sliding (adds mullion ticks). |
| `ACW` | Cuts hole and places a curtain wall. `S`=Spacing mode (then `S`=set value), `D`=Divisions mode (then `D`=set value). |

Type a number at the main prompt to set width directly (e.g. `AD` then `800` = 800mm door width).

### 2-click placement (no hole cutting)

| Command | What it does |
|---|---|
| `ADD` | Draw a door between two picked points. Same type shortcuts. |
| `AWW` | Draw a window between two picked points. |
| `ACWW` | Draw a curtain wall between two picked points. |

### Corner tools

| Command | What it does |
|---|---|
| `AXW` | Corner window: pick first wall end → corner → second wall end. `Divisions` sets glass panels per arm. `H`=run `HHX` first to cut the corner hole. |
| `HHX` | Cut a corner hole through two walls meeting at a picked corner. |

### Edit

| Command | What it does |
|---|---|
| `CW` | Click a placed door/window → prompts new width, resizes the opening + object together. `Base` locks a picked point while the opposite jamb moves. |
| `EW` | Select one or many placed doors/windows → deletes each and closes the wall back up. Works with LINE walls and open/closed LWPOLYLINE walls. If a column is picked, EW delegates to AKDColumn's `EC` (which handles group deletion and wall rejoin via its own xdata) — re-pick the column when prompted. |
| `VX` | Move a placed door/window along its wall. Pick object → base + destination (delta projected onto wall direction), or `Nudge` → type a signed distance. Old hole repaired, new hole cut, same type/width re-placed. |
| `VF` | Re-run the flip/mirror ghost for an already-placed door (single/double/sliding/pocket). Move mouse to re-flip, click to place, Esc to cancel. |
| `RH` | Repair Hole: pick the two cap lines of a hole (e.g. one cut by `HH` or `HHX`) → deletes them and merges the wall stubs back into continuous lines. Use when you cut a hole but decided against placing a door/window. |

### Counts, renumbering, labels, schedule

| Command | What it does |
|---|---|
| `WC` / `WR` | Window count by width / renumber. |
| `DC` / `DR` | Door count by width / renumber. |
| `LT` / `LC` | Labels on/off / continuous vs new-batch numbering. |
| `DWT` | Draw doors & windows schedule table at a picked point. |
| `DDW` | Thicken a line drawing into a door/window frame. Pick outer closed polyline (offset inward) + interior mullion lines (offset ±½ width each side, trimmed to inner boundary). Frame width from `*cfg-win-fw*`. |
| `SET` | Pop-up settings dialog (DCL). Pick Category → Setting → enter a new value, layer/color, or independent door/window label marker type (`Hexagon`, `Circle`, `Pill`, or `Rectangle`). Falls back to command-line prompts if `AKDDoorWin.dcl` isn't on the support path. |

## Wall requirements

`HH`, `AD`, `AW`, `ACW`, `CW`, `EW`, `RH` need the wall drawn as **two parallel lines** (`LINE` or `LWPOLYLINE`) on the same layer. Click one line — the parallel partner is auto-detected only among entities on that same layer, preventing axis or reference lines on other layers from being mistaken for the wall face. Cap lines are drawn on the wall's own layer so `EW`/`RH` can find them again later. Works with walls drawn by `WW.lsp`.

## Notes

- Placed objects reuse compact block definitions: `AKD-DS-900`, `AKD-DD-1600`, `AKD-DL-2400-4`, `AKD-DP-1550`, `AKD-WF-1200-2`, `AKD-WS-1800-3`, `AKD-CW-4800-4`, and corner windows such as `AKD-XW-1200X1500-90-2-L` (arm 1, arm 2, included angle, divisions per arm, handed direction). Rotation and mirroring belong to each block instance.
- Door/window number bubbles remain separate from the reusable block because their text varies per instance. The block reference carries the `ADOOR` or `AWIN` XData used by edit, count, renumber, and schedule commands.
- Each placed door/window block contains centered text showing its width. Its separately numbered label is linked to the block instance so `CW`/`EW` can find and manipulate both.
- Placement commands remember their last width, type, divisions, and Center/FromWall/Point mode in session state.
- `CW` and `EW` accept pre-selected objects: pick the door(s) first, then run the command.
- Color/layer/dimension config lives at the top of `AKDDoorWin.lsp`. Use `SET` in-drawing, or drop an `AKDDoorWin.cfg` file next to the .lsp with `(setq *cfg-...* ...)` lines to persist overrides across updates.

## Load

Drag `AKDDoorWin.lsp` onto AutoCAD, or use `APPLOAD` and add to the Startup Suite for persistent loading.
