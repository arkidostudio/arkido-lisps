# AKDDoorWin

Plan-view door and window tool that cuts the opening through a two-parallel-line wall and places the door/window inside it in one shot. Includes corner windows/holes, resize, erase, and hole-repair.

## Commands

### Placement (cuts hole + places object; loops until Esc/Enter)

| Command | What it does |
|---|---|
| `HH` | Cut a hole only (no door/window). Options: `Center`/`FromWall`/`Width`/`Gap`. |
| `AD` | Cuts hole and places a door. Options: `A`=Single leaf, `D`=Double, `S`=Sliding, `Panels`, plus width/placement/gap. |
| `AW` | Cuts hole and places a window. Options: `Divisions`, `S`=toggle sliding (adds mullion ticks). |
| `AC` | Cuts hole and places a curtain wall. `S`=Spacing mode (then `S`=set value), `D`=Divisions mode (then `D`=set value). |

Type a number at the main prompt to set width directly (e.g. `AD` then `800` = 800mm door width).

### 2-click placement (no hole cutting)

| Command | What it does |
|---|---|
| `ADD` | Draw a door between two picked points. Same type shortcuts. |
| `AWW` | Draw a window between two picked points. |
| `ACC` | Draw a curtain wall between two picked points. |

### Corner tools

| Command | What it does |
|---|---|
| `AXW` | Corner window: pick first wall end → corner → second wall end. `Divisions` sets glass panels per arm. `H`=run `HHX` first to cut the corner hole. |
| `HHX` | Cut a corner hole through two walls meeting at a picked corner. |

### Edit

| Command | What it does |
|---|---|
| `CW` | Click a placed door/window → prompts new width, resizes the opening + object together. `Base` locks a picked point while the opposite jamb moves. |
| `EW` | Select one or many placed doors/windows → deletes each and closes the wall back up. Works with LINE walls and open/closed LWPOLYLINE walls. |
| `RH` | Repair Hole: pick the two cap lines of a hole (e.g. one cut by `HH` or `HHX`) → deletes them and merges the wall stubs back into continuous lines. Use when you cut a hole but decided against placing a door/window. |

### Counts, renumbering, labels, schedule

| Command | What it does |
|---|---|
| `WC` / `WR` | Window count by width / renumber. |
| `DC` / `DR` | Door count by width / renumber. |
| `LT` / `LC` | Labels on/off / continuous vs new-batch numbering. |
| `DWT` | Draw doors & windows schedule table at a picked point. |

## Wall requirements

`HH`, `AD`, `AW`, `AC`, `CW`, `EW`, `RH` need the wall drawn as **two parallel lines** (`LINE` or `LWPOLYLINE`) on the same layer. Click one line — the parallel partner is auto-detected. Cap lines are drawn on the wall's own layer so `EW`/`RH` can find them again later. Works with walls drawn by `WW.lsp`.

## Notes

- Each placed door/window gets a centered text label (height 100) showing its width, grouped so `CW`/`EW` can find and manipulate it as a whole.
- Placement commands remember their last width, type, divisions, and Center/FromWall mode in session state.
- `CW` and `EW` accept pre-selected objects: pick the door(s) first, then run the command.
- Color/layer/dimension config lives at the top of `AKDDoorWin.lsp`.

## Load

Drag `AKDDoorWin.lsp` onto AutoCAD, or use `APPLOAD` and add to the Startup Suite for persistent loading.
