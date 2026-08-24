# AKDDoorWin

Plan-view door and window tool that cuts the opening through a two-parallel-line wall and places the door/window inside it in one shot. Includes resize (with wall repair) and erase (with wall repair).

## Commands

| Command | What it does |
|---|---|
| `AD` | Click a wall segment → cuts a hole and places a door in it. Options: `A`=Single leaf, `D`=Double, `S`=Sliding, `P`=sliding panels, `Width`, `Center`/`FromWall`, `Gap`. |
| `AW` | Same as `AD` but drops a window. Options: `Divisions`, `S`=toggle sliding window (adds ticks between panels), `Width`, `Center`/`FromWall`, `Gap`. |
| `ADD` | Legacy 2-click door: pick two points on the wall, no hole cutting. Same type shortcuts (`A`/`D`/`S`/`P`). |
| `AWW` | Legacy 2-click window: pick two points. `D` sets divisions, `S` toggles sliding. |
| `HH` | Cut a hole only (no door/window). Same `Center`/`FromWall`/`Width`/`Gap` options. |
| `CW` | Click any part of a placed door/window → prompts new width, resizes the opening and the door/window together. `Base` option locks a picked point in place while the opposite jamb moves. |
| `EW` | Select one or many placed doors/windows → deletes each and closes the wall back up (works with LINE walls and open or closed LWPOLYLINE walls). |
| `DC` / `DR` | Door count / door renumber. |
| `WC` / `WR` | Window count / window renumber. |
| `LT` / `LC` | Label toggle / label continuous vs new batch. |

## Wall requirements

`AD`, `AW`, `HH`, `CW`, `EW` need the wall drawn as **two parallel lines** (`LINE` or `LWPOLYLINE` segments) on the same layer. Click one line — the parallel partner is auto-detected. Cap lines are drawn on the wall's own layer so `EW` can find them again later.

## Notes

- Each placed door/window gets a centered text label (height 100) showing its width, and is grouped so `CW`/`EW` can find and manipulate it as a whole.
- `AD`/`AW` remember their last width, type, divisions, and Center/FromWall mode in session state.
- Type a number at the main prompt to set width directly (e.g. `AD` → `D` → `900` → click wall = double door, 900mm wide).
- `CW` and `EW` accept pre-selected objects: pick the door(s) first, then run the command.
- Color/layer/dimension config lives at the top of `AKDDoorWin.lsp`.

## Load

Drag `AKDDoorWin.lsp` onto AutoCAD, or use `APPLOAD` and add to the Startup Suite for persistent loading.
