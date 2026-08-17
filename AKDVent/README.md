# AKDVent

Auto-generates a **Ventilation Schedule** table in your AutoCAD drawing by picking room MTEXT tags. For each room it reads the name and area, then produces a formatted table row with the **required opening area** auto-calculated at **10% of room area** (Malaysian UBBL rule of thumb).

**Command:** `VE`
**Platform:** AutoCAD for Mac (also runs on Windows AutoCAD).

## Install

**Quickest:** drag `AKDVent.lsp` from Finder / Explorer onto the AutoCAD drawing window — it loads for the current session.

**Persistent (recommended):**
1. Type `APPLOAD` and press Enter.
2. Load `AKDVent.lsp`.
3. Add it to the **Startup Suite** so it auto-loads every session.

Then type `VE` to launch.

## Expected MTEXT format

The parser wants MTEXTs like:

```
MASTER BEDROOM
12.50 SQM
134.55 SQFT
```

- First line → room name.
- First number found → room area in SQM.
- Second number (SQFT) → read but not used in output.

Single-line MTEXT also works — the parser strips the numbers and unit words to recover the room name.

## Workflow

1. Type `VE`. The menu prompt loops:

   | Key | Action |
   |---|---|
   | `SetFloor` | Choose `0/1/2/3/…/T/M` (T = Terrace, M = type custom name). Inserts a bold floor-name divider row, then drops into pick mode. |
   | `PickArea` | Pick MTEXTs without inserting a floor divider. |
   | `Finish` | Exit picking. |

2. Click room MTEXTs. Press Enter to return to the menu.
3. `Finish`, then pick an insertion point → the whole table is drawn.

## Output

Title `VENTILATION SCHEDULE`, then columns:

| Room Name/Number | Room Area (SQM) | Opening Number | Required Opening Area (SQM) | Designed Opening Area (SQM) |
|---|---|---|---|---|

- **Required Opening Area** = `Room SQM × 0.10` — filled automatically.
- **Opening Number** = `X` placeholder — fill in later.
- **Designed Opening Area** = `0.00 SQM` placeholder — fill in later.
- Floor names become full-width divider rows (`GROUND FLOOR`, `1ST FLOOR`, `TERRACE`, etc.).

## Styling

Hard-coded in `gt:get-config` at the top of the file:

| Setting | Value |
|---|---|
| Outer frame color | Green (3) |
| Horizontal lines | Red (1) |
| Vertical lines | Yellow (2) |
| Header text | White (7) on `TB_BOLD` style if it exists, else current style |
| Data text | Yellow (2) |
| Column widths | 3200 / 2600 / 2200 / 3700 / 3700 (drawing units — mm at 1:1) |
| Row heights | Title 680, Header 750, Data 550 |

Edit those constants if you want a different look.

## Notes / limitations

- Only **MTEXT** is supported. `TEXT` objects are rejected.
- The **10%** opening ratio is hard-coded in `gt:parse-room` (`req = sqm × 0.10`). Change that constant if your jurisdiction requires a different ratio.
- No in-tool undo — if you mis-pick, `Finish`, `U` in AutoCAD to erase the drawn table, then re-run.
- The tool draws with `entmake` (raw LINE + TEXT entities), so the table isn't a native AutoCAD TABLE object.
