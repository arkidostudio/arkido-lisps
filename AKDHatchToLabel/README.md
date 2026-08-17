# AKDHatchToLabel

Turns a room hatch into a **formatted room label** — an MTEXT with the room name on line 1 and the area (SQM / SQFT / etc.) on the following lines, styled per your settings file. Duplicate rooms auto-number (`BEDROOM 1`, `BEDROOM 2`…).

**Command:** `HATX`
**Platform:** AutoCAD for Mac (also runs on Windows AutoCAD).

Pairs with [AKDVent](../AKDVent) — the labels this tool produces are the exact format `AKDVent` expects to parse when generating the ventilation schedule.

## Install

**Quickest:** drag `AKDHatchToLabel.lsp` from Finder / Explorer onto the AutoCAD drawing window — it loads for the current session.

**Persistent (recommended):**
1. Type `APPLOAD` and press Enter.
2. Load `AKDHatchToLabel.lsp`.
3. Add it to the **Startup Suite** so it auto-loads every session.

Then type `HATX` to launch.

> **Important:** `AKDHatchToLabel_Settings.txt` must sit **in the same folder** as the `.lsp` file. The tool looks for it there on every run.

## Workflow

1. Type `HATX`.
2. Mode: `SelectHatch` (pick hatch first) or `RoomName` (pick room first).
3. Pick one or more HATCH objects (or pre-select them).
4. For each hatch, pick the room from the keyword list (`BEDROOM/TOILET/LIVING/…`) built from the settings file.
5. Places MTEXT at the hatch:

   ```
   BEDROOM 1
   12.50 SQM
   134.55 SQFT
   ```

   Area is read from the hatch's true area, converted per the unit settings.

Duplicate rooms auto-number within one run.

### Edit an existing label

If your pre-selection includes an existing room MTEXT/TEXT, HATX prompts for a new room and rewrites only the first line — keeping the area lines untouched.

## Settings — `AKDHatchToLabel_Settings.txt`

| Key | What it controls |
|---|---|
| `Layer:` | Target text layer (default `X-TEXT`) |
| `RoomColor:` / `AreaColor:` | Color index (`256` = ByLayer, `2` = yellow, etc.) |
| `Rooms:` block | `SHORTCUT=Full Name` map. Each entry becomes a keyword at the room prompt. |
| `Units1:` / `Units2:` | Area lines. Format `UNIT\|DECIMALS`. Units: `SQM`, `SQFT`, `HA`, `ACRE`, or `0` to hide the line. |
| `Conv1:` / `Conv2:` | Optional custom multipliers (blank = built-ins). |
| `TextHeightRoom` / `TextHeightArea` / `LineSpacing` | MTEXT sizing (drawing units). |
| `Font`, `Bold:Yes/No`, `Uppercase:Yes/No` | Text style. |

Edit and save — the LISP re-reads the file on every run. No reload needed.

## Notes

- Only **HATCH** entities produce new labels. To label a room, you must have a hatch on it first.
- All formatting is baked into the MTEXT string as inline codes (font, height, color) — entity properties still track layer/color per your settings.
- The command remains `HATX` for muscle-memory continuity from the original tool name.
