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

### Settings file — where it lives

`AKDHatchToLabel_Settings.txt` holds your room list, units, colors, and text style. The LISP looks for it in this order on every run:

1. A previously remembered path (env var `AKDHATCH_SETTINGS`).
2. Next to the `.lsp` file (recommended — keep them together).
3. Anywhere on AutoCAD's Support File Search Paths.
4. **First-run fallback:** if none of the above worked, AutoCAD pops a file picker asking you to locate `AKDHatchToLabel_Settings.txt`. Pick it once — the path is saved and reused automatically after that.

If no settings file is found at all, the tool falls back to hardcoded defaults (only `BED`, `BAL`, `TOI`) and prints a warning. If you see only those three rooms, the settings file isn't being read.

### Editing the settings file

Open `AKDHatchToLabel_Settings.txt` in any text editor. Save. The LISP re-reads it on every `HATX` run — **no reload needed**.

To reset the remembered picker path (e.g. after moving the file), run this once at the AutoCAD command line:

```
(setenv "AKDHATCH_SETTINGS" "")
```

Next `HATX` run will re-detect or re-prompt.

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

**Font:** the `Font:` setting is applied to both the room name **and** the area lines (via inline MTEXT overrides), so labels look consistent regardless of the drawing's current text style. Only the room line is bolded; area lines use the same font in regular weight.

## Notes

- Only **HATCH** entities produce new labels. To label a room, you must have a hatch on it first.
- All formatting is baked into the MTEXT string as inline codes (font, height, color) — entity properties still track layer/color per your settings.
