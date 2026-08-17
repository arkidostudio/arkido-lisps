# AKDPickArea

Scaled area takeoff tool for AutoCAD. Picks closed polylines, totals their areas with unit and scale conversion, and drops a labeled MTEXT tag ready for a window / door / room schedule.

**Command:** `AA`
**Platform:** AutoCAD for Mac (also runs on Windows AutoCAD).

## Install

**Quickest:** drag `AKDPickArea.lsp` from Finder / Explorer onto the AutoCAD drawing window — it loads for the current session.

**Persistent (recommended):**
1. Type `APPLOAD` and press Enter.
2. Load `AKDPickArea.lsp`.
3. Add it to the **Startup Suite** so it auto-loads every session.

Then type `AA` to launch.

## Menu

Type the letter and press Enter:

| Key | Action |
|---|---|
| `P` | **Pick** — click closed polylines one after another. ENTER to stop. Duplicates blocked. |
| `S` | **Settings** — set unit (`MM2` / `M2` / `FT2`) and scale mode. Saved between sessions. |
| `B` | **Label** — prefix the final text: `D` = Door #, `W` = Window #, `C` = custom. |
| `U` | **Undo** — remove last pick. |
| `A` | **AddArea** — read a number out of existing TEXT/MTEXT and add it to the total. |
| `C` | **Clipboard** — print the total for manual Cmd+C, then exit without placing text. |
| `D` | **Done** — exit and place an MTEXT at a picked bottom-right point. |

## Scale factor

Applied as **`area × factor²`**.

| Mode | When to use |
|---|---|
| **None** | Polylines drawn at real size (model space, 1:1 mm). Normal AutoCAD workflow. |
| **Standard** (`1:x`) | Polylines traced over a PDF / image / xref inserted at print scale, so 1 drawing unit = 1 paper mm. Enter the denominator (e.g. `50` for 1:50). |
| **Custom** | Type the multiplier directly. Escape hatch for unusual scales. |

**Sanity check:** pick one polyline you know the real size of. If the total matches → scale is right. If it's off by 2500× you needed 1:50, off by 10000× you needed 1:100.

## Typical flow

1. `S` — set unit + scale once (usually `M2` + `None` for 1:1 mm drawings).
2. `B` — set label (e.g. `W` → `3` gives `W3`).
3. `P` — click the polylines that make up window 3.
4. `D` — click bottom-right insertion point.

Result on the drawing:

```
W3
1.44 SQM
```

## Notes

- Only closed **LWPOLYLINE** and **POLYLINE** entities count.
- Settings persist via AutoCAD env vars: `AREAPICK_UNIT`, `AREAPICK_MODE`, `AREAPICK_VAL`.
- The final MTEXT uses the current `TEXTSIZE` and `TEXTSTYLE`, right-justified to the picked point.
