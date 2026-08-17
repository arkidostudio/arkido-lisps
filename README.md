# Arkido LISPs

AutoCAD LISP tools developed for optimizing Architectural and Drafting workflows.

Each tool lives in its own folder with its own README (install + usage).

## Tools

| Folder | Command | What it does |
|---|---|---|
| [AKDPickArea](./AKDPickArea) | `AA` | Scaled area takeoff for windows, doors, and rooms. Picks closed polylines, totals with unit/scale conversion, drops a labeled MTEXT (e.g. `W3 / 1.44 SQM`). |
| [AKDVent](./AKDVent) | `VE` | Auto-generates a Ventilation Schedule table from room MTEXTs. Calculates required opening area at 10% of room area (Malaysian UBBL). |

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
