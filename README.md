# Arkido LISPs

AutoCAD LISP tools built for the Arkido (AKD) architecture workflow. Written and tested on **AutoCAD for Mac**; most should also run on Windows AutoCAD without changes.

Each tool lives in its own folder with its own README (install + usage).

## Tools

| Folder | Command | What it does |
|---|---|---|
| [AKDPickArea](./AKDPickArea) | `AA` | Scaled area takeoff for windows, doors, and rooms. Picks closed polylines, totals with unit/scale conversion, drops a labeled MTEXT (e.g. `W3 / 1.44 SQM`). |

More tools will be added here over time.

## Loading a LISP in AutoCAD

1. Open AutoCAD.
2. Type `APPLOAD` and press Enter.
3. Browse to the `.lsp` file inside the tool's folder and load it.
4. Type the command shown in the table above.

To auto-load on startup, add the `.lsp` to the **Startup Suite** in the same `APPLOAD` dialog.

## License

MIT — see [LICENSE](./LICENSE) if included, otherwise use freely with attribution.
