# AKDLayerTools — Session Handoff

## What was added this session

Two new commands in [AKDLayerTools.lsp](AKDLayerTools.lsp):

- **EREX** — Export all layers + real filter groups. Prompts TXT or JSON, writes via `getfiled`.
- **ERIM** — Import layers from a TXT or JSON produced by EREX. Dispatches on file extension.

## Mac AutoCAD constraints (important)

- **No VLA / ActiveX.** `vl-load-com` errors on Mac. All code is pure LISP: `tblnext`, `entget`, `entmake`, `entmod`, `dictsearch`.
- **`command-s` hangs on multi-prompt subcommand chains** (e.g. `-LAYER _M name _C 1 name _L ...`). Import was rewritten to use `entmake`/`entmod` directly on the LAYER record — no prompts.
- **Layer Description is not in the standard LAYER DXF record** and unreachable without VLA. Dropped from export/import.

## Layer filter groups on Mac

Stored in the LAYER table's extension dictionary under `ACAD_LAYERFILTERS`. Each filter is an **XRECORD**:
- `1` (1st): filter name (e.g. `"01 ARCHITECTURE"`)
- `1` (2nd): layer-name **wildcard** (e.g. `"*A-*"`)
- More `1` codes: color/linetype/plot/lw patterns (unused here)
- `70`: flag

`akd:read-filters` extracts `(name wildcard)` pairs; EREX groups layers via `(wcmatch layer-name wildcard)`. Layers matching no filter → `(ungrouped)`. Layers matching multiple filters appear in each.

Fallback if no filters exist: prefix-before-`-` grouping.

Diagnostic command **ERGDBG** dumps the xdict + filter dict + first filter's full DXF — leave it in for future debugging.

## Formats

### TXT (pipe-delimited)
```
name|color|linetype|lineweight|on|frozen|locked|plottable|group
```
`#` lines are comments (group headers). Bools are `true`/`false`. Lineweight is `"0.25mm"`, `"Default"`, `"ByLayer"`, or `"ByBlock"`.

### JSON
```json
{
  "groups": { "01 ARCHITECTURE": ["A-WALL", "A-DOOR", ...], ... },
  "layers": [
    { "name": "A-WALL", "color": "5", "linetype": "Continuous",
      "lineweight": "0.25mm", "on": true, "frozen": false,
      "locked": false, "plottable": true },
    ...
  ]
}
```
JSON import uses a small key-value scanner tuned to this exact shape — not a general JSON parser.

## Import mechanics

`akd:apply-row` builds a LAYER DXF record and calls:
- `entmake` if the layer doesn't exist
- `entmod` (via `akd:dxf-put` helper) if it does

DXF codes used:
- `62` color; **negative = off** (encoded from the `on` field)
- `70` flags; bit 1 = frozen, bit 4 = locked
- `6` linetype (falls back to `Continuous` if not loaded)
- `370` lineweight (hundredths mm; -3 = Default)
- `290` plottable (1/0)

Groups are **not** recreated on import — user re-creates in Layer Manager if needed. AutoCAD's `LAYER_FILTER` / XRECORD write path via `entmake` is fiddly and version-dependent; deliberately skipped.

## Command list (ERSC output)

`ER1 ERS ERT ERD ERDD ERF ERA ERAF ERL ERU EREX ERIM ERSC` (plus the undocumented `ERGDBG` diagnostic).

## Known gaps / possible next steps

- Property/rule filters that use non-name patterns (color/linetype/etc.) aren't distinguished — only the layer-name wildcard is honored. Fine for name-based filters, wrong for anything else.
- No round-trip of group filters through import.
- Description field is lost.
- JSON parser handles only the shape EREX emits; hand-edited JSON with different whitespace/escaping may break.
