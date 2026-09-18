# AKDWall — session handoff

## What this folder is
Double-line wall tool for AutoCAD Mac. Draws walls on layer `A-WALL`, tags each segment with an `AWALL` xdata POINT on `A-WALL-DATA` (hidden layer) storing centerline p1/p2 + thickness + height + baseElev — consumed by `AKDProjections` for elevation/section generation.

Files:
- `WW.lsp` — commands `WW` (interactive), `QW` (convert existing).
- `AKDWallTool.lsp` — `TW` (two-point wall), `FW`/`FIXWALLS` (cap + merge).

## Commands
### WW — interactive walls
- Prompts `[A1=inside A2=outside A3=center R=rectangle]` at start point.
- Align semantics (assumes CW click for "inside" to hit interior):
  - `A1` inside → thickness on right of travel
  - `A2` outside → thickness on left
  - `A3` center → thk/2 both sides
- `R` → rectangle mode: pick two corners, draws closed 4-segment wall.
- Globals: `*WW_Thickness*` (150), `*WW_Align*` (1), `*WW_Height*` (2700), `*WW_BaseElev*` (0).
- Corner cleanup inline; end caps drawn on open runs; `tw:cleanup-box` called for junctions.

### QW — convert lines/polylines to walls
- Accepts pre-selection (`ssget "_I"`) or interactive.
- Handles `LINE` and `LWPOLYLINE` (bulges ignored — treated as straight segments). `POLYLINE` (heavy) not supported.
- Pre-select prompt options: `[Align/Thickness]` — `Align` sub-prompts `[Inside/Outside/Center]` (shows in cmd bar + right-click menu + dyn input).
- Corner filleting: 2-way corners get pairwise face-line intersection (trimmed exactly). T-junctions (≥3 walls at endpoint) skip caps and defer to `tw:cleanup-box`.
- Deletes source entities after conversion.

### FW / FIXWALLS
Bbox-select an area, then two passes:
1. Cap open wall pairs (existing logic).
2. Merge colinear runs — `fw:merge-lines` collapses adjacent A-WALL LINEs; `fw:merge-markers` collapses AWALL POINTs bucketed by (thk, h, base). Gap tolerance `*fw:merge-gap*` = 5 (won't heal doorways unless raised).

## Recent changes this session
1. WW default align flipped to `1` (inside); relabeled options inside/outside/center.
2. Added `A1/A2/A3` runtime keywords at WW start prompt.
3. WW now writes lines to layer `A-WALL` (auto-created, color 7) and sets it current.
4. Added FW colinear-merge pass (both face lines and AWALL markers).
5. Added `WW>R` rectangle mode.
6. Added `QW` command with polyline support, corner intersection filleting, and Align/Thickness sub-prompts.

## Known gotchas
- **Align convention assumes CW click** for "inside" to mean room-interior. CCW clicks put walls on the wrong side. No auto-winding detection.
- **AWALL marker stores click endpoints, not centerline.** For `A2` (outside) and `A3` (center), the stored p1/p2 is the click line — offset from actual wall centerline. AKDProjections consumes this as-is.
- **Polyline arcs** silently treated as straight segments.
- **tw:cleanup-box only trims intersecting lines** — non-intersecting adjacent stubs won't fillet. QW handles 2-way corners inline for that reason.
- **AutoCAD Mac**: use `command-s` not `command`; avoid error-handler triggers on Escape (see `feedback-autolisp-mac`).

## Uncommitted
`git status` shows this folder is untracked (`AKDWall/`). Ready for `git add AKDWall/ && git commit`.
