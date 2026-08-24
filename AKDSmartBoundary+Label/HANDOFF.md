# SmartBoundary — session handoff

## What this is
AutoCAD-for-Mac LISP that acts like `BOUNDARY` (`BO`) but bridges door/window openings automatically. Command: `SB`.

Repo: https://github.com/arkidostudio/arkido-lisps/tree/main/AKDSmartBoundary
Local dev copy: `/Users/razzan/Documents/Claude Projects/CADLisps/SmartBoundary/SmartBoundary.lsp`
User pushes as `AKDSmartBoundary/AKDSmartBoundary.lsp` inside the `arkido-lisps` repo, following the existing `AKD…/` folder convention.

Git identity for commits: `razzan2510 <razzan2510@gmail.com>`. GitHub CLI is authed as `arkidostudio` but token lacks `delete_repo` scope.

## Environment gotchas
- **AutoCAD for Mac** — no Visual LISP / ActiveX. `vl-load-com`, `vlax-*`, VLIDE all unavailable. Code is pure AutoLISP.
- `-LAYER _D` on Mac is Description, not Delete. Layers are removed via `PURGE`, not `-LAYER`.
- `t` is a reserved constant — never use as a local variable name.
- User loads via `(load "/absolute/path/SmartBoundary.lsp")` at the command line after each edit.

## How SB works
Three detectors run and their bridges are combined, drawn on `SB_TEMP` layer, then `-BOUNDARY` runs at the pick point, then temps are erased.

1. **Free endpoints** (`sb:bridges`) — pairs open-curve endpoints not touching anything else within `*sb-fuzz*` (0.5). Requires collinearity with wall tangent (`*sb-align*` = 0.85 by default).
2. **Closed-polyline notches** (`sb:closed-notches`) — for closed LWPOLYLINEs, finds pairs of vertices whose entering segments are perpendicular to the direct line between them. Handles rooms drawn as one closed polyline with door jogs.
3. **Cap pairs** (`sb:cap-bridges`) — finds pairs of short parallel LINEs (wall-thickness end-caps) facing each other across a gap. This is the workhorse for typical double-line walls with capped opening ends.

## Options exposed via SB prompt
- `G` Gap — change `*sb-gap*` (max opening width)
- `L` Layer — output layer for the boundary polyline. Pick/Type/Current.
- `W` Walls — restrict analysis to specific layers. Pick/Type/All. Auto-prompted on first run of a session if unset.

When wall layers are set, SB **freezes every other layer** (except current, `SB_TEMP`, `*sb-out-layer*`, and the wall layers themselves) during `-BOUNDARY` so door arcs/frames don't get traced. To also hide the original current layer (e.g., user working on `A-DOOR`), it temporarily swaps `CLAYER` to the output layer and restores after.

## Tunables (top of file)
| Var | Default | Purpose |
|---|---|---|
| `*sb-gap*` | 1000 | max opening width bridged |
| `*sb-gap-min*` | 500 | min opening width — rejects degenerate corner bridges |
| `*sb-fuzz*` | 0.5 | endpoints closer than this are "already connected" |
| `*sb-align*` | 0.85 | collinearity threshold for free-endpoint pairing |
| `*sb-cap-max*` | 300 | max length considered a wall cap |
| `*sb-out-layer*` | `"X-AREA BOUNDARY"` | default output layer |
| `*sb-wall-layers*` | `nil` | list of wall layers; `nil` = all |
| `*sb-keep*` | `nil` | `T` = leave temp bridges visible for debugging |

## Debug switch
`(setq *sb-debug* T)` — keeps bridges visible, numbers them with TEXT labels on SB_TEMP, dumps `#N len= a=(x,y) b=(x,y)` per bridge, skips freeze+BOUNDARY. Turn off with `nil`.

## Freeze is required — do NOT remove it
`sb:freeze-non-wall` looks fragile (swaps CLAYER, layer name has a space) but it's what makes `-BOUNDARY` prefer wall+SB_TEMP over door arcs. Without it, boundary traces around doors instead of across the bridges. If it appears to break `-BOUNDARY` ("Valid hatch boundary not found"), suspect stale frozen layers from a crashed prior run — thaw all with `(command "_.-LAYER" "_T" "*" "")` before assuming the code is broken.

## Current status / open issue
Last iteration on a real floor plan with window openings between wall segments produced **24 bridges** on a room with maybe ~6 real openings. Some real bridges detected (D1 door corners), but the cap detector also created degenerate short bridges at wall corners and bridges between adjacent window mullions.

Just-added mitigation: `*sb-gap-min*` (default 500) rejects candidate cap bridges shorter than that. **Not yet tested on the user's plan** — user was asked to reload, run with `*sb-keep* T`, and post the new bridge count + screenshot. Session ended before that data came back.

## If the min-gap fix isn't enough
Next things to try, roughly ordered:
1. **Bridge-crossing rejection**: for each candidate bridge, drop it if it intersects any wall LINE/polyline segment or any already-accepted bridge. Requires a 2D segment-intersection helper.
2. **Prefer non-overlapping bridges** by greedy sorting: sort candidates by length ascending, accept only if endpoints not already used and no crossing.
3. **Ask the user which layers hold windows vs doors vs walls** — the "yellow rectangles" in their drawing appeared on the kept-visible A-WALL, but might belong on a separate window layer that should also be frozen (or handled as its own opening type).
4. If auto keeps mis-firing on this drawing style, consider a hybrid: auto-detect draws candidate bridges in red, user clicks the good ones (or all-accept), then BOUNDARY runs. (SBM was removed earlier as pointless — this hybrid is different: auto suggests, user filters.)

## User preferences observed
- Terse, direct answers. Not a fan of over-engineered code — pushed back on SBM as "just the polyline tool" and it was removed.
- Wants the tool to Just Work; asked repeatedly to make setup easier (default output layer, auto-prompt for walls, single-key options).
- Works on AutoCAD for Mac, mm units, architectural double-line walls.

## Testing loop
1. Edit `/Users/razzan/Documents/Claude Projects/CADLisps/SmartBoundary/SmartBoundary.lsp`
2. In AutoCAD: `(load "/Users/razzan/Documents/Claude Projects/CADLisps/SmartBoundary/SmartBoundary.lsp")`
3. Run `SB`, pick point.
4. Copy `.lsp` + `README.md` to `/tmp/arkido-lisps/AKDSmartBoundary/`, commit, push.
