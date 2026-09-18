# ASEC / ASECA — session handoff (2026-09-14)

## Files
- 2026-09-15: MERGED into `AS.lsp` (command `AS`) + `AS_GRAPHICS.txt`. Runtime PENDING.
  Old `ASEC.lsp` / `ASECauto.lsp` moved to `archive/` (rollback only, never loaded).
  Removed: c:ASEC, asec:main, c:ASECTEST, ASECA dependency check. Kept: SECTSET, manual collectors (Edit/Add, Guided).
- Graphics: asec:load-graphics at AS start (findfile, else AS.lsp folder, else asec:gfx-defaults); asec:gfx key lookup.
  Layers ELV-1..5, PROJ_WALL_LAYER (ELV-3, assumption), ground ACI 5, hatch via command-s -HATCH on output rects -> X-HATCH.
- Construction: asec:make-construction -> AS_CONSTRUCTION (ACI 4), tracked in asec:temp; only review labels are temp entities.
  asec:construction-report after AS (debug counts; warning if tracked survive).

- 2026-09-15 ASA pass (runtime PENDING): c:ASA four-way sections (asa: functions at end of AS.lsp).
  aseca:analyse-floor split -> aseca:discover-floor (no prompts) + review loop (takes pre-discovered data) + aseca:auto-record.
  View boundary (clip . T) = hard source limit: asec:clip-projected-wall ('uclip ends draw no edge), asec:clamp-opening-u.
  Cut window glass now sill+F -> head-F. South "flip": runtime trace confirmed output correct (viewer-facing, East left looking South). Not a bug; formula unchanged. Debug ORIENTATION TRACE kept.

- 2026-09-15 S-SLAB + A-RAILING pass (runtime PENDING): aseca:discover-extras / aseca:add-extras (floor keys lwalls plwalls scur stop).
  asec:slab-intervals rule: even edges -> pairs; single edge outside auto extent -> extend; else keep extent + unresolved note.
  Hidden S-SLAB of floor i drives slab at datum(i+1) (next floor if it has no current edges) or the roof. Low walls: occluder types LOWCUT/LOWPROJ.

- 2026-09-15 AS > Section pass (runtime PENDING): "[Section]" keyword on the AS line prompt -> asec:pick-section-points,
  view side with asec:ghost-section-marker preview; then the normal depth prompt.
  Marker drawn by asec:draw-section-marker inside asec:generate's UNDO group (asec:pending-marker); cancel before
  generation leaves none. Arrow = triangle minus disc via asec:marker-arrow (chains + clockwise closing arc, temp boundary).
  Keys SECTION_LINE_LAYER / SECTION_MARKER_* / SECTION_LINE_DASHED_* in AS_GRAPHICS.txt.

- 2026-09-15 marker refinements + front-most lines (runtime PENDING): marker text 180. asec:section-id -> (id renames):
  single verified X pair (two plain X texts on marker circles) renamed X1 inside generation UNDO, new X2; else highest+1.
  asec:flush-lines: buffered projected lines nearest-V first, clipped by nearer same-layer lines and cut rect edges
  (asec:cut-rect -> asec:gen-cut, depth 0), then the unchanged same-depth merge.

- 2026-09-15 row layout + marker block + depth pass (runtime PENDING):
  asa:layout: NORTH SOUTH EAST WEST in one row, ins p = NORTH, next left = prev right + ASA_VIEW_GAP, same Y.
  Marker = one block AKD-SECTION-<ID>[-n], base = GF reference point, entities in world coords, SOLID fills (ear-clipped)
  instead of HATCH; asec:section-id reads IDs from AKD-SECTION-* INSERT definitions (+ legacy loose markers).
  Occlusion depth: asec:depth-at (end-on -> nearest V, else linear V1/V2 at U); nearer-occluder-p takes the object's
  U range; projected wall/low-wall edges and hosted openings use depth at their U instead of Vmid.

- 2026-09-17 projected surfaces + projected slab edges (runtime PENDING):
  Occluders use NEAR-FACE depth (asec:near-depth-at = depth-at - thk/2*|dU|/L). Projected wall / low wall end edges use
  the silhouette test asec:visible-edge-z (kept only where one side is open; covers at depth <= edge + tol incl. self).
  S-SLAB segments inside the view boundary -> 'pslabs (aseca:projected-slab-edges); drawn as top + underside lines at
  the current/top level on PROJ_SLAB_LAYER (ELV-3) via asec:visible-u-varying; PSLAB bands and cut-slab CSLAB bands
  occlude; cut slab outlines are cut references.

## Hard constraints
AutoCAD for Mac. Standard AutoLISP only: no vl-/vla-/vlax, ActiveX, COM, XData, dictionaries.
Source plan is read-only. Output = LINE/LWPOLYLINE on ASEC-* layers. `command-s` only (UNDO group).
Guided philosophy: user identifies meaning, code does geometry. ASECA must mark ambiguity UNRESOLVED, never guess.
Distinguish static checks from AutoCAD runtime tests; the user runs all runtime tests.

## Milestone status (runtime confirmed by user unless noted)
- MS1 geometry: section line, view side, ref-point floor translation, wall pairs, slabs, ground line, insertion — done.
- MS2 doors/windows: block reading, cut hosting (plan strip), wall voids, projected openings, cut door panel — done.
- MS3 projected walls: finite ends (closing lines), host openings, depth order, orientation normalisation — done.
- Post-MS3 (this session, user confirmed "works now"):
  - Projection/view boundary shared by ASEC and ASECA.
  - ASECA automatic analysis with review/edit/guided fallback.
  - Occlusion: edges and openings behind cut walls / nearer projected walls on same floor are not drawn.
  - Reveal rule, hidden-layer filter, footprint U span for projected walls.

## Key conventions
- U = (P - floor section start) . dir, V = (P - start) . viewdir. V > 0 on picked view side, larger = farther.
- Orientation: U aligned with screen-right R = (Ny, -Nx); master line start/end swapped in memory if needed.
- Vertical: slab datum-slabT..datum; wall datum..next datum-slabT; ground = GF slab underside.
- Insertion point = leftmost cut wall face on ground line.
- Cut opening void = host cut wall uMin..uMax. Projected opening = jamb U span.
- Projected wall draws only its end edges (vertical LINEs), U span = full footprint (both faces, both ends).
- Items drawn farther -> nearer; hosted openings with their wall.

## Shared engine entry points (ASEC.lsp)
asec:get-master-section, asec:get-view-direction, asec:normalize-section-orientation,
asec:get-projection-boundary / asec:make-view-boundary / asec:clip-segment-to-view-boundary / asec:classify-in-view,
asec:floor-section, asec:make-floor-ctx, asec:make-floor-record, asec:add-floor (guided), asec:generate,
asec:collect-walls / collect-openings / collect-projected-walls (guided collectors),
asec:get-block-data / get-block-opening-span, asec:make-opening, asec:make-projected-wall,
asec:projected-wall-auto-extent, asec:find-projected-wall-closing-line, asec:projected-host-candidates,
asec:draw-section, asec:occluders-at, asec:span-hidden-p, asec:reveal-edge-p, asec:layer-visible-p.

## ASECA pipeline (ASECauto.lsp, aseca:analyse-floor)
1. wall-layer LINEs + door/window INSERTs via ssget "_X" (hidden layers skipped).
2. cut crossings (direct + opening-gap), merged, paired 1-2/3-4; odd or out-of-range -> UNRESOLVED.
3. cut openings: exactly one cut-wall strip host AND section crosses jambs.
4. projected faces: in view boundary, not cut faces, chained (small or opening-explained gaps), length >= 300.
   Pair = mutual unique partner; T-junction (one face vs collinear disjoint faces) pairs each, clipped.
   Ends: closing lines (wall layers only during ASECA), then extended over an opening block at an end.
5. projected openings: one host -> hosted; none -> unhosted (kept); several -> UNRESOLVED.
6. Review labels C/CD/CW/P/PD/PW/?, [Generate/Edit/Guided/Cancel]; Edit = Remove/Add(guided collectors)/Ignore.

## Tunable globals
*asec-debug* *aseca-debug* (nil), *asec-wall-extension-limit* 3000, *asec-wall-host-tolerance* 10,
*asec-parallel-tolerance* 0.02, *asec-proj-wall-end-search* 300, *asec-proj-wall-end-tolerance* 10,
*asec-reveal-tolerance* 150, *asec-closing-layers* (nil guided), *asec-last-projection-depth*,
*aseca-wall-layers* ("A-WALL") *aseca-door-layers* ("A-DOOR") *aseca-window-layers* ("A-WINDOW"),
*aseca-min/max-wall-thickness* 50/600, *aseca-collinear-tolerance* 5, *aseca-small-gap-tolerance* 50,
*aseca-min-face-length* 300, *asec-wall-warning-thickness* 500.

## Occlusion engine (2026-09-14, runtime confirmed by user)
- Memory-only opaque pieces: asec:build-floor-occluders (cut walls V=0, projected V=Vmid) via asec:wall-pieces minus asec:wall-voids.
- asec:visible-u-intervals a b z v occ -> visible U intervals; interval helpers merge/subtract-u-interval(s).
- Tolerances *asec-occlusion-u/z/depth-tolerance* 1/1/10.
- Test: `(asec:occlusion-selftest)` pure math; `ASECTEST` draws a plan test line's visible parts on ASEC-TEST after ASEC/ASECA.
- Migration (2026-09-15, runtime PENDING): projected wall edges + projected door/window graphics clipped via
  asec:vis-poly / asec:segment-visible (H/V segments; diagonals drawn whole). asec:visible-z-intervals added;
  U/Z share asec:merge/subtract-intervals. Owner exclusion ign = (("PROJECTED" . k)). Boundary coordinate sampled
  either side (edge on jamb shows through void). Cut pieces widened by U tol for vertical tests.
  Legacy asec:occluders-at path removed (engine confirmed).
- Conventions pass (2026-09-15, runtime PENDING):
  - *asec-occlusion-policy* "CLOSED": floor occluders = whole wall envelopes (doors/windows block). "OPENING-AWARE" kept in code.
  - asec:continued-edge-p: projected end edge dropped when a collinear projected wall continues past it within *aseca-small-gap-tolerance* (now defaulted in ASEC.lsp too).
  - Clipped H/V lines go to asec:gen-lines; asec:flush-lines merges same layer/orientation/coord/V (depth tol) overlapping or touching intervals, then entmakes. Unclipped closed shapes stay LWPOLYLINE.

## Known limits / open items
- Closing-line search uses ssget "_C": wall ends must be visible on screen.
- Occlusion is whole-or-nothing per U interval; holes in nearer walls ignored; same floor only.
- Guided ASEC does not extend walls over openings (manual extent there).
- Guided wall-end search accepts any visible layer unless *asec-closing-layers* is set.
- ASECA: LINE wall faces only; projected doors default SINGLE; no Resolve step (use Add + Ignore).
- Esc during ASECA leaves *asec-closing-layers* = wall layers for the session.
- Temporary debug print blocks still exist (off by default).
- Not yet run: guided-vs-ASECA record comparison, reversed line/view, upper floors, diagonal sections for ASECA.
- Future (not started): hatching, dimensions, levels, titles, beams, parapets, roofs, elevations, settings dialog.
