# AKD Roof V1 — RoofTool.lsp

## Loading
1. AutoCAD for Mac: `APPLOAD` and pick `RoofTool.lsp`. The command line shows
   `AKD Roof V1 loaded. Command: RF   (development probe: RFMLTEST)`.
   `RFMLTEST` is temporary and is removed once the MLEADER question is settled.
2. Pure AutoLISP: no ActiveX/VLA/VLAX/COM, no `vl-load-com`.

## Command
`RF`

## Workflow (one RF run)
```
Select closed roof boundary polyline or [Settings]:     S = Settings (only here)
  Footprint: 6 vertices, concave (1 reflex corner(s)).
Roof type [Hip/Gable/Flat] <Hip>:          Gable only for rectangles
  Hip: solved topology preview "6 faces, 2 ridge(s), 5 hip(s), 1 valley(s)" -> accept
Framing mode [Line/Offset] <Offset>:       once per run: Line = centrelines, Offset = actual width
Add rafters? / Rafter spacing <600>:       preview, S = Spacing -> accept
Add hip rafters?                           only if HIP edges exist
Add valley rafters?                        only if VALLEY edges exist
Add ridge beam?                            only if a non-zero RIDGE exists (0..N beams)
Add battens? / Batten spacing <600>:       preview, S = Spacing -> accept
Add fascia?                                perimeter -> accept
IF Callouts = On, for each member type actually created:
  Place RAFTER callout or [Skip] <Skip>:   ... HIP RAFTER, VALLEY RAFTER, RIDGE BEAM, BATTEN, FASCIA
Match callouts? [Yes/No] <Yes>:            only if >= 2 callouts were placed
  Select callouts to match: / Text alignment [Left/Right] <Left>: / Equal spacing? [Yes/No] <Yes>:
  Specify callout match position:
  Match another callout group? [Yes/No] <No>:
RF complete.
```
**S key:** at selection it means Settings; inside a rafter/batten preview it means Spacing; at a callout prompt it means Skip.

**Esc:** Esc anywhere clears ghosts and highlights, closes the Undo group and restores `CMDECHO`, `CLAYER`, `OSMODE` and `DIMTXT`.
One `U` removes the entire RF result.

## Footprints
- **Accepted:** one closed LWPOLYLINE with straight segments, flat in WCS XY (any elevation), and any simple polygon: rectangle,
  square, L, T, U, stepped, irregular convex or irregular concave.
- **Normalisation:** a duplicated closing vertex and redundant collinear vertices are removed, winding is made CCW,
  and the source entity is never modified.
- **Rejected, with a message:**
  - LINE, SPLINE, ARC, CIRCLE and other non-LWPOLYLINE objects;
  - open polylines;
  - arc bulges ("Curved roof boundaries are not supported yet. Use a straight-segment closed polyline.");
  - duplicate consecutive vertices or zero-length edges;
  - self-intersecting boundaries (e.g. a bow-tie);
  - near-zero area;
  - non-WCS extrusion.
- **Corner classification:** each vertex is classified convex or reflex (`REFLEX` flags), and the count is reported on selection.
- **Gable:** rectangles only. For other shapes RF reports "Irregular Gable roofs are not supported yet. Use Hip or Flat."
- **Flat:** any valid polygon becomes one flat face, with framing spanning across its longest edge.

## Equal-pitch HIP topology (straight skeleton)
`akd:skl-topology` solves every Hip roof, and a rectangle is just one case of it. There is no rectangle, L, T or U special-casing.
- **Wavefront:** every eave line moves inward at unit speed. Wavefront polygons hold vertices moving on the bisector `v·nL = v·nR = 1`.
- **Events:** the earliest event over the whole wavefront is processed, and all vertices advance to that time.
  - *EDGE event:* an edge shrinks to zero and its vertices merge.
  - *SPLIT event:* a reflex vertex reaches a non-adjacent advancing edge and the wavefront splits. This is what produces valleys and separate ridge branches.
  - *Degenerate states, resolved immediately:* coincident neighbours merge. Antiparallel neighbours (parallel eaves meeting) trace a level **RIDGE** to the nearer neighbour.
- **Graph:** vertex traces become a node/edge graph, and collinear pieces with the same face pair are merged. One face is walked per eave.
- **Classification:**
  - *RIDGE:* the two eaves are parallel, so the edge is level.
  - *HIP or VALLEY:* otherwise, probing just inside the face: if its own eave height is lower than the neighbour's, the edge is a HIP (convex); if higher, a VALLEY.
- **Checks (any failure rejects the whole roof; nothing partial is created):**
  - every face positive and CCW;
  - face areas sum to the footprint area;
  - every skeleton edge typed;
  - every node inside or on the footprint;
  - the event count stays within its limit.

  The message is "Unable to solve this roof footprint (reason)". Reasons include degenerate skeleton event, ambiguous or open roof face, faces not covering the footprint, node outside footprint, and unsupported simultaneous event.
- **Faces:** general simple polygons (any vertex count, possibly non-convex) with `EAVE` index, `ALONG`, and `DOWN` = −inward eave normal.
- **Shared edges:** one record per RIDGE / HIP / VALLEY with both faces. HIP and VALLEY `P0` is the higher end.
- **Preview:** footprint, ridges and hips in the roof colour, valleys in `GHOST_VALLEY`.
  The command line reports "N faces, r ridge(s), h hip(s), v valley(s)".
- **Roof lines:** every non-zero ridge, hip and valley is a LINE on `A-ROOF`.

## Framing on general faces
- **Rafter module:** requested spacing is the maximum. The governing run is the **longest** non-zero ridge, or else the longest hip/valley plan run.
  `bays = ceil(run/requested)`, `actual = run/bays`.
- **Shared stations:**
  - *RIDGE:* `round(len/actual)` even bays, both ends included.
  - *HIP:* top (COMMON) plus `k·actual` along the eave.
  - *VALLEY:* same rule from its top end.
- **Rafters per face:** every station of the face's shared edges defines a line in DOWN. It is clipped to the face with the general
  clipper, and the piece ending at the station is kept: top end for ridge/hip stations, bottom end for valley stations.
  Other ends are snapped to the face's shared station points. Lines closer than 0.35 × module merge with priority RIDGE > HIP > VALLEY.
  The kind is COMMON when a ridge or hip-top rafter reaches the eave, JACK otherwise, and valley jacks end on the valley centreline.
- **Battens:** one roof-level datum `TOPH` (highest roof point, as plan distance from its eave). Rows sit at `TOPH − k·spacing`
  on every face, so course k is the same height everywhere and meets at the same hip points.
- **Course joining:** same course, faces sharing a HIP, and the point on that hip. **RIDGE and VALLEY never join.** One course can
  give 0..N LWPOLYLINE chains, closed only when the chain truly returns.
- **Structural members:** one hip rafter per HIP edge, one valley rafter per VALLEY edge (`A-ROOF-VALLEY-RAFTER`, 50×150), and one
  ridge beam per non-zero RIDGE edge.
- **Fascia:** the complete footprint perimeter, including concave corners.
- **Callouts:** `50x150 VALLEY RAFTER` is added (one callout per member type, target nearest the datum).

## Callout matching: equal spacing
After `Text alignment [Left/Right]`, RF asks `Equal spacing? [Yes/No] <Yes>`.
- **Yes:**
  - the group is sorted by current text Y (top to bottom, drawing order kept);
  - rows keep the group's top and bottom Y with an equal pitch;
  - if the pitch is below `CALLOUT_MIN_ROW_FACTOR` (1.5) × text height, the group expands about its middle;
  - each leader is re-solved to hit its row exactly, and a callout that can't is rejected ("Unable to create a clean matched group at this position.");
  - the cursor still sets the common text edge only.
- **No:** unchanged. Each callout stays near its previous Y (shift ≤ `CALLOUT_MATCH_MAX_SHIFT`).

## Batten courses
- **Course number:** each batten row carries a course index `k`, the k-th row below the top datum (`top + k·spacing`).
  The same k on every face is the same course.
- **Joining rule:** two course pieces join only when all three conditions hold:
  1. they have the same course index,
  2. their faces share a HIP edge (RIDGE and VALLEY never join),
  3. the shared endpoint lies on that edge (within TOL).
  Endpoint coincidence alone never joins, and ridges never join courses: two slopes meeting at a ridge stay separate.
- **Chaining:** chaining is general. It extends at either end and handles any number of pieces, so it isn't limited to two.
- **Closed:** a polyline is closed only if the chain returns to its start through a linked hip. No closing segment is ever invented.
- **Output:** one LWPOLYLINE per chain on `A-ROOF-BATTEN`. Unjoined pieces (gable) are 2-vertex open LWPOLYLINEs.
  The preview still draws plain screen segments.
- **Other members:** rafters, jacks, hip rafters and ridge beams stay LINE.

## Callouts
- **Construction:** native `LEADER` command with command-line MTEXT annotation, which is editable and Mac-safe.
  MLEADER is avoided because pure-AutoLISP MLEADER creation isn't reliable without ActiveX.
- **Layer / height:** layer `LAYER_ANNO` = `A-ROOF-ANNO`. Text height is `CALLOUT_TEXT_HEIGHT` (250), applied through a
  temporary `DIMTXT = height / DIMSCALE` (DIMSCALE 0 is treated as 1). OSMODE is 0 while placing, and everything is restored.
- **Target:** the leader points at the midpoint of the created segment nearest the roof datum. That gives the central common
  rafter, the innermost batten course, the long fascia edge, one hip rafter, and the ridge beam.
- **Angle snap:** the target→cursor angle is snapped inside RF (`akd:rf-callout-geometry`). The raw WCS angle
  (normalised to 0…360) is rounded to the nearest `CALLOUT_ANGLE_INCREMENT` (15°), and the cursor distance becomes the
  leader length. POLARANG, SNAPANG, ORTHOMODE and AUTOSNAP are never touched. Angles follow the drawing (WCS), not the roof axes.
- **Landing:** a horizontal segment of `CALLOUT_LANDING_LENGTH` (500) is added at the elbow. It extends right when
  cos(snapped angle) ≥ 0, including straight up and down, and left otherwise. LEADER then places the text on the landing side.
- **Preview:** the target member is highlighted. An XOR ghost shows the **snapped** leader and landing, redrawn only
  when the snapped result changes. The click commits the last previewed vertices exactly, so there is no jump. If there
  was no preview yet, it uses the click point's snapped geometry. Text is created only after the click.
- **LEADER sequence sent:** `_.LEADER`, target, elbow, landing end, `""` (end points → annotation), text, `""` (end text).
- **Text:** taken from the current spec and this run's requested spacing:

| Member | Text |
|---|---|
| Rafters | `{W}x{D} TIMBER RAFTERS @ {requested} C/C MAX` (requested, never the solved actual) |
| Hip rafter | `{W}x{D} HIP RAFTER` |
| Ridge beam | `{W}x{D} RIDGE BEAM` |
| Battens | `{W}x{D} TIMBER BATTENS @ {requested} C/C` |
| Fascia | `{W}x{D} FASCIA` |

- **Settings:** `CALLOUTS` in config is the default. `*AKD-RF-CALLOUTS*` remembers the session choice.
- **Code:** callout code is isolated in `akd:rf-callout-text / -target / -preview / -create / -place` and `akd:rf-settings`.

## Test status
**Verified off-CAD:** the course generator and chaining (with the hip/valley link rule) were ported line-for-line to
Python and run below. The source was checked for parenthesis balance, undefined `akd:` calls and local names
that shadow AutoLISP built-ins. **Interactive AutoCAD for Mac tests (prompts, LEADER output, Undo) have not been run yet.**

### T0 — general topology and framing (off-CAD verification)
The solver and the framing generators were ported line-for-line to Python (`skel.py`, `frame_test.py` in the
session scratchpad) and run on every regression footprint. Independent check of each skeleton: for every skeleton edge, both
endpoints are equidistant from the two adjacent eave lines. Worst error over all cases was **2.7e-12**, faces are never below
their eave (≥ −5e-13), and ridges are level.

| Footprint | Verts / reflex | Faces (vertex counts) | Ridges | Hips | Valleys | Nodes inside | CAD |
|---|---|---|---|---|---|---|---|
| Rectangle 10000×6000 | 4 / 0 | 4 (4,3,4,3) | 1 × 4000 | 4 × 4242.64 | 0 | ✅ | ☐ |
| same, rotated 17° / 30° / CW input / closing dup | identical | identical | identical | identical | 0 | ✅ | ☐ |
| Square 6000 | 4 / 0 | 4 triangles | **0** (no ridge beam prompt) | 4 | 0 | ✅ | ☐ |
| Irregular quadrilateral | 4 / 0 | 4 (4,3,4,3) | 0 (sloping crest is a HIP) | 5 | 0 (none invented) | ✅ | ☐ |
| Pentagon | 5 / 0 | 5 (5,3,4,4,3) | 0 | 7 | 0 | ✅ | ☐ |
| Regular hexagon | 6 / 0 | 6 triangles | 0 | 6 × 6000 | 0 | ✅ | ☐ |
| **L** (0,0)(10000,0)(10000,6000)(6000,6000)(6000,10000)(0,10000) | 6 / 1 | 6 (4,3,4,4,3,4) | 2 × 4000 | 5 | **1** × 4242.64 | ✅ | ☐ |
| L rotated 23° | identical | identical | identical | identical | identical | ✅ | ☐ |
| L uneven arms | 6 / 1 | 6 (4,3,5,4,3,5) | 4000, 7000 | 6 | 1 | ✅ | ☐ |
| T | 8 / 2 | 8 | 3 | 6 | 2 | ✅ | ☐ |
| U | 8 / 2 | 8 | 3 (6000, 6000, 8000) | 6 | 2 | ✅ | ☐ |
| Stepped rectilinear | 8 / 2 | 8 | 2 | 8 | 2 | ✅ | ☐ |
| Irregular concave pentagon | 5 / 1 | 5 | 0 | 6 | 1 | ✅ | ☐ |
| Bow-tie | — | **rejected: self-intersecting** | | | | | ☐ |
| Zero-length edge | — | **rejected** | | | | | ☐ |
| Bulged segment (AutoCAD only) | — | **rejected: curved boundaries not supported** | | | | | ☐ |

Face areas sum to the footprint area in every solved case (checked in the solver), so there are no gaps or overlaps.

**Framing (rafters 600 max, battens 600):**

| Footprint | Module | Rafters per face | Hip A / B | Ridge A / B | Valley A (on centreline) | Battens: segments → polylines | CAD |
|---|---|---|---|---|---|---|---|
| Rectangle (0° / 17° / 30°) | 571.43 (7) | 18, 11, 18, 11 (unchanged) | 24 pairs, dist 0 | 8 pairs, dist 0 | — | 16 → 4 closed rings | ☐ |
| Square | 600 (5) | 9 ×4 | 20 pairs, 0 | — | — | 16 → 4 closed | ☐ |
| Irregular quad | 563.31 (6) | 16, 11, 14, 9 | 28 pairs, 0 | — | — | 16 → 4 closed | ☐ |
| Pentagon | 577.38 (6) | 15, 10, 13, 13, 8 | 34 pairs, 0 | — | — | 29 → 6 closed | ☐ |
| **L** (0° and 23°, identical) | 571.43 (7) | 18, 11, 12, 12, 11, 18 | 30 pairs, 0 | 14 pairs, 0 (1 end with no partner face*) | 5 valley pairs, ≤ 9e-13 | 24 → 4 open (stop at valley) | ☐ |
| L battens 450 / 400 / 300 | — | — | — | — | — | 36 → 6, 42 → 7, 54 → 9, all open; hip pairs 30 / 35 / 45 | ☐ |
| T | 600 (10) | 10, 13, 7, 13, 10, 7, 21, 7 | 24 pairs, 0 | 24 pairs, 0 | on line (6 unpaired**) | 24 → 6 | ☐ |
| U | 571.43 (14) | 21, 18, 7, 14, 13, 14, 7, 18 | 24 pairs, 0 | 35 pairs (3 no-partner*) | on line (6 unpaired**) | 24 → 6 | ☐ |

Every rafter in every case lies inside its own face (both ends and midpoint). No rafter crosses a hip, ridge or valley.

\* Ridge end at a junction node where the neighbouring face has zero width, so no rafter is geometrically possible there.
\*\* Valley ends always lie on the valley centreline (TEST A). They pair exactly only where the adjacent ridge module aligns with
the valley stations (true for the regular L). On T, U and uneven L the ridge line takes priority and the neighbouring valley jack
ends at its own clipped point. This is a documented V1 limitation.

**Equal spacing rows:**
- **Three rows:** 12000 / 10300 / 9000 → 12000 / 10500 / 9000.
- **Four rows:** 9000 / 12000 / 10000 / 9500 in selection order become 9000 / 12000 / 11000 / 10000. Assigned top to bottom, the top and bottom are kept.
- **Crowded rows:** 10000 / 10050 / 10100 / 10120 expand to a 375 pitch (1.5 × 250).

**AutoCAD for Mac checklist for this pass:**
| # | Check | CAD |
|---|---|---|
| G1 | Rectangle hip: same roof, rafters, battens and callouts as before this pass | ☐ |
| G2 | L hip: preview shows 2 ridges, 5 hips, 1 valley (valley in blue); commit creates 8 A-ROOF lines | ☐ |
| G3 | L: "Add valley rafters?" asked; 1 line on A-ROOF-VALLEY-RAFTER; "50x150 VALLEY RAFTER" callout offered | ☐ |
| G4 | Gable on L: "Irregular Gable roofs are not supported yet. Use Hip or Flat." and reprompt | ☐ |
| G5 | Flat on L: one flat region; fascia follows the concave perimeter | ☐ |
| G6 | T, U, stepped, irregular convex/concave: solve or give a clean "Unable to solve" with nothing created | ☐ |
| G7 | Bow-tie, bulged segment, zero-length edge: rejected before roof type prompt | ☐ |
| G8 | Square: no ridge beam prompt; pyramid rafters and battens | ☐ |
| G9 | Callout Match with Equal spacing Yes (Left, then Right) and No: rows, edges and 15° legs as described | ☐ |
| G10 | Full RF on L (roof, rafters, hip, valley, ridge, battens, fascia, callouts, 2 match rounds) then one `U` | ☐ |
| G11 | Esc in every preview (including valley rafters and the equal-spacing match): no ghosts or highlights left | ☐ |

### C1 — batten course joining
| Roof | Spacing | Segments (old LINEs) | Committed LWPOLYLINEs | Closed | Vertices each | Joined vertices on hip | Invented edges | CAD |
|---|---|---|---|---|---|---|---|---|
| Hip 10000×6000 | 600 | 16 | 4 (courses 1–4) | 4 | 4 | max 0 | 0 | ☐ |
| Hip 10000×6000 | 450 | 24 | 6 | 6 | 4 | max 2e-13 | 0 | ☐ |
| Hip 10000×6000 | 400 | 28 | 7 | 7 | 4 | 0 | 0 | ☐ |
| Hip 10000×6000 | 300 | 36 | 9 | 9 | 4 | 0 | 0 | ☐ |
| Hip rotated 30° | 600/450/400/300 | identical | identical | identical | 4 | ≤ 3.3e-12 | 0 | ☐ |
| Pyramid 6000 | 600 / 450 | 16 / 24 | 4 / 6 | all | 4 | ≤ 5e-13 | 0 | ☐ |
| Gable, long ridge | 600 / 450 | 8 / 12 | 8 / 12 (no join across ridge) | 0 | 2 | — | 0 | ☐ |
| Gable, short ridge | 600 / 450 | 16 / 22 | 16 / 22 | 0 | 2 | — | 0 | ☐ |

On an equal-pitch hip every course genuinely rings the roof (4 faces → 4 pieces), so a closed polyline is correct.
Corresponding face pieces still meet at the same hip points (TEST B from the previous pass is unchanged).

### C2 — callouts (AutoCAD only)
| # | Case | Expected | CAD |
|---|---|---|---|
| K1 | Settings → Callouts Off, full hip RF | zero entities on A-ROOF-ANNO; everything else unchanged | ☐ |
| K2 | Callouts On, full hip RF (defaults) | 5 callouts: `50x150 TIMBER RAFTERS @ 600 C/C MAX`, `50x150 HIP RAFTER`, `50x200 RIDGE BEAM`, `50x100 TIMBER BATTENS @ 600 C/C`, `25x250 FASCIA` | ☐ |
| K3 | Rafters 450, battens 400 | `… RAFTERS @ 450 C/C MAX`, `… BATTENS @ 400 C/C` | ☐ |
| K4 | Gable | no HIP RAFTER callout prompt | ☐ |
| K5 | Pyramid | no RIDGE BEAM callout prompt | ☐ |
| K6 | Rafters / battens / fascia answered No | no matching callout prompt | ☐ |
| K7 | Skip one callout (S or Enter) | that one omitted, others placed | ☐ |
| K8 | Esc during callout placement | ghost cleared, CLAYER/OSMODE/DIMTXT restored | ☐ |
| K9 | Complete RF then `U` | roof, rafters, hip rafters, ridge beam, battens, fascia and callouts all removed | ☐ |
| K10 | Current layer after RF | unchanged | ☐ |
| K11 | Second RF run | Settings value remembered | ☐ |
| K12 | Text height | 250 with DIMSCALE 1; still 250 with DIMSCALE 100 | ☐ |

### C3 — callout 15° snap
**Verified off-CAD** by porting the exact rounding to Python (target at an arbitrary non-origin point, 3000 away):

| Raw | Snap | Landing | | Raw | Snap | Landing |
|---|---|---|---|---|---|---|
| 7 | 0 | right | | 135 | 135 | left |
| 8 | 15 | right | | 225 | 225 | left |
| 22 | 15 | right | | 315 | 315 | right |
| 23 | 30 | right | | 352 | 345 | right |
| 37 | 30 | right | | 353 / 359.9 / 0.1 / −7 | 0 | right |
| 38 | 45 | right | | −8 | 345 | right |
| 82 / 83 / 97 / 98 | 75 / 90 / 90 / 105 | right / right / right / left | | 172 / 173 / 187 / 188 | 165 / 180 / 180 / 195 | left |
| 262 / 263 / 277 / 278 | 255 / 270 / 270 / 285 | left / right / right / right | | zero-length cursor | no leader | — |

A sweep from 0° to 360° in 0.01° steps had 0 mismatches against nearest-15°. Leader length always equals the cursor distance.

**AutoCAD for Mac (required):**
| # | Check | CAD |
|---|---|---|
| A1 | Ghost locks through 0/15/30/45…; never shows the raw angle | ☐ |
| A2 | Committed leader angle = last ghost angle (check with LIST / properties) | ☐ |
| A3 | Landing is horizontal, 500 long, right side for right/vertical leaders, left side for left | ☐ |
| A4 | Text created at the landing, correct side; command does not stall | ☐ |
| A5 | Roof rotated 17°: leaders still at 0/15/30… relative to the drawing | ☐ |
| A6 | Enter / Space / right-click / S = skip; click = place | ☐ |
| A7 | Esc during placement: ghost gone; OSMODE, DIMTXT, CLAYER restored | ☐ |
| A8 | POLARANG / SNAPANG / ORTHOMODE unchanged after RF | ☐ |
| A9 | One `U` removes all RF output including callouts | ☐ |
| A10 | If LEADER stalls or misplaces text: copy the command-line transcript (F2) for correction | ☐ |

### C4 — matched callout groups
**Workflow** (after callout placement, only if this run created ≥ 2 callouts):
```
Match callouts? [Yes/No] <Yes>:
Select callouts to match:              normal ssget (pick / window / crossing / remove / Enter)
  3 RF callout(s) selected. 4 other object(s) ignored.
  (< 2) At least 2 RF callouts are required.  Match group [Reselect/Cancel] <Reselect>:
Text alignment [Left/Right] <Left>:
Specify callout match position:        cursor X = match datum; ghost + vertical guide; group highlighted
  click = commit exactly the ghost
  click on an invalid position = "Unable to create a clean matched callout at this position." (stays in preview)
  Enter / right-click = abandon this round;  Esc = RF cancel
Match another callout group? [Yes/No] <No>:
```
Left and Right describe the **text edge**, not a side of the roof. Either can be used anywhere.

**Matched callout group:**
- **Match datum:** one WCS-vertical match datum per group. Text is always horizontal, landings are horizontal, and roof rotation is ignored.
- **Left text alignment:** each landing runs right and ends exactly on the datum. Native LEADER attaches the MTEXT
  left-justified at the landing end, so every text **starts** at the datum.
- **Right text alignment:** each landing runs left and ends on the datum. LEADER attaches the MTEXT right-justified, so every text **ends** at the datum.
- **Text length:** text strings are never changed or padded. Different lengths are expected.
- **Landing length:** landing length is free per callout, and at least `CALLOUT_MIN_LANDING_LENGTH` (500).
  `CALLOUT_LANDING_LENGTH` (500) still applies only to individually placed callouts.

**Re-solve rule** (`akd:rf-match-path`; the target PATH[0] never changes):
- **Frame:** Right is solved in an X-mirrored frame, so one routine serves both modes.
- **Constraint:** elbow X ≤ datum − min landing (Left), or the mirror of that for Right.
- **Per angle:** for every allowed leg angle (0, 15 … 345), the leg length range is set by that constraint and by MIN_SEG_LEN.
  The length is clamped to the value that puts the elbow nearest the callout's **previous text Y**.
- **Choice:** the smallest Y shift wins, ties going to the angle closest to the previous leg direction. Position is preserved, not the old angle.
- **Rejection:** if any callout in the group would need a Y shift above `CALLOUT_MATCH_MAX_SHIFT` (2000), or has no valid angle,
  the whole position is rejected. The preview shows nothing and a click reports the message. No malformed geometry is created.

**Record:** `ID NAME TEXT PATH JUST ENTS`. After commit, PATH, JUST and ENTS are replaced, so a later round uses the latest geometry.
On commit, the replacement LEADER is created first, then the old entities are erased. Everything is in the single RF Undo group.

**Preview (screen only, XOR):** re-solved legs and landings, an estimated text box on the text side of the datum (width ≈ 0.8 × height ×
characters, for visual feedback only), and a vertical match-datum guide spanning the group.

**Off-CAD solver verification** (Python port, 10000×6000 hip-like targets, real RF strings):

| Case | Result | Landings (varied, ≥ 500) | Text Y before → after |
|---|---|---|---|
| Group A: RAFTERS, BATTENS, FASCIA; Left, datum X=13000 | PASS, all starts on datum, legs 30/30/345° | 1938 / 4414 / 1239 | unchanged ×3 |
| Group B: HIP RAFTER, RIDGE BEAM; Right, datum X=−3000 | PASS, all ends on datum, legs 150/165° | 1036 / 2204 | unchanged ×2 |
| Group A nudged +200 | PASS, landings +200 each, no jump in Y | 2138 / 4614 / 1439 | unchanged |
| A+B+C Left X=12000, then C (latest record) + HIP Right X=−2500 | PASS, C re-solved from the updated PATH | 938 / 3414 / 3862, then 10638 / 536 | unchanged |
| Datum across a target (Left X=5200, target X=5000) | PASS, legs flip to 105° so landing still runs right | 1138 / 755 | unchanged |
| Datum left of targets (Left X=2000) | PASS, legs 135/150° | 500 / 586 | unchanged |
| Datum far on the wrong side (Left X=−20000) | **Rejected**, needs > 2000 Y shift | — | — |
| Right alignment used on right side (Right X=13000) | PASS | 5062 / 500 | 6500 / −1812 → −2278 (within cap) |
| Roof rotated 30°, Left X=12000 | PASS, annotation stays WCS | 3108 / 4384 / 908 | unchanged |

Every PASS row met all of these checks: target unchanged, leg on a 15° multiple, leg > MIN_SEG_LEN, landing horizontal, landing end exactly on
the datum, and landing ≥ 500 on the correct side.

**AutoCAD for Mac (required; Mac LEADER output is the source of truth):**
| # | Check | CAD |
|---|---|---|
| M1 | Normal callout creation and LEADER prompt sequence still complete without stalling | ☐ |
| M2 | Left group: `(entget (car (entsel)))` on each MTEXT shows left attachment (group 71 = 1/4/7), and text starts coincide | ☐ |
| M3 | Right group: MTEXT attachment right (71 = 3/6/9), and text ends coincide with different string lengths | ☐ |
| M4 | Selecting only a leader, or only its text, is recognised as the same RF callout | ☐ |
| M5 | Window with roof, framing and unrelated text: only RF callouts counted | ☐ |
| M6 | Group A (3, Left) then Group B (2, Right): B does not disturb A | ☐ |
| M7 | Targets unchanged; legs at 15° multiples; landings horizontal with individual lengths | ☐ |
| M8 | Ghost = committed geometry (no jump); guide and highlight gone afterwards | ☐ |
| M9 | Invalid datum: message, no geometry created, preview continues | ☐ |
| M10 | Re-match C in a second round: no duplicate leader or text, no stale entity | ☐ |
| M11 | Roof rotated 30°: text, landings and datum stay drawing-oriented | ☐ |
| M12 | Enter / right-click abandons the round; Esc leaves no ghost or highlight and restores OSMODE / DIMTXT / CLAYER | ☐ |
| M13 | Roof + framing + callouts + 2 match rounds, then one `U`: everything removed | ☐ |
| M14 | If justification is wrong or anything is left behind, paste the F2 transcript and the `entget` of the LEADER and MTEXT | ☐ |

### Earlier passes (unchanged, regression)
- **Topology:** hip = 2 trapezoids + 2 triangles, 4 hips, 1 ridge; pyramid = 4 triangles, 0 ridges; gable = 2 rectangles.
- **Rafters:** even distribution, e.g. 4000 ridge @600 → 7 bays at 571.43; 3600 → 6 bays at 600. Shared ridge and hip
  stations coincide (TEST A + TEST B, distance 0), including rotated 30°, pyramid and both gable orientations.
- **Battens:** top datum with requested spacing (600/450/400/300); hip meeting points coincide.
- **UX:** spacing prompts, S = Spacing in preview, remembered values, and Esc/Undo per stage.

## Framing mode — Line / Offset
One framing engine, two representations. The mode is asked **once per RF run**, after the roof is accepted and before the first
framing stage:
```
Framing mode [Line/Offset] <Offset>:      L = Line, O = Offset
```
It applies to **rafters, hip rafters, valley rafters and ridge beams** for that whole run. It is remembered for the AutoCAD session
(the next run offers your last choice as the default) and is also available under `RF > Settings > Framing`. No drawing settings are changed.

| | Line | Offset |
|---|---|---|
| Output | LINE per member | closed LWPOLYLINE per member |
| Geometry | the centreline masters themselves | actual plan width from `*_WIDTH` |
| Rafter ends | ridge / hip / valley **centreline** (original convention) | ridge / hip / valley **face**, angled cut |
| Structural nodes | masters meet at the node | mitred on the angle bisector |
| Ghost preview | centreline ghosts | actual-width outline ghosts |
| Layers | unchanged | unchanged |

- **Shared engine:** topology, roof faces, shared edges, stations, requested and solved spacing, and common/jack classification are
  computed identically. The mode is read **only** at the output layer (`akd:rf-display-outlines` returns nil in Line mode, so preview
  and commit fall back to the masters). There are no per-mode framing generators.
- **Specifications are unchanged:** a member is still 50×150 in Line mode; the centreline just represents it. Callout text is therefore
  identical in both modes (`50x150 TIMBER RAFTERS @ 600 C/C MAX`), which keeps future schedules and quantities correct.
- **Not affected:** battens stay course polylines and fascia is unchanged, in both modes.
- **Line mode is not `DEBUG_CENTRELINES`.** Line mode is intentional production output; `DEBUG_CENTRELINES` is a developer overlay drawn
  on top of Offset geometry. They remain separate systems.

### F1 — AutoCAD for Mac checklist
| # | Case | Expected | CAD |
|---|---|---|---|
| F1 | Rectangle 10000×6000 hip, mode **Line** | rafters, hip rafters and ridge beam are LINEs; no closed outlines created | ☐ |
| F2 | Same roof, mode **Offset** | all four member types are closed LWPOLYLINEs; face clipping and node mitres as before | ☐ |
| F3 | Master equality | same roof and spacing in both modes: identical member count, endpoints and stations (Line endpoints sit on the ridge/hip centreline; Offset outlines are cut back to the faces) | ☐ |
| F4 | Spacing 600 then 450, both modes | same solved stations and rafter count per mode pair | ☐ |
| F5 | Square 6000 | Line: 4 hip masters meeting at the apex. Offset: 4 width members, cleanly mitred | ☐ |
| F6 | L-shape, both modes | identical ridges, hips, valleys and rafter masters; only representation differs | ☐ |
| F7 | L rotated 23°, both modes | same result rotated; no WCS-dependent behaviour | ☐ |
| F8 | T and U, both modes | no shape-specific behaviour | ☐ |
| F9 | Session memory | run RF with Line, finish, run again: prompt shows `<Line>`; switching to Offset updates the default | ☐ |
| F10 | Esc before the mode prompt, during a Line preview, and during an Offset preview | no partial framing, no ghost remnants, mode not corrupted, system variables restored | ☐ |
| F11 | Undo | one `U` removes the complete RF result in either mode | ☐ |
| F12 | Callouts | identical text in both modes (still `50x150 …`) | ☐ |

## Actual-width framing (plan)
Centreline members remain the **master geometry**: topology, shared stations and spacing are untouched. This layer only derives
plan outlines from them:
```
ROOF TOPOLOGY -> MEMBER CENTRELINE -> MEMBER WIDTH -> ACTUAL MEMBER OUTLINE
```
Converted in this pass: **rafters (common + jack), hip rafters, valley rafters, ridge beams**.
**Not converted (unchanged):** battens (course polylines) and fascia. Annotation is frozen.

- **Outline:** each member becomes one closed LWPOLYLINE, width `*_WIDTH` from config (50 for rafters/hips/valleys, ridge beam per config),
  centred on the centreline: `A ± N·w/2`, `B ± N·w/2`. Calculated directly, never by OFFSET.
- **Eave end:** square cut perpendicular to the rafter. No overhang, birdsmouth or fascia trimming.
- **Structural ends:** the rafter outline is **clipped by the face half-plane** of each structural member its end sits on, so the cut is
  correctly angled. A square cut would push one corner into the hip and leave a gap at the other.
  A rafter top can land on a node where three or more edges meet (a ridge end plus two hips), so **every** edge containing that endpoint is
  clipped — stopping at the first one leaves timber inside the others by up to half a width (this was a real defect, found and fixed in testing).
- **Node mitring:** structural members meeting at a skeleton node are clipped by the **angle bisector** against their angular neighbours.
  This is one generic helper for 2, 3, 4 or more incident edges (a pyramid apex is just the 4-edge case), so there is no per-shape node code.
  True mitre for equal widths.
- **Minimum length:** a member whose trimmed centreline is shorter than `MIN_MEMBER_LENGTH` (50) is skipped rather than drawn degenerate.
- **Preview:** ghosts now show actual plan width, so framing density is visible before accepting.
- **Centreline output:** not drawn. Set `DEBUG_CENTRELINES` to `"On"` in config to also emit the centreline LINEs for debugging.
- **Layers:** unchanged (`A-ROOF-RAFTER`, `A-ROOF-HIP-RAFTER`, `A-ROOF-VALLEY-RAFTER`, `A-ROOF-RIDGE-BEAM`).

### W0 — off-CAD verification
The width layer (outline, face clipping, node mitring) was ported to Python and run over the solved roofs. Checks per case: every
outline vertex exactly w/2 from its centreline; **no vertex inside any structural member's band**; at least one vertex touching each face
it was cut against; no structural outline vertex inside another structural outline.

| Case | Rafters (skipped) | Structural | Width errors | Timber through a member | Face contacts | Structural overlaps |
|---|---|---|---|---|---|---|
| Rectangle 10000×6000 | 58 (0) | 5 | 0 | **0** | 116 | 0 |
| Rectangle rotated 30° | 58 (0) | 5 | 0 | **0** | 116 | 0 |
| Square 6000 (pyramid apex) | 36 (0) | 4 | 0 | **0** | 80 | 0 |
| L | 82 (0) | 8 | 0 | **0** | 184 | 0 |
| L rotated 23° | 82 (0) | 8 | 0 | **0** | 180 | 0 |
| T | 88 (0) | 11 | 0 | **0** | 198 | 0 |
| U | 112 (0) | 11 | 0 | **0** | 248 | 0 |
| Pentagon | 57 (2 short jacks skipped) | 7 | 0 | **0** | 114 | 0 |
| Rectangle / L at w=75 and w=100 | 58 / 82 (0) | 5 / 8 | 0 | **0** | 116 / 184 | 0 |

Centreline masters are **identical at 50, 75 and 100** — width changes only the derived outlines and cut positions.

### W1 — AutoCAD for Mac checklist
| # | Check | CAD |
|---|---|---|
| W1 | Rectangle hip: every rafter is a closed 50-wide polyline, not a line | ☐ |
| W2 | Rafters stop at the ridge beam faces, not its centreline; nothing crosses the beam | ☐ |
| W3 | Jacks stop against the hip rafter faces with an angled cut: no overlap, no gap | ☐ |
| W4 | Rotated 30°: identical widths and cuts (no WCS-only offset assumption) | ☐ |
| W5 | Square: 4 mitred hip rafters at the apex, no uncontrolled overlap, no ridge beam | ☐ |
| W6 | Rectangular gable: rafters terminate against both ridge faces | ☐ |
| W7 | L: rafters, hip rafters, valley rafters and both ridge beams all actual width; valley jacks cut to the valley faces | ☐ |
| W8 | Widths 50 / 75 / 100 via config: spacing and centrelines unchanged, only outlines differ | ☐ |
| W9 | Preview shows real width before accepting; Esc leaves no outlines or ghosts | ☐ |
| W10 | Battens still course polylines, fascia unchanged, callouts unchanged | ☐ |
| W11 | One `U` removes the whole RF result | ☐ |

## MLEADER investigation — DEFERRED (annotation work is frozen)
> **Status: parked.** `RFMLTEST` has been removed from the source. RF callouts stay on **LEADER + MTEXT** and behave exactly as before.
> Nothing below is being worked on; it is kept only so the findings are not lost. Skip this section unless annotation work restarts.
RF callouts still use **LEADER + MTEXT**. Nothing in RF's behaviour changed. `RFMLTEST` is an isolated probe to find out
whether pure AutoLISP can create a native MLEADER reliably on AutoCAD for Mac. The conversion happens only if it can.

### What the first probe run found (AutoCAD for Mac)
The real prompt sequence, captured from an F2 transcript:
```
_.MLEADER
Specify leader arrowhead location or [pre enter Text/leader Landing first/Content first/Options] <Options>:   <- point OK
Specify leader landing location:                                                                              <- point OK
Overwrite default text [Yes/No] <No>:                                                                         <- NOT a text prompt
```
- **The text is not a bare argument** after the two points. Passing the string gave `Invalid option keyword.`
  (this is what the first version of the probe did wrong).
- **Both points are accepted normally**, so RF's solved target and landing can drive the command.
- **The style in use was annotative:** `CMLEADERSTYLE = AKD MULTILEADER`, `MLEADERSCALE = 0`, `DIMSCALE = 100`.
  With `MLEADERSCALE = 0` the size follows the annotation scale and the style, so `CALLOUT_TEXT_HEIGHT` (250) probably will **not**
  apply per callout. This is the main open question; the dump shows what actually happened.
- **`(command)` cannot be called from `*error*`** unless `(*push-error-using-command*)` was called first. The probe left MLEADER
  active because of this. Both `RFMLTEST` and `RF` now call it, so a cancel from the error handler works.

### Running it
```
Command: RFMLTEST
Pick arrow target:
Pick text location:
Enter text <TEST CALLOUT>:
Variant [Yes/No/Text/Manual] <Yes>:
```
| Variant | Sequence sent | Question it answers |
|---|---|---|
| **Yes** | points, then `_Yes`, then the text, then Enter | does answering "overwrite" accept a scripted string? |
| **No** | points, then `_No` | does it create cleanly with the style's default text? The probe then tries `entmod` on group 304 to set the text afterwards |
| **Text** | `T` (pre enter Text), the text, then the points | does the pre-enter-text route avoid the overwrite prompt? |
| **Manual** | points, then PAUSE | you type the text; shows whether a command-line prompt or an in-place editor appears |

The probe sets CMDECHO 1 so the prompts land in the F2 transcript, puts the result on `A-ROOF-ANNO`, sets OSMODE 0,
restores every system variable, cancels the command if it is still active, and prints `CMLEADERSTYLE`, `MLEADERSCALE`,
`DIMSCALE`, the entity count and a full `entget` dump.

**Run each variant once and send the whole F2 transcript**, including the dump. If a run leaves the command active, press Esc.

### GO / NO-GO
Convert only if all of these hold for at least one variant:
| Check | Why it matters |
|---|---|
| Exactly one user-facing entity (MULTILEADER) | one callout = one entity |
| Command exits cleanly, no "command still active" warning | scripted creation is safe inside RF |
| Arrow lands on the picked target | RF's solved geometry survives |
| Landing and text position follow the given points, not the style's own layout | RF must own the layout |
| The text is the string we passed (directly, or via the `entmod` fallback) and stays editable | callout text stays useful |
| One click anywhere (arrow, leg, landing, text) selects it | simplifies Match selection |
| MOVE / ERASE / text edit behave normally outside RF | the main reason for converting |
| Esc during the command leaves nothing behind and no stuck command | RF's Esc contract |

If no variant passes, report it and **RF keeps LEADER + MTEXT**. A half-converted callout system is worse than the working one.

**Text height:** if height follows the style rather than `CALLOUT_TEXT_HEIGHT`, that limitation gets documented before any
style-management subsystem is considered — RF must not permanently modify the user's MLEADER style.

**If it is a GO**, the conversion is confined to `akd:rf-callout-create` (marked in the source as the single backend abstraction)
plus the record's `ENTS` list. RF keeps solving target, 15° leg, elbow, landing, text position and Left/Right justification;
MLEADER never lays the callout out itself. Match, equal spacing, rounds, ghost preview, safe rebuild and the single Undo group
stay as they are, and `RFMLTEST` is removed.

## Known limitations
- **Straight segments only:** curved boundaries are rejected, not tessellated.
- **Equal pitch only:** there is no unequal-pitch or user-chosen gable end on irregular footprints (Gable = rectangles only).
- **Simultaneous events:** many exactly simultaneous skeleton events are resolved sequentially at the same time value. If the
  resulting graph fails any check, the roof is rejected rather than approximated.
- **Valley pairing:** valley jack pairing is exact only where the ridge module aligns (see T0). Valley ends are always on the valley centreline.
- **Plan spacing only:** there is no pitch or true slope spacing.
- **Actual width:** rafters, hip rafters, valley rafters and ridge beams are plan outlines; depth (150 etc.) is not used in 2D yet.
  Battens and fascia are still centrelines. No birdsmouths, overhangs or real carpentry joints; node mitres assume equal widths.
- **Callouts:** one LEADER + MTEXT pair per callout, styled by the current dimension style, with no collision avoidance.
  Native MLEADER is under investigation (see RFMLTEST); RF is unchanged until that probe passes on AutoCAD for Mac.
