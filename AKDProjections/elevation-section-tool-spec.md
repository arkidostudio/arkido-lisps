# Plan-to-Elevation/Section Generator — Implementation Guide

## Goal
Build a tool that generates elevations and sections automatically from a 2D floor plan, the way the YQArch AutoCAD plugin does — via short commands that project or cut through the plan rather than manual redrawing.

---

## 1. Data Model

Define the plan as structured objects, not raw lines:

```
Wall {
  id
  startPoint (x, y)
  endPoint (x, y)
  thickness
  height          // or reference to a story height
  baseElevation   // Z of wall base, for multi-story
}

Opening {         // door or window
  id
  wallId          // which wall it sits in
  positionAlongWall   // distance from wall start
  width
  sillHeight      // Z of bottom (0 for doors)
  headHeight      // Z of top
  type            // "door" | "window", plus style key
}

Story {
  id
  baseElevation
  height
}

RoofEdge / RoofPlane (optional, for roof projection)
```

This is the data every generator function reads from.

---

## 2. Core Generator Functions

### `generateElevation(direction, plan, options)`
- `direction`: "N" | "S" | "E" | "W", or a picked wall/wall group
- Steps:
  1. Select all walls facing that direction (normal vector matches, or picked explicitly).
  2. Project each wall's plan-view width onto a horizontal axis (this becomes the elevation's X axis).
  3. For each wall, draw a rectangle from `baseElevation` to `baseElevation + height`.
  4. For each opening on that wall, compute its X position from `positionAlongWall`, and draw a rectangle from `sillHeight` to `headHeight` at that X — call the matching detail-drawing function for door/window style.
  5. If `options.includeRoof`, project roof edges above the wall tops.
  6. Return a 2D drawing (list of polylines/rectangles) representing that elevation.

### `generateAllElevations(plan, options)`
- Runs `generateElevation` for N/S/E/W in sequence, laying them out on a sheet.
- Equivalent to the "4-in-1" auto elevation command.

### `generateSection(cutLine, plan, options)`
- `cutLine`: two points defining where the plan is sliced.
- Steps:
  1. Find every wall, floor, and structural element the cut line crosses.
  2. For each crossed element, draw its vertical profile (wall thickness in section, floor slab thickness, stair stringer profile if applicable) using stored heights.
  3. Elements the cut line doesn't cross but are visible beyond the cut plane can optionally be drawn in elevation-style (dashed/lighter) for depth context.
  4. Return the section drawing.

### Detail sub-generators (called by the above)
- `drawDoorElevation(width, height, style)`
- `drawWindowElevation(width, sillHeight, headHeight, style)`
- `drawDoorSection(width, wallThickness, style)`
- `drawWindowSection(width, wallThickness, sillHeight, headHeight, style)`

Keep these separate from the projection logic — they just need a bounding box and style key, and return standard detail geometry (frame, panel/glass, hardware).

---

## 3. Command Interface (User-Facing)

Mirror the short-command workflow so it feels fast to use:

| Command | Action |
|---|---|
| `ELEV` | Generate a single elevation — prompts for direction or pick a wall |
| `ELEV4` | Generate all four elevations at once |
| `SECT` | Generate a section — prompts for two points defining the cut line |
| `REPEAT` / `QQ` | Re-run the last command with the same options, for the next wall/cut |

### Example interaction flow — single elevation
```
> ELEV
Pick a wall, or type a direction (N/S/E/W): S
Include roof? (y/n): y
Style: [default]
→ generates South elevation, places it on the sheet
```

### Example interaction flow — section
```
> SECT
Pick first point of cut line: (click)
Pick second point of cut line: (click)
Show elements beyond cut plane? (y/n): y
→ generates section drawing along that line
```

### Example — repeat
```
> ELEV
... generates North elevation ...
> QQ
Pick a wall, or type a direction (N/S/E/W): E
→ generates East elevation using same options as last time
```

---

## 4. Implementation Notes for Claude Code

- Build the data model first (Wall, Opening, Story) with simple JSON or in-memory objects — this is the single source of truth both the plan view and the generators read from.
- Write `generateElevation` and `generateSection` as pure functions: input = plan data + parameters, output = drawing geometry. Keep them independent of any UI/command-line layer so they're testable.
- Layer the command interface (`ELEV`, `SECT`, `ELEV4`, `QQ`) as a thin wrapper that collects parameters and calls the pure functions — this makes it easy to also expose the same functionality via a GUI or API later.
- Start with rectangular walls and simple door/window rectangles before adding roof projection or curved walls — get the projection math solid first.
- For sections, reuse the same wall/opening data as elevations; the only difference is filtering by "does the cut line cross this element" vs "does this wall face this direction."
