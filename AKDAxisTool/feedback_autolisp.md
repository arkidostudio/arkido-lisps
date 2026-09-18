---
name: feedback-autolisp
description: "Working preferences for the CADLisps AutoLISP work — direct-manipulation over commands, diagnostics on demand, terse iteration."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: f77bc70a-74b8-4442-a065-4e28c6ffbd36
  modified: 2026-08-18T04:42:18.140Z
---

For AutoLISP tools in this project, prefer direct entity manipulation (`entmod`, `entmakex`) over `(command "...")` invocations of AutoCAD commands like FILLET/BREAK.
**Why:** During XWW development, FILLET was unreliable at corners needing wall extension (both endpoints past current segment) and BREAK's point-selection was fragile — both were replaced with direct endpoint moves / segment splits and only then worked consistently.
**How to apply:** When something needs to "trim/extend to intersection" reach for `ax-move-nearest-endpoint` (or equivalent entmod). When "break out a middle piece", reach for `ax-split-line`. Use `command` only for things without a direct DXF-modification path (e.g., `_.-layer _Make`, `_.undo _begin/_end`).

When a run "doesn't work", ask the user for the diagnostic line output (`Junctions detected: L=? T=? X=?`) and/or a screenshot before guessing — they iterate fast with visual feedback and answer promptly.

User prefers minimal churn per turn: one focused change, brief explanation, wait for the next screenshot. Don't preemptively refactor unrelated code.
