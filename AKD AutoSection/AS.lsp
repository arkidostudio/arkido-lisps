;;; ============================================================
;;; AS.lsp - AS: AUTO SECTION
;;; AutoCAD for Mac. Standard AutoLISP only: no VLA/VLAX/ActiveX/COM, no XData.
;;; Source plan is read-only. Output = LINE / LWPOLYLINE / HATCH on section layers.
;;;
;;; GEOMETRY CATEGORIES
;;;   SOURCE        plan entities: read only, never modified
;;;   CONSTRUCTION  temporary AS entities (review labels) on CONSTRUCTION_LAYER (AS_CONSTRUCTION,
;;;                 cyan). Created via asec:make-construction, tracked in asec:temp, erased by
;;;                 normal cleanup and by the error handler. Only tracked enames are erased,
;;;                 never "everything on the layer": a leftover cyan entity = a tracking bug.
;;;                 Math (extensions, intersections) stays in memory; grdraw previews stay screen-only.
;;;   FINAL         section output on the AS_GRAPHICS.txt layers
;;;
;;; GRAPHICS CONFIG: AS_GRAPHICS.txt (KEY=VALUE, # comments), re-read at every AS start.
;;;   Lookup: findfile (support path, drawing folder), else the folder AS.lsp was found in,
;;;   else built-in defaults (asec:gfx-defaults). Missing keys keep their defaults.
;;;   Optional WALL_SOURCE_LAYERS / DOOR_SOURCE_LAYERS / WINDOW_SOURCE_LAYERS (comma lists)
;;;   replace *aseca-wall-layers* / *aseca-door-layers* / *aseca-window-layers*.
;;;
;;; COMMANDS
;;;   AS        automatic architectural section (discovery -> review -> generate)
;;;   SECTSET   session settings (heights, slab, openings)
;;;
;;; FILE STRUCTURE
;;;   1. core geometry, settings, records, projected walls   (asec:)
;;;   2. generator: walls, openings, occlusion, line consolidation   (asec:)
;;;   3. automatic discovery + review/edit                    (aseca:)
;;;   4. command AS
;;;   Review Edit/Add and Guided reuse the manual collectors in part 1.
;;;
;;; USAGE
;;;   AS
;;;   1. Select section line, viewing side, projection depth, GF reference point
;;;      (orientation rules below).
;;;   2. Each floor is analysed automatically from layers + geometry:
;;;        CUT WALLS        wall-layer LINEs crossing the floor's section, paired
;;;                         in U order (plus faces broken by an opening block)
;;;        CUT OPENINGS     door/window INSERTs hosted by a cut wall AND crossed
;;;        PROJECTED WALLS  wall-layer LINEs inside the view boundary, chained into
;;;                         logical faces, paired only when partners are unique
;;;        PROJ. OPENINGS   remaining INSERTs in the boundary, hosted by exactly one
;;;                         finite projected wall
;;;      Anything ambiguous becomes UNRESOLVED (never guessed).
;;;   3. Review: temporary labels C#, CD#, CW#, P#, PD#, PW#, ?# and a summary.
;;;        Generate  accept this floor (slab prompt)
;;;        Edit      Remove / Add (manual collectors) / Ignore unresolved
;;;        Guided    redo this floor manually, stage by stage
;;;        Cancel    exit
;;;   4. Upper floors: matching reference point, same analysis. Then roof,
;;;      insertion point and the generator.
;;;
;;; SETTINGS (session globals)
;;;   *aseca-wall-layers* *aseca-door-layers* *aseca-window-layers*   (case-insensitive)
;;;   *aseca-min-wall-thickness* 50  *aseca-max-wall-thickness* 600
;;;   *aseca-collinear-tolerance* 5  *aseca-small-gap-tolerance* 50
;;;   *aseca-min-face-length* 300 (shorter LINEs are jamb returns/nibs, not wall faces)
;;;   Projected openings in view but in no projected wall are kept unhosted.
;;;   *asec-wall-warning-thickness* 500 (cut walls above it are MEDIUM, not rejected)
;;;   *aseca-debug* nil
;;;
;;; CONFIDENCE
;;;   HIGH    direct cut crossings / projected faces with closing lines at both ends
;;;   MEDIUM  cut face recovered through an opening gap, thick wall, uncertain end
;;;   UNRESOLVED  odd crossings, out-of-range pairing, face with several partners,
;;;               unreadable block jambs, multiple/no hosts, overlapping openings
;;;
;;; SLAB LIMITS (per floor)
;;;   AUTO   : that floor's outermost cut-wall faces
;;;            (no walls -> asks [Manual/None], never guesses)
;;;   MANUAL : select two slab-edge LINEs crossing that floor's section line
;;;   NONE   : no slab for that floor
;;;   ROOF   : optional; uses top floor's slab extent, else [Manual/None]
;;;
;;; VERTICAL CONVENTION (used for every floor, GF included)
;;;   datum(i)   = finished floor level of floor i (GF = 0)
;;;   slab(i)    = datum(i) - slabT  ->  datum(i)
;;;   wall(i)    = datum(i)          ->  datum(i+1) - slabT
;;;   roof slab  = datum(n) - slabT  ->  datum(n)   (n = floor count)
;;;   ground     = datum(0) - slabT (underside of GF slab)
;;;   door       = datum -> datum + door height
;;;   window     = datum + sill -> datum + sill + window height
;;;
;;; SECTION COORDINATES
;;;   For plan point P on floor f with section start S_f:
;;;   U = (P - S_f) . dir      (station along section)
;;;   V = (P - S_f) . viewdir  (depth toward viewer's looking side)
;;;   uLeft = min uMin of all walls (0 if none)
;;;   Section X = insX - uLeft + U,  Section Y = insY + slabT + elevation.
;;;
;;; BLOCKS (doors/windows)
;;;   Block definition is read with tblobjname/entnext. LINE, LWPOLYLINE,
;;;   ARC, CIRCLE contribute points. Opening = block-local X extent
;;;   (jambs at local xmin/xmax), transformed by insertion, rotation,
;;;   X/Y scale (negative scale = mirrored, supported).
;;;   Auto width is refused (-> manual jambs) when the block has nested
;;;   INSERTs, flipped extrusion, or no readable geometry. Blocks whose
;;;   opening runs along local Y need Manual.
;;;
;;; CUT OPENING HOSTING
;;;   Each wall keeps its two face LINEs. They define a plan-space strip.
;;;   Host = wall whose strip contains the jamb midpoint (band widened by
;;;   the block's half depth + *asec-wall-host-tolerance*, default 10).
;;;   Several matches -> nearest wall center U; exact tie -> ask.
;;;   No match -> [Reselect/Assign/Skip]. Point-mode-only walls -> ask.
;;;   Cut check: the wall's section cut point must lie between the jambs.
;;;   Void U = host wall uMin -> uMax (cut thickness), never plan width.
;;;   Overlapping openings in one wall are rejected at selection.
;;;   Walls are drawn as rectangles decomposed around opening voids.
;;;
;;; VIEW BOUNDARY (projection zone)
;;;   After the viewing side: Projection depth [Point/Distance/Previous]. Near edge =
;;;   section line, far edge = +depth along the viewing direction (live preview).
;;;   Stored as section 'view: near/far corners, uMin 0, uMax length, vNear 0, vFar depth.
;;;   Guided projected walls are classified INSIDE / PARTIAL / OUTSIDE (OUTSIDE asks
;;;   Keep/Skip); unhosted projected openings outside ask Keep/Skip. No trimming yet.
;;;   The same boundary bounds automatic discovery.
;;;
;;; SECTION ORIENTATION
;;;   U direction is aligned with screen-right R = (Ny, -Nx) of the viewing
;;;   direction N, so reversing the master LINE does not mirror the output;
;;;   picking the opposite viewing side does.
;;;
;;; PROJECTED WALLS + DEPTH (MS3)
;;;   Faces must be parallel (*asec-parallel-tolerance*). Thickness = perpendicular
;;;   face separation. Wall length is FINITE: Auto ends come from local closing
;;;   LINEs (*asec-proj-wall-end-search* 300, *asec-proj-wall-end-tolerance* 10),
;;;   else matched/long face ends with confirmation; Manual = picked start/end.
;;;   Centerline ends -> (U1,V1),(U2,V2); span uMin->uMax; Vmid = average.
;;;   Height = same as cut walls (datum -> next slab underside). Output is only the
;;;   wall's two true end edges (vertical LINEs); edge-on -> one LINE.
;;;   Projected openings host on the wall whose strip and run contain the jamb
;;;   midpoint; void/graphics use the opening's jamb U span (not wall thickness).
;;;   No projected walls on the floor -> openings stay unhosted (MS2 behaviour).
;;;   Draw order: farther (larger V) first. No hidden-line clipping.
;;;   Skip at host prompts ends the current stage (same as cut openings).
;;;
;;; DATA MODEL (assoc lists, in memory only)
;;;   section: (master . (S E dir len ang)) (viewdir . v)
;;;            (config . (gfH typH slabT)) (floors . (floor ...))
;;;            (roof . (u0 u1) | nil) (ins . pt)
;;;   floor:   (index . i) (ref . p) (start . S) (end . E)
;;;            (elev . z) (walls . (wall ...)) (slab . (u0 u1) | nil)
;;;            (cutdoors . ops) (cutwins . ops) (projdoors . ops) (projwins . ops)
;;;            (pwalls . (pwall ...))
;;;   pwall:   (face1 . (a b)) (face2 . (a b)) (dir . d) (origin . pt)
;;;            (strip . (a1 n a2 n)) (t0 . t) (t1 . t) (cstart . pt) (cend . pt)
;;;            (thk . t) (u1 . u) (v1 . v) (u2 . u) (v2 . v) (umin . u) (umax . u)
;;;            (vmid . v) (bottom . z) (top . z)
;;;   projected opening adds (phost . pwall#) when hosted
;;;   wall:    (p1 . pt) (p2 . pt) (s1 . u) (s2 . u)
;;;            (umin . u) (umax . u) (thk . t)
;;;   opening: (kind . "DOOR"|"WINDOW") (mode . "CUT"|"PROJECTED")
;;;            (ename . e) (name . blk) (host . wall# | nil)
;;;            (u . u) (v . v) (umin . u) (umax . u) (width . w)
;;;            (bottom . abs z) (sill . s|nil) (height . h)
;;;            (type . "SINGLE"|"DOUBLE"|nil)
;;;   Openings reference their host wall by number; the generator
;;;   groups them per wall.
;;; ============================================================

(setq asec:tol 1e-6)

;;; ---------- vector helpers (2D) ----------
(defun asec:v- (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun asec:v+ (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(defun asec:dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun asec:cross (a b) (- (* (car a) (cadr b)) (* (cadr a) (car b))))
(defun asec:2d (p) (list (car p) (cadr p)))
(defun asec:get (k al) (cdr (assoc k al)))

;;; ---------- master section ----------
;;; section LINE record, or "Section" when the Draw Section option is chosen
(defun asec:get-master-section (/ sel e ed s en d len)
  (while
    (progn
      (setvar "ERRNO" 0)
      (initget "Section")
      (setq sel (entsel "\nSelect section line or [Section] <Select>: "))
      (cond
        ((= (type sel) 'STR) nil)
        ((null sel) (if (= (getvar "ERRNO") 7) T (exit)))
        ((/= (asec:get 0 (setq ed (entget (car sel)))) "LINE")
         (princ "\nSection line must be a straight LINE.") T)
        ((< (distance (asec:2d (asec:get 10 ed)) (asec:2d (asec:get 11 ed))) asec:tol)
         (princ "\nSection line has zero length.") T)
      )
    )
  )
  (if (= (type sel) 'STR)
    "Section"
    (asec:make-master (asec:2d (asec:get 10 ed)) (asec:2d (asec:get 11 ed))))
)

;;; master record (S E dir len ang) from two plan points
(defun asec:make-master (s en / len)
  (setq len (distance s en))
  (list s en (list (/ (- (car en) (car s)) len) (/ (- (cadr en) (cadr s)) len)) len (angle s en))
)

;;; ---------- viewing direction ----------
;;; normal of section on the side of plan point p (nil if p on the line)
(defun asec:side-normal (m p / side d)
  (setq d (caddr m)
        side (asec:cross d (asec:v- p (car m))))
  (cond ((< (abs side) asec:tol) nil)
        ((> side 0) (list (- (cadr d)) (car d)))   ; left normal
        (T (list (cadr d) (- (car d)))))           ; right normal
)

(defun asec:v* (v k) (list (* (car v) k) (* (cadr v) k)))

;;; ghost arrow from section midpoint along n (temporary vectors only)
(defun asec:draw-arrow (m n / mid sz tip bk pp)
  (setq mid (asec:v* (asec:v+ (car m) (cadr m)) 0.5)
        sz  (* 0.15 (nth 3 m))
        tip (asec:v+ mid (asec:v* n sz))
        bk  (asec:v+ tip (asec:v* n (* -0.3 sz)))
        pp  (asec:v* (caddr m) (* 0.15 sz)))
  (redraw)
  (foreach seg (list (list mid tip) (list tip (asec:v+ bk pp)) (list tip (asec:v- bk pp)))
    (grdraw (trans (car seg) 0 1) (trans (cadr seg) 0 1) 2 0))
  (if asec:marker-ghost (asec:ghost-section-marker m n))
)

(defun asec:get-view-direction (m / g p n done)
  (princ "\nSpecify viewing side: ")
  (while (not done)
    (setq g (grread T 15 0))
    (cond
      ((= (car g) 5)
       (if (setq n (asec:side-normal m (asec:2d (trans (cadr g) 1 0))))
         (asec:draw-arrow m n)))
      ((= (car g) 3)
       (if (setq n (asec:side-normal m (asec:2d (trans (cadr g) 1 0))))
         (setq done T)
         (princ "\nPoint is on the section line. Specify viewing side: ")))
      ((member (car g) '(11 25)) (exit))
    )
  )
  (redraw)
  n
)

;;; ---------- section orientation ----------
;;; Output reads left->right along screen-right R = (Ny, -Nx) of viewing direction N.
;;; If the master LINE was drawn against R, start/end are swapped (logically only;
;;; the source LINE is untouched). A future "flip section" option inverts this test.
(defun asec:normalize-section-orientation (m vd / d r dt)
  (setq d (caddr m) r (list (cadr vd) (- (car vd))) dt (asec:dot d r))
  (asec:dbg (strcat "\n--- SECTION ORIENTATION DEBUG ---"
                    "\nMaster start: " (asec:pt-str (car m)) "  end: " (asec:pt-str (cadr m))
                    "\nRaw direction: " (asec:pt-str d) "  Viewing direction: " (asec:pt-str vd)
                    "  Screen-right: " (asec:pt-str r) "\nDot: " (rtos dt 2 4)))
  (if (< dt 0)
    (setq m (list (cadr m) (car m) (asec:v* d -1.0) (nth 3 m) (angle (cadr m) (car m)))))
  (asec:dbg (strcat "\nFinal U direction: " (asec:pt-str (caddr m)) (if (< dt 0) " (flipped)" "")))
  m
)

;;; ---------- view boundary (projection zone) ----------
;;; Generic plan-space viewing volume shared by AS and future elevations.
;;; Near edge = baseline (section line), far edge = baseline + viewing dir * depth.
;;; Limits live in the view's own U/V: uMin 0 .. uMax baseline length, vNear 0 .. vFar
;;; depth, so the same record applies to every floor's translated section.
(defun asec:make-view-boundary (s e vd depth / off)
  (setq off (asec:v* vd depth))
  (list (cons 'near-start s) (cons 'near-end e)
        (cons 'far-start (asec:v+ s off)) (cons 'far-end (asec:v+ e off))
        (cons 'umin 0.0) (cons 'umax (distance s e))
        (cons 'vnear 0.0) (cons 'vfar depth) (cons 'depth depth) (cons 'vd vd))
)

;;; temporary grdraw rectangle (cleared by the next redraw)
(defun asec:preview-projection-boundary (vb / pts)
  (redraw)
  (setq pts (mapcar '(lambda (k) (trans (asec:get k vb) 0 1)) '(near-start near-end far-end far-start)))
  (mapcar '(lambda (a b) (grdraw a b 3 1)) pts (append (cdr pts) (list (car pts))))
)

(defun asec:uv-in-view-boundary-p (uv vb)
  (and (<= (- (asec:get 'umin vb) asec:tol) (car uv) (+ (asec:get 'umax vb) asec:tol))
       (<= (- (asec:get 'vnear vb) asec:tol) (cadr uv) (+ (asec:get 'vfar vb) asec:tol)))
)

(defun asec:point-in-view-boundary-p (p s dir vd vb)
  (asec:uv-in-view-boundary-p (asec:section-coordinate p s dir vd) vb)
)

;;; Liang-Barsky clip of UV segment a->b to the boundary: clipped (a' b') or nil
(defun asec:clip-segment-to-view-boundary (a b vb / du dv t0 t1 ok r)
  (setq du (- (car b) (car a)) dv (- (cadr b) (cadr a)) t0 0.0 t1 1.0 ok T)
  (foreach pq (list (cons (- du) (- (car a) (- (asec:get 'umin vb) asec:tol)))
                    (cons du (- (+ (asec:get 'umax vb) asec:tol) (car a)))
                    (cons (- dv) (- (cadr a) (- (asec:get 'vnear vb) asec:tol)))
                    (cons dv (- (+ (asec:get 'vfar vb) asec:tol) (cadr a))))
    (if ok
      (if (equal (car pq) 0.0 1e-12)
        (if (< (cdr pq) 0) (setq ok nil))
        (progn
          (setq r (/ (cdr pq) (car pq)))
          (if (< (car pq) 0)
            (if (> r t1) (setq ok nil) (setq t0 (max t0 r)))
            (if (< r t0) (setq ok nil) (setq t1 (min t1 r))))))))
  (if ok
    (list (list (+ (car a) (* t0 du)) (+ (cadr a) (* t0 dv)))
          (list (+ (car a) (* t1 du)) (+ (cadr a) (* t1 dv)))))
)

;;; "INSIDE" / "PARTIAL" / "OUTSIDE" for a UV segment
(defun asec:classify-in-view (a b vb)
  (cond ((and (asec:uv-in-view-boundary-p a vb) (asec:uv-in-view-boundary-p b vb)) "INSIDE")
        ((asec:clip-segment-to-view-boundary a b vb) "PARTIAL")
        (T "OUTSIDE"))
)

;;; plan segment a-b (floor section start s) overlaps the view boundary
(defun asec:entity-bounds-overlap-view-p (a b s dir vd vb)
  (asec:clip-segment-to-view-boundary (asec:section-coordinate a s dir vd)
                                      (asec:section-coordinate b s dir vd) vb)
)

;;; Projection depth [Point/Distance/Previous]; Point shows a live rectangle preview
(defun asec:get-projection-boundary (m vd / r depth g v done)
  (initget (if *asec-last-projection-depth* "Point Distance Previous" "Point Distance"))
  (setq r (getkword (strcat "\nProjection depth ["
                            (if *asec-last-projection-depth* "Point/Distance/Previous" "Point/Distance")
                            "] <Point>: ")))
  (cond
    ((= r "Distance")
     (initget 7)
     (setq depth (getdist "\nProjection depth: ")))
    ((= r "Previous") (setq depth *asec-last-projection-depth*))
    (T
     (princ "\nPick far edge of projection zone: ")
     (while (not done)
       (setq g (grread T 15 0))
       (cond
         ((member (car g) '(3 5))
          (setq v (asec:dot (asec:v- (asec:2d (trans (cadr g) 1 0)) (car m)) vd))
          (cond
            ((= (car g) 5)
             (if (> v asec:tol)
               (asec:preview-projection-boundary (asec:make-view-boundary (car m) (cadr m) vd v))
               (redraw)))
            ((> v asec:tol) (setq depth v done T))
            (T (princ "\nPoint is on the wrong side of the section line. Pick again: "))))
         ((member (car g) '(11 25)) (exit))))))
  (redraw)
  (setq *asec-last-projection-depth* depth)
  (asec:dbg (strcat "\nVIEW BOUNDARY  depth = " (rtos depth 2 2)
                    "  U 0 -> " (rtos (nth 3 m) 2 2)))
  (asec:make-view-boundary (car m) (cadr m) vd depth)
)

(defun asec:put (k v al)
  (if (assoc k al) (subst (cons k v) (assoc k al) al) (cons (cons k v) al))
)

;;; projected wall cut back to a clipping view boundary. Centerline (u1,v1)-(u2,v2) is
;;; Liang-Barsky clipped; run t0/t1, cstart/cend, U span and Vmid follow. A cut end is a
;;; field limit, not a wall end: its U is stored in 'uclip and no end edge is drawn there.
;;; Fully outside -> record unchanged (discovery already requires overlap).
(defun asec:clip-projected-wall (pw vb / a b c L f0 f1 pad t0 t1 cs ce u1 u2 umin umax cl)
  (setq a (list (asec:get 'u1 pw) (asec:get 'v1 pw))
        b (list (asec:get 'u2 pw) (asec:get 'v2 pw))
        c (asec:clip-segment-to-view-boundary a b vb)
        L (distance a b))
  (if (or (null c) (< L asec:tol))
    pw
    (progn
      (setq f0 (/ (distance a (car c)) L) f1 (/ (distance a (cadr c)) L)
            pad (- (asec:get 'umax pw) (max (car a) (car b)))
            t0 (asec:get 't0 pw) t1 (asec:get 't1 pw)
            cs (asec:get 'cstart pw) ce (asec:get 'cend pw)
            u1 (car (car c)) u2 (car (cadr c))
            umin (max (asec:get 'umin vb) (- (min u1 u2) pad))
            umax (min (asec:get 'umax vb) (+ (max u1 u2) pad)))
      (if (> f0 asec:tol) (setq cl (cons (if (<= u1 u2) umin umax) cl)))
      (if (< f1 (- 1.0 asec:tol)) (setq cl (cons (if (<= u1 u2) umax umin) cl)))
      (if (< (- (min u1 u2) pad) (- (asec:get 'umin vb) asec:tol)) (setq cl (cons umin cl)))
      (if (> (+ (max u1 u2) pad) (+ (asec:get 'umax vb) asec:tol)) (setq cl (cons umax cl)))
      (foreach kv (list (cons 'u1 u1) (cons 'v1 (cadr (car c))) (cons 'u2 u2) (cons 'v2 (cadr (cadr c)))
                        (cons 'umin umin) (cons 'umax umax)
                        (cons 'vmid (* 0.5 (+ (cadr (car c)) (cadr (cadr c)))))
                        (cons 't0 (+ t0 (* f0 (- t1 t0)))) (cons 't1 (+ t0 (* f1 (- t1 t0))))
                        (cons 'cstart (asec:v+ cs (asec:v* (asec:v- ce cs) f0)))
                        (cons 'cend (asec:v+ cs (asec:v* (asec:v- ce cs) f1)))
                        (cons 'uclip cl))
        (setq pw (asec:put (car kv) (cdr kv) pw)))
      pw))
)

(defun asec:clamp-opening-u (o vb)
  (asec:put 'umin (max (asec:get 'umin vb) (asec:get 'umin o))
            (asec:put 'umax (min (asec:get 'umax vb) (asec:get 'umax o)) o))
)

(defun asec:clipped-edge-p (u pw / hit)
  (foreach x (asec:get 'uclip pw) (if (< (abs (- x u)) *asec-occlusion-u-tolerance*) (setq hit T)))
  hit
)

;;; guided projected wall vs boundary: record (+ view-rel) or 'bad
(defun asec:check-projected-wall-view (r ctx / vb rel)
  (setq vb (asec:get 'view ctx))
  (if (null vb)
    r
    (progn
      (setq rel (asec:classify-in-view (list (asec:get 'u1 r) (asec:get 'v1 r))
                                       (list (asec:get 'u2 r) (asec:get 'v2 r)) vb))
      (cond
        ((= rel "OUTSIDE")
         (princ "\nProjected wall is outside the current projection boundary.")
         (initget "Keep Skip")
         (if (= (getkword "\n[Keep/Skip] <Skip>: ") "Keep") (cons (cons 'view-rel rel) r) 'bad))
        ((= rel "PARTIAL")
         (princ "\nProjected wall crosses the projection boundary.")
         (cons (cons 'view-rel rel) r))
        (T (cons (cons 'view-rel rel) r)))))
)

;;; unhosted projected opening vs boundary: record or 'bad
(defun asec:check-unhosted-opening-view (rec ctx / vb)
  (setq vb (asec:get 'view ctx))
  (if (and vb
           (= "OUTSIDE" (asec:classify-in-view (list (asec:get 'umin rec) (asec:get 'v rec))
                                               (list (asec:get 'umax rec) (asec:get 'v rec)) vb)))
    (progn
      (princ "\nProjected opening is outside the current projection boundary.")
      (initget "Keep Skip")
      (if (= (getkword "\n[Keep/Skip] <Skip>: ") "Keep") rec 'bad))
    rec)
)

;;; ---------- settings (session-global; SECTSET is only a UI over this) ----------
;;; ponytail: session-only global; persist via setcfg/dictionary if needed across sessions
(setq asec:defaults
  '(("GF_HEIGHT"      . 3200.0)
    ("TYP_HEIGHT"     . 3000.0)
    ("SLAB_THK"       . 150.0)
    ("WALL_TOP"       . "TO_SLAB_UNDERSIDE")
    ("SLAB_EXTENT"    . "AUTO")
    ("GROUND_LINE"    . "YES")
    ("DOOR_HEIGHT"    . 2100.0)
    ("WIN_HEIGHT"     . 1500.0)
    ("WIN_SILL"       . 900.0)
    ("FRAME_THK"      . 50.0)
    ;; placeholders for future modules (unused)
    ("BEAM_DEPTH"     . 450.0)
    ("PARAPET_HEIGHT" . 1050.0)))

(if (null asec:settings) (setq asec:settings asec:defaults))

(defun asec:setting (k) (asec:get k asec:settings))

(defun asec:set-setting (k v)
  (setq asec:settings (subst (cons k v) (assoc k asec:settings) asec:settings))
)

(defun asec:get-config ()
  (list (asec:setting "GF_HEIGHT") (asec:setting "TYP_HEIGHT") (asec:setting "SLAB_THK"))
)

;;; ---------- SECTSET command-line UI ----------
(defun asec:ask-real (k msg / v)
  (initget 6)
  (if (setq v (getreal (strcat "\n" msg " <" (rtos (asec:setting k) 2 0) ">: ")))
    (asec:set-setting k v))
)

;;; kws = list of (keyword . value)
(defun asec:ask-kw (k msg kws / cur r)
  (setq cur (car (nth (asec:index-of (asec:setting k) (mapcar 'cdr kws)) kws)))
  (initget (apply 'strcat (mapcar '(lambda (x) (strcat (car x) " ")) kws)))
  (if (setq r (getkword (strcat "\n" msg " <" cur ">: ")))
    (asec:set-setting k (asec:get r kws)))
)

(defun asec:index-of (x lst / i r)
  (setq i 0)
  (foreach e lst (if (and (null r) (equal e x)) (setq r i)) (setq i (1+ i)))
  (if r r 0)
)

(defun c:SECTSET ()
  (princ "\nAUTO SECTION SETTINGS")
  (princ "\n-- BUILDING --")
  (asec:ask-real "GF_HEIGHT" "Ground Floor Height")
  (asec:ask-real "TYP_HEIGHT" "Typical Floor Height")
  (asec:ask-real "SLAB_THK" "Slab Thickness")
  (asec:ask-kw "SLAB_EXTENT" "Default Slab Extent [Auto/Manual/None]" asec:slab-kws)
  (asec:ask-kw "GROUND_LINE" "Ground Line [Yes/No]" '(("Yes" . "YES") ("No" . "NO")))
  (princ "\n-- DOORS --")
  (asec:ask-real "DOOR_HEIGHT" "Door Height")
  (princ "\n-- WINDOWS --")
  (asec:ask-real "WIN_SILL" "Window Sill Height")
  (asec:ask-real "WIN_HEIGHT" "Window Height")
  (asec:ask-real "FRAME_THK" "Frame Thickness")
  (princ "\n-- FUTURE PLACEHOLDERS (not used yet) --")
  (asec:ask-real "BEAM_DEPTH" "Beam Depth")
  (asec:ask-real "PARAPET_HEIGHT" "Parapet Height")
  (princ "\nSettings updated.")
  (princ)
)

;;; ---------- slab limits (per floor) ----------
(setq asec:slab-kws '(("Auto" . "AUTO") ("Manual" . "MANUAL") ("None" . "NONE")))

;;; (u0 u1) spanning outermost faces of walls, or nil
(defun asec:wall-bounds (walls / us)
  (foreach w walls (setq us (cons (asec:get 'umin w) (cons (asec:get 'umax w) us))))
  (if us (list (apply 'min us) (apply 'max us)))
)

;;; station of a picked LINE crossing section se; nil if user presses Enter
(defun asec:pick-slab-edge (msg se dir / sel ed ip r done)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq sel (entsel msg))
    (cond
      ((null sel)
       (if (= (getvar "ERRNO") 7) (princ "\nNothing selected.") (setq done T)))
      ((/= (asec:get 0 (setq ed (entget (car sel)))) "LINE")
       (princ "\nSlab edge must be a straight LINE."))
      ((null (setq ip (inters (car se) (cadr se)
                              (asec:2d (asec:get 10 ed)) (asec:2d (asec:get 11 ed)) T)))
       (princ "\nSelected slab edge does not intersect the section line."))
      (T (setq r (asec:station-on-section (asec:2d ip) (car se) dir) done T))
    )
  )
  r
)

;;; (u0 u1) from two slab-edge LINEs, or nil
(defun asec:get-manual-slab (se dir / a b)
  (if (and (setq a (asec:pick-slab-edge "\nSelect first slab-edge LINE: " se dir))
           (setq b (asec:pick-slab-edge "\nSelect second slab-edge LINE: " se dir)))
    (if (< (abs (- b a)) asec:tol)
      (progn (princ "\nZero-length slab; no slab created.") nil)
      (list (min a b) (max a b))))
)

;;; [Manual/None] <Manual> fallback; returns (u0 u1) or nil
(defun asec:ask-manual-or-none (msg se dir)
  (initget "Manual None")
  (if (= (getkword msg) "None")
    nil
    (asec:get-manual-slab se dir))
)

;;; returns (u0 u1) or nil for no slab
(defun asec:get-floor-slab (i se m walls / def r)
  (setq def (car (nth (asec:index-of (asec:setting "SLAB_EXTENT") (mapcar 'cdr asec:slab-kws))
                      asec:slab-kws)))
  (initget "Auto Manual None")
  (setq r (getkword (strcat "\n" (asec:floor-name i) " slab extent [Auto/Manual/None] <" def ">: ")))
  (if (null r) (setq r def))
  (cond
    ((= r "None") nil)
    ((= r "Manual") (asec:get-manual-slab se (caddr m)))
    ((asec:wall-bounds walls))
    (T (princ "\nNo cut walls available to determine automatic slab limits.")
       (asec:ask-manual-or-none "\nSlab extent [Manual/None] <Manual>: " se (caddr m)))
  )
)

;;; roof/terrace slab: isolated for future parapet/roof modules
(defun asec:get-roof-slab (top dir)
  (initget "Yes No")
  (cond
    ((= (getkword "\nAdd roof/terrace slab? [Yes/No] <Yes>: ") "No") nil)
    ((asec:get 'slab top))
    (T (princ "\nTop floor has no slab extent.")
       (asec:ask-manual-or-none "\nRoof slab extent [Manual/None] <Manual>: "
                                (list (asec:get 'start top) (asec:get 'end top)) dir))
  )
)

(defun asec:floor-elevation (i cfg)
  (if (= i 0) 0.0 (+ (car cfg) (* (1- i) (cadr cfg))))
)

(defun asec:getpt (msg / p)
  (setq p (getpoint msg))
  (if (null p) (exit))
  (asec:2d (trans p 1 0))
)

;;; ---------- floors ----------
(defun asec:derive-floor-section (m gfref ref / tv)
  (setq tv (asec:v- ref gfref))
  (list (asec:v+ (car m) tv) (asec:v+ (cadr m) tv))
)

(defun asec:floor-name (i) (if (= i 0) "GROUND FLOOR" (strcat "FLOOR " (itoa i))))

;;; ---- shared by automatic discovery and manual collectors ----
;;; (ref se): picks the matching reference point for upper floors unless ref is
;;; given, derives the translated section and previews its view boundary.
(defun asec:floor-section (i m vd vb gfref ref / se)
  (if (null ref)
    (setq ref (if (= i 0)
                gfref
                (asec:getpt (strcat "\nPick matching reference point for Floor " (itoa i) ": ")))))
  (setq se (asec:derive-floor-section m gfref ref))
  (asec:preview-projection-boundary (asec:make-view-boundary (car se) (cadr se) vd (asec:get 'depth vb)))
  (list ref se)
)

(defun asec:make-floor-ctx (i se m vd vb cfg walls)
  (list (cons 'fname (asec:floor-name i)) (cons 'se se) (cons 'dir (caddr m))
        (cons 'vd vd) (cons 'elev (asec:floor-elevation i cfg)) (cons 'walls walls)
        (cons 'top (- (asec:floor-elevation (1+ i) cfg) (caddr cfg)))
        (cons 'view vb))
)

;;; A view boundary with (clip . T) (ASA field) is a hard source limit: projected walls are
;;; cut back to it and projected opening U spans clamped to its U range.
(defun asec:make-floor-record (i ref se ctx pws cd cw pd pw slab / vb)
  (if (asec:get 'clip (setq vb (asec:get 'view ctx)))
    (setq pws (mapcar '(lambda (w) (asec:clip-projected-wall w vb)) pws)
          pd  (mapcar '(lambda (o) (asec:clamp-opening-u o vb)) pd)
          pw  (mapcar '(lambda (o) (asec:clamp-opening-u o vb)) pw)))
  (list (cons 'index i) (cons 'ref ref)
        (cons 'start (car se)) (cons 'end (cadr se))
        (cons 'elev (asec:get 'elev ctx))
        (cons 'walls (asec:get 'walls ctx))
        (cons 'pwalls pws)
        (cons 'cutdoors cd) (cons 'cutwins cw)
        (cons 'projdoors pd) (cons 'projwins pw)
        (cons 'slab slab))
)

;;; guided floor collection; ref nil = pick it
(defun asec:add-floor (i m vd vb gfref cfg ref / fs se walls ctx cd cw pws pctx pd pw)
  (setq fs (asec:floor-section i m vd vb gfref ref) se (cadr fs))
  (princ (strcat "\n" (asec:floor-name i) " - CUT WALLS"))
  (setq walls (asec:collect-walls se (caddr m))
        ctx   (asec:make-floor-ctx i se m vd vb cfg walls)
        cd    (asec:collect-openings "DOOR" "CUT" ctx)
        cw    (asec:collect-openings "WINDOW" "CUT" (cons (cons 'prior cd) ctx))
        pws   (asec:collect-projected-walls ctx)
        pctx  (cons (cons 'pwalls pws) ctx)
        pd    (asec:collect-openings "DOOR" "PROJECTED" pctx)
        pw    (asec:collect-openings "WINDOW" "PROJECTED" pctx))
  (asec:make-floor-record i (car fs) se ctx pws cd cw pd pw (asec:get-floor-slab i se m walls))
)

;;; ---------- geometry engine ----------
(defun asec:line-intersection (a1 a2 b1 b2) (inters a1 a2 b1 b2 T))

(defun asec:section-coordinate (p s dir viewdir)
  (list (asec:dot (asec:v- p s) dir) (asec:dot (asec:v- p s) viewdir))
)

(defun asec:station-on-section (p s dir) (asec:dot (asec:v- p s) dir))

;;; max distance a broken wall face may be extended without confirmation
(if (null *asec-wall-extension-limit*) (setq *asec-wall-extension-limit* 3000.0))

;;; point p lies on finite section se (within tol along its length)
(defun asec:on-section-p (p se / u)
  (setq u (asec:dot (asec:v- p (car se))
                    (asec:v* (asec:v- (cadr se) (car se)) (/ 1.0 (distance (car se) (cadr se))))))
  (and (>= u (- asec:tol)) (<= u (+ (distance (car se) (cadr se)) asec:tol)))
)

;;; LINE face -> (pt . note) or 'bad. Direct crossing first; otherwise the
;;; LINE's infinite supporting line is intersected with the finite section.
(defun asec:line-face (ed se / a b ip ext)
  (setq a (asec:2d (asec:get 10 ed)) b (asec:2d (asec:get 11 ed)))
  (cond
    ((< (distance a b) asec:tol) (princ "\nWall face LINE has zero length.") 'bad)
    ((setq ip (asec:line-intersection (car se) (cadr se) a b))
     (list (asec:2d ip) "direct" (list a b)))
    ((or (null (setq ip (inters (car se) (cadr se) a b nil)))
         (not (asec:on-section-p (setq ip (asec:2d ip)) se)))
     (princ "\nSelected wall face does not intersect the section line, even when extended.") 'bad)
    (T
     (setq ext (min (distance ip a) (distance ip b)))
     (if (<= ext *asec-wall-extension-limit*)
       (list ip (strcat "extended " (rtos ext 2 0)) (list a b))
       (progn
         (princ (strcat "\nPossible incorrect wall face. Required extension: " (rtos ext 2 0)))
         (initget "Yes No")
         (if (= (getkword "\nUse this face? [Yes/No] <No>: ") "Yes")
           (list ip (strcat "extended " (rtos ext 2 0)) (list a b))
           'bad))))
  )
)

;;; Point fallback: pick the theoretical face point, projected onto section
(defun asec:point-face (se / p u d)
  (if (setq p (getpoint "\nWall face station point: "))
    (progn
      (setq d (asec:v* (asec:v- (cadr se) (car se)) (/ 1.0 (distance (car se) (cadr se))))
            u (asec:dot (asec:v- (asec:2d (trans p 1 0)) (car se)) d)
            p (asec:v+ (car se) (asec:v* d u)))
      (if (asec:on-section-p p se)
        (list p "point" nil)
        (progn (princ "\nPoint projects outside the section line.") 'bad)))
    'bad)
)

;;; returns (pt note (a b)|nil), "Undo", nil (done), or 'bad
;;; (a b) = source face LINE, kept for plan-space wall-strip hosting
(defun asec:validate-wall-face (msg kw se / sel ed)
  (setvar "ERRNO" 0)
  (initget kw)
  (setq sel (entsel msg))
  (cond
    ((= sel "Point") (asec:point-face se))
    ((= (type sel) 'STR) sel)
    ((null sel) (if (= (getvar "ERRNO") 7) (progn (princ "\nNothing selected.") 'bad) nil))
    ((/= (asec:get 0 (setq ed (entget (car sel)))) "LINE")
     (princ "\nWall face must be a straight LINE.") 'bad)
    (T (asec:line-face ed se))
  )
)

;;; returns wall record, "Undo", "Done"/nil, or 'bad
(defun asec:get-wall-pair (se dir / p1 p2 s1 s2)
  (setq p1 (asec:validate-wall-face "\nSelect first face of wall [Point/Undo/Done] <Done>: "
                                    "Point Undo Done" se))
  (cond
    ((or (null p1) (= p1 "Done")) nil)
    ((or (= p1 'bad) (= p1 "Undo")) p1)
    (T
     (setq p2 (asec:validate-wall-face "\nSelect second face [Point]: " "Point" se))
     (cond
       ((or (null p2) (= p2 'bad) (= (type p2) 'STR)) (princ " Wall discarded.") 'bad)
       (T
        (setq s1 (asec:station-on-section (car p1) (car se) dir)
              s2 (asec:station-on-section (car p2) (car se) dir))
        (if (< (abs (- s2 s1)) asec:tol)
          (progn (princ "\nWall faces produce zero thickness. Please select again.") 'bad)
          (list (cons 'p1 (car p1)) (cons 'p2 (car p2)) (cons 's1 s1) (cons 's2 s2)
                (cons 'f1 (cadr p1)) (cons 'f2 (cadr p2))
                (cons 'l1 (caddr p1)) (cons 'l2 (caddr p2))
                (cons 'umin (min s1 s2)) (cons 'umax (max s1 s2))
                (cons 'thk (abs (- s2 s1))))))))
  )
)

(defun asec:collect-walls (se dir / walls r)
  (while (setq r (asec:get-wall-pair se dir))
    (cond
      ((= r 'bad))
      ((= r "Undo")
       (if walls
         (progn (asec:unlabel (asec:get 'label (car walls)))
                (setq walls (cdr walls)) (princ "\nLast wall removed."))
         (princ "\nNo wall to undo.")))
      (T
       (setq walls (cons (cons (cons 'label
                                     (asec:label (asec:v* (asec:v+ (asec:get 'p1 r) (asec:get 'p2 r)) 0.5)
                                                 (strcat "W" (itoa (1+ (length walls)))) se))
                               r)
                         walls))
       (princ (strcat "\nWall " (itoa (length walls))
                      ":  Face 1: " (asec:get 'f1 r) "  Face 2: " (asec:get 'f2 r)
                      "  Thickness: " (rtos (asec:get 'thk r) 2 2))))
    )
  )
  (princ (strcat "\n" (itoa (length walls)) " wall(s) recorded."))
  (reverse walls)
)

;;; ---------- block reading ----------
(defun asec:get-block-data (e / ed ex)
  (setq ed (entget e) ex (asec:get 210 ed))
  (list (cons 'ename e) (cons 'name (asec:get 2 ed))
        (cons 'ins (asec:2d (trans (asec:get 10 ed) e 0)))
        (cons 'rot (cond ((asec:get 50 ed)) (0.0)))
        (cons 'sx (cond ((asec:get 41 ed)) (1.0)))
        (cons 'sy (cond ((asec:get 42 ed)) (1.0)))
        (cons 'layer (asec:get 8 ed))
        (cons 'flip (and ex (< (caddr ex) 0))))
)

;;; returns nil after printing a message, or block data
(defun asec:validate-opening-block (e kind / ed lay)
  (setq ed (entget e) lay (strcat "A-" kind))
  (if (and (= (asec:get 0 ed) "INSERT") (= (strcase (asec:get 8 ed)) lay))
    (asec:get-block-data e)
    (progn (princ (strcat "\nSelected object is not a " (strcase kind T)
                          " block on layer " lay ". Please select again."))
           nil))
)

;;; angle a lies on CCW arc a0 -> a1
(defun asec:ang-on-arc (a a0 a1 / two)
  (setq two (* 2 pi))
  (<= (rem (+ (rem (- a a0) two) two) two) (rem (+ (rem (- a1 a0) two) two) two))
)

;;; (unreliable-flag . block-local 2D points)
(defun asec:get-block-geometry (name / e ed typ pts bad c r a0 a1)
  (setq e (tblobjname "BLOCK" name))
  (if (null e) (setq bad T))
  (while (and e (setq e (entnext e))
              (/= (setq typ (asec:get 0 (setq ed (entget e)))) "ENDBLK"))
    (if (and (asec:get 210 ed) (< (caddr (asec:get 210 ed)) -0.5)) (setq bad T))
    (cond
      ((= typ "LINE")
       (setq pts (cons (asec:2d (asec:get 10 ed)) (cons (asec:2d (asec:get 11 ed)) pts))))
      ((= typ "LWPOLYLINE")
       (foreach g ed (if (= (car g) 10) (setq pts (cons (asec:2d (cdr g)) pts)))))
      ((member typ '("ARC" "CIRCLE"))
       (setq c (asec:get 10 ed) r (asec:get 40 ed)
             a0 (cond ((asec:get 50 ed)) (0.0)) a1 (cond ((asec:get 51 ed)) ((* 2 pi))))
       (if (= typ "ARC")
         (setq pts (cons (asec:2d (polar c a0 r)) (cons (asec:2d (polar c a1 r)) pts))))
       (foreach q (list 0.0 (* 0.5 pi) pi (* 1.5 pi))
         (if (or (= typ "CIRCLE") (asec:ang-on-arc q a0 a1))
           (setq pts (cons (asec:2d (polar c q r)) pts)))))
      ((= typ "INSERT") (setq bad T))   ; ponytail: nested blocks -> manual jambs
    )
  )
  (cons bad pts)
)

;;; single source of truth for block-local -> WCS
(defun asec:transform-block-point (bd p base / lx ly c s)
  (setq lx (* (- (car p) (car base)) (asec:get 'sx bd))
        ly (* (- (cadr p) (cadr base)) (asec:get 'sy bd))
        c  (cos (asec:get 'rot bd))
        s  (sin (asec:get 'rot bd)))
  (asec:v+ (asec:get 'ins bd) (list (- (* lx c) (* ly s)) (+ (* lx s) (* ly c))))
)

;;; (J1 J2 width halfdepth) from block-local X extent, or nil if not reliable
;;; halfdepth = half the block's local Y extent (swing arcs etc.), used by hosting
(defun asec:get-block-opening-span (bd / g pts base xs ys xmin xmax ym)
  (setq g (asec:get-block-geometry (asec:get 'name bd)) pts (cdr g))
  (if (and (not (car g)) pts (not (asec:get 'flip bd)))
    (progn
      (setq base (asec:2d (asec:get 10 (entget (tblobjname "BLOCK" (asec:get 'name bd)))))
            xs (mapcar 'car pts) ys (mapcar 'cadr pts)
            xmin (apply 'min xs) xmax (apply 'max xs)
            ym (* 0.5 (+ (apply 'min ys) (apply 'max ys))))
      (if (> (* (- xmax xmin) (abs (asec:get 'sx bd))) asec:tol)
        (asec:jambs (asec:transform-block-point bd (list xmin ym) base)
                    (asec:transform-block-point bd (list xmax ym) base)
                    (* 0.5 (- (apply 'max ys) (apply 'min ys)) (abs (asec:get 'sy bd)))))))
)

(defun asec:jambs (j1 j2 hd) (list j1 j2 (distance j1 j2) hd))

;;; (J1 J2 width) from picked jambs, or nil
(defun asec:get-manual-opening-span (/ p1 p2)
  (if (and (setq p1 (getpoint "\nSpecify first jamb: "))
           (setq p2 (getpoint p1 "\nSpecify second jamb: ")))
    (if (< (distance p1 p2) asec:tol)
      (progn (princ "\nJambs coincide. Please select again.") nil)
      (asec:jambs (asec:2d (trans p1 1 0)) (asec:2d (trans p2 1 0)) 0.0)))
)

(defun asec:get-opening-jambs (bd kind / auto)
  (if (setq auto (asec:get-block-opening-span bd))
    (progn
      (princ (strcat "\n" (asec:cap kind) " detected. Opening width: " (rtos (caddr auto) 2 2)))
      (initget "Accept Manual")
      (if (= (getkword "\n[Accept/Manual] <Accept>: ") "Manual")
        (asec:get-manual-opening-span)
        auto))
    (progn
      (princ "\nOpening width could not be determined reliably.")
      (asec:get-manual-opening-span)))
)

(defun asec:cap (kind) (if (= kind "DOOR") "Door" "Window"))

;;; ---------- opening selection ----------
(defun asec:opening-heights (kind elev / sill)
  ;; (bottom sill height) - stored per opening so later overrides need no generator change
  (if (= kind "DOOR")
    (list elev nil (asec:setting "DOOR_HEIGHT"))
    (list (+ elev (setq sill (asec:setting "WIN_SILL"))) sill (asec:setting "WIN_HEIGHT")))
)

;;; ---------- cut opening hosting ----------
;;; Host = the cut wall whose PLAN-SPACE strip (between its two face lines)
;;; contains the opening. Cut void = host wall's full U band.
(if (null *asec-wall-host-tolerance*) (setq *asec-wall-host-tolerance* 10.0))

(defun asec:unit (v / l) (setq l (distance '(0 0) v)) (asec:v* v (/ 1.0 l)))
(defun asec:perp (v) (list (- (cadr v)) (car v)))
(defun asec:pt-str (p) (strcat "(" (rtos (car p) 2 2) ", " (rtos (cadr p) 2 2) ")"))

;;; (p1 n1 p2 n2): face points on section + aligned face normals, or nil if
;;; both faces are Point-mode. One Point face borrows the other face's direction.
(defun asec:wall-strip (wl / l1 l2 d1 d2 n1 n2)
  (setq l1 (asec:get 'l1 wl) l2 (asec:get 'l2 wl))
  (if (or l1 l2)
    (progn
      (setq d1 (asec:unit (apply 'asec:v- (reverse (if l1 l1 l2))))
            d2 (asec:unit (apply 'asec:v- (reverse (if l2 l2 l1))))
            n1 (asec:perp d1)
            n2 (asec:perp d2))
      (if (< (asec:dot n1 n2) 0) (setq n2 (asec:v* n2 -1.0)))
      (list (asec:get 'p1 wl) n1 (asec:get 'p2 wl) n2)))
)

;;; signed offsets of p from face 1 and face 2; inside strip when signs differ
(defun asec:strip-offsets (p st)
  (list (asec:dot (asec:v- p (car st)) (cadr st))
        (asec:dot (asec:v- p (caddr st)) (cadddr st)))
)

;;; opening band [mid-hd, mid+hd] across the wall overlaps the strip
(defun asec:point-in-wall-strip (d hd / tl)
  (setq tl (+ hd *asec-wall-host-tolerance*))
  (and (<= (min (car d) (cadr d)) tl) (>= (max (car d) (cadr d)) (- tl)))
)

(defun asec:ask-host (nums / r)
  (princ "\nCandidate walls:")
  (foreach k nums (princ (strcat " Wall " (itoa k))))
  (setq r (getint "\nSelect host wall number <Skip>: "))
  (cond ((member r nums) r)
        (T (if r (princ "\nNot a candidate wall.")) 'bad))
)

;;; returns wall number, 'bad, or 'done
(defun asec:find-host-wall (jm ctx / se dir walls mid midu hd k cu st d cands unk
                                   near nd best bd r all)
  (setq se (asec:get 'se ctx) dir (asec:get 'dir ctx) walls (asec:get 'walls ctx)
        mid (asec:v* (asec:v+ (car jm) (cadr jm)) 0.5)
        midu (asec:station-on-section mid (car se) dir)
        hd (cadddr jm) k 0)
  (foreach wl walls
    (setq k (1+ k) all (cons k all)
          cu (* 0.5 (+ (asec:get 'umin wl) (asec:get 'umax wl))))
    (asec:dbg (strcat "\nWall " (itoa k) ":  U = " (rtos (asec:get 'umin wl) 2 2)
                   " -> " (rtos (asec:get 'umax wl) 2 2) "  center U = " (rtos cu 2 2)))
    (if (setq st (asec:wall-strip wl))
      (progn
        (setq d (asec:strip-offsets mid st))
        (asec:dbg (strcat "  face offsets = " (rtos (car d) 2 2) " / " (rtos (cadr d) 2 2)))
        (if (asec:point-in-wall-strip d hd)
          (progn (asec:dbg "  inside strip = Yes") (setq cands (cons k cands)))
          (progn
            (setq nd (- (min (abs (car d)) (abs (cadr d))) hd))
            (asec:dbg (strcat "  inside strip = No  distance = " (rtos nd 2 2)))
            (if (or (null near) (< nd (cdr near))) (setq near (cons k nd))))))
      (progn
        (asec:dbg "  inside strip = Unknown (Point-mode faces)")
        (if (<= (abs (- cu midu)) (max (caddr jm) (asec:get 'thk wl)))
          (setq unk (cons k unk))))))
  (setq cands (reverse cands) unk (reverse unk) all (reverse all))
  (cond
    ((= (length cands) 1) (car cands))
    (cands
     ;; several strips contain it: closest wall center U to opening midpoint U
     (foreach c cands
       (setq cu (abs (- midu (* 0.5 (+ (asec:get 'umin (nth (1- c) walls))
                                        (asec:get 'umax (nth (1- c) walls)))))))
       (cond ((or (null best) (< cu (- bd asec:tol))) (setq best c bd cu))
             ((< (abs (- cu bd)) asec:tol) (setq best 'tie))))
     (if (= best 'tie)
       (progn (princ "\nOpening matches multiple cut walls.") (asec:ask-host cands))
       best))
    (unk
     (princ "\nOpening host cannot be determined automatically for this wall.")
     (asec:ask-host unk))
    (T
     (princ (strcat "\nNo cut wall contains this opening.  Opening midpoint: " (asec:pt-str mid)))
     (if near
       (princ (strcat "\nNearest cut wall: Wall " (itoa (car near))
                      "  Distance: " (rtos (cdr near) 2 2))))
     (initget "Reselect Assign Skip")
     (setq r (getkword "\n[Reselect/Assign/Skip] <Reselect>: "))
     (cond ((= r "Assign") (if all (asec:ask-host all) (progn (princ "\nNo cut walls recorded.") 'bad)))
           ((= r "Skip") 'done)
           (T 'bad))))
)

;;; section cut point in host wall (midpoint of its two face stations) lies
;;; between the jambs, measured along the jamb direction
(defun asec:section-crosses-opening (jm wl / w cut a)
  (setq w   (asec:unit (asec:v- (cadr jm) (car jm)))
        cut (asec:v* (asec:v+ (asec:get 'p1 wl) (asec:get 'p2 wl)) 0.5)
        a   (asec:dot (asec:v- (car jm) cut) w))
  (and (<= a asec:tol) (>= (+ a (caddr jm)) (- asec:tol)))
)

;;; diagnostics: off by default; (setq *asec-debug* T) to re-enable
(if (null (boundp '*asec-debug-set*)) (setq *asec-debug* nil *asec-debug-set* T))

;;; prints only when *asec-debug*; always returns T (safe inside cond tests)
(defun asec:dbg (s) (if *asec-debug* (princ s)) T)

(defun asec:debug-cut-opening (bd jm kind ctx / se dir vd mid)
  (setq se (asec:get 'se ctx) dir (asec:get 'dir ctx) vd (asec:get 'vd ctx)
        mid (asec:v* (asec:v+ (car jm) (cadr jm)) 0.5))
  (asec:dbg (strcat "\n--- CUT " kind " DEBUG ---"
                 "\nBlock: " (asec:get 'name bd) "  Insertion: " (asec:pt-str (asec:get 'ins bd))
                 "  Rotation: " (rtos (* 180.0 (/ (asec:get 'rot bd) pi)) 2 2)
                 "  Scale: (" (rtos (asec:get 'sx bd) 2 3) ", " (rtos (asec:get 'sy bd) 2 3) ")"
                 "\nJamb 1: " (asec:pt-str (car jm)) "  Jamb 2: " (asec:pt-str (cadr jm))
                 "  Midpoint: " (asec:pt-str mid)
                 "\nOpening width: " (rtos (caddr jm) 2 2) "  Half depth: " (rtos (cadddr jm) 2 2)
                 "\nSection: " (asec:pt-str (car se)) " -> " (asec:pt-str (cadr se))
                 "\nJamb 1 U: " (rtos (asec:station-on-section (car jm) (car se) dir) 2 2)
                 "  Jamb 2 U: " (rtos (asec:station-on-section (cadr jm) (car se) dir) 2 2)
                 "  Midpoint U: " (rtos (asec:station-on-section mid (car se) dir) 2 2)
                 "  Midpoint V: " (rtos (asec:dot (asec:v- mid (car se)) vd) 2 2)))
)

;;; existing opening in same wall whose U and Z ranges overlap rec
(defun asec:validate-wall-openings (rec others / hit b0 b1)
  (setq b0 (asec:get 'bottom rec) b1 (+ b0 (asec:get 'height rec)))
  (foreach o others
    (if (and (= (asec:get 'host o) (asec:get 'host rec))
             (< (asec:get 'umin rec) (- (asec:get 'umax o) asec:tol))
             (> (asec:get 'umax rec) (+ (asec:get 'umin o) asec:tol))
             (< b0 (- (+ (asec:get 'bottom o) (asec:get 'height o)) asec:tol))
             (> b1 (+ (asec:get 'bottom o) asec:tol)))
      (setq hit o)))
  hit
)

(defun asec:make-opening (kind mode bd jm host umin umax ctx typ / h mid)
  (setq h (asec:opening-heights kind (asec:get 'elev ctx))
        mid (asec:v* (asec:v+ (car jm) (cadr jm)) 0.5))
  (list (cons 'kind kind) (cons 'mode mode)
        (cons 'ename (asec:get 'ename bd)) (cons 'name (asec:get 'name bd))
        (cons 'host host)
        (cons 'u (* 0.5 (+ umin umax)))
        (cons 'v (asec:dot (asec:v- mid (car (asec:get 'se ctx))) (asec:get 'vd ctx)))
        (cons 'umin umin) (cons 'umax umax) (cons 'width (caddr jm))
        (cons 'bottom (car h)) (cons 'sill (cadr h)) (cons 'height (caddr h))
        (cons 'type typ))
)

(defun asec:report-opening (o / s)
  (setq s (strcat "\n" (if (= (asec:get 'mode o) "CUT") "Cut " "Projected ")
                  (strcase (asec:get 'kind o) T) " detected."
                  (if (asec:get 'type o) (strcat "  Type: " (asec:cap-word (asec:get 'type o))) "")
                  "  Width: " (rtos (asec:get 'width o) 2 2)))
  (if (asec:get 'sill o) (setq s (strcat s "  Sill: " (rtos (asec:get 'sill o) 2 2))))
  (setq s (strcat s "  Height: " (rtos (asec:get 'height o) 2 2)))
  (princ (strcat s (if (asec:get 'host o)
                     (strcat "  Host wall: Wall " (itoa (asec:get 'host o)))
                     (strcat "  Depth: " (rtos (asec:get 'v o) 2 2)
                             (if (asec:get 'phost o)
                               (strcat "  Projected wall: " (itoa (asec:get 'phost o)))
                               "")))))
)

(defun asec:cap-word (s) (strcat (substr s 1 1) (strcase (substr s 2) T)))

;;; returns opening record, 'bad, or 'done
(defun asec:opening-section-position (bd jm kind mode ctx lst / se dir w host rec u0 u1 hit typ)
  (setq se (asec:get 'se ctx) dir (asec:get 'dir ctx))
  (cond
    ((= mode "CUT")
     (asec:debug-cut-opening bd jm kind ctx)
     (setq host (asec:find-host-wall jm ctx))
     (cond
       ((not (numberp host)) host)
       ((not (asec:dbg (strcat "\nChosen host: Wall " (itoa host)))))
       ((not (setq w (nth (1- host) (asec:get 'walls ctx)))))
       ((not (asec:section-crosses-opening jm w))
        (asec:dbg "\nSection crosses opening: No")
        (princ (strcat "\nSelected " (strcase kind T) " is not crossed by the section line."))
        (initget "Reselect Skip")
        (if (= (getkword "\n[Reselect/Skip] <Reselect>: ") "Skip") 'done 'bad))
       ((setq hit (asec:validate-wall-openings
                    ;; void spans host wall's cut thickness, not the plan opening width
                    (setq rec (asec:make-opening kind mode bd jm host
                                                 (asec:get 'umin w) (asec:get 'umax w) ctx nil))
                    (append (asec:get 'prior ctx) lst)))
        (princ (strcat "\nThis " (strcase kind T) " overlaps a recorded "
                       (strcase (asec:get 'kind hit) T) " in Wall " (itoa host)
                       ". Please select again."))
        'bad)
       (T
        (asec:dbg (strcat "\nSection crosses opening: Yes"
                       "\nVoid: U = " (rtos (asec:get 'umin rec) 2 2) " -> " (rtos (asec:get 'umax rec) 2 2)
                       "  Z = " (rtos (asec:get 'bottom rec) 2 2) " -> "
                       (rtos (+ (asec:get 'bottom rec) (asec:get 'height rec)) 2 2)))
        rec)))
    (T
     (setq u0 (asec:station-on-section (car jm) (car se) dir)
           u1 (asec:station-on-section (cadr jm) (car se) dir))
     (if (< (abs (- u1 u0)) asec:tol)
       (progn (princ "\nOpening is edge-on to the section (no visible width). Please select again.") 'bad)
       (progn
         (if (= kind "DOOR")
           (progn (initget "Single Double")
                  (setq typ (if (= (getkword "\nDoor type [Single/Double] <Single>: ") "Double")
                              "DOUBLE" "SINGLE"))))
         (asec:host-projected-opening
           (asec:make-opening kind mode bd jm nil (min u0 u1) (max u0 u1) ctx typ) jm ctx)))))
)

;;; returns opening record, 'undo, 'bad, or 'done
(defun asec:get-opening (kind mode ctx lst / sel bd jm)
  (setvar "ERRNO" 0)
  (initget "Undo Done")
  (setq sel (entsel (strcat "\nSelect " (strcase mode T) " " (strcase kind T) " [Undo/Done] <Done>: ")))
  (cond
    ((= (type sel) 'STR) (if (= sel "Undo") 'undo 'done))
    ((null sel) (if (= (getvar "ERRNO") 7) (progn (princ "\nNothing selected.") 'bad) 'done))
    ((null (setq bd (asec:validate-opening-block (car sel) kind))) 'bad)
    ((null (setq jm (asec:get-opening-jambs bd kind))) 'bad)
    (T (asec:opening-section-position bd jm kind mode ctx lst))
  )
)

;;; one stage: kind "DOOR"/"WINDOW", mode "CUT"/"PROJECTED"
(defun asec:collect-openings (kind mode ctx / lst r)
  (princ (strcat "\n" (asec:get 'fname ctx) " - " mode " " kind "S"))
  (while (/= 'done (setq r (asec:get-opening kind mode ctx lst)))
    (cond
      ((= r 'undo)
       (if lst
         (progn (setq lst (cdr lst)) (princ "\nLast opening removed."))
         (princ "\nNothing to undo.")))
      ((= r 'bad))
      (T (setq lst (cons r lst)) (asec:report-opening r))
    )
  )
  (reverse lst)
)

;;; ---------- projected walls (MS3) ----------
;;; A projected wall is defined by its two face lines in plan. Its section span
;;; comes from projecting the centerline run onto U; depth from V at both ends.
(if (null *asec-parallel-tolerance*) (setq *asec-parallel-tolerance* 0.02)) ; sin of max face angle (~1.1 deg)

;;; returns (a b) face line, "Undo", nil (done), or 'bad. Point = two picked points on the face.
(defun asec:get-projected-face (msg kw / sel ed a b p q)
  (setvar "ERRNO" 0)
  (initget kw)
  (setq sel (entsel msg))
  (cond
    ((= sel "Point")
     (if (and (setq p (getpoint "\nFirst point on wall face: "))
              (setq q (getpoint p "\nSecond point on wall face: ")))
       (progn
         (setq a (asec:2d (trans p 1 0)) b (asec:2d (trans q 1 0)))
         (if (< (distance a b) asec:tol) (progn (princ "\nFace points coincide.") 'bad) (list a b)))
       'bad))
    ((= (type sel) 'STR) sel)
    ((null sel) (if (= (getvar "ERRNO") 7) (progn (princ "\nNothing selected.") 'bad) nil))
    ((/= (asec:get 0 (setq ed (entget (car sel)))) "LINE")
     (princ "\nWall face must be a straight LINE.") 'bad)
    (T
     (setq a (asec:2d (asec:get 10 ed)) b (asec:2d (asec:get 11 ed)))
     (if (< (distance a b) asec:tol) (progn (princ "\nWall face LINE has zero length.") 'bad) (list a b))))
)

;;; (dir normal signed-offset) or 'bad; thickness = perpendicular face separation
(defun asec:validate-projected-wall-faces (f1 f2 / d1 d2 n off)
  (setq d1 (asec:unit (asec:v- (cadr f1) (car f1)))
        d2 (asec:unit (asec:v- (cadr f2) (car f2))))
  (cond
    ((> (abs (asec:cross d1 d2)) *asec-parallel-tolerance*)
     (princ "\nProjected wall faces are not parallel. Please reselect.") 'bad)
    ((< (abs (setq off (asec:dot (asec:v- (car f2) (car f1)) (setq n (asec:perp d1))))) asec:tol)
     (princ "\nProjected wall faces coincide (zero thickness). Please reselect.") 'bad)
    (T (list d1 n off)))
)

;;; ---------- finite wall extent ----------
;;; Supporting lines give direction/strip/thickness only; wall LENGTH is finite:
;;;   CLOSING-LINES : a LINE near the end crosses both face supports (return wall,
;;;                   T-junction, end cap). Nearest to the face endpoint wins.
;;;   MATCHED-ENDS  : both faces end at the same station (no closing line found)
;;;   LONG-FACE     : one face runs further (other broken/shorter)
;;;   MANUAL        : picked start/end projected onto the wall axis
;;; Auto is confident only when both ends are CLOSING-LINES; otherwise the manual collector asks.
(if (null *asec-proj-wall-end-search*) (setq *asec-proj-wall-end-search* 300.0))
(if (null *asec-proj-wall-end-tolerance*) (setq *asec-proj-wall-end-tolerance* 10.0))

(defun asec:project-point-wall-axis (p origin d) (asec:dot (asec:v- p origin) d))

;;; layer is on and thawed (hidden layers never contribute automatic geometry)
(defun asec:layer-visible-p (name / td)
  (and (setq td (tblsearch "LAYER" name))
       (> (asec:get 62 td) 0)                 ; negative colour = layer off
       (= 0 (logand 1 (asec:get 70 td))))     ; bit 1 = frozen
)

;;; closing-line candidates: LINEs, limited to *asec-closing-layers* when set (AS sets
;;; it to its wall layers) so axis/grid/annotation lines are never taken as wall ends
(defun asec:closing-line-filter (/ s)
  (foreach l *asec-closing-layers* (setq s (if s (strcat s "," l) l)))
  (if s (list '(0 . "LINE") (cons 8 s)) '((0 . "LINE")))
)

;;; station of the closing LINE nearest end station e, or nil.
;;; ponytail: ssget "_C" only finds LINEs visible on screen; Manual covers the rest.
(defun asec:find-projected-wall-closing-line (f1 f2 d n off e start / origin tl r c ss i ed a b len dl p1 p2 st dist best bd be)
  (setq origin (car f1) tl *asec-proj-wall-end-tolerance*
        r (+ (abs off) *asec-proj-wall-end-search*)
        c (asec:v+ (asec:v+ origin (asec:v* d e)) (asec:v* n (* 0.5 off)))
        ss (ssget "_C" (trans (list (- (car c) r) (- (cadr c) r) 0.0) 0 1)
                       (trans (list (+ (car c) r) (+ (cadr c) r) 0.0) 0 1)
                  (asec:closing-line-filter))
        i 0)
  (while (and ss (< i (sslength ss)))
    (setq ed (entget (ssname ss i)) i (1+ i)
          a (asec:2d (asec:get 10 ed)) b (asec:2d (asec:get 11 ed)) len (distance a b))
    (if (and (> len asec:tol)
             (asec:layer-visible-p (asec:get 8 ed))
             (> (abs (asec:cross (setq dl (asec:unit (asec:v- b a))) d)) 0.26)   ; not ~parallel (>15 deg)
             (setq p1 (inters a b (car f1) (cadr f1) nil))
             (setq p2 (inters a b (car f2) (cadr f2) nil))
             ;; both support crossings must lie on the closing segment (within tolerance)
             (<= (- tl) (asec:dot (asec:v- p1 a) dl) (+ len tl))
             (<= (- tl) (asec:dot (asec:v- p2 a) dl) (+ len tl)))
      (progn
        (setq st (if start
                   (min (asec:project-point-wall-axis p1 origin d) (asec:project-point-wall-axis p2 origin d))
                   (max (asec:project-point-wall-axis p1 origin d) (asec:project-point-wall-axis p2 origin d)))
              dist (abs (- st e)))
        (if (and (<= dist r) (or (null best) (< dist bd)))
          (setq best st bd dist be ed)))))
  (if best
    (asec:dbg (strcat "\nCLOSING LINE  end " (if start "start" "end") " station " (rtos best 2 2)
                      "  layer " (asec:get 8 be) "  handle " (asec:get 5 be))))
  best
)

(defun asec:get-projected-wall-manual-extent (origin d / p q)
  (if (and (setq p (getpoint "\nSpecify wall start: "))
           (setq q (getpoint p "\nSpecify wall end: ")))
    (progn
      (setq p (asec:project-point-wall-axis (asec:2d (trans p 1 0)) origin d)
            q (asec:project-point-wall-axis (asec:2d (trans q 1 0)) origin d))
      (if (< (abs (- q p)) asec:tol)
        (progn (princ "\nZero-length wall run.") 'bad)
        (list (min p q) (max p q) "MANUAL" "MANUAL")))
    'bad)
)

;;; (t0 t1 start-method end-method) along wall axis from face1 start, or 'bad
;;; non-interactive Auto extent (shared with automatic discovery): (e0 e1 m0 m1 confident)
(defun asec:projected-wall-auto-extent (f1 f2 g / d n off origin tl ts1 ts2 e0 e1 c0 c1 m0 m1)
  (setq d (car g) n (cadr g) off (caddr g) origin (car f1) tl *asec-proj-wall-end-tolerance*
        ts1 (mapcar '(lambda (p) (asec:project-point-wall-axis p origin d)) f1)
        ts2 (mapcar '(lambda (p) (asec:project-point-wall-axis p origin d)) f2)
        e0 (min (apply 'min ts1) (apply 'min ts2))
        e1 (max (apply 'max ts1) (apply 'max ts2))
        c0 (asec:find-projected-wall-closing-line f1 f2 d n off e0 T)
        c1 (asec:find-projected-wall-closing-line f1 f2 d n off e1 nil)
        m0 (cond (c0 "CLOSING-LINES")
                 ((<= (abs (- (apply 'min ts1) (apply 'min ts2))) tl) "MATCHED-ENDS")
                 (T "LONG-FACE"))
        m1 (cond (c1 "CLOSING-LINES")
                 ((<= (abs (- (apply 'max ts1) (apply 'max ts2))) tl) "MATCHED-ENDS")
                 (T "LONG-FACE")))
  (list (if c0 c0 e0) (if c1 c1 e1) m0 m1 (and c0 c1))
)

(defun asec:get-projected-wall-extent (f1 f2 g / d origin a ext r)
  (setq d (car g) origin (car f1))
  (initget "Auto Manual")
  (if (= (getkword "\nProjected wall extent [Auto/Manual] <Auto>: ") "Manual")
    (asec:get-projected-wall-manual-extent origin d)
    (progn
      (setq a   (asec:projected-wall-auto-extent f1 f2 g)
            ext (list (car a) (cadr a) (caddr a) (cadddr a)))
      (princ (strcat "\nProjected wall length: " (rtos (- (cadr a) (car a)) 2 2)
                     "  Start source: " (caddr a) "  End source: " (cadddr a)))
      (cond
        ((< (- (cadr a) (car a)) asec:tol)
         (princ "\nCould not determine projected wall extent reliably.")
         (asec:get-projected-wall-manual-extent origin d))
        ((nth 4 a)
         (initget "Yes Manual")
         (if (= (getkword "\nAccept projected wall? [Yes/Manual] <Yes>: ") "Manual")
           (asec:get-projected-wall-manual-extent origin d)
           ext))
        (T
         (princ "\nAutomatic projected wall extent is uncertain (wall ends not confirmed by closing lines).")
         (initget "Manual Accept Cancel")
         (setq r (getkword "\nSpecify wall extent [Manual/Accept/Cancel] <Manual>: "))
         (cond ((= r "Accept") ext)
               ((= r "Cancel") 'bad)
               (T (asec:get-projected-wall-manual-extent origin d)))))))
)

(defun asec:debug-projected-wall (r / f1 f2)
  (setq f1 (asec:get 'face1 r) f2 (asec:get 'face2 r))
  (asec:dbg (strcat "\n--- PROJECTED WALL DEBUG ---"
                    "\nFace 1: " (asec:pt-str (car f1)) " -> " (asec:pt-str (cadr f1))
                    "\nFace 2: " (asec:pt-str (car f2)) " -> " (asec:pt-str (cadr f2))
                    "\nWall direction: " (asec:pt-str (asec:get 'dir r))
                    "  Thickness: " (rtos (asec:get 'thk r) 2 2)
                    "\nExtent method: start " (car (asec:get 'method r))
                    "  end " (cadr (asec:get 'method r))
                    "\nPlan start: " (asec:pt-str (asec:get 'cstart r))
                    "  Plan end: " (asec:pt-str (asec:get 'cend r))
                    "\nU1: " (rtos (asec:get 'u1 r) 2 2) "  V1: " (rtos (asec:get 'v1 r) 2 2)
                    "\nU2: " (rtos (asec:get 'u2 r) 2 2) "  V2: " (rtos (asec:get 'v2 r) 2 2)
                    "\nVmid: " (rtos (asec:get 'vmid r) 2 2)))
)

;;; returns projected wall record, "Undo", nil (done), or 'bad
(defun asec:get-projected-wall (ctx / f1 f2 g ext)
  (setq f1 (asec:get-projected-face
             "\nSelect first face of projected wall [Point/Undo/Done] <Done>: " "Point Undo Done"))
  (cond
    ((or (null f1) (= f1 "Done")) nil)
    ((or (= f1 'bad) (= f1 "Undo")) f1)
    ((member (setq f2 (asec:get-projected-face "\nSelect second face of projected wall [Point]: " "Point"))
             '(nil bad))
     (princ " Wall discarded.") 'bad)
    ((= (setq g (asec:validate-projected-wall-faces f1 f2)) 'bad) 'bad)
    ((= (setq ext (asec:get-projected-wall-extent f1 f2 g)) 'bad) 'bad)
    (T (asec:make-projected-wall f1 f2 g ext ctx)))
)

;;; projected wall record from faces f1 f2, (dir normal offset) g and (t0 t1 m0 m1) ext.
;;; Shared by automatic discovery and manual collectors so both produce identical records.
(defun asec:make-projected-wall (f1 f2 g ext ctx / d n off c0 cs ce se dir vd uv1 uv2)
  (progn
     (setq d   (car g) n (cadr g) off (caddr g)
           c0  (asec:v+ (car f1) (asec:v* n (* 0.5 off)))   ; centerline origin
           cs  (asec:v+ c0 (asec:v* d (car ext)))
           ce  (asec:v+ c0 (asec:v* d (cadr ext)))
           se  (asec:get 'se ctx) dir (asec:get 'dir ctx) vd (asec:get 'vd ctx)
           uv1 (asec:section-coordinate cs (car se) dir vd)
           uv2 (asec:section-coordinate ce (car se) dir vd))
     (list (cons 'face1 f1) (cons 'face2 f2) (cons 'dir d) (cons 'origin (car f1))
           (cons 'strip (list (car f1) n (car f2) n))
           (cons 't0 (car ext)) (cons 't1 (cadr ext)) (cons 'method (cddr ext))
           (cons 'cstart cs) (cons 'cend ce) (cons 'thk (abs off))
           (cons 'u1 (car uv1)) (cons 'v1 (cadr uv1))
           (cons 'u2 (car uv2)) (cons 'v2 (cadr uv2))
           ;; U span of the full footprint (both faces at both ends), not just the centerline:
           ;; a wall running away from the viewer shows its end, thickness wide, not an axis line
           (cons 'umin (- (min (car uv1) (car uv2)) (abs (asec:dot (asec:v* n (* 0.5 off)) dir))))
           (cons 'umax (+ (max (car uv1) (car uv2)) (abs (asec:dot (asec:v* n (* 0.5 off)) dir))))
           (cons 'vmid (* 0.5 (+ (cadr uv1) (cadr uv2))))
           (cons 'bottom (asec:get 'elev ctx)) (cons 'top (asec:get 'top ctx))))
)

(defun asec:collect-projected-walls (ctx / pws r)
  (princ (strcat "\n" (asec:get 'fname ctx) " - PROJECTED WALLS"))
  (while (setq r (asec:get-projected-wall ctx))
    (if (= (type r) 'LIST) (setq r (asec:check-projected-wall-view r ctx)))
    (cond
      ((= r 'bad))
      ((= r "Undo")
       (if pws
         (progn (asec:unlabel (asec:get 'label (car pws)))
                (setq pws (cdr pws)) (princ "\nLast projected wall removed."))
         (princ "\nNo projected wall to undo.")))
      (T
       (setq pws (cons (cons (cons 'label
                                   (asec:label (asec:v* (asec:v+ (asec:get 'cstart r) (asec:get 'cend r)) 0.5)
                                               (strcat "P" (itoa (1+ (length pws)))) (asec:get 'se ctx)))
                             r)
                       pws))
       (princ (strcat "\nProjected wall " (itoa (length pws))
                      ":  Thickness: " (rtos (asec:get 'thk r) 2 2)
                      "  Length: " (rtos (- (asec:get 't1 r) (asec:get 't0 r)) 2 2)
                      "  U: " (rtos (asec:get 'umin r) 2 2) " -> " (rtos (asec:get 'umax r) 2 2)
                      "  Depth V1/V2: " (rtos (asec:get 'v1 r) 2 2) " / " (rtos (asec:get 'v2 r) 2 2)))
       (if (< (- (asec:get 'umax r) (asec:get 'umin r)) asec:tol)
         (princ "\nProjected wall is edge-on."))
       (asec:debug-projected-wall r))
    )
  )
  (princ (strcat "\n" (itoa (length pws)) " projected wall(s) recorded."))
  (reverse pws)
)

;;; host = projected wall whose plan strip AND run contain the jamb midpoint.
;;; returns wall number, 'keep (unhosted), 'bad, or 'done
(defun asec:find-projected-host-wall (jm ctx / pws mid hd tl k d tm cands best bd r all)
  (setq pws (asec:get 'pwalls ctx))
  (if (null pws)
    'keep
    (progn
      (setq mid (asec:v* (asec:v+ (car jm) (cadr jm)) 0.5)
            hd (cadddr jm) tl *asec-wall-host-tolerance* k 0)
      (foreach pw pws
        (setq k  (1+ k) all (cons k all)
              d  (asec:strip-offsets mid (asec:get 'strip pw))
              tm (asec:dot (asec:v- mid (asec:get 'origin pw)) (asec:get 'dir pw)))
        (asec:dbg (strcat "\nProjected wall " (itoa k) ":  face offsets = " (rtos (car d) 2 2)
                          " / " (rtos (cadr d) 2 2) "  along = " (rtos tm 2 2)
                          "  run = " (rtos (asec:get 't0 pw) 2 2) " -> " (rtos (asec:get 't1 pw) 2 2)))
        (if (and (asec:point-in-wall-strip d hd)
                 (>= tm (- (asec:get 't0 pw) tl))
                 (<= tm (+ (asec:get 't1 pw) tl)))
          (progn (asec:dbg "  contains = Yes")
                 (setq cands (cons (cons k (abs (* 0.5 (+ (car d) (cadr d))))) cands)))
          (asec:dbg "  contains = No")))
      (setq all (reverse all) cands (reverse cands))
      (cond
        ((= (length cands) 1) (caar cands))
        (cands
         ;; several: nearest wall centerline in plan
         (foreach c cands
           (cond ((or (null best) (< (cdr c) (- bd asec:tol))) (setq best (car c) bd (cdr c)))
                 ((< (abs (- (cdr c) bd)) asec:tol) (setq best 'tie))))
         (if (= best 'tie)
           (progn (princ "\nOpening matches multiple projected walls (numbers are projected walls).")
                  (asec:ask-host (mapcar 'car cands)))
           best))
        (T
         (princ "\nProjected opening does not match a selected projected wall.")
         (initget "Assign Keep Skip")
         (setq r (getkword "\n[Assign/Keep/Skip] (Keep = leave unhosted) <Assign>: "))
         (cond ((= r "Keep") 'keep)
               ((= r "Skip") 'done)
               (T (princ "\n(numbers are projected walls)") (asec:ask-host all)))))))
)

;;; non-interactive projected host candidates: ((wall# . centerline-distance) ...)
;;; same test as asec:find-projected-host-wall (strip + finite run); used by automatic discovery
(defun asec:projected-host-candidates (jm pws / mid hd tl k d tm cands)
  (setq mid (asec:v* (asec:v+ (car jm) (cadr jm)) 0.5)
        hd (cadddr jm) tl *asec-wall-host-tolerance* k 0)
  (foreach pw pws
    (setq k  (1+ k)
          d  (asec:strip-offsets mid (asec:get 'strip pw))
          tm (asec:dot (asec:v- mid (asec:get 'origin pw)) (asec:get 'dir pw)))
    (if (and (asec:point-in-wall-strip d hd)
             (>= tm (- (asec:get 't0 pw) tl))
             (<= tm (+ (asec:get 't1 pw) tl)))
      (setq cands (cons (cons k (abs (* 0.5 (+ (car d) (cadr d))))) cands))))
  (reverse cands)
)

;;; attach host to a projected opening record; U span stays the jamb span
(defun asec:host-projected-opening (rec jm ctx / host pw tl r)
  (setq host (asec:find-projected-host-wall jm ctx) tl *asec-wall-host-tolerance*)
  (cond
    ((= host 'keep) (asec:check-unhosted-opening-view rec ctx))
    ((not (numberp host)) host)
    ((and (setq pw (nth (1- host) (asec:get 'pwalls ctx)))
          (>= (asec:get 'umin rec) (- (asec:get 'umin pw) tl))
          (<= (asec:get 'umax rec) (+ (asec:get 'umax pw) tl)))
     (cons (cons 'phost host) rec))
    (T
     (princ "\nWarning: Projected opening exceeds host wall extent.")
     (initget "Accept Reselect Skip")
     (setq r (getkword "\n[Accept/Reselect/Skip] <Reselect>: "))
     (cond ((= r "Accept") (cons (cons 'phost host) rec))
           ((= r "Skip") 'done)
           (T 'bad))))
)

;;; ---------- output ----------
(defun asec:ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 name) '(70 . 0)
                   (cons 62 color) '(6 . "Continuous"))))
)

(defun asec:rect (x1 y1 x2 y2 lay)
  (entmakex (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                  '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1)
                  (list 10 x1 y1) (list 10 x2 y1) (list 10 x2 y2) (list 10 x1 y2)))
)

(defun asec:poly (pts closed lay)
  (entmakex (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                          '(100 . "AcDbPolyline") (cons 90 (length pts)) (cons 70 closed))
                    (mapcar '(lambda (p) (cons 10 p)) pts)))
)

(defun asec:line (p q lay)
  (entmakex (list '(0 . "LINE") (cons 8 lay) (list 10 (car p) (cadr p) 0.0)
                  (list 11 (car q) (cadr q) 0.0)))
)

;;; org = section origin (U=0, elev=0) in WCS
(defun asec:draw-slab (org elev ext slabT)
  (asec:hatch
    (asec:cut-rect (+ (car org) (car ext)) (+ (cadr org) elev (- slabT))
               (+ (car org) (cadr ext)) (+ (cadr org) elev) (asec:gfx "RCC_CUT_LAYER"))
    "RCC")
)

;;; hatch a closed output polyline; kind "WALL" | "RCC" -> <kind>_HATCH_PATTERN/_SCALE/_ANGLE.
;;; Pattern NONE/empty = no hatch. Scale/angle <DEFAULT> or missing = AutoCAD's current default.
;;; Result moved to HATCH_LAYER.
(defun asec:hatch (e kind / pat sc an last h)
  (setq pat (asec:gfx (strcat kind "_HATCH_PATTERN"))
        sc  (asec:gfx (strcat kind "_HATCH_SCALE"))
        an  (asec:gfx (strcat kind "_HATCH_ANGLE"))
        sc  (if (or (null sc) (= (strcase sc) "<DEFAULT>")) "" sc)
        an  (if (or (null an) (= (strcase an) "<DEFAULT>")) "" an)
        last (entlast))
  (if (and e pat (/= pat "") (/= (strcase pat) "NONE"))
    (progn
      (if (= (strcase pat) "SOLID")
        (command-s "_.-HATCH" "_P" "_S" "_S" e "" "")
        (command-s "_.-HATCH" "_P" pat sc an "_S" e "" ""))
      (if (and (setq h (entlast)) (not (eq h last)) (= (asec:get 0 (entget h)) "HATCH"))
        (entmod (subst (cons 8 (asec:gfx "HATCH_LAYER")) (assoc 8 (entget h)) (entget h))))))
  e
)

(defun asec:draw-roof-slab (org elev ext slabT)
  (asec:draw-slab org elev ext slabT)
)

;;; ground line at underside of GF slab, full master section length
(defun asec:draw-ground (org len slabT / y)
  (setq y (- (cadr org) slabT))
  (entmakex (list '(0 . "LINE") (cons 8 (asec:gfx "GROUND_LAYER"))
                  (cons 62 (atoi (asec:gfx "GROUND_COLOR")))
                  (list 10 (car org) y 0.0)
                  (list 11 (+ (car org) len) y 0.0)))
)

(defun asec:draw-wall (org w bot top)
  (asec:hatch
    (asec:cut-rect (+ (car org) (asec:get 'umin w)) (+ (cadr org) bot)
                   (+ (car org) (asec:get 'umax w)) (+ (cadr org) top) (asec:gfx "WALL_CUT_LAYER"))
    "WALL")
)

;;; ---------- wall decomposition ----------
(defun asec:insert-sorted (x lst less)
  (cond ((null lst) (list x))
        ((apply less (list x (car lst))) (cons x lst))
        (T (cons (car lst) (asec:insert-sorted x (cdr lst) less))))
)

(defun asec:sort (lst less / out)
  (foreach x lst (setq out (asec:insert-sorted x out less)))
  out
)

(defun asec:uniq (nums / out)
  (foreach x nums
    (if (or (null out) (> (- x (car out)) asec:tol)) (setq out (cons x out))))
  (reverse out)
)

;;; wall [a,b]x[bot,top] minus voids ops ((u0 u1 z0 z1) ...) -> solid rects ((x0 z0 x1 z1) ...)
;;; U is cut into strips at every void edge; each strip is filled between its voids.
(defun asec:wall-pieces (a b bot top ops / xs out x0 x1 cov cur)
  (setq xs (list a b))
  (foreach o ops
    (setq xs (cons (max a (min b (car o))) (cons (max a (min b (cadr o))) xs))))
  (setq xs (asec:uniq (asec:sort xs '<)))
  (while (cdr xs)
    (setq x0 (car xs) x1 (cadr xs) xs (cdr xs) cur bot cov nil)
    (foreach o ops
      (if (and (<= (car o) (+ x0 asec:tol)) (>= (cadr o) (- x1 asec:tol)))
        (setq cov (cons o cov))))
    (foreach o (asec:sort cov '(lambda (p q) (< (caddr p) (caddr q))))
      (if (> (caddr o) (+ cur asec:tol)) (setq out (cons (list x0 cur x1 (caddr o)) out)))
      (setq cur (max cur (cadddr o))))
    (if (> top (+ cur asec:tol)) (setq out (cons (list x0 cur x1 top) out))))
  out
)

;;; voids ((u0 u1 z0 z1) ...) of openings whose host key (host | phost) = k, clamped to bot..top.
;;; Opening U span is whatever the record holds: cut = host wall thickness, projected = jambs.
(defun asec:wall-voids (k key bot top ops / voids z0 z1)
  (foreach o ops
    (if (= (asec:get key o) k)
      (progn
        (setq z0 (max bot (asec:get 'bottom o))
              z1 (min top (+ (asec:get 'bottom o) (asec:get 'height o))))
        (if (> (- z1 z0) asec:tol)
          (setq voids (cons (list (asec:get 'umin o) (asec:get 'umax o) z0 z1) voids))))))
  voids
)

(defun asec:draw-wall-with-openings (org w k bot top ops / voids)
  (setq voids (asec:wall-voids k 'host bot top ops))
  (if voids
    (foreach pc (asec:wall-pieces (asec:get 'umin w) (asec:get 'umax w) bot top voids)
      (asec:hatch
        (asec:cut-rect (+ (car org) (car pc)) (+ (cadr org) (cadr pc))
                       (+ (car org) (caddr pc)) (+ (cadr org) (cadddr pc)) (asec:gfx "WALL_CUT_LAYER"))
        "WALL"))
    (asec:draw-wall org w bot top))
)

;;; ---------- projected openings ----------
(defun asec:opening-box (org o / y0)
  (setq y0 (+ (cadr org) (asec:get 'bottom o)))
  (list (+ (car org) (asec:get 'umin o)) y0
        (+ (car org) (asec:get 'umax o)) (+ y0 (asec:get 'height o)))
)

;;; vis = (org v occ ign) -> clipped by the occlusion engine; nil -> drawn whole
(defun asec:draw-projected-door (org o vis / bx x0 y0 x1 y1 fr xm)
  (setq bx (asec:opening-box org o) fr (asec:setting "FRAME_THK")
        x0 (car bx) y0 (cadr bx) x1 (caddr bx) y1 (cadddr bx))
  (asec:vis-rect x0 y0 x1 y1 (asec:gfx "OPENING_PROJ_BORDER_LAYER") vis)
  (if (and (> (- x1 x0) (* 4 fr)) (> (- y1 y0) (* 2 fr)))
    (progn
      (asec:vis-poly (list (list (+ x0 fr) y0) (list (+ x0 fr) (- y1 fr))
                           (list (- x1 fr) (- y1 fr)) (list (- x1 fr) y0)) 0 (asec:gfx "OPENING_PROJ_DETAIL_LAYER") vis)
      (setq y1 (- y1 fr))))
  (if (= (asec:get 'type o) "DOUBLE")
    (progn (setq xm (* 0.5 (+ x0 x1)))
           (asec:vis-poly (list (list xm y0) (list xm y1)) 0 (asec:gfx "OPENING_PROJ_DETAIL_LAYER") vis)))
)

(defun asec:draw-projected-window (org o vis / bx x0 y0 x1 y1 fr)
  (setq bx (asec:opening-box org o) fr (asec:setting "FRAME_THK")
        x0 (car bx) y0 (cadr bx) x1 (caddr bx) y1 (cadddr bx))
  (asec:vis-rect x0 y0 x1 y1 (asec:gfx "OPENING_PROJ_BORDER_LAYER") vis)
  (if (and (> (- x1 x0) (* 2 fr)) (> (- y1 y0) (* 2 fr)))
    (asec:vis-rect (+ x0 fr) (+ y0 fr) (- x1 fr) (- y1 fr) (asec:gfx "OPENING_PROJ_DETAIL_LAYER") vis))
)

;;; ---------- cut openings (drawn inside the wall void) ----------
;;; Frame members: *asec-cut-frame-width* (100) across the wall thickness, centred,
;;; FRAME_THK (50) deep; both clamped to the void. Uncut wall edges beyond the cut
;;; (jambs at both wall faces) run the full opening height on OPENING_PROJ_BORDER_LAYER.
(if (null *asec-cut-frame-width*) (setq *asec-cut-frame-width* 100.0))
(if (null *asec-door-panel-thk*) (setq *asec-door-panel-thk* 35.0))

(defun asec:cut-frame (bx y0 y1 / xm hw)
  (setq xm (* 0.5 (+ (car bx) (caddr bx)))
        hw (* 0.5 (min *asec-cut-frame-width* (- (caddr bx) (car bx)))))
  (asec:rect (- xm hw) y0 (+ xm hw) y1 (asec:gfx "OPENING_CUT_LAYER"))
)

(defun asec:cut-jambs (bx)
  (foreach x (list (car bx) (caddr bx))
    (asec:line (list x (cadr bx)) (list x (cadddr bx)) (asec:gfx "OPENING_PROJ_BORDER_LAYER")))
)

;;; door: head frame below the head; panel *asec-door-panel-thk* centred, floor -> frame underside
(defun asec:draw-cut-door (org o / bx fr xm pw)
  (setq bx (asec:opening-box org o)
        fr (min (asec:setting "FRAME_THK") (asec:get 'height o))
        xm (* 0.5 (+ (car bx) (caddr bx)))
        pw (* 0.5 (min *asec-door-panel-thk* (- (caddr bx) (car bx)))))
  (asec:cut-jambs bx)
  (asec:cut-frame bx (- (cadddr bx) fr) (cadddr bx))
  (if (> (- (cadddr bx) fr (cadr bx)) asec:tol)
    (asec:rect (- xm pw) (cadr bx) (+ xm pw) (- (cadddr bx) fr) (asec:gfx "OPENING_CUT_LAYER")))
)

;;; window: head frame below the head, sill frame above the sill, glass = one line between
;;; the inner frame faces (sill + F -> head - F); omitted when no positive glass height is left
(defun asec:draw-cut-window (org o / bx fr xm g0 g1)
  (setq bx (asec:opening-box org o)
        fr (min (asec:setting "FRAME_THK") (* 0.5 (asec:get 'height o)))
        xm (* 0.5 (+ (car bx) (caddr bx)))
        g0 (+ (cadr bx) fr)
        g1 (- (cadddr bx) fr))
  (asec:cut-jambs bx)
  (asec:cut-frame bx g1 (cadddr bx))
  (asec:cut-frame bx (cadr bx) g0)
  (if (> (- g1 g0) asec:tol)
    (asec:line (list xm g0) (list xm g1) (asec:gfx "OPENING_PROJ_BORDER_LAYER")))
)

(defun asec:draw-projected-opening (org o vis)
  (if (= (asec:get 'kind o) "DOOR")
    (asec:draw-projected-door org o vis)
    (asec:draw-projected-window org o vis))
)

;;; projected wall elevation uMin->uMax x bottom->top minus hosted opening voids
;;; (jamb U span), then the existing projected door/window graphics in the voids
;;; wall end at a projected opening's jamb is that opening's reveal: the opening graphics
;;; take precedence, so no full-height edge. Openings hosted by this wall, or unhosted
;;; ones at this wall's depth, within *asec-reveal-tolerance* of a jamb.
(if (null *asec-reveal-tolerance*) (setq *asec-reveal-tolerance* 150.0))

(defun asec:reveal-edge-p (u pw k ops / tl hit)
  (setq tl *asec-reveal-tolerance*)
  (foreach o ops
    (if (and (or (= (asec:get 'phost o) k)
                 (and (null (asec:get 'phost o))
                      (<= (abs (- (asec:get 'v o) (asec:get 'vmid pw))) (+ (asec:get 'thk pw) tl))))
             (or (<= (abs (- u (asec:get 'umin o))) tl)
                 (<= (abs (- u (asec:get 'umax o))) tl)))
      (setq hit T)))
  hit
)

;;; Source fragmentation is not a wall end: an end edge is dropped when another projected
;;; wall on the same support line (parallel, centerlines within depth tolerance) continues
;;; past it within *aseca-small-gap-tolerance* (the logical wall runs on).

(defun asec:continued-edge-p (u pw pws / d gap hit)
  (setq d (asec:get 'dir pw) gap *aseca-small-gap-tolerance*)
  (foreach q pws
    (if (and (not (eq q pw))
             (<= (abs (asec:cross d (asec:get 'dir q))) *asec-parallel-tolerance*)
             (<= (abs (asec:cross (asec:v- (asec:get 'cstart q) (asec:get 'cstart pw)) d))
                 *asec-occlusion-depth-tolerance*)
             (<= (- (asec:get 'umin q) gap) u (+ (asec:get 'umax q) gap))
             (if (> u (* 0.5 (+ (asec:get 'umin pw) (asec:get 'umax pw))))
               (> (asec:get 'umax q) (+ u *asec-occlusion-u-tolerance*))
               (< (asec:get 'umin q) (- u *asec-occlusion-u-tolerance*))))
      (setq hit T)))
  hit
)

;;; focc = floor occluder pieces, pws = floor projected walls
;;; vis record for geometry owned by wall pw at section position u (hosted openings): near-face
;;; depth there, host wall ignored
(defun asec:wall-vis (org pw u focc ign) (list org (asec:near-depth-at pw u) focc ign))

;;; debug: the wall's two faces in U/V and which one faces the viewer
(defun asec:debug-projected-surface (pw k / u1 u2 v1 v2 du dv L h nu nv mA mB f)
  (defun f (x) (if x (rtos x 2 1) "-"))
  (setq u1 (asec:get 'u1 pw) u2 (asec:get 'u2 pw) v1 (asec:get 'v1 pw) v2 (asec:get 'v2 pw)
        du (- u2 u1) dv (- v2 v1) L (max 1e-9 (sqrt (+ (* du du) (* dv dv))))
        h (* 0.5 (cond ((asec:get 'thk pw)) (0.0))) nu (* h (/ (- dv) L)) nv (* h (/ du L))
        mA (+ (* 0.5 (+ v1 v2)) nv) mB (- (* 0.5 (+ v1 v2)) nv))
  (princ (strcat "\n--- PROJECTED SURFACE DEBUG ---  Wall " (itoa k) "  handles:"
                 (apply 'strcat (mapcar '(lambda (e) (if (entget e) (strcat " " (asec:get 5 (entget e))) ""))
                                        (asec:get 'segs pw)))
                 "\n  Face A  U1/V1 " (f (+ u1 nu)) "/" (f (+ v1 nv)) "  U2/V2 " (f (+ u2 nu)) "/" (f (+ v2 nv))
                 "\n  Face B  U1/V1 " (f (- u1 nu)) "/" (f (- v1 nv)) "  U2/V2 " (f (- u2 nu)) "/" (f (- v2 nv))
                 "\n  Near surface: "
                 (cond ((< (abs du) 1.0) "END-ON (exposed end face at nearest end)")
                       ((< mA (- mB 1e-6)) "A (B is the hidden rear face)")
                       ((< mB (- mA 1e-6)) "B (A is the hidden rear face)")
                       (T "VARIABLE"))
                 "\n  Faces are never drawn as lines; drawable = end/corner edges that pass the silhouette test"))
)

(defun asec:debug-edge-occlusion (pw k u d focc bot top / h ivs)
  (setq ivs (asec:visible-edge-z u bot top d focc))
  (princ (strcat "\n  Edge U " (rtos u 2 1) "  depth " (rtos d 2 1)))
  (foreach oc focc
    (if (<= (- (asec:get 'umin oc) 1.0) u (+ (asec:get 'umax oc) 1.0))
      (princ (strcat "\n    covers " (asec:get 'type oc) " " (itoa (asec:get 'index oc))
                     (if (and (= (asec:get 'type oc) "PROJECTED") (= (asec:get 'index oc) k)) " (self)" "")
                     "  U " (rtos (asec:get 'umin oc) 2 1) " -> " (rtos (asec:get 'umax oc) 2 1)
                     "  Z " (rtos (asec:get 'zmin oc) 2 1) " -> " (rtos (asec:get 'zmax oc) 2 1)
                     "  depth " (rtos (asec:depth-over oc u u) 2 1)))))
  (princ (strcat "\n    Visible Z " (asec:ivstr ivs) "  Result: "
                 (cond ((null ivs) "HIDDEN (SELF/EXTERNALLY-OCCLUDED: no open side)")
                       ((and (= (length ivs) 1) (< (abs (- (caar ivs) bot)) 1.0) (< (abs (- (cadar ivs) top)) 1.0))
                        "DRAW (EXPOSED: open side)")
                       (T "PARTIAL"))))
)

;;; focc = floor occluder pieces, pws = floor projected walls
;;; A projected wall shows only its visible surfaces' boundaries: end/corner edges that pass the
;;; silhouette test (asec:visible-edge-z). Its hidden rear face never produces a line.
(defun asec:draw-projected-wall-with-openings (org pw k ops focc pws / bot top mine ign d)
  (setq bot (asec:get 'bottom pw) top (asec:get 'top pw)
        ;; hosted opening graphics never hidden by their own wall
        ign (list (cons "PROJECTED" k)))
  (if *asec-debug* (asec:debug-projected-surface pw k))
  (foreach o ops
    (if (= (asec:get 'phost o) k) (setq mine (cons o mine))))
  (foreach u (if (< (- (asec:get 'umax pw) (asec:get 'umin pw)) asec:tol)
               (list (* 0.5 (+ (asec:get 'u1 pw) (asec:get 'u2 pw))))
               (list (asec:get 'umin pw) (asec:get 'umax pw)))
    (cond
      ((asec:reveal-edge-p u pw k ops))
      ((asec:continued-edge-p u pw pws))
      ((asec:clipped-edge-p u pw))
      (T (setq d (asec:near-depth-at pw u))
         (if *asec-debug* (asec:debug-edge-occlusion pw k u d focc bot top))
         (asec:draw-vedge org u bot top d focc (asec:gfx "PROJ_WALL_LAYER")))))
  (foreach o mine (asec:draw-projected-opening org o (asec:wall-vis org pw (asec:get 'u o) focc ign)))
)

;;; ---------- occlusion engine (memory only, section space U/Z/V) ----------
;;; OCCLUDER PIECE: opaque wall rectangle
;;;   (type . CUT|PROJECTED) (index . k) (source . wall) (umin) (umax) (zmin) (zmax) (v) [(v1) (v2)]
;;; Cut walls V = 0. Projected walls V = Vmid.
;;; ponytail: Vmid approximates oblique walls; V(U) from v1/v2 when needed.
;;; Visibility of a horizontal object (U a..b, Z, V) = its U span minus the U spans of all
;;; pieces nearer than V (by *asec-occlusion-depth-tolerance*) whose Z range contains Z.
;;; Pure list math: no entities, no floor records; callers pass the occluder list.
;;; VISIBILITY POLICY: doors/windows are CLOSED, so floor occluders are whole wall envelopes.
;;; ponytail: fixed policy; "OPENING-AWARE" (wall minus voids) is kept for a future option.
(if (null *asec-occlusion-policy*) (setq *asec-occlusion-policy* "CLOSED"))
(if (null *asec-occlusion-u-tolerance*) (setq *asec-occlusion-u-tolerance* 1.0))
(if (null *asec-occlusion-z-tolerance*) (setq *asec-occlusion-z-tolerance* 1.0))
(if (null *asec-occlusion-depth-tolerance*) (setq *asec-occlusion-depth-tolerance* 10.0))

;;; scalar interval math shared by U and Z; tl = sliver/overlap tolerance
(defun asec:interval-overlap-p (p q tl)
  (and (< (car p) (- (cadr q) tl)) (> (cadr p) (+ (car q) tl)))
)

;;; sorted, touching/overlapping intervals joined
(defun asec:merge-intervals (ivs tl / out)
  (foreach iv (asec:sort ivs '(lambda (p q) (< (car p) (car q))))
    (if (and out (<= (car iv) (+ (cadar out) tl)))
      (setq out (cons (list (caar out) (max (cadar out) (cadr iv))) (cdr out)))
      (setq out (cons iv out))))
  (reverse out)
)

;;; iv minus blk -> 0..2 intervals; slivers <= tl dropped
(defun asec:subtract-interval (iv blk tl / out)
  (if (not (asec:interval-overlap-p iv blk tl))
    (list iv)
    (progn
      (if (> (- (car blk) (car iv)) tl) (setq out (list (list (car iv) (car blk)))))
      (if (> (- (cadr iv) (cadr blk)) tl) (setq out (append out (list (list (cadr blk) (cadr iv))))))
      out))
)

(defun asec:subtract-intervals (ivs blks tl / nxt)
  (foreach blk blks
    (setq nxt nil)
    (foreach iv ivs (setq nxt (append nxt (asec:subtract-interval iv blk tl))))
    (setq ivs nxt))
  ivs
)

(defun asec:merge-u-intervals (ivs) (asec:merge-intervals ivs *asec-occlusion-u-tolerance*))
(defun asec:subtract-u-intervals (ivs blks) (asec:subtract-intervals ivs blks *asec-occlusion-u-tolerance*))

;;; opaque pieces of wall w (umin/umax) over bot..top minus voids
(defun asec:wall-occluder-pieces (typ k w v bot top voids / out)
  (foreach pc (asec:wall-pieces (asec:get 'umin w) (asec:get 'umax w) bot top voids)
    (setq out (cons (list (cons 'type typ) (cons 'index k) (cons 'source w)
                          (cons 'umin (car pc)) (cons 'umax (caddr pc))
                          (cons 'zmin (cadr pc)) (cons 'zmax (cadddr pc))
                          (cons 'v v) (cons 'v1 (asec:get 'v1 w)) (cons 'v2 (asec:get 'v2 w))
                          (cons 'u1 (asec:get 'u1 w)) (cons 'u2 (asec:get 'u2 w))
                          (cons 'thk (asec:get 'thk w)))
                    out)))
  out
)

;;; cut wall: V = 0, voids = hosted cut openings (U = cut wall thickness)
(defun asec:cut-wall-occluder-pieces (w k bot top ops)
  (asec:wall-occluder-pieces "CUT" k w 0.0 bot top (asec:wall-voids k 'host bot top ops))
)

;;; projected wall: V = Vmid, voids = hosted projected openings (U = jamb span)
(defun asec:projected-wall-occluder-pieces (pw k ops / bot top)
  (setq bot (asec:get 'bottom pw) top (asec:get 'top pw))
  (asec:wall-occluder-pieces "PROJECTED" k pw (asec:get 'vmid pw) bot top
                             (asec:wall-voids k 'phost bot top ops))
)

;;; floor-local occluders; unhosted openings are not occluders.
;;; CLOSED policy: no voids (openings passed as nil) -> one envelope piece per wall.
(defun asec:build-floor-occluders (fl cfg / bot top ops k occ open zt)
  (setq open (= *asec-occlusion-policy* "OPENING-AWARE")
        bot (asec:get 'elev fl)
        top (- (asec:floor-elevation (1+ (asec:get 'index fl)) cfg) (caddr cfg))
        ops (if open (append (asec:get 'cutdoors fl) (asec:get 'cutwins fl)))
        k 0)
  (foreach w (asec:get 'walls fl)
    (setq k (1+ k) occ (append occ (asec:cut-wall-occluder-pieces w k bot top ops))))
  (setq ops (if open (append (asec:get 'projdoors fl) (asec:get 'projwins fl))) k 0)
  (foreach pw (asec:get 'pwalls fl)
    (setq k (1+ k) occ (append occ (asec:projected-wall-occluder-pieces pw k ops))))
  ;; low walls: opaque only over their own bottom..top
  (setq k 0)
  (foreach w (asec:get 'lwalls fl)
    (setq k (1+ k) occ (append occ (asec:wall-occluder-pieces "LOWCUT" k w 0.0 (asec:get 'bottom w) (asec:get 'top w) nil))))
  (setq k 0)
  (foreach w (asec:get 'plwalls fl)
    (setq k (1+ k) occ (append occ (asec:wall-occluder-pieces "LOWPROJ" k w (asec:get 'vmid w)
                                                              (asec:get 'bottom w) (asec:get 'top w) nil))))
  ;; projected slab edges: the slab band (level - slab thickness .. level) along the segment
  (setq k 0)
  (foreach r (asec:get 'pslabs fl)
    (setq k (1+ k)
          zt (asec:pslab-level r fl cfg)
          occ (append occ (asec:wall-occluder-pieces "PSLAB" k r (* 0.5 (+ (asec:get 'v1 r) (asec:get 'v2 r)))
                                                     (- zt (caddr cfg)) zt nil))))
  occ
)

(defun asec:pslab-level (r fl cfg)
  (if (asec:get 'top r) (asec:floor-elevation (1+ (asec:get 'index fl)) cfg) (asec:get 'elev fl))
)

;;; projected slab edge: the visible slab edge band (top and underside lines at the slab's level),
;;; projection linework on PROJ_SLAB_LAYER, never hatched; own band never hides itself
(defun asec:draw-projected-slab (org r k occ zt slabT / ign lay a b)
  (setq ign (list (cons "PSLAB" k)) lay (asec:gfx "PROJ_SLAB_LAYER")
        a (asec:draw-hline-varying org r (asec:get 'umin r) (asec:get 'umax r) zt occ ign lay)
        b (asec:draw-hline-varying org r (asec:get 'umin r) (asec:get 'umax r) (- zt slabT) occ ign lay))
  (if *asec-debug*
    (princ (strcat "\n--- PROJECTED SLAB DEBUG ---  source " (asec:get 'handle r) "  linetype " (asec:get 'lt r)
                   "  level " (rtos zt 2 1) (if (asec:get 'top r) " (TOP)" " (CURRENT)")
                   "\n  U1/V1 " (rtos (asec:get 'u1 r) 2 1) "/" (rtos (asec:get 'v1 r) 2 1)
                   "  U2/V2 " (rtos (asec:get 'u2 r) 2 1) "/" (rtos (asec:get 'v2 r) 2 1)
                   "\n  top visible " (asec:ivstr a) "  underside visible " (asec:ivstr b)
                   "  result " (cond ((and (null a) (null b)) "HIDDEN")
                                     ((and (= (length a) 1) (< (abs (- (caar a) (asec:get 'umin r))) 1.0)
                                           (< (abs (- (cadar a) (asec:get 'umax r))) 1.0)) "DRAW")
                                     (T "PARTIAL")))))
)

;;; projected low wall: true end edges (fragment/field-limit ends suppressed) + top edge,
;;; all clipped by nearer occluders; never hatched
(defun asec:draw-projected-low-wall (org pw k focc pws / bot top ign lay)
  (setq bot (asec:get 'bottom pw) top (asec:get 'top pw) lay (asec:gfx "PROJ_WALL_LAYER")
        ign (list (cons "LOWPROJ" k)))
  (foreach u (if (< (- (asec:get 'umax pw) (asec:get 'umin pw)) asec:tol)
               (list (* 0.5 (+ (asec:get 'u1 pw) (asec:get 'u2 pw))))
               (list (asec:get 'umin pw) (asec:get 'umax pw)))
    (if (not (or (asec:continued-edge-p u pw pws) (asec:clipped-edge-p u pw)))
      (asec:draw-vedge org u bot top (asec:near-depth-at pw u) focc lay)))
  (asec:draw-hline-varying org pw (asec:get 'umin pw) (asec:get 'umax pw) top focc ign lay)
)

(defun asec:occluder-blocks-at-z-p (oc z)
  (<= (- (asec:get 'zmin oc) *asec-occlusion-z-tolerance*) z
      (+ (asec:get 'zmax oc) *asec-occlusion-z-tolerance*))
)

;;; DEPTH ALONG A WALL. Records with a centerline (u1 v1)-(u2 v2) do not have one depth:
;;;   end-on (|u2-u1| < 1)  -> nearest end (its exposed face is in front of everything behind it)
;;;   otherwise             -> linear V at u, clamped to the centerline ends
;;; Records without a centerline (cut walls, test pieces) use their constant 'v.
(defun asec:depth-at (r u / u1 u2 v1 v2 tt)
  (setq u1 (asec:get 'u1 r) u2 (asec:get 'u2 r) v1 (asec:get 'v1 r) v2 (asec:get 'v2 r))
  (cond
    ((or (null u1) (null u2) (null v1) (null v2)) (asec:get 'v r))
    ((< (abs (- u2 u1)) 1.0) (min v1 v2))
    (T (setq tt (max 0.0 (min 1.0 (/ (- u u1) (- u2 u1)))))
       (+ v1 (* tt (- v2 v1)))))
)

;;; depth of the wall's NEAR FACE at u: centerline depth minus the half thickness seen along V.
;;; In U/V the centerline direction is (du dv)/L, its normal's V component is du/L, so the offset
;;; is thk/2 * |du|/L: full half-thickness for walls across the view, zero for end-on walls
;;; (their nearest end already is the exposed face). No centerline/thickness -> depth-at.
(defun asec:near-depth-at (r u / du dv L)
  (setq du (if (and (asec:get 'u1 r) (asec:get 'u2 r)) (- (asec:get 'u2 r) (asec:get 'u1 r)))
        dv (if (and (asec:get 'v1 r) (asec:get 'v2 r)) (- (asec:get 'v2 r) (asec:get 'v1 r))))
  (if (and du dv (asec:get 'thk r) (> (setq L (sqrt (+ (* du du) (* dv dv)))) 1e-9))
    (- (asec:depth-at r u) (* 0.5 (asec:get 'thk r) (/ (abs du) L)))
    (asec:depth-at r u))
)

;;; nearest (near-face) depth of occluder oc over U a..b (clipped to the piece; linear -> min at an end)
(defun asec:depth-over (oc a b)
  (min (asec:near-depth-at oc (max a (asec:get 'umin oc)))
       (asec:near-depth-at oc (min b (asec:get 'umax oc))))
)

;;; SILHOUETTE test for a vertical END EDGE at u (depth v), Z z0..z1. The edge is sampled just
;;; either side of u; a side is covered where any occluder piece spans it at depth <= v + depth
;;; tolerance, the edge's own wall included. The edge survives only where at least one side is
;;; open: a real wall end or exterior corner. Edges with an equal-or-nearer surface on both sides
;;; (a side wall's inner face inside a facade, a seam between flush walls, an edge behind a nearer
;;; wall) disappear. Cut pieces keep their U-tolerance pad.
(defun asec:visible-edge-z (u z0 z1 v occ / at zt res blk pad)
  (setq at *asec-occlusion-u-tolerance* zt *asec-occlusion-z-tolerance*)
  (foreach s (list (- u at) (+ u at))
    (setq blk nil)
    (foreach oc occ
      (setq pad (if (= (asec:get 'type oc) "CUT") at 0.0))
      (if (and (<= (- (asec:get 'umin oc) pad) s (+ (asec:get 'umax oc) pad))
               (<= (asec:depth-over oc s s) (+ v *asec-occlusion-depth-tolerance*)))
        (setq blk (cons (list (asec:get 'zmin oc) (asec:get 'zmax oc)) blk))))
    (setq res (append res (asec:subtract-intervals (list (list (min z0 z1) (max z0 z1)))
                                                   (asec:merge-intervals blk zt) zt))))
  (asec:merge-intervals res zt)
)

;;; horizontal object at height z along a record rec whose depth varies with U (slab edge, low
;;; wall top): each occluder overlap is judged at its midpoint, object farther by more than the
;;; depth tolerance -> hidden there (so a slab edge flush with its own facade stays visible)
(defun asec:visible-u-varying (a b z rec occ ign / at iv res blk lo hi)
  (setq at *asec-occlusion-u-tolerance* iv (list (list (min a b) (max a b))))
  (foreach s (list (- z *asec-occlusion-z-tolerance*) (+ z *asec-occlusion-z-tolerance*))
    (setq blk nil)
    (foreach oc occ
      (if (and (not (asec:ignored-occluder-p oc ign))
               (<= (asec:get 'zmin oc) s (asec:get 'zmax oc))
               (asec:interval-overlap-p (car iv) (list (asec:get 'umin oc) (asec:get 'umax oc)) at))
        (progn
          (setq lo (max (caar iv) (asec:get 'umin oc)) hi (min (cadar iv) (asec:get 'umax oc)))
          (if (> (asec:near-depth-at rec (* 0.5 (+ lo hi))) (+ (asec:depth-over oc lo hi) *asec-occlusion-depth-tolerance*))
            (setq blk (cons (list lo hi) blk))))))
    (setq res (append res (asec:subtract-intervals iv (asec:merge-intervals blk at) at))))
  (asec:merge-intervals res at)
)

;;; emitters into the generated-line buffer (front-most + same-depth stages still apply)
(defun asec:draw-vedge (org u bot top v occ lay / ivs)
  (setq ivs (asec:visible-edge-z u bot top v occ))
  (foreach iv ivs
    (asec:emit-line (list (+ (car org) u) (+ (cadr org) (car iv)))
                    (list (+ (car org) u) (+ (cadr org) (cadr iv))) lay v))
  ivs
)

(defun asec:draw-hline-varying (org rec a b z occ ign lay / ivs)
  (setq ivs (asec:visible-u-varying a b z rec occ ign))
  (foreach iv ivs
    (asec:emit-line (list (+ (car org) (car iv)) (+ (cadr org) z))
                    (list (+ (car org) (cadr iv)) (+ (cadr org) z))
                    lay (asec:near-depth-at rec (* 0.5 (+ (car iv) (cadr iv))))))
  ivs
)

;;; object at depth v over U a..b is farther than the occluder there (equal depth never hides)
(defun asec:nearer-occluder-p (oc v a b)
  (> v (+ (asec:depth-over oc a b) *asec-occlusion-depth-tolerance*))
)

;;; -> ((u0 u1) ...) visible parts of U a..b at height z, depth v; nil = fully hidden
(defun asec:visible-u-intervals (a b z v occ / res)
  (setq res (asec:visible-u-intervals-ign a b z v occ nil))
  (if *asec-debug* (asec:debug-occlusion-query a b z v occ res))
  res
)

;;; ln = (u1 u2 z v)
(defun asec:horizontal-visible-pieces (ln occ)
  (asec:visible-u-intervals (car ln) (cadr ln) (caddr ln) (cadddr ln) occ)
)

;;; ign = ((type . index) ...) owners whose pieces never block (self / host wall)
(defun asec:ignored-occluder-p (oc ign)
  (member (cons (asec:get 'type oc) (asec:get 'index oc)) ign)
)

;;; blockers of one sample: nearer, not ignored, sample s inside piece's range on axis
;;; (lo/hi keys); cut walls widened by the U tolerance on U (edges on a cut face stay hidden)
;;; ua..ub = the object's U extent used for the occluder's depth there
(defun asec:axis-blockers (s lo hi v ua ub occ ign okey1 okey2 / pad out)
  (foreach oc occ
    (setq pad (if (and (= lo 'umin) (= (asec:get 'type oc) "CUT")) *asec-occlusion-u-tolerance* 0.0))
    (if (and (asec:nearer-occluder-p oc v ua ub) (not (asec:ignored-occluder-p oc ign))
             (<= (- (asec:get lo oc) pad) s (+ (asec:get hi oc) pad)))
      (setq out (cons (list (asec:get okey1 oc) (asec:get okey2 oc)) out))))
  out
)

;;; visible parts of a0..a1 on one axis, at fixed coordinate c on the other axis.
;;; c exactly on a piece boundary (wall end, jamb, sill, head) is ambiguous, so c is
;;; sampled just either side and the union kept: an edge on a jamb shows through the void.
(defun asec:visible-axis-intervals (a0 a1 c v occ ign across along / ct at iv s res)
  (if (= along 'u)
    (setq ct *asec-occlusion-z-tolerance* at *asec-occlusion-u-tolerance*)
    (setq ct *asec-occlusion-u-tolerance* at *asec-occlusion-z-tolerance*))
  (setq iv (list (list (min a0 a1) (max a0 a1))))
  (foreach s (list (- c ct) (+ c ct))
    (setq res (append res
                (asec:subtract-intervals iv
                  (asec:merge-intervals
                    (if (= along 'u)
                      (asec:axis-blockers s 'zmin 'zmax v (min a0 a1) (max a0 a1) occ ign 'umin 'umax)
                      (asec:axis-blockers s 'umin 'umax v s s occ ign 'zmin 'zmax))
                    at)
                  at))))
  (asec:merge-intervals res at)
)

(defun asec:visible-u-intervals-ign (a b z v occ ign)
  (asec:visible-axis-intervals a b z v occ ign 'z 'u)
)

;;; vertical object at U u, Z z0..z1, depth v -> ((z0 z1) ...)
(defun asec:visible-z-intervals (u z0 z1 v occ ign)
  (asec:visible-axis-intervals z0 z1 u v occ ign 'u 'z)
)

;;; ---------- clipped generated linework ----------
;;; pts absolute section points; vis = (org v occ ign). Horizontal/vertical segments are
;;; clipped; an unclipped shape keeps its original LWPOLYLINE, otherwise visible LINE pieces.
;;; ponytail: diagonal segments drawn whole (none in current projected graphics)
(defun asec:segment-visible (p q vis / org ox oz u0 u1 z0 z1)
  (setq org (car vis)
        u0 (- (car p) (car org)) z0 (- (cadr p) (cadr org))
        u1 (- (car q) (car org)) z1 (- (cadr q) (cadr org)))
  (cond
    ((< (abs (- z1 z0)) asec:tol)
     (mapcar '(lambda (iv) (list (list (+ (car org) (car iv)) (cadr p)) (list (+ (car org) (cadr iv)) (cadr p))))
             (asec:visible-u-intervals-ign u0 u1 z0 (cadr vis) (caddr vis) (cadddr vis))))
    ((< (abs (- u1 u0)) asec:tol)
     (mapcar '(lambda (iv) (list (list (car p) (+ (cadr org) (car iv))) (list (car p) (+ (cadr org) (cadr iv)))))
             (asec:visible-z-intervals u0 z0 z1 (cadr vis) (caddr vis) (cadddr vis))))
    (T (list (list p q))))
)

;;; ---------- generated line buffer: same-depth consolidation before entmake ----------
;;; record (layer orient coord a b v): "H" coord = Y, a..b = X; "V" coord = X, a..b = Y.
;;; Lines already clipped by visibility are buffered; asec:flush-lines merges records with the
;;; same layer + orientation, coordinate within U tolerance and V within depth tolerance,
;;; whose intervals overlap or touch (U tolerance). Different depths never merge.
(defun asec:emit-line (p q lay v)
  (cond
    ((< (abs (- (cadr p) (cadr q))) asec:tol)
     (setq asec:gen-lines (cons (list lay "H" (cadr p) (min (car p) (car q)) (max (car p) (car q)) v) asec:gen-lines)))
    ((< (abs (- (car p) (car q))) asec:tol)
     (setq asec:gen-lines (cons (list lay "V" (car p) (min (cadr p) (cadr q)) (max (cadr p) (cadr q)) v) asec:gen-lines)))
    (T (asec:line p q lay)))
)

;;; cut rectangle (wall / low wall): drawn as before, and its four edges recorded as depth-0
;;; references so no projected line is emitted on top of a cut edge
(defun asec:cut-rect (x0 y0 x1 y1 lay)
  (setq asec:gen-cut
         (append (list (list 'CUT "V" x0 (min y0 y1) (max y0 y1) 0.0)
                       (list 'CUT "V" x1 (min y0 y1) (max y0 y1) 0.0)
                       (list 'CUT "H" y0 (min x0 x1) (max x0 x1) 0.0)
                       (list 'CUT "H" y1 (min x0 x1) (max x0 x1) 0.0))
                 asec:gen-cut))
  (asec:rect x0 y0 x1 y1 lay)
)

(defun asec:debug-suppression (r blk vis / s)
  (defun s (q) (strcat (if (eq (car q) 'CUT) "CUT" (car q)) " " (cadr q) " at " (rtos (caddr q) 2 1)
                       "  span " (rtos (nth 3 q) 2 1) " -> " (rtos (nth 4 q) 2 1) "  V " (rtos (nth 5 q) 2 1)))
  (princ (strcat "\n--- OVERLAP SUPPRESSION ---\nCandidate: " (s r)))
  (foreach q blk (princ (strcat "\nOccluded by: " (s q))))
  (princ (strcat "\nRemaining intervals: " (asec:ivstr vis)))
)

;;; FRONT-MOST stage, then same-depth consolidation, then entmake.
;;; 1. records nearest first (smallest V); each is cut back by already accepted records on the
;;;    same line (orientation, coordinate within U tol, same layer) that are nearer by more than
;;;    the depth tolerance, and by cut rectangle edges (depth 0, any layer); remainders kept.
;;;    Only buffered projected linework takes part: hatches, markers, text, cut drawing never do.
;;; 2. same layer/line/depth records merge where they overlap or touch (unchanged).
(defun asec:flush-lines (/ groups g hit tl out dt acc blk vis)
  (setq tl *asec-occlusion-u-tolerance* dt *asec-occlusion-depth-tolerance*)
  (foreach r (asec:sort asec:gen-lines '(lambda (p q) (< (nth 5 p) (nth 5 q))))
    (setq blk nil)
    (foreach q (append asec:gen-cut acc)
      (if (and (= (cadr q) (cadr r))
               (<= (abs (- (caddr q) (caddr r))) tl)
               (or (eq (car q) 'CUT) (= (car q) (car r)))
               (< (nth 5 q) (- (nth 5 r) dt))
               (asec:interval-overlap-p (list (nth 3 r) (nth 4 r)) (list (nth 3 q) (nth 4 q)) tl))
        (setq blk (cons q blk))))
    (setq vis (if blk
                (asec:subtract-intervals (list (list (nth 3 r) (nth 4 r)))
                                         (asec:merge-intervals (mapcar '(lambda (q) (list (nth 3 q) (nth 4 q))) blk) tl)
                                         tl)
                (list (list (nth 3 r) (nth 4 r)))))
    (if (and blk *asec-debug*) (asec:debug-suppression r blk vis))
    (foreach iv vis
      (setq acc (cons (list (car r) (cadr r) (caddr r) (car iv) (cadr iv) (nth 5 r)) acc))))
  (foreach r acc
    (setq hit nil out nil)
    (foreach g groups
      (if (and (not hit)
               (= (car r) (car (car g))) (= (cadr r) (cadr (car g)))
               (<= (abs (- (caddr r) (caddr (car g)))) tl)
               (<= (abs (- (nth 5 r) (nth 5 (car g)))) *asec-occlusion-depth-tolerance*))
        (setq hit T g (list (car g) (cons (list (nth 3 r) (nth 4 r)) (cadr g)))))
      (setq out (cons g out)))
    (setq groups (if hit out (cons (list r (list (list (nth 3 r) (nth 4 r)))) out))))
  (foreach g groups
    (foreach iv (asec:merge-intervals (cadr g) tl)
      (if (= (cadr (car g)) "H")
        (asec:line (list (car iv) (caddr (car g))) (list (cadr iv) (caddr (car g))) (car (car g)))
        (asec:line (list (caddr (car g)) (car iv)) (list (caddr (car g)) (cadr iv)) (car (car g))))))
  (setq asec:gen-lines nil)
)

(defun asec:vis-poly (pts closed lay vis / segs pieces whole r)
  (if (null vis)
    (if (cddr pts) (asec:poly pts closed lay) (asec:line (car pts) (cadr pts) lay))
    (progn
      (setq segs pts whole T)
      (while (cdr segs)
        (setq pieces (cons (list (car segs) (cadr segs)) pieces) segs (cdr segs)))
      (if (= closed 1) (setq pieces (cons (list (last pts) (car pts)) pieces)))
      (foreach sg pieces
        (setq r (cons (asec:segment-visible (car sg) (cadr sg) vis) r))
        (if (not (and (= (length (car r)) 1)
                      (< (abs (- (distance (caaar r) (cadaar r)) (distance (car sg) (cadr sg))))
                         *asec-occlusion-u-tolerance*)))
          (setq whole nil)))
      (if (and whole (cddr pts))
        (asec:poly pts closed lay)
        (foreach sg r (foreach pc sg (asec:emit-line (car pc) (cadr pc) lay (cadr vis)))))))
)

(defun asec:vis-rect (x0 y0 x1 y1 lay vis)
  (asec:vis-poly (list (list x0 y0) (list x1 y0) (list x1 y1) (list x0 y1)) 1 lay vis)
)

(defun asec:debug-occlusion-query (a b z v occ res / r2)
  (defun r2 (x) (rtos x 2 2))
  (princ (strcat "\n--- OCCLUSION QUERY ---\nOBJECT  U: " (r2 a) " -> " (r2 b)
                 "  Z: " (r2 z) "  V: " (r2 v) "\nBLOCKERS"))
  (foreach oc occ
    (princ (strcat "\n" (if (= (asec:get 'type oc) "CUT") "Wall W" "Wall P")
                   (itoa (asec:get 'index oc))
                   "  V: " (r2 (asec:get 'v oc))
                   "  U: " (r2 (asec:get 'umin oc)) " -> " (r2 (asec:get 'umax oc))
                   "  Z: " (r2 (asec:get 'zmin oc)) " -> " (r2 (asec:get 'zmax oc))
                   "  BLOCKS: " (if (and (asec:nearer-occluder-p oc v a b)
                                         (asec:occluder-blocks-at-z-p oc z)) "YES" "NO"))))
  (princ "\nVISIBLE INTERVALS")
  (if (null res) (princ "\n(none)"))
  (foreach iv res (princ (strcat "\n" (r2 (car iv)) " -> " (r2 (cadr iv)))))
)

;;; pure-math check of the engine: (asec:occlusion-selftest) -> T when all pass
(defun asec:occlusion-selftest (/ wall same check ok win door solid)
  (defun wall (u0 u1 v voids)
    (asec:wall-occluder-pieces "PROJECTED" 1 (list (cons 'umin u0) (cons 'umax u1)) v 0.0 3000.0 voids))
  (defun same (p q)
    (and (= (length p) (length q))
         (apply 'and (mapcar '(lambda (x y) (and (equal (car x) (car y) 1e-6)
                                                 (equal (cadr x) (cadr y) 1e-6))) p q))))
  (defun check (name got want)
    (princ (strcat "\n" (if (same got want) "PASS  " (progn (setq ok nil) "FAIL  ")) name))
    (if (not (same got want)) (princ (strcat "  got " (fmt-ivs got)))))
  (defun fmt-ivs (x / s)
    (setq s "") (foreach iv x (setq s (strcat s "(" (rtos (car iv) 2 0) " " (rtos (cadr iv) 2 0) ")"))) s)
  (setq ok T
        win   (wall 2000.0 6000.0 2000.0 '((3500.0 4700.0 900.0 2400.0)))
        door  (wall 2000.0 6000.0 2000.0 '((3500.0 4400.0 0.0 2100.0)))
        solid (wall 3000.0 5000.0 3000.0 nil))
  (check "window Z1500" (asec:visible-u-intervals 0.0 8000.0 1500.0 6000.0 win)
         '((0.0 2000.0) (3500.0 4700.0) (6000.0 8000.0)))
  (check "window Z500"  (asec:visible-u-intervals 0.0 8000.0 500.0 6000.0 win)
         '((0.0 2000.0) (6000.0 8000.0)))
  (check "window Z2600" (asec:visible-u-intervals 0.0 8000.0 2600.0 6000.0 win)
         '((0.0 2000.0) (6000.0 8000.0)))
  (check "door Z1000"   (asec:visible-u-intervals 0.0 8000.0 1000.0 6000.0 door)
         '((0.0 2000.0) (3500.0 4400.0) (6000.0 8000.0)))
  (check "door Z2500"   (asec:visible-u-intervals 0.0 8000.0 2500.0 6000.0 door)
         '((0.0 2000.0) (6000.0 8000.0)))
  (check "object nearer than wall" (asec:visible-u-intervals 0.0 8000.0 1500.0 1000.0 win)
         '((0.0 8000.0)))
  (check "same depth" (asec:visible-u-intervals 0.0 8000.0 1500.0 2005.0 win) '((0.0 8000.0)))
  (check "window wall + solid wall behind it"
         (asec:visible-u-intervals 0.0 8000.0 1500.0 6000.0 (append win solid))
         '((0.0 2000.0) (6000.0 8000.0)))
  (check "fully hidden" (asec:visible-u-intervals 2500.0 3000.0 500.0 6000.0 win) nil)
  (check "edge through window" (asec:visible-z-intervals 4000.0 0.0 3000.0 6000.0 win nil)
         '((900.0 2400.0)))
  (check "edge through door" (asec:visible-z-intervals 4000.0 0.0 3000.0 6000.0 door nil)
         '((0.0 2100.0)))
  (check "edge on jamb" (asec:visible-z-intervals 3500.0 0.0 3000.0 6000.0 win nil)
         '((900.0 2400.0)))
  (check "edge beyond wall end" (asec:visible-z-intervals 1500.0 0.0 3000.0 6000.0 win nil)
         '((0.0 3000.0)))
  (check "edge on wall end" (asec:visible-z-intervals 2000.0 0.0 3000.0 6000.0 win nil)
         '((0.0 3000.0)))
  (check "edge: window wall + solid wall" (asec:visible-z-intervals 4000.0 0.0 3000.0 6000.0 (append win solid) nil)
         nil)
  (check "ignored owner" (asec:visible-z-intervals 2500.0 0.0 3000.0 6000.0 win (list (cons "PROJECTED" 1)))
         '((0.0 3000.0)))
  (princ (if ok "\nOcclusion selftest: ALL PASS" "\nOcclusion selftest: FAILURES"))
  ok
)

;;; debug only: context + every element's plan position -> local U -> final drawing X.
;;; Pure printing; identifies whether a reversal is in plan->U or in U->X.
(defun asec:debug-orientation-trace (sec org uleft / m vd vb r x)
  (defun x (u) (rtos (+ (car org) u) 2 1))
  (setq m (asec:get 'master sec) vd (asec:get 'viewdir sec) vb (asec:get 'view sec)
        r (list (cadr vd) (- (car vd))))
  (princ (strcat "\n--- ORIENTATION TRACE ---"
                 "\nView N: " (asec:pt-str vd) "  Screen-right R: " (asec:pt-str r)
                 "\nNormalized start: " (asec:pt-str (car m)) "  end: " (asec:pt-str (cadr m))
                 "\nU direction: " (asec:pt-str (caddr m)) "  length: " (rtos (nth 3 m) 2 1)
                 "\nView U " (rtos (asec:get 'umin vb) 2 1) " -> " (rtos (asec:get 'umax vb) 2 1)
                 "  V " (rtos (asec:get 'vnear vb) 2 1) " -> " (rtos (asec:get 'vfar vb) 2 1)
                 (if (asec:get 'clip vb) "  (ASA field)" "")
                 "\nInsertion: " (asec:pt-str (asec:get 'ins sec)) "  uLeft: " (rtos uleft 2 1)
                 "  origin X: " (rtos (car org) 2 1)))
  (foreach fl (asec:get 'floors sec)
    (princ (strcat "\n" (asec:floor-name (asec:get 'index fl))
                   "  floor section " (asec:pt-str (asec:get 'start fl)) " -> " (asec:pt-str (asec:get 'end fl))))
    (foreach w (asec:get 'walls fl)
      (princ (strcat "\n  CUT WALL  faces " (asec:pt-str (asec:get 'p1 w)) " / " (asec:pt-str (asec:get 'p2 w))
                     "  stations " (rtos (asec:get 's1 w) 2 1) " / " (rtos (asec:get 's2 w) 2 1)
                     "  U " (rtos (asec:get 'umin w) 2 1) " -> " (rtos (asec:get 'umax w) 2 1)
                     "  X " (x (asec:get 'umin w)) " -> " (x (asec:get 'umax w)))))
    (foreach w (asec:get 'pwalls fl)
      (princ (strcat "\n  PROJ WALL  plan " (asec:pt-str (asec:get 'cstart w)) " -> " (asec:pt-str (asec:get 'cend w))
                     "  U1 " (rtos (asec:get 'u1 w) 2 1) "  U2 " (rtos (asec:get 'u2 w) 2 1)
                     "  U " (rtos (asec:get 'umin w) 2 1) " -> " (rtos (asec:get 'umax w) 2 1)
                     "  X " (x (asec:get 'umin w)) " -> " (x (asec:get 'umax w))
                     "  Vmid " (rtos (asec:get 'vmid w) 2 1))))
    (foreach o (append (asec:get 'cutdoors fl) (asec:get 'cutwins fl)
                       (asec:get 'projdoors fl) (asec:get 'projwins fl))
      (princ (strcat "\n  " (asec:get 'mode o) " " (asec:get 'kind o)
                     "  insert " (if (asec:get 'ename o) (asec:pt-str (asec:2d (asec:get 10 (entget (asec:get 'ename o))))) "-")
                     "  U " (rtos (asec:get 'umin o) 2 1) " -> " (rtos (asec:get 'umax o) 2 1)
                     "  X " (x (asec:get 'umin o)) " -> " (x (asec:get 'umax o))))))
)

(defun asec:draw-section (sec / cfg slabT floors n i k top allw uleft org ops items pops focc slabs ss si prev es)
  (setq cfg    (asec:get 'config sec)
        slabT  (caddr cfg)
        floors (asec:get 'floors sec)
        n      (length floors))
  (foreach fl floors (setq allw (append allw (asec:get 'walls fl))))
  (setq uleft (if allw (car (asec:wall-bounds allw)) 0.0)
        ;; insertion point = leftmost wall face on ground line
        org   (list (- (car (asec:get 'ins sec)) uleft)
                    (+ (cadr (asec:get 'ins sec)) slabT)))
  (setq asec:gen-lines nil asec:gen-cut nil)
  (if *asec-debug* (asec:debug-orientation-trace sec org uleft))
  ;; FINAL output layers (created white if missing; existing layers untouched)
  (foreach k '("WALL_CUT_LAYER" "OPENING_CUT_LAYER" "OPENING_PROJ_BORDER_LAYER"
               "OPENING_PROJ_DETAIL_LAYER" "RCC_CUT_LAYER" "PROJ_WALL_LAYER" "PROJ_SLAB_LAYER" "HATCH_LAYER")
    (asec:ensure-layer (asec:gfx k) 7))
  (foreach fl floors
    (setq i   (asec:get 'index fl)
          ;; WALL_TOP: only TO_SLAB_UNDERSIDE exists
          top (- (asec:floor-elevation (1+ i) cfg) slabT)
          ops (append (asec:get 'cutdoors fl) (asec:get 'cutwins fl))
          k   0)
    ;; slab: this floor's current S-SLAB edges, else the storey below's top (hidden) edges
    (setq si (asec:slab-intervals (asec:get 'slab fl) (cond ((asec:get 'scur fl)) (prev (asec:get 'stop prev)))))
    (if *asec-debug*
      (asec:debug-slab-extent (asec:floor-name i) (asec:get 'slab fl) (asec:get 'scur fl)
                              (if prev (asec:get 'stop prev)) si))
    (if (cadr si) (princ (strcat "\n" (asec:floor-name i) " slab: " (cadr si) " - automatic extent kept.")))
    (setq es nil)
    (foreach iv (car si)
      (setq es (cons (asec:draw-slab org (asec:get 'elev fl) iv slabT) es) slabs (cons (car es) slabs)))
    (if *asec-debug* (asec:debug-slab-draw (asec:floor-name i) (car si) (asec:get 'elev fl) slabT (reverse es)))
    (setq prev fl)
    (foreach w (asec:get 'walls fl)
      (setq k (1+ k))
      (asec:draw-wall-with-openings org w k (asec:get 'elev fl) top ops))
    ;; cut low walls: cut-wall treatment, floor datum -> datum + RAILING_HEIGHT
    (foreach w (asec:get 'lwalls fl)
      (asec:hatch (asec:cut-rect (+ (car org) (asec:get 'umin w)) (+ (cadr org) (asec:get 'bottom w))
                                 (+ (car org) (asec:get 'umax w)) (+ (cadr org) (asec:get 'top w))
                                 (asec:gfx "WALL_CUT_LAYER"))
                  "WALL"))
    (foreach o (asec:get 'cutdoors fl) (asec:draw-cut-door org o))
    (foreach o (asec:get 'cutwins fl) (asec:draw-cut-window org o))
    ;; projected items are collected here and drawn below in depth order
    (setq k 0 pops (append (asec:get 'projdoors fl) (asec:get 'projwins fl)))
    (setq focc (asec:build-floor-occluders fl cfg))
    ;; this floor's cut slab bands block projected geometry behind them (depth 0)
    (foreach iv (car si)
      (setq focc (append focc (asec:wall-occluder-pieces "CSLAB" 0 (list (cons 'umin (car iv)) (cons 'umax (cadr iv)))
                                                         0.0 (- (asec:get 'elev fl) slabT) (asec:get 'elev fl) nil))))
    (foreach pw (asec:get 'pwalls fl)
      (setq k (1+ k) items (cons (list (asec:get 'vmid pw) pw k pops focc (asec:get 'pwalls fl)) items)))
    (foreach o pops
      (if (null (asec:get 'phost o))
        (setq items (cons (list (asec:get 'v o) nil nil o focc) items))))
    (setq k 0)
    (foreach pw (asec:get 'plwalls fl)
      (setq k (1+ k) items (cons (list (asec:get 'vmid pw) pw k 'LOW focc (asec:get 'plwalls fl)) items)))
    (setq k 0)
    (foreach r (asec:get 'pslabs fl)
      (setq k (1+ k) items (cons (list (* 0.5 (+ (asec:get 'v1 r) (asec:get 'v2 r))) r k 'PSLAB focc
                                       (asec:pslab-level r fl cfg))
                                 items)))
  )
  ;; DEPTH: V > 0 on the picked viewing side; larger V = farther beyond the cut.
  ;; Drawn farther -> nearer (walls by Vmid, unhosted openings by V) so nearer
  ;; entities are created last. Oblique walls are ordered by average depth only.
  (setq items (asec:sort items '(lambda (p q) (> (car p) (car q)))))
  (asec:dbg (strcat "\n--- DEPTH ORDER (farther -> nearer) ---  Viewing direction: "
                    (asec:pt-str (asec:get 'viewdir sec))))
  (foreach it items
    (asec:dbg (strcat "\n" (if (cadr it) (strcat "Projected wall " (itoa (caddr it)))
                                         (strcat "Unhosted " (strcase (asec:get 'kind (cadddr it)) T)))
                      "  V = " (rtos (car it) 2 2))))
  (foreach it items
    (cond
      ((eq (cadddr it) 'PSLAB)
       (asec:draw-projected-slab org (cadr it) (caddr it) (nth 4 it) (nth 5 it) slabT))
      ((eq (cadddr it) 'LOW)
       (asec:draw-projected-low-wall org (cadr it) (caddr it) (nth 4 it) (nth 5 it)))
      ((cadr it)
       (asec:draw-projected-wall-with-openings org (cadr it) (caddr it) (cadddr it) (nth 4 it) (nth 5 it)))
      (T (asec:draw-projected-opening org (cadddr it) (list org (car it) (nth 4 it) nil)))))
  (asec:flush-lines)
  ;; roof: top floor's hidden S-SLAB edges (top of that storey), else the roof extent
  (if (asec:get 'roof sec)
    (progn
      (setq si (asec:slab-intervals (asec:get 'roof sec) (asec:get 'stop prev)))
      (if *asec-debug* (asec:debug-slab-extent "ROOF" (asec:get 'roof sec) nil (asec:get 'stop prev) si))
      (if (cadr si) (princ (strcat "\nRoof slab: " (cadr si) " - roof extent kept.")))
      (setq es nil)
      (foreach iv (car si)
        (setq es (cons (asec:draw-roof-slab org (asec:floor-elevation n cfg) iv slabT) es) slabs (cons (car es) slabs)))
      (if *asec-debug* (asec:debug-slab-draw "ROOF" (car si) (asec:floor-elevation n cfg) slabT (reverse es)))))
  (if (= (asec:setting "GROUND_LINE") "YES")
    (progn (asec:ensure-layer (asec:gfx "GROUND_LAYER") 7)
           (asec:draw-ground org (nth 3 (asec:get 'master sec)) slabT)))
  ;; slab outlines above everything else (walls, hatches, openings)
  (setq ss (ssadd))
  (foreach e slabs (if e (ssadd e ss)))
  (if (> (sslength ss) 0) (command-s "_.DRAWORDER" ss "" "_F"))
)

;;; ---------- graphics config (AS_GRAPHICS.txt) ----------
(setq asec:gfx-defaults
  '(("WALL_CUT_LAYER" . "ELV-1") ("OPENING_CUT_LAYER" . "ELV-2")
    ("OPENING_PROJ_BORDER_LAYER" . "ELV-3") ("OPENING_PROJ_DETAIL_LAYER" . "ELV-4")
    ("RCC_CUT_LAYER" . "ELV-5") ("PROJ_WALL_LAYER" . "ELV-3") ("PROJ_SLAB_LAYER" . "ELV-3")
    ("GROUND_LAYER" . "ELV-5") ("GROUND_COLOR" . "5")
    ("HATCH_LAYER" . "X-HATCH") ("WALL_HATCH_PATTERN" . "ANSI31")
    ("WALL_HATCH_SCALE" . "20") ("WALL_HATCH_ANGLE" . "0") ("RCC_HATCH_PATTERN" . "SOLID")
    ("CONSTRUCTION_LAYER" . "AS_CONSTRUCTION") ("CONSTRUCTION_COLOR" . "4")
    ("SECTION_MARK_LAYER" . "AS-SECTION-MARK") ("SECTION_MARK_COLOR" . "BYLAYER")
    ("SECTION_MARK_SIZE" . "500") ("SECTION_MARK_TEXT_HEIGHT" . "150")
    ("SECTION_TITLE_LAYER" . "AS-TEXT") ("SECTION_TITLE_HEIGHT" . "250")
    ("SECTION_TITLE_OFFSET" . "500") ("ASA_VIEW_GAP" . "2000")
    ("RAILING_HEIGHT" . "1200")
    ("SECTION_LINE_LAYER" . "X-TAGS&SYMBOLS") ("SECTION_MARKER_RADIUS" . "250")
    ("SECTION_MARKER_TRIANGLE_SIZE" . "350") ("SECTION_MARKER_SOLID_LENGTH" . "350")
    ("SECTION_MARKER_TEXT_HEIGHT" . "180") ("SECTION_MARKER_TEXT_COLOR" . "2")
    ("SECTION_LINE_DASHED_LINETYPE" . "DASHED") ("SECTION_LINE_DASHED_COLOR" . "8")))

;;; numeric key: value if it parses to >= lo, else the built-in default (bad values never abort)
(defun asec:gfx-num (k lo / v d)
  (setq v (atof (cond ((asec:gfx k)) (""))) d (atof (cdr (assoc k asec:gfx-defaults))))
  (if (and (asec:gfx k) (distof (asec:gfx k)) (>= v lo)) v d)
)
(if (null asec:gfx-values) (setq asec:gfx-values asec:gfx-defaults))

(defun asec:gfx (k) (cdr (assoc k asec:gfx-values)))

(defun asec:trim (s)
  (while (and (> (strlen s) 0) (member (substr s 1 1) '(" " "\t"))) (setq s (substr s 2)))
  (while (and (> (strlen s) 0) (member (substr s (strlen s) 1) '(" " "\t" "\r")))
    (setq s (substr s 1 (1- (strlen s)))))
  s
)

;;; "a, b,c" -> ("a" "b" "c")
(defun asec:split (s / i out cur c)
  (setq i 1 cur "")
  (while (<= i (1+ (strlen s)))
    (setq c (if (<= i (strlen s)) (substr s i 1) ","))
    (if (= c ",")
      (progn (if (/= (asec:trim cur) "") (setq out (cons (asec:trim cur) out))) (setq cur ""))
      (setq cur (strcat cur c)))
    (setq i (1+ i)))
  (reverse out)
)

(defun asec:gfx-path (/ f i)
  (cond
    ((findfile "AS_GRAPHICS.txt"))
    ((setq f (findfile "AS.lsp"))
     (setq i (strlen f))
     (while (and (> i 0) (not (member (substr f i 1) '("/" "\\")))) (setq i (1- i)))
     (findfile (strcat (substr f 1 i) "AS_GRAPHICS.txt")))))

;;; re-read at every AS start so saved edits apply without reloading AS.lsp
(defun asec:load-graphics (/ f fh ln i k v)
  (setq asec:gfx-values asec:gfx-defaults)
  (if (and (setq f (asec:gfx-path)) (setq fh (open f "r")))
    (progn
      (while (setq ln (read-line fh))
        (setq ln (asec:trim ln) i 1)
        (while (and (<= i (strlen ln)) (/= (substr ln i 1) "=")) (setq i (1+ i)))
        (if (and (> (strlen ln) 0) (/= (substr ln 1 1) "#") (< 1 i (1+ (strlen ln))))
          (setq k (strcase (asec:trim (substr ln 1 (1- i))))
                v (asec:trim (substr ln (1+ i)))
                asec:gfx-values (cons (cons k v)
                                      (if (assoc k asec:gfx-values)
                                        (asec:remove-key k asec:gfx-values)
                                        asec:gfx-values)))))
      (close fh)
      (princ (strcat "\nGraphics: " f)))
    (princ "\nAS_GRAPHICS.txt not found: using built-in graphics defaults."))
  (if (setq v (asec:gfx "WALL_SOURCE_LAYERS")) (setq *aseca-wall-layers* (asec:split v)))
  (if (setq v (asec:gfx "DOOR_SOURCE_LAYERS")) (setq *aseca-door-layers* (asec:split v)))
  (if (setq v (asec:gfx "WINDOW_SOURCE_LAYERS")) (setq *aseca-window-layers* (asec:split v)))
  (if (setq v (asec:gfx "SLAB_SOURCE_LAYERS")) (setq *aseca-slab-layers* (asec:split v)))
  (if (setq v (asec:gfx "RAILING_SOURCE_LAYERS")) (setq *aseca-railing-layers* (asec:split v)))
)

(defun asec:remove-key (k al / out)
  (foreach x al (if (/= (car x) k) (setq out (cons x out))))
  (reverse out)
)

;;; ---------- construction entities (temporary, tracked) ----------
;;; ed = entity data without layer. Creates on CONSTRUCTION_LAYER (layer made if missing,
;;; existing layer untouched), registers the ename in asec:temp, returns it.
(defun asec:make-construction (ed / e)
  (asec:ensure-layer (asec:gfx "CONSTRUCTION_LAYER") (atoi (asec:gfx "CONSTRUCTION_COLOR")))
  (if (setq e (entmakex (append ed (list (cons 8 (asec:gfx "CONSTRUCTION_LAYER"))))))
    (setq asec:temp (cons e asec:temp) asec:temp-created (1+ (cond (asec:temp-created) (0)))))
  e
)

;;; review labels: TEXT construction entities. W/C = cut wall, P = projected wall.
(defun asec:label (p s se / h)
  (setq h (* 0.02 (distance (car se) (cadr se))))
  (asec:make-construction (list '(0 . "TEXT") (list 10 (car p) (cadr p) 0.0)
                                (list 11 (car p) (cadr p) 0.0) (cons 40 h) (cons 1 s)
                                '(72 . 1) '(73 . 2)))
)

(defun asec:unlabel (e)
  (if (and e (entget e))
    (progn (entdel e) (setq asec:temp-cleaned (1+ (cond (asec:temp-cleaned) (0)))))))

(defun asec:clear-labels ()
  (foreach e asec:temp (asec:unlabel e))
  (setq asec:temp nil)
)

(defun asec:construction-report (/ n)
  (setq n 0)
  (foreach e asec:temp (if (entget e) (setq n (1+ n))))
  (if *asec-debug*
    (princ (strcat "\nAS construction cleanup:\nCreated: " (itoa (cond (asec:temp-created) (0)))
                   "\nCleaned: " (itoa (cond (asec:temp-cleaned) (0)))
                   "\nRemaining tracked: " (itoa n))))
  (if (> n 0) (princ (strcat "\nWARNING:\nAS construction entities remain: " (itoa n))))
)

(defun asec:cleanup ()
  (asec:clear-labels)
  (setq asec:pending-marker nil asec:marker-ghost nil)
  (setq *asec-closing-layers* aseca:old-closing-layers)
  (redraw)
  (if asec:undo-open (progn (command-s "_.UNDO" "_E") (setq asec:undo-open nil)))
  (if asec:cmdecho (setvar "CMDECHO" asec:cmdecho))
)

(defun asec:error (msg)
  (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
    (princ (strcat "\nAS error: " msg)))
  (asec:cleanup)
  (setq *error* asec:old-error)
  (princ)
)

;;; generation tail; floors newest first
(defun asec:generate (m vd vb cfg floors / sec roof)
  (setq roof (asec:get-roof-slab (car floors) (caddr m)))
  (redraw)
  (setq sec (list (cons 'master m) (cons 'viewdir vd) (cons 'config cfg) (cons 'view vb)
                  (cons 'floors (reverse floors)) (cons 'roof roof)
                  (cons 'ins (asec:getpt "\nSpecify section insertion point: "))))
  (setq asec:cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (asec:clear-labels)
  (command-s "_.UNDO" "_BE")
  (setq asec:undo-open T)
  ;; AS > Section: the permanent marker is made here, inside the same UNDO group, so a
  ;; cancelled run leaves no marker and one Undo removes marker + section together
  (if asec:pending-marker
    (progn (apply 'asec:draw-section-marker asec:pending-marker) (setq asec:pending-marker nil)))
  (asec:draw-section sec)
  (princ (strcat "\nSection generated: " (itoa (length floors)) " floor(s)."))
)

;;; ============================================================
;;; AUTOMATIC DISCOVERY + REVIEW
;;; ============================================================
(if (null *aseca-wall-layers*) (setq *aseca-wall-layers* '("A-WALL")))
(if (null *aseca-door-layers*) (setq *aseca-door-layers* '("A-DOOR")))
(if (null *aseca-window-layers*) (setq *aseca-window-layers* '("A-WINDOW")))
(if (null *aseca-slab-layers*) (setq *aseca-slab-layers* '("S-SLAB")))
(if (null *aseca-railing-layers*) (setq *aseca-railing-layers* '("A-RAILING")))
(if (null *aseca-min-wall-thickness*) (setq *aseca-min-wall-thickness* 50.0))
(if (null *aseca-max-wall-thickness*) (setq *aseca-max-wall-thickness* 600.0))
(if (null *aseca-collinear-tolerance*) (setq *aseca-collinear-tolerance* 5.0))
(if (null *aseca-small-gap-tolerance*) (setq *aseca-small-gap-tolerance* 50.0))
(if (null *aseca-min-face-length*) (setq *aseca-min-face-length* 300.0))   ; shorter = jamb return / nib
(if (null *asec-wall-warning-thickness*) (setq *asec-wall-warning-thickness* 500.0))

(defun aseca:dbg (s) (if *aseca-debug* (princ s)) T)


;;; ---------- small utilities ----------
(defun aseca:layer-p (lay lst) (member (strcase lay) (mapcar 'strcase lst)))

(defun aseca:join (lst sep / s)
  (foreach x lst (setq s (if s (strcat s sep x) x)))
  (if s s "")
)

;;; escape wildcard characters so layer names match literally in ssget filters
(defun aseca:wc-escape (s / i c out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq c (substr s i 1) i (1+ i)
          out (strcat out (if (member c '("#" "@" "." "*" "?" "~" "[" "]" "`")) (strcat "`" c) c))))
  out
)

;;; enames of TYP on LAYERS in the current space (database filter, no screen dependency)
(defun aseca:ss-list (typ layers / ss i out)
  (setq ss (ssget "_X" (list (cons 0 typ)
                             (cons 8 (aseca:join (mapcar 'aseca:wc-escape layers) ","))
                             (cons 410 (getvar "CTAB"))))
        i 0)
  (while (and ss (< i (sslength ss)))
    ;; "_X" also returns entities on off/frozen layers: hidden layers are never analysed
    (if (asec:layer-visible-p (asec:get 8 (entget (ssname ss i))))
      (setq out (cons (ssname ss i) out)))
    (setq i (1+ i)))
  out
)

;;; (ename a b) for a LINE
(defun aseca:line (e / ed)
  (setq ed (entget e))
  (list e (asec:2d (asec:get 10 ed)) (asec:2d (asec:get 11 ed)))
)

(defun aseca:unres (reason pt)
  (aseca:dbg (strcat "\nUNRESOLVED: " reason))
  (list (cons 'reason reason) (cons 'pt pt))
)

(defun aseca:memq (x lst / r) (foreach y lst (if (eq x y) (setq r T))) r)

(defun aseca:parallel-p (d1 d2) (<= (abs (asec:cross d1 d2)) *asec-parallel-tolerance*))

(defun aseca:mid (a b) (asec:v* (asec:v+ a b) 0.5))

(defun aseca:uv->plan (u v ctx)
  (asec:v+ (asec:v+ (car (asec:get 'se ctx)) (asec:v* (asec:get 'dir ctx) u))
           (asec:v* (asec:get 'vd ctx) v))
)

;;; ---------- blocks ----------
;;; ((kind . "DOOR"|"WINDOW") (bd . block data) (jm . jambs|nil)) for every opening INSERT
(defun aseca:collect-blocks (/ out bd)
  (foreach e (aseca:ss-list "INSERT" (append *aseca-door-layers* *aseca-window-layers*))
    (setq bd (asec:get-block-data e)
          out (cons (list (cons 'kind (if (aseca:layer-p (asec:get 'layer bd) *aseca-door-layers*)
                                        "DOOR" "WINDOW"))
                          (cons 'bd bd)
                          (cons 'jm (asec:get-block-opening-span bd)))
                    out)))
  out
)

;;; ---------- cut walls ----------
;;; crossings (u pt (a b) enames source), sorted by U and merged within collinear tolerance
(defun aseca:cut-crossings (se dir lines blocks / out ip gap jm j1 w n width hd ta tb tlo thi)
  (setq gap *aseca-small-gap-tolerance*)
  (foreach ln lines
    (if (and (> (distance (cadr ln) (caddr ln)) asec:tol)
             (setq ip (inters (car se) (cadr se) (cadr ln) (caddr ln) T)))
      (setq out (cons (list (asec:station-on-section (asec:2d ip) (car se) dir) (asec:2d ip)
                            (cdr ln) (list (car ln)) "direct")
                      out))))
  ;; face broken by an opening: a parallel wall segment ends at a jamb and its
  ;; supporting line crosses the section inside that opening's jamb span
  (foreach ob blocks
    (if (setq jm (asec:get 'jm ob))
      (progn
        (setq j1 (car jm) width (caddr jm) hd (cadddr jm)
              w (asec:unit (asec:v- (cadr jm) j1)) n (asec:perp w))
        (foreach ln lines
          (if (and (> (distance (cadr ln) (caddr ln)) asec:tol)
                   (aseca:parallel-p (asec:unit (asec:v- (caddr ln) (cadr ln))) w)
                   (<= (abs (asec:dot (asec:v- (cadr ln) j1) n)) (+ hd *aseca-max-wall-thickness*))
                   (not (inters (car se) (cadr se) (cadr ln) (caddr ln) T))
                   (setq ta (asec:dot (asec:v- (cadr ln) j1) w)
                         tb (asec:dot (asec:v- (caddr ln) j1) w)
                         tlo (min ta tb) thi (max ta tb))
                   (or (<= (abs thi) gap) (<= (abs (- tlo width)) gap))
                   (setq ip (inters (car se) (cadr se) (cadr ln) (caddr ln) nil))
                   (asec:on-section-p (setq ip (asec:2d ip)) se)
                   (<= (- gap) (asec:dot (asec:v- ip j1) w) (+ width gap)))
            (setq out (cons (list (asec:station-on-section ip (car se) dir) ip
                                  (cdr ln) (list (car ln)) "opening-gap")
                            out)))))))
  (setq out (aseca:merge-crossings (asec:sort out '(lambda (p q) (< (car p) (car q))))))
  (foreach x out
    (aseca:dbg (strcat "\nCUT FACE  U = " (rtos (car x) 2 2) "  " (nth 4 x) "  layers/handles:"
                       (apply 'strcat (mapcar '(lambda (e) (strcat " " (asec:get 8 (entget e)) "/" (asec:get 5 (entget e))))
                                              (cadddr x))))))
  out
)

;;; same face seen twice (collinear pieces / duplicates): keep one, prefer direct, merge enames
(defun aseca:merge-crossings (xs / out base)
  (foreach x xs
    (if (and out (<= (- (car x) (car (car out))) *aseca-collinear-tolerance*))
      (setq base (if (= (nth 4 x) "direct") x (car out))
            out  (cons (list (car base) (cadr base) (caddr base)
                             (append (cadddr x) (cadddr (car out))) (nth 4 base))
                       (cdr out)))
      (setq out (cons x out))))
  (reverse out)
)

;;; (walls unresolved) - pairs 1-2, 3-4 ... ; any bad pair leaves ALL walls unresolved
(defun aseca:pair-cut-walls (xs / walls a b thk bad)
  (cond
    ((null xs) (list nil nil))
    ((= (rem (length xs) 2) 1)
     (list nil (list (aseca:unres (strcat (itoa (length xs))
                                          " cut wall-face crossings found; an even number is required")
                                  (cadr (car xs))))))
    (T
     (while xs
       (setq a (car xs) b (cadr xs) xs (cddr xs) thk (- (car b) (car a)))
       (aseca:dbg (strcat "\nCUT PAIR  U " (rtos (car a) 2 2) " -> " (rtos (car b) 2 2)
                          "  thickness " (rtos thk 2 2)))
       (if (or (< thk *aseca-min-wall-thickness*) (> thk *aseca-max-wall-thickness*))
         (setq bad (cadr a))
         (setq walls (cons (list (cons 'p1 (cadr a)) (cons 'p2 (cadr b))
                                 (cons 's1 (car a)) (cons 's2 (car b))
                                 (cons 'f1 (strcat "auto " (nth 4 a))) (cons 'f2 (strcat "auto " (nth 4 b)))
                                 (cons 'l1 (caddr a)) (cons 'l2 (caddr b))
                                 (cons 'umin (car a)) (cons 'umax (car b)) (cons 'thk thk)
                                 (cons 'enames (append (cadddr a) (cadddr b)))
                                 (cons 'conf (if (and (= (nth 4 a) "direct") (= (nth 4 b) "direct")
                                                      (<= thk *asec-wall-warning-thickness*))
                                               "HIGH" "MEDIUM")))
                           walls))))
     (if bad
       (list nil (list (aseca:unres "cut wall pairing gives a thickness outside the automatic range" bad)))
       (list (reverse walls) nil))))
)

;;; ---------- cut openings ----------
(defun aseca:section-through-opening-p (jm ctx / se w ip)
  (setq se (asec:get 'se ctx) w (asec:unit (asec:v- (cadr jm) (car jm))))
  (and (setq ip (inters (car se) (cadr se) (car jm) (cadr jm) nil))
       (asec:on-section-p (setq ip (asec:2d ip)) se)
       (<= (- asec:tol) (asec:dot (asec:v- ip (car jm)) w) (+ (caddr jm) asec:tol)))
)

;;; block insertion near this floor's section or inside its view boundary
(defun aseca:block-near-view-p (ob ctx / se uv vb mt)
  (setq se (asec:get 'se ctx) vb (asec:get 'view ctx) mt *aseca-max-wall-thickness*
        uv (asec:section-coordinate (asec:get 'ins (asec:get 'bd ob)) (car se) (asec:get 'dir ctx) (asec:get 'vd ctx)))
  (and (<= (- mt) (car uv) (+ (asec:get 'umax vb) mt))
       (<= (- mt) (cadr uv) (+ (asec:get 'vfar vb) mt)))
)

;;; (cutdoors cutwins not-cut-blocks unresolved)
(defun aseca:classify-cut-openings (blocks walls ctx / cd cw rest un jm bd kind mid k hosts wl rec)
  (foreach ob blocks
    (setq jm (asec:get 'jm ob) bd (asec:get 'bd ob) kind (asec:get 'kind ob) hosts nil k 0)
    (cond
      ((null jm)
       (if (aseca:block-near-view-p ob ctx)
         (setq un (cons (aseca:unres (strcat (strcase kind T) " jamb geometry could not be read ("
                                             (asec:get 'name bd) ")")
                                     (asec:get 'ins bd))
                        un))
         (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " ignored: unreadable jambs, far from view"))))
      (T
       (setq mid (aseca:mid (car jm) (cadr jm)))
       (foreach w walls
         (setq k (1+ k))
         (if (and (asec:wall-strip w)
                  (asec:point-in-wall-strip (asec:strip-offsets mid (asec:wall-strip w)) (cadddr jm))
                  (asec:section-crosses-opening jm w))
           (setq hosts (cons k hosts))))
       (cond
         ((> (length hosts) 1)
          (setq un (cons (aseca:unres (strcat (strcase kind T) " matches several cut walls") mid) un)))
         ((= (length hosts) 1)
          (setq wl  (nth (1- (car hosts)) walls)
                rec (cons (cons 'conf "HIGH")
                          (asec:make-opening kind "CUT" bd jm (car hosts)
                                             (asec:get 'umin wl) (asec:get 'umax wl) ctx nil)))
          (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " -> CUT in C" (itoa (car hosts))))
          (cond
            ((asec:validate-wall-openings rec (append cd cw))
             (setq un (cons (aseca:unres (strcat "cut " (strcase kind T) " overlaps another cut opening") mid) un)))
            ((= kind "DOOR") (setq cd (cons rec cd)))
            (T (setq cw (cons rec cw)))))
         ((aseca:section-through-opening-p jm ctx)
          (setq un (cons (aseca:unres (strcat "section crosses " (strcase kind T) " but no cut wall hosts it") mid) un)))
         (T (setq rest (cons ob rest)))))))
  (list (reverse cd) (reverse cw) (reverse rest) un)
)

;;; ---------- projected walls ----------
(defun aseca:seg-interval (ln origin d / ta tb)
  (setq ta (asec:dot (asec:v- (cadr ln) origin) d) tb (asec:dot (asec:v- (caddr ln) origin) d))
  (list (min ta tb) (max ta tb) (car ln))
)

;;; gap [g0,g1] along a face is explained by a parallel opening block spanning it
(defun aseca:gap-explained-p (g0 g1 origin d n blocks / jm hit ja jb gap)
  (setq gap *aseca-small-gap-tolerance*)
  (foreach ob blocks
    (if (and (setq jm (asec:get 'jm ob))
             (aseca:parallel-p (asec:unit (asec:v- (cadr jm) (car jm))) d)
             (<= (abs (asec:dot (asec:v- (car jm) origin) n)) (+ (cadddr jm) *aseca-max-wall-thickness*))
             (setq ja (asec:dot (asec:v- (car jm) origin) d) jb (asec:dot (asec:v- (cadr jm) origin) d))
             (<= (min ja jb) (+ g0 gap))
             (>= (max ja jb) (- g1 gap)))
      (setq hit T)))
  hit
)

(defun aseca:make-face (cur origin d)
  (list (cons 'a (asec:v+ origin (asec:v* d (car cur))))
        (cons 'b (asec:v+ origin (asec:v* d (cadr cur))))
        (cons 'dir d) (cons 'len (- (cadr cur) (car cur)))
        (cons 'segs (caddr cur)))
)

;;; parallel + collinear LINEs grouped into logical faces, split at gaps that are
;;; neither small nor explained by an opening. Source lines are never joined.
(defun aseca:chain-faces (cand blocks / ln d n grp rest cur faces)
  (while cand
    (setq ln (car cand) cand (cdr cand)
          d (asec:unit (asec:v- (caddr ln) (cadr ln))) n (asec:perp d)
          grp (list ln) rest nil cur nil)
    (foreach o cand
      (if (and (aseca:parallel-p (asec:unit (asec:v- (caddr o) (cadr o))) d)
               (<= (abs (asec:dot (asec:v- (cadr o) (cadr ln)) n)) *aseca-collinear-tolerance*))
        (setq grp (cons o grp))
        (setq rest (cons o rest))))
    (setq cand (reverse rest))
    (foreach sg (asec:sort (mapcar '(lambda (g) (aseca:seg-interval g (cadr ln) d)) grp)
                           '(lambda (p q) (< (car p) (car q))))
      (if (and cur
               (or (<= (- (car sg) (cadr cur)) *aseca-small-gap-tolerance*)
                   (aseca:gap-explained-p (cadr cur) (car sg) (cadr ln) d n blocks)))
        (setq cur (list (car cur) (max (cadr cur) (cadr sg)) (cons (caddr sg) (caddr cur))))
        (progn
          (if cur (setq faces (cons (aseca:make-face cur (cadr ln) d) faces)))
          (setq cur (list (car sg) (cadr sg) (list (caddr sg)))))))
    (if cur (setq faces (cons (aseca:make-face cur (cadr ln) d) faces))))
  (foreach f faces
    (aseca:dbg (strcat "\nLOGICAL FACE  " (asec:pt-str (asec:get 'a f)) " -> " (asec:pt-str (asec:get 'b f))
                       "  segments " (itoa (length (asec:get 'segs f)))
                       "  layers/handles:"
                       (apply 'strcat (mapcar '(lambda (e) (strcat " " (asec:get 8 (entget e)) "/" (asec:get 5 (entget e))))
                                              (asec:get 'segs f))))))
  faces
)

;;; faces that could be the other side of the same wall
(defun aseca:face-partners (f faces / out sep g0 g1 ov)
  (foreach g faces
    (if (and (not (eq f g))
             (aseca:parallel-p (asec:get 'dir f) (asec:get 'dir g))
             (setq sep (abs (asec:dot (asec:v- (asec:get 'a g) (asec:get 'a f)) (asec:perp (asec:get 'dir f)))))
             (>= sep *aseca-min-wall-thickness*)
             (<= sep *aseca-max-wall-thickness*)
             (setq g0 (asec:dot (asec:v- (asec:get 'a g) (asec:get 'a f)) (asec:get 'dir f))
                   g1 (asec:dot (asec:v- (asec:get 'b g) (asec:get 'a f)) (asec:get 'dir f))
                   ov (- (min (asec:get 'len f) (max g0 g1)) (max 0.0 (min g0 g1))))
             (>= ov (* 0.5 (min (asec:get 'len f) (asec:get 'len g)))))
      (setq out (cons g out))))
  out
)

(defun aseca:partners-of (f pl / r) (foreach e pl (if (eq (car e) f) (setq r (cdr e)))) r)

;;; partners all lie on one support line without overlapping each other
;;; (e.g. an outer wall face opposite two inner faces split by a T-junction)
(defun aseca:collinear-disjoint-p (ps / ok d a0 a1 b0 b1)
  (setq ok T)
  (foreach g ps
    (foreach h ps
      (if (and ok (not (eq g h)))
        (progn
          (setq d (asec:get 'dir g)
                a0 0.0 a1 (asec:get 'len g)
                b0 (asec:dot (asec:v- (asec:get 'a h) (asec:get 'a g)) d)
                b1 (asec:dot (asec:v- (asec:get 'b h) (asec:get 'a g)) d))
          (if (or (not (aseca:parallel-p d (asec:get 'dir h)))
                  (> (abs (asec:dot (asec:v- (asec:get 'a h) (asec:get 'a g)) (asec:perp d)))
                     *aseca-collinear-tolerance*)
                  (> (- (min a1 (max b0 b1)) (max a0 (min b0 b1))) *aseca-collinear-tolerance*))
            (setq ok nil))))))
  ok
)

;;; portion of face f opposite face g, as (a b) on f's support line
(defun aseca:clip-face-to (f g / d t0 t1)
  (setq d (asec:get 'dir f)
        t0 (max 0.0 (min (asec:dot (asec:v- (asec:get 'a g) (asec:get 'a f)) d)
                         (asec:dot (asec:v- (asec:get 'b g) (asec:get 'a f)) d)))
        t1 (min (asec:get 'len f) (max (asec:dot (asec:v- (asec:get 'a g) (asec:get 'a f)) d)
                                       (asec:dot (asec:v- (asec:get 'b g) (asec:get 'a f)) d))))
  (list (asec:v+ (asec:get 'a f) (asec:v* d t0)) (asec:v+ (asec:get 'a f) (asec:v* d t1)))
)

;;; A jamb return at a door/window hole crosses both faces and looks like a wall end.
;;; If an opening block starts at a wall end, the wall continues over it: extend the end
;;; to the far jamb, then look for a real closing line there. ext = (e0 e1 m0 m1 confident)
(defun aseca:extend-over-openings (ext f1 f2 gg blocks / d n off origin e0 e1 m0 m1 c0 c1 x0 x1 i changed
                                       jm ja jb lo hi lat gap)
  (setq d (car gg) n (cadr gg) off (caddr gg) origin (car f1) gap *aseca-small-gap-tolerance*
        e0 (car ext) e1 (cadr ext) m0 (caddr ext) m1 (cadddr ext) i 0 changed T)
  (while (and changed (< i 5))
    (setq changed nil i (1+ i))
    (foreach ob blocks
      (if (and (setq jm (asec:get 'jm ob))
               (aseca:parallel-p (asec:unit (asec:v- (cadr jm) (car jm))) d)
               (setq lat (- (asec:dot (asec:v- (aseca:mid (car jm) (cadr jm)) origin) n) (* 0.5 off)))
               (<= (abs lat) (+ (cadddr jm) (abs off)))
               (setq ja (asec:dot (asec:v- (car jm) origin) d)
                     jb (asec:dot (asec:v- (cadr jm) origin) d)
                     lo (min ja jb) hi (max ja jb)))
        (cond
          ((and (<= (abs (- lo e1)) gap) (> hi (+ e1 asec:tol)))
           (setq e1 hi x1 T changed T))
          ((and (<= (abs (- hi e0)) gap) (< lo (- e0 asec:tol)))
           (setq e0 lo x0 T changed T))))))
  (if x0 (setq c0 (asec:find-projected-wall-closing-line f1 f2 d n off e0 T)
               m0 (if c0 "OPENING+CLOSING" "OPENING") e0 (if c0 c0 e0)))
  (if x1 (setq c1 (asec:find-projected-wall-closing-line f1 f2 d n off e1 nil)
               m1 (if c1 "OPENING+CLOSING" "OPENING") e1 (if c1 c1 e1)))
  (list e0 e1 m0 m1
        (and (if x0 c0 (= m0 "CLOSING-LINES")) (if x1 c1 (= m1 "CLOSING-LINES"))))
)

;;; projected wall record (with conf/view-rel/segs) from face lines f1 f2, or nil
(defun aseca:build-projected-wall (f1 f2 segs ctx blocks / gg ext rec vb)
  (setq vb (asec:get 'view ctx) gg (asec:validate-projected-wall-faces f1 f2))
  (if (/= gg 'bad)
    (progn
      (setq ext (aseca:extend-over-openings (asec:projected-wall-auto-extent f1 f2 gg) f1 f2 gg blocks))
      (aseca:dbg (strcat "\nPROJECTED PAIR  " (asec:pt-str (car f1)) " / " (asec:pt-str (car f2))
                         "  ends " (caddr ext) " / " (cadddr ext)
                         "  owned segments " (itoa (length segs))))
      (if (> (- (cadr ext) (car ext)) asec:tol)
        (progn
          (setq rec (asec:make-projected-wall f1 f2 gg (list (car ext) (cadr ext) (caddr ext) (cadddr ext)) ctx))
          (append (list (cons 'conf (if (nth 4 ext) "HIGH" "MEDIUM"))
                        (cons 'view-rel (asec:classify-in-view
                                          (list (asec:get 'u1 rec) (asec:get 'v1 rec))
                                          (list (asec:get 'u2 rec) (asec:get 'v2 rec)) vb))
                        (cons 'segs segs))
                  rec)))))
)

(defun aseca:face-line (f) (list (asec:get 'a f) (asec:get 'b f)))

;;; (pwalls unresolved). A face pairs with its unique partner; one face opposite several
;;; collinear non-overlapping faces (T-junction) pairs with each of them. Faces shorter
;;; than *aseca-min-face-length* (jamb returns, nibs) are not wall-face candidates.
(defun aseca:discover-projected-walls (lines excl blocks ctx / se dir vd vb cand faces pl used un pws
                                             ps g rec)
  (setq se (asec:get 'se ctx) dir (asec:get 'dir ctx) vd (asec:get 'vd ctx) vb (asec:get 'view ctx))
  (foreach ln lines
    (if (and (not (member (car ln) excl))
             (> (distance (cadr ln) (caddr ln)) asec:tol)
             (asec:entity-bounds-overlap-view-p (cadr ln) (caddr ln) (car se) dir vd vb))
      (setq cand (cons ln cand))))
  (foreach f (aseca:chain-faces (reverse cand) blocks)
    (if (>= (asec:get 'len f) *aseca-min-face-length*) (setq faces (cons f faces))))
  (foreach f faces (setq pl (cons (cons f (aseca:face-partners f faces)) pl)))
  (foreach f faces
    (setq ps (aseca:partners-of f pl))
    (cond
      ((aseca:memq f used))
      ((and (> (length ps) 1) (aseca:collinear-disjoint-p ps))
       (aseca:dbg (strcat "\nT-JUNCTION FACE  " (asec:pt-str (asec:get 'a f)) "  opposite "
                          (itoa (length ps)) " collinear faces"))
       (setq used (cons f used))
       (foreach g ps
         (if (and (not (aseca:memq g used))
                  (= (length (aseca:partners-of g pl)) 1))
           (progn
             (setq used (cons g used))
             (if (setq rec (aseca:build-projected-wall (aseca:clip-face-to f g) (aseca:face-line g)
                                                       (append (asec:get 'segs f) (asec:get 'segs g)) ctx blocks))
               (setq pws (cons rec pws)))))))
      ((> (length ps) 1)
       (setq un (cons (aseca:unres (strcat "wall face has " (itoa (length ps)) " possible partner faces")
                                   (asec:get 'a f))
                      un)))
      ((and (= (length ps) 1)
            (setq g (car ps))
            (= (length (aseca:partners-of g pl)) 1)
            (not (aseca:memq g used)))
       (setq used (cons f (cons g used)))
       (if (setq rec (aseca:build-projected-wall (aseca:face-line f) (aseca:face-line g)
                                                 (append (asec:get 'segs f) (asec:get 'segs g)) ctx blocks))
         (setq pws (cons rec pws))))))
  (list (reverse pws) un)
)

;;; ---------- projected openings ----------
;;; (projdoors projwins unresolved)
(defun aseca:classify-projected-openings (blocks pws ctx / se dir vd vb pd pw un jm bd kind uv0 uv1 rec
                                               cands pwl tl)
  (setq se (asec:get 'se ctx) dir (asec:get 'dir ctx) vd (asec:get 'vd ctx) vb (asec:get 'view ctx)
        tl *asec-wall-host-tolerance*)
  (foreach ob blocks
    (setq jm (asec:get 'jm ob) bd (asec:get 'bd ob) kind (asec:get 'kind ob)
          uv0 (asec:section-coordinate (car jm) (car se) dir vd)
          uv1 (asec:section-coordinate (cadr jm) (car se) dir vd))
    (cond
      ((= (asec:classify-in-view uv0 uv1 vb) "OUTSIDE")                 ; not in view: ignore
       (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " ignored: outside view boundary  V = "
                          (rtos (cadr uv0) 2 0) " / " (rtos (cadr uv1) 2 0)
                          "  (vFar " (rtos (asec:get 'vfar vb) 2 0) ")")))
      ((< (abs (- (car uv1) (car uv0))) asec:tol)                        ; edge-on: invisible
       (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " ignored: edge-on to the section")))
      (T
       (setq rec (asec:make-opening kind "PROJECTED" bd jm nil (min (car uv0) (car uv1))
                                    (max (car uv0) (car uv1)) ctx (if (= kind "DOOR") "SINGLE"))
             cands (asec:projected-host-candidates jm pws))
       (cond
         ((> (length cands) 1)
          (setq un (cons (aseca:unres (strcat "projected " (strcase kind T) " matches several projected walls")
                                      (aseca:mid (car jm) (cadr jm)))
                         un)))
         ;; in view but in no projected wall (e.g. door between a wall end and another wall):
         ;; keep it unhosted, as the manual collector's Keep does
         ((null cands)
          (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " -> PROJECTED unhosted"))
          (setq rec (cons (cons 'conf "MEDIUM") rec))
          (if (= kind "DOOR") (setq pd (cons rec pd)) (setq pw (cons rec pw))))
         ((and (setq pwl (nth (1- (caar cands)) pws))
               (>= (asec:get 'umin rec) (- (asec:get 'umin pwl) tl))
               (<= (asec:get 'umax rec) (+ (asec:get 'umax pwl) tl)))
          (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " -> PROJECTED in P" (itoa (caar cands))))
          (setq rec (append (list (cons 'phost (caar cands)) (cons 'conf "HIGH")) rec))
          (if (= kind "DOOR") (setq pd (cons rec pd)) (setq pw (cons rec pw))))
         (T
          (setq un (cons (aseca:unres (strcat "projected " (strcase kind T) " exceeds its host wall extent")
                                      (aseca:mid (car jm) (cadr jm)))
                         un)))))))
  (list (reverse pd) (reverse pw) un)
)

;;; ---------- review ----------
(defun aseca:conf (r) (cond ((asec:get 'conf r)) ("GUIDED")))

(defun aseca:label-ops (ops pre ctx / k)
  (setq k 0)
  (foreach o ops
    (setq k (1+ k))
    (asec:label (aseca:uv->plan (asec:get 'u o) (asec:get 'v o) ctx) (strcat pre (itoa k)) (asec:get 'se ctx)))
)

(defun aseca:label-all (walls cd cw pws pd pw un ctx / se k)
  (asec:clear-labels)
  (setq se (asec:get 'se ctx) k 0)
  (foreach w walls
    (setq k (1+ k))
    (asec:label (aseca:mid (asec:get 'p1 w) (asec:get 'p2 w)) (strcat "C" (itoa k)) se))
  (aseca:label-ops cd "CD" ctx)
  (aseca:label-ops cw "CW" ctx)
  (setq k 0)
  (foreach w pws
    (setq k (1+ k))
    (asec:label (aseca:mid (asec:get 'cstart w) (asec:get 'cend w)) (strcat "P" (itoa k)) se))
  (aseca:label-ops pd "PD" ctx)
  (aseca:label-ops pw "PW" ctx)
  (setq k 0)
  (foreach u un
    (setq k (1+ k))
    (if (asec:get 'pt u) (asec:label (asec:get 'pt u) (strcat "?" (itoa k)) se)))
)

(defun aseca:print-ops (ops pre hostkey hostpre / k)
  (setq k 0)
  (foreach o ops
    (setq k (1+ k))
    (princ (strcat "\n  " pre (itoa k) "  width " (rtos (asec:get 'width o) 2 0)
                   (if (asec:get hostkey o) (strcat "  -> " hostpre (itoa (asec:get hostkey o))) "  unhosted")
                   "  " (aseca:conf o))))
)

(defun aseca:print-summary (i walls cd cw pws pd pw un / k)
  (princ (strcat "\n\nAUTO SECTION - " (asec:floor-name i) " ANALYSIS"))
  (princ "\nCUT WALLS")
  (setq k 0)
  (foreach w walls
    (setq k (1+ k))
    (princ (strcat "\n  C" (itoa k) "  " (rtos (asec:get 'thk w) 2 0) "  " (aseca:conf w))))
  (princ "\nCUT OPENINGS")
  (aseca:print-ops cd "CD" 'host "C")
  (aseca:print-ops cw "CW" 'host "C")
  (princ "\nPROJECTED WALLS")
  (setq k 0)
  (foreach w pws
    (setq k (1+ k))
    (princ (strcat "\n  P" (itoa k) "  " (rtos (asec:get 'thk w) 2 0)
                   "  length " (rtos (- (asec:get 't1 w) (asec:get 't0 w)) 2 0)
                   "  " (aseca:conf w)
                   (if (asec:get 'view-rel w) (strcat "  " (asec:get 'view-rel w)) ""))))
  (princ "\nPROJECTED OPENINGS")
  (aseca:print-ops pd "PD" 'phost "P")
  (aseca:print-ops pw "PW" 'phost "P")
  (princ "\nUNRESOLVED")
  (setq k 0)
  (foreach u un
    (setq k (1+ k))
    (princ (strcat "\n  ?" (itoa k) "  " (asec:get 'reason u))))
  (if (null un) (princ "\n  none"))
)

(defun aseca:drop-nth (lst n / i out)
  (setq i 0)
  (foreach x lst (setq i (1+ i)) (if (/= i n) (setq out (cons x out))))
  (reverse out)
)

(defun aseca:without (key o / out)
  (foreach x o (if (not (eq (car x) key)) (setq out (cons x out))))
  (reverse out)
)

;;; after removing wall n: openings on n are dropped (drop T) or unhosted, higher hosts shift down
(defun aseca:rehost (ops key n drop / out h)
  (foreach o ops
    (setq h (asec:get key o))
    (cond
      ((or (null h) (< h n)) (setq out (cons o out)))
      ((= h n) (if (not drop) (setq out (cons (aseca:without key o) out))))
      (T (setq out (cons (subst (cons key (1- h)) (assoc key o) o) out)))))
  (reverse out)
)

(defun aseca:valid-n (n lst)
  (if (and n (<= 1 n (length lst))) T (progn (princ "\nInvalid number.") nil))
)

;;; ---------- floor analysis + review ----------
;;; non-interactive discovery for one floor of one view -> data record (review input)
;;; ref nil = pick it (upper floors)
(defun aseca:discover-floor (i m vd vb gfref cfg ref / fs se dir lines blocks xs pr walls excl ctx
                                 cut prj pro cd cw pws pd pw un ext)
  (setq fs (asec:floor-section i m vd vb gfref ref) ref (car fs) se (cadr fs) dir (caddr m))
  (setq lines  (mapcar 'aseca:line (aseca:ss-list "LINE" *aseca-wall-layers*))
        blocks (aseca:collect-blocks)
        xs     (aseca:cut-crossings se dir lines blocks)
        pr     (aseca:pair-cut-walls xs)
        walls  (car pr)
        un     (cadr pr)
        excl   (apply 'append (mapcar 'cadddr xs))   ; cut faces are never projected faces
        ctx    (asec:make-floor-ctx i se m vd vb cfg walls)
        cut    (aseca:classify-cut-openings blocks walls ctx)
        cd     (car cut)
        cw     (cadr cut)
        un     (append un (cadddr cut))
        prj    (aseca:discover-projected-walls lines excl blocks ctx)
        pws    (car prj)
        un     (append un (cadr prj))
        pro    (aseca:classify-projected-openings (caddr cut) pws ctx)
        pd     (car pro)
        pw     (cadr pro)
        un     (append un (caddr pro)))
  (setq ext (aseca:discover-extras i se dir ctx))
  (list (cons 'ref ref) (cons 'se se) (cons 'walls walls) (cons 'cd cd) (cons 'cw cw)
        (cons 'pws pws) (cons 'pd pd) (cons 'pw pw) (cons 'un (append un (asec:get 'un ext)))
        (cons 'lwalls (asec:get 'lwalls ext)) (cons 'plwalls (asec:get 'plwalls ext))
        (cons 'scur (asec:get 'scur ext)) (cons 'stop (asec:get 'stop ext))
        (cons 'pslabs (asec:get 'pslabs ext)))
)

;;; ---------- S-SLAB explicit slab edges + A-RAILING low walls ----------
;;; S-SLAB LINE crossing the finite section = one slab edge (u pt). Effective linetype
;;; (entity, else layer when BYLAYER/unset) HIDDEN* -> 'stop (slab at the top of this
;;; storey = datum(i+1)), otherwise 'scur (this floor's slab). Source linetype is semantic only.
;;; A-RAILING = solid LOW WALL: cut faces paired like cut walls, projected faces like projected
;;; walls; bottom = floor datum, top = datum + RAILING_HEIGHT.
(defun asec:effective-linetype (ed / lt)
  (setq lt (asec:get 6 ed))
  (if (or (null lt) (= (strcase lt) "BYLAYER"))
    (setq lt (asec:get 6 (tblsearch "LAYER" (asec:get 8 ed)))))
  (if lt (strcase lt) "CONTINUOUS")
)

(defun asec:railing-height () (asec:gfx-num "RAILING_HEIGHT" 1.0))

;;; straight segments of an S-SLAB entity in WCS: ((a b bulge n) ...)
;;; LINE -> one segment. LWPOLYLINE -> vertices (group 10, OCS at elevation 38) converted with
;;; trans via the entity's own OCS, segment i uses the bulge (42) stored after vertex i;
;;; closed (70 bit 1) adds last -> first. Other types -> nil.
(defun asec:entity-segments (e / ed typ vs z pts out n k)
  (setq ed (entget e) typ (asec:get 0 ed))
  (cond
    ((= typ "LINE")
     (list (list (asec:2d (asec:get 10 ed)) (asec:2d (asec:get 11 ed)) 0.0 1)))
    ((= typ "LWPOLYLINE")
     (setq z (cond ((asec:get 38 ed)) (0.0)))
     (foreach g ed
       (cond ((= (car g) 10) (setq vs (cons (list (cadr g) (caddr g) 0.0) vs)))
             ((and (= (car g) 42) vs) (setq vs (cons (list (car (car vs)) (cadr (car vs)) (cdr g)) (cdr vs))))))
     (setq vs (reverse vs)
           pts (mapcar '(lambda (v) (list (asec:2d (trans (list (car v) (cadr v) z) e 0)) (caddr v))) vs)
           k 0)
     (if (= 1 (logand 1 (cond ((asec:get 70 ed)) (0))))
       (setq pts (append pts (list (car pts)))))
     (while (cdr pts)
       (setq k (1+ k) out (cons (list (car (car pts)) (car (cadr pts)) (cadr (car pts)) k) out) pts (cdr pts)))
     (reverse out)))
)

;;; S-SLAB crossings of the finite section se -> (current top curved-points)
;;; Every straight segment actually crossing the section gives one edge (u pt); crossings of one
;;; entity within *aseca-collinear-tolerance* (vertex hits) are one edge, and so are identical
;;; crossings from different entities. Bulged segments are never treated as chords.
(defun aseca:slab-edges (i se dir / ed lt hid ip u rec cur top curved segs mine reason dup)
  (if *asec-debug*
    (progn
      (princ (strcat "\n--- S-SLAB DEBUG ---  Floor: " (asec:floor-name i)
                     "\nSource layers: " (aseca:join *aseca-slab-layers* ",")
                     "\nSection start: " (asec:pt-str (car se)) "  end: " (asec:pt-str (cadr se))))
      (foreach typ '("POLYLINE" "INSERT" "SPLINE" "ARC")
        (if (aseca:ss-list typ *aseca-slab-layers*)
          (princ (strcat "\nIgnored " typ " on slab layer: " (itoa (length (aseca:ss-list typ *aseca-slab-layers*)))
                         " (LINE and LWPOLYLINE are read)"))))))
  (foreach e (append (aseca:ss-list "LINE" *aseca-slab-layers*) (aseca:ss-list "LWPOLYLINE" *aseca-slab-layers*))
    (setq ed (entget e) lt (asec:effective-linetype ed) hid (wcmatch lt "HIDDEN*")
          segs (asec:entity-segments e) mine nil)
    (asec:dbg (strcat "\n  Handle " (asec:get 5 ed) "  Layer " (asec:get 8 ed) "  Type " (asec:get 0 ed)
                      "  Entity LT " (cond ((asec:get 6 ed)) ("(unset)"))
                      "  Layer LT " (cond ((asec:get 6 (tblsearch "LAYER" (asec:get 8 ed)))) ("?"))
                      "  Effective " lt "  Class " (if hid "TOP" "CURRENT")
                      (if (= (asec:get 0 ed) "LWPOLYLINE")
                        (strcat "  Closed " (if (= 1 (logand 1 (cond ((asec:get 70 ed)) (0)))) "Yes" "No")
                                "  Vertices " (itoa (length (asec:dxf-10s ed))) "  Segments " (itoa (length segs)))
                        "")))
    (foreach sg segs
      (setq ip nil u nil)
      (setq reason
        (cond
          ((<= (distance (car sg) (cadr sg)) asec:tol) "ZERO-LENGTH")
          ((not (setq ip (inters (car se) (cadr se) (car sg) (cadr sg) T))) "NO-CROSS")
          ((> (abs (caddr sg)) 1e-6) (setq curved (cons (asec:2d ip) curved)) "CURVED-UNSUPPORTED")
          ((progn (setq u (asec:station-on-section (asec:2d ip) (car se) dir) dup nil)
                  (foreach m (append mine (if hid top cur))
                    (if (<= (abs (- u (car m))) *aseca-collinear-tolerance*) (setq dup T)))
                  dup)
           "DUPLICATE")
          (T (setq rec (list u (asec:2d ip)) mine (cons rec mine))
             (if hid (setq top (cons rec top)) (setq cur (cons rec cur)))
             "CROSSES")))
      (if (or (/= reason "NO-CROSS") (= (asec:get 0 ed) "LINE"))
        (asec:dbg (strcat "\n    Segment " (itoa (cadddr sg)) "  " (asec:pt-str (car sg)) " -> " (asec:pt-str (cadr sg))
                          "  Bulge " (rtos (caddr sg) 2 4)
                          (if ip (strcat "  Crossing " (asec:pt-str (asec:2d ip))) "")
                          (if u (strcat "  U " (rtos u 2 1)) "")
                          "  Accepted: " (if (= reason "CROSSES") "Yes" "No") "  Reason: " reason)))))
  (setq cur (asec:sort cur '(lambda (p q) (< (car p) (car q))))
        top (asec:sort top '(lambda (p q) (< (car p) (car q)))))
  (asec:dbg (strcat "\nCURRENT S-SLAB STATIONS: " (asec:ustr cur) "\nTOP S-SLAB STATIONS: " (asec:ustr top)))
  (list cur top curved)
)

(defun asec:dxf-10s (ed / out) (foreach g ed (if (= (car g) 10) (setq out (cons g out)))) out)

(defun asec:ustr (edges / s)
  (setq s "(")
  (foreach e edges (setq s (strcat s " " (rtos (if (listp e) (car e) e) 2 1))))
  (strcat s " )")
)

(defun asec:ivstr (ivs / s)
  (setq s "(")
  (foreach iv ivs (setq s (strcat s " (" (rtos (car iv) 2 1) " " (rtos (cadr iv) 2 1) ")")))
  (strcat s " )")
)

;;; explicit edges -> (intervals note). base = automatic/manual extent (u0 u1) or nil.
;;;   no edges            -> base unchanged
;;;   even count          -> pairs 1-2, 3-4 ... (gaps between pairs are voids)
;;;   one edge outside base -> base extended to it (e.g. balcony edge beyond the wall)
;;;   anything else       -> base unchanged + note (reported, never guessed)
(defun asec:slab-intervals (base edges / us out)
  (setq us (mapcar 'car edges))
  ;; -> (intervals note rule)
  (cond
    ((null us) (list (if base (list base)) nil "NONE"))
    ((= (rem (length us) 2) 0)
     (while us (setq out (cons (list (car us) (cadr us)) out) us (cddr us)))
     (list (reverse out) nil "EVEN-PAIRS"))
    ((and (= (length us) 1) base (< (car us) (- (car base) asec:tol)))
     (list (list (list (car us) (cadr base))) nil "SINGLE-OUTSIDE"))
    ((and (= (length us) 1) base (> (car us) (+ (cadr base) asec:tol)))
     (list (list (list (car base) (car us))) nil "SINGLE-OUTSIDE"))
    ((and (= (length us) 1) base)
     (list (list base) "single slab edge inside the automatic extent (side unknown)" "SINGLE-INSIDE"))
    ((= (length us) 1)
     (list nil "single slab edge with no automatic extent to extend" "ODD-UNRESOLVED"))
    (T (list (if base (list base))
             (strcat (itoa (length us)) " slab edges (odd) cannot define slab intervals") "ODD-UNRESOLVED")))
)

(defun asec:debug-slab-extent (name base cur inh ivs)
  (princ (strcat "\n--- SLAB EXTENT DEBUG ---  " name
                 "\nAutomatic slab interval BEFORE S-SLAB: " (if base (asec:ivstr (list base)) "none")
                 "\nCurrent S-SLAB edges: " (asec:ustr cur)
                 "\nInherited top edges from floor below: " (asec:ustr inh)
                 "\nEdges selected: " (asec:ustr (if cur cur inh))
                 "\nRule: " (caddr ivs)
                 "\nFinal slab intervals: " (asec:ivstr (car ivs))
                 (if (cadr ivs) (strcat "\nUnresolved reason: " (cadr ivs)) "")))
)

(defun asec:debug-slab-draw (name ivs elev slabT es)
  (princ (strcat "\n--- SLAB DRAW DEBUG ---  " name "  Intervals: " (asec:ivstr ivs)
                 "\nSlab bottom Z: " (rtos (- elev slabT) 2 1) "  top Z: " (rtos elev 2 1)))
  (foreach iv ivs
    (princ (strcat "\n  U1 " (rtos (car iv) 2 1) "  U2 " (rtos (cadr iv) 2 1)
                   "  Outline created: " (if (car es) "Yes" "No")
                   "  Hatch attempted: "
                   (if (member (strcase (cond ((asec:gfx "RCC_HATCH_PATTERN")) (""))) '("" "NONE")) "No" "Yes")))
    (setq es (cdr es)))
)

(defun aseca:discover-extras (i se dir ctx / lines xs pr lw prj plw un sl h old)
  (setq h (asec:railing-height)
        lines (mapcar 'aseca:line (aseca:ss-list "LINE" *aseca-railing-layers*))
        xs (aseca:cut-crossings se dir lines nil)
        pr (aseca:pair-cut-walls xs)
        lw (mapcar '(lambda (w) (asec:put 'top (+ (asec:get 'elev ctx) h) (asec:put 'bottom (asec:get 'elev ctx) w)))
                   (car pr))
        un (mapcar '(lambda (u) (asec:put 'reason (strcat "low wall: " (asec:get 'reason u)) u)) (cadr pr))
        old *asec-closing-layers* *asec-closing-layers* *aseca-railing-layers*
        prj (aseca:discover-projected-walls lines (apply 'append (mapcar 'cadddr xs)) nil ctx)
        *asec-closing-layers* old
        plw (mapcar '(lambda (w) (asec:put 'top (+ (asec:get 'bottom w) h) w)) (car prj))
        un (append un (mapcar '(lambda (u) (asec:put 'reason (strcat "low wall: " (asec:get 'reason u)) u)) (cadr prj)))
        sl (aseca:slab-edges i se dir))
  (foreach e (list (cons "current" (car sl)) (cons "top" (cadr sl)))
    (if (cadr (asec:slab-intervals (asec:wall-bounds (asec:get 'walls ctx)) (cdr e)))
      (setq un (cons (aseca:unres (strcat (car e) " slab: "
                                          (cadr (asec:slab-intervals (asec:wall-bounds (asec:get 'walls ctx)) (cdr e))))
                                  (cadr (car (cdr e))))
                     un))))
  (foreach pt (caddr sl)
    (setq un (cons (aseca:unres "S-SLAB curved polyline segment crosses the section (not supported)" pt) un)))
  (list (cons 'lwalls lw) (cons 'plwalls plw) (cons 'scur (car sl)) (cons 'stop (cadr sl)) (cons 'un un)
        (cons 'pslabs (aseca:projected-slab-edges i ctx)))
)

;;; PROJECTED SLAB EDGES: every straight S-SLAB segment (LINE / LWPOLYLINE) inside this view's
;;; boundary, clipped to it, with its own U1/V1-U2/V2 (depth varies along U). Segments running
;;; straight into the view (|dU| < 1) have no width in the section and are skipped; bulged
;;; segments are skipped (reported in debug). Linetype keeps the current/top semantics.
(defun aseca:projected-slab-edges (i ctx / se dir vd vb ed lt c out ncurve)
  (setq se (asec:get 'se ctx) dir (asec:get 'dir ctx) vd (asec:get 'vd ctx) vb (asec:get 'view ctx) ncurve 0)
  (foreach e (append (aseca:ss-list "LINE" *aseca-slab-layers*) (aseca:ss-list "LWPOLYLINE" *aseca-slab-layers*))
    (setq ed (entget e) lt (asec:effective-linetype ed))
    (foreach sg (asec:entity-segments e)
      (cond
        ((<= (distance (car sg) (cadr sg)) asec:tol))
        ((> (abs (caddr sg)) 1e-6) (setq ncurve (1+ ncurve)))
        ((and (setq c (asec:clip-segment-to-view-boundary
                        (asec:section-coordinate (car sg) (car se) dir vd)
                        (asec:section-coordinate (cadr sg) (car se) dir vd) vb))
              (>= (abs (- (car (cadr c)) (car (car c)))) 1.0))
         (setq out (cons (list (cons 'u1 (car (car c))) (cons 'v1 (cadr (car c)))
                               (cons 'u2 (car (cadr c))) (cons 'v2 (cadr (cadr c)))
                               (cons 'umin (min (car (car c)) (car (cadr c))))
                               (cons 'umax (max (car (car c)) (car (cadr c))))
                               (cons 'top (wcmatch lt "HIDDEN*")) (cons 'lt lt)
                               (cons 'handle (asec:get 5 ed)))
                         out))))))
  (asec:dbg (strcat "\nPROJECTED SLAB EDGES  " (asec:floor-name i) ": " (itoa (length out)) " segment(s)"
                    (if (> ncurve 0) (strcat ", " (itoa ncurve) " curved segment(s) skipped") "")))
  (reverse out)
)

;;; new element data from discovery d into floor record rec (projected low walls clipped for ASA)
(defun aseca:add-extras (rec d ctx / vb plw)
  (setq vb (asec:get 'view ctx) plw (asec:get 'plwalls d))
  (if (asec:get 'clip vb) (setq plw (mapcar '(lambda (w) (asec:clip-projected-wall w vb)) plw)))
  (asec:put 'lwalls (asec:get 'lwalls d)
    (asec:put 'plwalls plw (asec:put 'scur (asec:get 'scur d) (asec:put 'stop (asec:get 'stop d)
                                                                         (asec:put 'pslabs (asec:get 'pslabs d) rec)))))
)

(defun aseca:print-extras (d)
  (princ (strcat "\nLOW WALLS  cut " (itoa (length (asec:get 'lwalls d)))
                 "  projected " (itoa (length (asec:get 'plwalls d)))
                 "\nSLAB EDGES  current " (itoa (length (asec:get 'scur d)))
                 "  top " (itoa (length (asec:get 'stop d)))
                 "\nPROJECTED SLAB EDGES  " (itoa (length (asec:get 'pslabs d)))))
)

(defun aseca:label-extras (d / k se)
  (setq se (asec:get 'se d) k 0)
  (foreach w (asec:get 'lwalls d)
    (setq k (1+ k)) (asec:label (aseca:mid (asec:get 'p1 w) (asec:get 'p2 w)) (strcat "L" (itoa k)) se))
  (setq k 0)
  (foreach w (asec:get 'plwalls d)
    (setq k (1+ k)) (asec:label (aseca:mid (asec:get 'cstart w) (asec:get 'cend w)) (strcat "PL" (itoa k)) se))
  (setq k 0)
  (foreach e (asec:get 'scur d) (setq k (1+ k)) (asec:label (cadr e) (strcat "SC" (itoa k)) se))
  (setq k 0)
  (foreach e (asec:get 'stop d) (setq k (1+ k)) (asec:label (cadr e) (strcat "ST" (itoa k)) se))
)

;;; floor record without prompts: slab from SLAB_EXTENT (NONE -> none, else cut-wall bounds)
(defun aseca:auto-record (i d m vd vb cfg / ctx)
  (setq ctx (asec:make-floor-ctx i (asec:get 'se d) m vd vb cfg (asec:get 'walls d)))
  (aseca:add-extras
    (asec:make-floor-record i (asec:get 'ref d) (asec:get 'se d) ctx (asec:get 'pws d)
                            (asec:get 'cd d) (asec:get 'cw d) (asec:get 'pd d) (asec:get 'pw d)
                            (if (/= (asec:setting "SLAB_EXTENT") "NONE") (asec:wall-bounds (asec:get 'walls d))))
    d ctx)
)

;;; interactive review of one floor of one view; d = discovered data (nil = discover now)
(defun aseca:analyse-floor (i m vd vb gfref cfg d / ref se dir walls ctx cd cw pws pd pw un r res kind n)
  (princ (strcat "\nAUTO SECTION - analysing " (asec:floor-name i) "..."))
  (if (null d) (setq d (aseca:discover-floor i m vd vb gfref cfg nil)))
  (setq ref (asec:get 'ref d) se (asec:get 'se d) dir (caddr m)
        walls (asec:get 'walls d) cd (asec:get 'cd d) cw (asec:get 'cw d)
        pws (asec:get 'pws d) pd (asec:get 'pd d) pw (asec:get 'pw d) un (asec:get 'un d))
  (while (not res)
    (setq ctx (asec:make-floor-ctx i se m vd vb cfg walls))
    (aseca:label-all walls cd cw pws pd pw un ctx)
    (aseca:label-extras d)
    (aseca:print-summary i walls cd cw pws pd pw un)
    (aseca:print-extras d)
    (initget "Generate Edit Guided Cancel")
    (setq r (getkword (strcat "\n[Generate/Edit/Guided/Cancel] <" (if un "Edit" "Generate") ">: ")))
    (if (null r) (setq r (if un "Edit" "Generate")))
    (cond
      ((= r "Cancel") (exit))
      ((= r "Guided")
       (asec:clear-labels)
       (setq res (asec:add-floor i m vd vb gfref cfg ref)))
      ((= r "Generate")
       (asec:clear-labels)
       (setq res (aseca:add-extras
                   (asec:make-floor-record i ref se ctx pws cd cw pd pw (asec:get-floor-slab i se m walls))
                   d ctx)))
      (T
       (initget "Remove Add Ignore Back")
       (setq r (getkword "\nEdit [Remove/Add/Ignore/Back] <Back>: "))
       (cond
         ((= r "Ignore") (setq un nil) (princ "\nUnresolved items ignored."))
         ((member r '("Remove" "Add"))
          (initget "CutWall CutDoor CutWindow ProjectedWall ProjectedDoor ProjectedWindow")
          (setq kind (getkword (strcat "\n" r " [CutWall/CutDoor/CutWindow/ProjectedWall/ProjectedDoor/ProjectedWindow]: ")))
          (if (= r "Remove")
            (progn
              (setq n (getint "\nNumber to remove: "))
              (cond
                ((= kind "CutWall")
                 (if (aseca:valid-n n walls)
                   (setq walls (aseca:drop-nth walls n) cd (aseca:rehost cd 'host n T) cw (aseca:rehost cw 'host n T))))
                ((= kind "CutDoor") (if (aseca:valid-n n cd) (setq cd (aseca:drop-nth cd n))))
                ((= kind "CutWindow") (if (aseca:valid-n n cw) (setq cw (aseca:drop-nth cw n))))
                ((= kind "ProjectedWall")
                 (if (aseca:valid-n n pws)
                   (setq pws (aseca:drop-nth pws n) pd (aseca:rehost pd 'phost n nil) pw (aseca:rehost pw 'phost n nil))))
                ((= kind "ProjectedDoor") (if (aseca:valid-n n pd) (setq pd (aseca:drop-nth pd n))))
                ((= kind "ProjectedWindow") (if (aseca:valid-n n pw) (setq pw (aseca:drop-nth pw n))))))
            ;; Add = the manual collectors, appended to the automatic records
            (progn
              (asec:clear-labels)
              (cond
                ((= kind "CutWall") (setq walls (append walls (asec:collect-walls se dir))))
                ((= kind "CutDoor")
                 (setq cd (append cd (asec:collect-openings "DOOR" "CUT" (cons (cons 'prior (append cd cw)) ctx)))))
                ((= kind "CutWindow")
                 (setq cw (append cw (asec:collect-openings "WINDOW" "CUT" (cons (cons 'prior (append cd cw)) ctx)))))
                ((= kind "ProjectedWall") (setq pws (append pws (asec:collect-projected-walls ctx))))
                ((= kind "ProjectedDoor")
                 (setq pd (append pd (asec:collect-openings "DOOR" "PROJECTED" (cons (cons 'pwalls pws) ctx)))))
                ((= kind "ProjectedWindow")
                 (setq pw (append pw (asec:collect-openings "WINDOW" "PROJECTED" (cons (cons 'pwalls pws) ctx)))))))))))))
  res
)

;;; ============================================================
;;; AS > SECTION: DRAWN SECTION CUT + PERMANENT MARKER
;;; P1/P2 = analytical cut (same master record as a picked LINE) and the two marker centres.
;;; Marker on SECTION_LINE_LAYER: per end a CIRCLE, the section ID TEXT (MC, colour 2), and a
;;; SOLID-filled arrow = triangle (C+V*s, C+T*s, C-T*s) minus the circle disc, plus solid
;;; end lines (SOLID_LENGTH along the cut from the inner triangle corner) and a dashed middle
;;; LINE (colour 8). ID family X when |dx| >= |dy| else Y; next = highest existing suffix + 1.
;;; ============================================================
(defun asec:pick-section-points (/ p q)
  (setq p (asec:getpt "\nSpecify first section point: "))
  (while
    (progn
      (setq q (getpoint (trans (list (car p) (cadr p) 0.0) 0 1) "\nSpecify second section point: "))
      (if (null q) (exit))
      (setq q (asec:2d (trans q 1 0)))
      (if (< (distance p q) asec:tol) (progn (princ "\nSection has zero length.") T))))
  (asec:make-master p q)
)

(defun asec:digits-p (s / ok i)
  (setq ok (> (strlen s) 0) i 1)
  (while (and ok (<= i (strlen s)))
    (if (not (wcmatch (substr s i 1) "#")) (setq ok nil))
    (setq i (1+ i)))
  ok
)

;;; legacy loose-entity marker (pre-block builds): TEXT centred on a CIRCLE on the marker layer
(defun asec:marker-text-p (ed lay / p ss i hit)
  (setq p (asec:2d (if (or (/= 0 (cond ((asec:get 72 ed)) (0))) (/= 0 (cond ((asec:get 73 ed)) (0))))
                     (asec:get 11 ed) (asec:get 10 ed)))
        ss (ssget "_X" (list '(0 . "CIRCLE") (cons 8 (aseca:wc-escape lay))))
        i 0)
  (while (and ss (not hit) (< i (sslength ss)))
    (if (< (distance p (asec:2d (asec:get 10 (entget (ssname ss i))))) 1.0) (setq hit T))
    (setq i (1+ i)))
  hit
)

;;; TEXT entities inside a block definition
(defun asec:block-texts (name / e ed out)
  (setq e (tblobjname "BLOCK" name))
  (while (and e (setq e (entnext e)) (/= (asec:get 0 (setq ed (entget e))) "ENDBLK"))
    (if (= (asec:get 0 ed) "TEXT") (setq out (cons e out))))
  out
)

(defun asec:fam-suffix (s fam)
  (if (and (> (strlen s) 1) (= (substr s 1 1) fam) (asec:digits-p (substr s 2))) (atoi (substr s 2)))
)

;;; -> (new-id texts-to-rename inserts-to-refresh). Existing markers of family fam:
;;;   AKD-SECTION-* INSERTs (ID = text in the block definition) and legacy loose marker TEXT
;;;   sitting on a marker circle. Stray notes on the layer are ignored.
;;;   none                                        -> fam
;;;   one plain block marker, nothing numbered    -> its texts -> fam1, new fam2
;;;   one legacy plain pair, nothing else         -> those texts -> fam1, new fam2
;;;   otherwise                                   -> fam(highest suffix + 1), nothing renamed
(defun asec:section-id (fam lay / ss i e tx s n best pb pl)
  (setq ss (ssget "_X" '((0 . "INSERT") (2 . "AKD-SECTION-*"))) i 0)
  (while (and ss (< i (sslength ss)))
    (setq e (ssname ss i) i (1+ i)
          tx (asec:block-texts (asec:get 2 (entget e)))
          s (if tx (strcase (asec:get 1 (entget (car tx)))) ""))
    (cond ((= s fam) (setq pb (cons (list e tx) pb)))
          ((setq n (asec:fam-suffix s fam)) (setq best (max (cond (best) (0)) n)))))
  (setq ss (ssget "_X" (list '(0 . "TEXT") (cons 8 (aseca:wc-escape lay)))) i 0)
  (while (and ss (< i (sslength ss)))
    (setq e (ssname ss i) i (1+ i) s (strcase (asec:get 1 (entget e))))
    (if (asec:marker-text-p (entget e) lay)
      (cond ((= s fam) (setq pl (cons e pl)))
            ((setq n (asec:fam-suffix s fam)) (setq best (max (cond (best) (0)) n))))))
  (cond
    ((and (null best) (null pb) (null pl)) (list fam nil nil))
    ((and (null best) (= (length pb) 1) (null pl))
     (list (strcat fam "2") (cadr (car pb)) (list (car (car pb)))))
    ((and (null best) (null pb) (= (length pl) 2)) (list (strcat fam "2") pl nil))
    (T
     (if (or pb pl)
       (asec:dbg (strcat "\nSECTION ID: unnumbered " fam " markers not renamed (not a single marker)")))
     (list (strcat fam (itoa (1+ (cond (best) (0))))) nil nil)))
)

;;; linetype name if available (loaded from acad.lin / acadiso.lin when missing), else nil
(defun asec:ensure-linetype (name / f fd)
  (if (and name (/= name "") (not (tblsearch "LTYPE" name))
           (setq f (cond ((findfile "acad.lin")) ((findfile "acadiso.lin")))))
    (progn
      (setq fd (getvar "FILEDIA"))
      (setvar "FILEDIA" 0)
      (command-s "_.-LINETYPE" "_L" name f "")
      (setvar "FILEDIA" fd)))
  (if (and name (tblsearch "LTYPE" name)) name)
)

;;; ---------- marker entity data (DXF lists, placed into the marker block) ----------
(defun asec:dxf-line (a b lay col lt)
  (append (list '(0 . "LINE") (cons 8 lay))
          (if col (list (cons 62 col)))
          (if lt (list (cons 6 lt)))
          (list (list 10 (car a) (cadr a) 0.0) (list 11 (car b) (cadr b) 0.0)))
)

(defun asec:dxf-lwpoly (pts closed lay)
  (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay) '(100 . "AcDbPolyline")
                (cons 90 (length pts)) (cons 70 closed))
          (mapcar '(lambda (p) (cons 10 p)) pts))
)

(defun asec:dxf-solid (a b c lay)
  (list '(0 . "SOLID") (cons 8 lay)
        (list 10 (car a) (cadr a) 0.0) (list 11 (car b) (cadr b) 0.0)
        (list 12 (car c) (cadr c) 0.0) (list 13 (car c) (cadr c) 0.0))
)

(defun asec:drop-index (lst n / i out)
  (setq i 0)
  (foreach x lst (if (/= i n) (setq out (cons x out))) (setq i (1+ i)))
  (reverse out)
)

(defun asec:pt-in-tri (p a b c)
  (and (>= (asec:cross (asec:v- b a) (asec:v- p a)) -1e-9)
       (>= (asec:cross (asec:v- c b) (asec:v- p b)) -1e-9)
       (>= (asec:cross (asec:v- a c) (asec:v- p c)) -1e-9))
)

;;; simple polygon -> triangles (ear clipping); polygon made CCW first
(defun asec:ear-clip (pts / ar n i a b c ok j out found guard)
  (setq ar 0.0 i 0 n (length pts))
  (repeat n
    (setq ar (+ ar (asec:cross (nth i pts) (nth (rem (1+ i) n) pts))) i (1+ i)))
  (if (< ar 0.0) (setq pts (reverse pts)))
  (setq guard 0)
  (while (and (> (length pts) 3) (< guard 1000))
    (setq n (length pts) i 0 found nil guard (1+ guard))
    (while (and (not found) (< i n))
      (setq a (nth (rem (+ i n -1) n) pts) b (nth i pts) c (nth (rem (1+ i) n) pts))
      (if (> (asec:cross (asec:v- b a) (asec:v- c b)) 1e-9)
        (progn
          (setq ok T j 0)
          (foreach p pts
            (if (and ok (/= j i) (/= j (rem (+ i n -1) n)) (/= j (rem (1+ i) n)) (asec:pt-in-tri p a b c))
              (setq ok nil))
            (setq j (1+ j)))
          (if ok (setq found T out (cons (list a b c) out) pts (asec:drop-index pts i)))))
      (setq i (1+ i)))
    (if (not found) (setq guard 1000)))
  (if (= (length pts) 3) (setq out (cons pts out)))
  out
)

;;; One end arrow as DXF data: triangle (C+V*s, C+T*s, C-T*s) minus the disc (C, r).
;;; The triangle boundary is walked CCW and cut where edges cross the circle; each outside
;;; chain (entry ... exit) is an outline LWPOLYLINE, and chain + circle arc exit->entry
;;; (clockwise, sampled every 5 deg) is one fill region, ear-clipped into SOLID triangles.
;;; Fills come first so outlines draw over them.
(defun asec:marker-arrow-dxf (c tv vd r s lay / a b d poly lst k p q dd aa bb cc disc t1 t2 i chains cur
                                             th n0 arc fills outs)
  (setq a (asec:v+ c (asec:v* vd s)) b (asec:v+ c (asec:v* tv s)) d (asec:v- c (asec:v* tv s))
        poly (if (> (asec:cross (asec:v- b d) (asec:v- a d)) 0) (list d b a) (list b d a))
        k 0)
  (repeat 3
    (setq p (nth k poly) q (nth (rem (1+ k) 3) poly) k (1+ k)
          lst (cons (list p 'v) lst)
          dd (asec:v- q p) aa (asec:dot dd dd)
          bb (* 2.0 (asec:dot dd (asec:v- p c)))
          cc (- (asec:dot (asec:v- p c) (asec:v- p c)) (* r r))
          disc (- (* bb bb) (* 4.0 aa cc)))
    (if (> disc 1e-9)
      (progn
        (setq t1 (/ (- (- bb) (sqrt disc)) (* 2.0 aa)) t2 (/ (+ (- bb) (sqrt disc)) (* 2.0 aa)))
        (if (and (< 0.0 t1 1.0) (< 0.0 t2 1.0))
          (setq lst (cons (list (asec:v+ p (asec:v* dd t2)) 'entry)
                          (cons (list (asec:v+ p (asec:v* dd t1)) 'exit) lst)))))))
  (setq lst (reverse lst) i 0 k nil)
  (foreach x lst (if (and (null k) (eq (cadr x) 'exit)) (setq k i)) (setq i (1+ i)))
  (if (null k)
    ;; ponytail: circle wholly inside the triangle (non-default sizes) -> outline only, no fill
    (progn
      (asec:dbg "\nSECTION MARKER: triangle contains the circle; arrow drawn unfilled")
      (list (asec:dxf-lwpoly poly 1 lay)))
    (progn
      (setq lst (append (asec:nthcdr* (1+ k) lst) (asec:take (1+ k) lst)))
      (foreach x lst
        (setq cur (cons (car x) cur))
        (if (eq (cadr x) 'exit)
          (setq chains (cons (reverse cur) chains) cur nil)))
      (foreach ch chains
        (setq th (- (angle c (last ch)) (angle c (car ch))))
        (if (< th 0.0) (setq th (+ th (* 2.0 pi))))
        (setq n0 (max 2 (fix (+ 1 (/ th (/ pi 36.0))))) i 1 arc nil)
        (while (< i n0)
          (setq arc (cons (polar c (- (angle c (last ch)) (* th (/ (float i) n0))) r) arc) i (1+ i)))
        (foreach tri (asec:ear-clip (append ch (reverse arc)))
          (setq fills (cons (asec:dxf-solid (car tri) (cadr tri) (caddr tri) lay) fills)))
        (setq outs (cons (asec:dxf-lwpoly ch 0 lay) outs)))
      (append fills outs)))
)

(defun asec:take (n lst / out)
  (repeat n (setq out (cons (car lst) out) lst (cdr lst)))
  (reverse out)
)

(defun asec:nthcdr* (n lst) (repeat n (setq lst (cdr lst))) lst)

;;; permanent marker for cut p1 -> p2 viewed toward vd, as ONE block INSERT at the Ground Floor
;;; reference point R. The definition's base point is R and its entities keep their world
;;; coordinates, so inserting at R (scale 1, rotation 0) shows them exactly where they were
;;; computed. Returns the section ID.
(defun asec:draw-section-marker (p1 p2 vd rp / lay r s sl th tc dc tv L id idr e0 a b lt ents base nm k)
  (setq lay (asec:gfx "SECTION_LINE_LAYER")
        r   (asec:gfx-num "SECTION_MARKER_RADIUS" 1.0)
        s   (asec:gfx-num "SECTION_MARKER_TRIANGLE_SIZE" 1.0)
        sl  (asec:gfx-num "SECTION_MARKER_SOLID_LENGTH" 0.0)
        th  (asec:gfx-num "SECTION_MARKER_TEXT_HEIGHT" 1.0)
        tc  (fix (asec:gfx-num "SECTION_MARKER_TEXT_COLOR" 0.0))
        dc  (fix (asec:gfx-num "SECTION_LINE_DASHED_COLOR" 0.0))
        tv  (asec:unit (asec:v- p2 p1))
        L   (distance p1 p2)
        idr (asec:section-id (if (>= (abs (car tv)) (abs (cadr tv))) "X" "Y") lay)
        id  (car idr)
        e0  (max r s)
        a   (asec:v+ p1 (asec:v* tv e0))
        b   (asec:v- p2 (asec:v* tv e0)))
  (asec:ensure-layer lay 7)
  ;; commands and table changes must happen before the block definition is opened
  (setq lt (asec:ensure-linetype (asec:gfx "SECTION_LINE_DASHED_LINETYPE")))
  ;; the existing single marker X/Y becomes X1/Y1 (inside the same UNDO group)
  (foreach e (cadr idr)
    (entmod (subst (cons 1 (strcat (substr id 1 1) "1")) (assoc 1 (entget e)) (entget e))))
  (foreach e (caddr idr) (entupd e))
  ;; entity order = draw order inside the block: fills, outlines, cut line, circles, text
  (if (> s (+ r asec:tol))
    (foreach c (list p1 p2) (setq ents (append ents (asec:marker-arrow-dxf c tv vd r s lay)))))
  (cond
    ((> L (+ (* 2.0 (+ e0 sl)) asec:tol))
     (setq ents (append ents
                  (list (asec:dxf-line a (asec:v+ a (asec:v* tv sl)) lay nil nil)
                        (asec:dxf-line (asec:v+ a (asec:v* tv sl)) (asec:v- b (asec:v* tv sl)) lay dc lt)
                        (asec:dxf-line (asec:v- b (asec:v* tv sl)) b lay nil nil)))))
    ;; too short for a dashed middle: one solid line between the markers, or none
    ((> L (+ (* 2.0 e0) asec:tol)) (setq ents (append ents (list (asec:dxf-line a b lay nil nil))))))
  (foreach c (list p1 p2)
    (setq ents (append ents (list (list '(0 . "CIRCLE") (cons 8 lay) (list 10 (car c) (cadr c) 0.0) (cons 40 r))))))
  (foreach c (list p1 p2)
    (setq ents (append ents (list (list '(0 . "TEXT") (cons 8 lay) (cons 62 tc) (list 10 (car c) (cadr c) 0.0)
                                        (list 11 (car c) (cadr c) 0.0) (cons 40 th) (cons 1 id)
                                        '(72 . 1) '(73 . 2))))))
  ;; unique definition name; the visible ID stays authoritative if names and IDs later differ
  (setq base (strcat "AKD-SECTION-" id) nm base k 1)
  (while (tblsearch "BLOCK" nm) (setq nm (strcat base "-" (itoa k)) k (1+ k)))
  (entmake (list '(0 . "BLOCK") '(8 . "0") (cons 2 nm) '(70 . 0) (list 10 (car rp) (cadr rp) 0.0)))
  (foreach ed ents (entmake ed))
  (entmake '((0 . "ENDBLK")))
  (entmakex (list '(0 . "INSERT") (cons 8 lay) (cons 2 nm) (list 10 (car rp) (cadr rp) 0.0)
                  '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) '(50 . 0.0)))
  (princ (strcat "\nSection marker " id " created (block " nm ")."))
  id
)

;;; screen-only preview while picking the view side: circles, arrow triangles, cut line
(defun asec:ghost-section-marker (m n / r s tv gd k)
  (defun gd (p q col hl) (grdraw (trans p 0 1) (trans q 0 1) col hl))
  (setq r (asec:gfx-num "SECTION_MARKER_RADIUS" 1.0)
        s (asec:gfx-num "SECTION_MARKER_TRIANGLE_SIZE" 1.0)
        tv (caddr m))
  (gd (car m) (cadr m) 8 1)
  (foreach c (list (car m) (cadr m))
    (setq k 0)
    (repeat 24
      (gd (polar c (* k (/ pi 12.0)) r) (polar c (* (1+ k) (/ pi 12.0)) r) 2 0)
      (setq k (1+ k)))
    (gd (asec:v+ c (asec:v* n s)) (asec:v+ c (asec:v* tv s)) 2 0)
    (gd (asec:v+ c (asec:v* n s)) (asec:v- c (asec:v* tv s)) 2 0)
    (gd (asec:v+ c (asec:v* tv s)) (asec:v- c (asec:v* tv s)) 2 0))
)

;;; ============================================================
;;; COMMAND
;;; ============================================================
(defun c:AS (/ m vd vb gfref cfg floors i)
  (princ "\nAUTO SECTION")
  (asec:load-graphics)   ; before discovery: may set the source layer lists
  (setq asec:old-error *error* *error* asec:error
        asec:undo-open nil asec:cmdecho nil asec:temp nil
        asec:temp-created 0 asec:temp-cleaned 0
        aseca:old-closing-layers *asec-closing-layers*
        *asec-closing-layers* *aseca-wall-layers*   ; wall ends only from wall-layer LINEs
        asec:pending-marker nil asec:marker-ghost nil)
  (setq m (asec:get-master-section))
  (if (= m "Section")
    ;; Draw Section only replaces LINE selection: picked cut + view side with marker preview.
    ;; The permanent marker is created by asec:generate inside the UNDO group.
    (setq m     (asec:pick-section-points)
          asec:marker-ghost T
          vd    (asec:get-view-direction m)
          asec:marker-ghost nil
          asec:pending-marker (list (car m) (cadr m) vd))
    (setq vd    (asec:get-view-direction m)))
  ;; shared pipeline from here on (both entry paths)
  (setq m     (asec:normalize-section-orientation m vd)
        vb    (asec:get-projection-boundary m vd)
        gfref (asec:getpt "\nPick Ground Floor reference point: ")
        cfg   (asec:get-config))
  (aseca:dbg (strcat "\nVIEW BOUNDARY  U 0 -> " (rtos (asec:get 'umax vb) 2 2)
                     "  V 0 -> " (rtos (asec:get 'vfar vb) 2 2)))
  ;; the Ground Floor reference point is the marker block's base/insertion point
  (if asec:pending-marker (setq asec:pending-marker (append asec:pending-marker (list gfref))))
  (setq floors (list (aseca:analyse-floor 0 m vd vb gfref cfg nil)) i 0)
  (while (progn (initget "Yes No")
                (= (getkword "\nAdd upper floor? [Yes/No] <No>: ") "Yes"))
    (setq i (1+ i)
          floors (cons (aseca:analyse-floor i m vd vb gfref cfg nil) floors)))
  (asec:generate m vd vb cfg floors)
  (asec:cleanup)
  (asec:construction-report)
  (setq *error* asec:old-error)
  (princ)
)

;;; ============================================================
;;; ASA - AUTO SECTION ALL DIRECTIONS
;;; Rectangular field (world axes) + compass point inside it -> four ordinary AS views:
;;;   NORTH +Y / SOUTH -Y  cut Y = Cy, U across the field width
;;;   EAST  +X / WEST  -X  cut X = Cx, U across the field height
;;; Each view = master LINE across the field through C, normalised by the shared
;;; asec:normalize-section-orientation, and a view boundary covering the field half on its
;;; side (depth = field edge - compass) with (clip . T): the field is a hard source limit.
;;; Discovery, review, occlusion and drawing are the AS functions. Upper floors: one
;;; matching compass point per floor, shared by all four views.
;;; ============================================================
(setq asa:names '("NORTH" "SOUTH" "EAST" "WEST"))

(defun asa:field-pts (f)
  (list (list (car f) (cadr f)) (list (caddr f) (cadr f))
        (list (caddr f) (cadddr f)) (list (car f) (cadddr f)))
)

(defun asa:grpoly (pts col / u)
  (setq u (mapcar '(lambda (p) (trans p 0 1)) pts))
  (mapcar '(lambda (a b) (grdraw a b col 1)) u (append (cdr u) (list (car u))))
)

;;; screen-only field rectangle + compass crosshair
(defun asa:ghost (f c / s)
  (redraw)
  (asa:grpoly (asa:field-pts f) 3)
  (if c
    (progn
      (setq s (* 0.5 (asec:gfx-num "SECTION_MARK_SIZE" 1.0)))
      (grdraw (trans (list (- (car c) s) (cadr c)) 0 1) (trans (list (+ (car c) s) (cadr c)) 0 1) 4)
      (grdraw (trans (list (car c) (- (cadr c) s)) 0 1) (trans (list (car c) (+ (cadr c) s)) 0 1) 4)
      (grdraw (trans (list (car c) (+ (cadr c) s)) 0 1)
              (trans (list (- (car c) (* 0.2 s)) (+ (cadr c) (* 0.7 s))) 0 1) 4)
      (grdraw (trans (list (car c) (+ (cadr c) s)) 0 1)
              (trans (list (+ (car c) (* 0.2 s)) (+ (cadr c) (* 0.7 s))) 0 1) 4)))
)

;;; (xmin ymin xmax ymax) in WCS
(defun asa:get-field (/ p q)
  (setq p (getpoint "\nSpecify first corner of section field: "))
  (if (null p) (exit))
  (setq q (getcorner p "\nSpecify opposite corner: "))
  (if (null q) (exit))
  (setq p (trans p 1 0) q (trans q 1 0))
  (if (or (< (abs (- (car q) (car p))) asec:tol) (< (abs (- (cadr q) (cadr p))) asec:tol))
    (progn (princ "\nSection field has no area.") (exit)))
  (list (min (car p) (car q)) (min (cadr p) (cadr q)) (max (car p) (car q)) (max (cadr p) (cadr q)))
)

;;; depths ((name . depth) ...) for compass c in field f
(defun asa:depths (f c)
  (list (cons "NORTH" (- (cadddr f) (cadr c))) (cons "SOUTH" (- (cadr c) (cadr f)))
        (cons "EAST" (- (caddr f) (car c))) (cons "WEST" (- (car c) (car f))))
)

(defun asa:get-compass (f / g p c bad)
  (princ "\nPlace section compass inside field: ")
  (while (not c)
    (setq g (grread T 15 0))
    (cond
      ((= (car g) 5) (asa:ghost f (asec:2d (trans (cadr g) 1 0))))
      ((= (car g) 3)
       (setq p (asec:2d (trans (cadr g) 1 0)) bad nil)
       (foreach d (asa:depths f p)
         (if (and (not bad) (<= (cdr d) 1.0))
           (setq bad (if (< (cdr d) (- asec:tol))
                       "\nCompass point must be inside the ASA field."
                       (strcat "\n" (asec:cap-word (car d)) " projection depth is too small.")))))
       (if bad (princ (strcat bad "\nPick compass point: ")) (setq c p)))
      ((member (car g) '(11 25)) (exit))))
  (asa:ghost f c)
  c
)

;;; view: (name m vd vb). Master LINE spans the field through c; orientation is normalised.
(defun asa:make-view (name f c / vd s e m d)
  (cond
    ((= name "NORTH") (setq vd '(0.0 1.0)))
    ((= name "SOUTH") (setq vd '(0.0 -1.0)))
    ((= name "EAST") (setq vd '(1.0 0.0)))
    (T (setq vd '(-1.0 0.0))))
  (if (member name '("NORTH" "SOUTH"))
    (setq s (list (car f) (cadr c)) e (list (caddr f) (cadr c)))
    (setq s (list (car c) (cadr f)) e (list (car c) (cadddr f))))
  (asec:dbg (strcat "\n\n=== " name " SECTION  View = " (asec:pt-str vd)
                    "  Compass " (asec:pt-str c) "  Raw start " (asec:pt-str s) "  Raw end " (asec:pt-str e)))
  (setq d (asec:unit (asec:v- e s))
        m (asec:normalize-section-orientation (list s e d (distance s e) (angle s e)) vd))
  (list name m vd
        (cons (cons 'clip T)
              (asec:make-view-boundary (car m) (cadr m) vd (cdr (assoc name (asa:depths f c))))))
)

(defun asa:count (d)
  (list (length (asec:get 'walls d)) (length (asec:get 'pws d))
        (+ (length (asec:get 'cd d)) (length (asec:get 'pd d)))
        (+ (length (asec:get 'cw d)) (length (asec:get 'pw d)))
        (length (asec:get 'un d))
        (+ (length (asec:get 'lwalls d)) (length (asec:get 'plwalls d)))
        (+ (length (asec:get 'scur d)) (length (asec:get 'stop d))))
)

;;; combined review of one floor: ds = discovered data per view, rs = edited records (or nil)
(defun asa:review-floor (i views ds gfref cfg / rs r k v st)
  (setq rs (list nil nil nil nil))
  (while (/= r "Generate")
    (princ (strcat "\n\nASA ANALYSIS - " (asec:floor-name i)))
    (setq k 0)
    (foreach v views
      (setq st (asa:count (nth k ds)))
      (princ (strcat "\n" (car v)
                     (if (nth k rs) "  (edited)" "")
                     "\n  Cut walls: " (itoa (car st)) "  Low walls: " (itoa (nth 5 st))
                     "  Slab edges: " (itoa (nth 6 st)) "  Projected walls: " (itoa (cadr st))
                     "  Doors: " (itoa (caddr st)) "  Windows: " (itoa (cadddr st))
                     "  Unresolved: " (itoa (nth 4 st))))
      (setq k (1+ k)))
    (initget "Generate Edit Cancel")
    (setq r (getkword "\n[Generate/Edit/Cancel] <Generate>: "))
    (cond
      ((null r) (setq r "Generate"))
      ((= r "Cancel") (exit))
      ((= r "Edit")
       (initget "North South East West Back")
       (setq v (getkword "\nEdit direction [North/South/East/West/Back] <Back>: "))
       (if (and v (/= v "Back"))
         (progn
           (setq k (asec:index-of (strcase v) asa:names) v (nth k views))
           ;; existing AS review loop for that view; its Generate returns the floor record
           (setq rs (asa:set-nth rs k (aseca:analyse-floor i (cadr v) (caddr v) (cadddr v) gfref cfg (nth k ds))))
           (asec:clear-labels))))))
  (setq k -1)
  (mapcar '(lambda (v)
             (setq k (1+ k))
             (cond ((nth k rs))
                   (T (aseca:auto-record i (nth k ds) (cadr v) (caddr v) (cadddr v) cfg))))
          views)
)

(defun asa:set-nth (lst n x / i)
  (setq i -1)
  (mapcar '(lambda (y) (setq i (1+ i)) (if (= i n) x y)) lst)
)

(defun asa:ensure-annotation-layer (k)
  (asec:ensure-layer (asec:gfx k) 7)
  (asec:gfx k)
)

(defun asa:text (p s h lay col / ed)
  (setq ed (list '(0 . "TEXT") (cons 8 lay) (list 10 (car p) (cadr p) 0.0) (list 11 (car p) (cadr p) 0.0)
                 (cons 40 h) (cons 1 s) '(72 . 1) '(73 . 2)))
  (entmakex (if col (append ed (list (cons 62 col))) ed))
)

;;; permanent compass: two axis LINEs, CIRCLE, N/S/E/W TEXT on SECTION_MARK_LAYER
(defun asa:draw-compass (c / lay s th col off ed)
  (setq lay (asa:ensure-annotation-layer "SECTION_MARK_LAYER")
        s   (* 0.5 (asec:gfx-num "SECTION_MARK_SIZE" 1.0))
        th  (asec:gfx-num "SECTION_MARK_TEXT_HEIGHT" 1.0)
        col (if (distof (cond ((asec:gfx "SECTION_MARK_COLOR")) ("")))
              (atoi (asec:gfx "SECTION_MARK_COLOR")))
        off (+ s (* 0.8 th)))
  (foreach seg (list (list (list (- (car c) s) (cadr c)) (list (+ (car c) s) (cadr c)))
                     (list (list (car c) (- (cadr c) s)) (list (car c) (+ (cadr c) s))))
    (setq ed (list '(0 . "LINE") (cons 8 lay) (cons 10 (append (car seg) '(0.0))) (cons 11 (append (cadr seg) '(0.0)))))
    (entmakex (if col (append ed (list (cons 62 col))) ed)))
  (setq ed (list '(0 . "CIRCLE") (cons 8 lay) (list 10 (car c) (cadr c) 0.0) (cons 40 (* 0.2 s))))
  (entmakex (if col (append ed (list (cons 62 col))) ed))
  (asa:text (list (car c) (+ (cadr c) off)) "N" th lay col)
  (asa:text (list (car c) (- (cadr c) off)) "S" th lay col)
  (asa:text (list (+ (car c) off) (cadr c)) "E" th lay col)
  (asa:text (list (- (car c) off) (cadr c)) "W" th lay col)
)

;;; local output bounds of a section relative to its 'ins: (xl xr yb yt)
;;; X = ins - uleft + U over U 0..field width; Y = ground (ins) .. top slab/wall
(defun asa:bounds (m fls cfg / allw uleft)
  (foreach fl fls (setq allw (append allw (asec:get 'walls fl))))
  (setq uleft (if allw (car (asec:wall-bounds allw)) 0.0))
  (list (- uleft) (- (nth 3 m) uleft) 0.0
        (+ (caddr cfg) (asec:floor-elevation (length fls) cfg)))
)

;;; insertion points in one row, in asa:names order (NORTH SOUTH EAST WEST): p is NORTH's
;;; insertion point; each next view's left edge = previous right edge + ASA_VIEW_GAP.
;;; Every insertion point has p's Y, so all Ground Floor datums line up.
(defun asa:layout (p bs / g right out)
  (setq g (asec:gfx-num "ASA_VIEW_GAP" 0.0))
  (foreach b bs
    (setq out (cons (if right (list (- (+ right g) (car b)) (cadr p)) p) out)
          right (+ (car (car out)) (cadr b))))
  (reverse out)
)

(defun c:ASA (/ f c cfg views floors i ds recs roofq ins bs lay th sec k b ref v)
  (princ "\nAUTO SECTION - ALL DIRECTIONS")
  (asec:load-graphics)
  (setq asec:old-error *error* *error* asec:error
        asec:undo-open nil asec:cmdecho nil asec:temp nil
        asec:temp-created 0 asec:temp-cleaned 0
        aseca:old-closing-layers *asec-closing-layers*
        *asec-closing-layers* *aseca-wall-layers*)
  (setq f (asa:get-field)
        c (asa:get-compass f)
        cfg (asec:get-config)
        views (mapcar '(lambda (n) (asa:make-view n f c)) asa:names)
        floors (list nil nil nil nil)
        i 0)
  (while i
    (setq ref (if (= i 0) c (asec:getpt (strcat "\nPick matching compass point for Floor " (itoa i) ": "))))
    (princ (strcat "\nASA - analysing " (asec:floor-name i) "..."))
    (setq ds (mapcar '(lambda (v) (aseca:discover-floor i (cadr v) (caddr v) (cadddr v) c cfg ref)) views)
          recs (asa:review-floor i views ds c cfg)
          floors (mapcar 'cons recs floors))
    (asa:ghost f c)
    (initget "Yes No")
    (setq i (if (= (getkword "\nAdd upper floor? [Yes/No] <No>: ") "Yes") (1+ i))))
  (initget "Yes No")
  (setq roofq (/= (getkword "\nAdd roof/terrace slab? [Yes/No] <Yes>: ") "No")
        bs (mapcar '(lambda (v fl) (asa:bounds (cadr v) fl cfg)) views floors))
  (redraw)
  (setq ins (asec:getpt "\nSpecify insertion point for ASA sections: "))
  (setq asec:cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (asec:clear-labels)
  (command-s "_.UNDO" "_BE")
  (setq asec:undo-open T)
  (asa:draw-compass c)
  (setq lay (asa:ensure-annotation-layer "SECTION_TITLE_LAYER")
        th (asec:gfx-num "SECTION_TITLE_HEIGHT" 1.0)
        k 0)
  (foreach p (asa:layout ins bs)
    (setq v (nth k views) b (nth k bs)
          sec (list (cons 'master (cadr v)) (cons 'viewdir (caddr v)) (cons 'config cfg)
                    (cons 'view (cadddr v)) (cons 'floors (reverse (nth k floors)))
                    (cons 'roof (if roofq (asec:get 'slab (car (nth k floors)))))
                    (cons 'ins p)))
    (asec:draw-section sec)
    (asa:text (list (+ (car p) (* 0.5 (+ (car b) (cadr b))))
                    (- (cadr p) (asec:gfx-num "SECTION_TITLE_OFFSET" 0.0) (* 0.5 th)))
              (strcat (car v) " SECTION") th lay nil)
    (setq k (1+ k)))
  (princ "\nASA: four sections generated.")
  (asec:cleanup)
  (asec:construction-report)
  (setq *error* asec:old-error)
  (princ)
)

(princ "\nAS loaded. Type AS (single section) or ASA (four directions).")
(princ)
