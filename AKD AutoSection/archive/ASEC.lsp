;;; ============================================================
;;; ASEC.lsp - Auto Section (MS2: cut/projected doors + windows)
;;; AutoCAD for Mac compatible. No VLA/VLAX/ActiveX/XData.
;;;
;;; USAGE
;;;   ASEC
;;;   1. Select Ground Floor section line (LINE).
;;;   2. Move cursor: ghost arrow shows viewing direction. Click.
;;;   3. Pick GF reference point (grid intersection, corner...).
;;;      Heights/slab/openings are read from SECTSET.
;;;   4. Per floor, each stage optional (Enter skips, Undo drops last):
;;;        CUT WALLS          two face LINEs per wall (numbered Wall 1..n)
;;;                           A face LINE broken by an opening may be picked
;;;                           anywhere: it is extended mathematically to the
;;;                           section (confirm if > *asec-wall-extension-limit*,
;;;                           default 3000). Point = pick face point directly.
;;;        CUT DOORS          INSERT on A-DOOR, hosted by a cut wall
;;;        CUT WINDOWS        INSERT on A-WINDOW, hosted by a cut wall
;;;        PROJECTED WALLS    two parallel face LINEs (or Point = 2 face points),
;;;                           run [Auto/Manual]; numbered Projected wall 1..n
;;;        PROJECTED DOORS    INSERT on A-DOOR  (+ Single/Double)
;;;        PROJECTED WINDOWS  INSERT on A-WINDOW
;;;        SLAB               [Auto/Manual/None]
;;;      Opening width is read from the block; [Accept/Manual] lets
;;;      you pick the two jambs instead. If width cannot be read
;;;      reliably, jambs are asked for directly.
;;;   5. Add upper floor? Yes -> pick the SAME reference point on
;;;      that floor's plan. Section line is translated (preview
;;;      shown in red). Stages as above. Repeat.
;;;   6. Add roof/terrace slab? [Yes/No]
;;;   7. Pick insertion point = where the leftmost wall's outer face
;;;      meets the ground line (GF slab underside).
;;;      Output: LWPOLYLINE/LINE on ASEC-SLAB, ASEC-WALL, ASEC-DOOR,
;;;      ASEC-WINDOW, ASEC-GROUND.
;;;
;;;   SECTSET  - view/change session settings (defaults built in).
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
;;;   The same boundary bounds ASECA's automatic discovery (ASECauto.lsp).
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
(defun asec:get-master-section (/ sel e ed s en d len)
  (while
    (progn
      (setq sel (entsel "\nSelect Ground Floor section line: "))
      (cond
        ((null sel) (if (= (getvar "ERRNO") 7) T (exit)))
        ((/= (asec:get 0 (setq ed (entget (car sel)))) "LINE")
         (princ "\nSection line must be a straight LINE.") T)
        ((< (distance (asec:2d (asec:get 10 ed)) (asec:2d (asec:get 11 ed))) asec:tol)
         (princ "\nSection line has zero length.") T)
      )
    )
  )
  (setq s   (asec:2d (asec:get 10 ed))
        en  (asec:2d (asec:get 11 ed))
        len (distance s en)
        d   (list (/ (- (car en) (car s)) len) (/ (- (cadr en) (cadr s)) len)))
  (list s en d len (angle s en))
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
;;; Generic plan-space viewing volume shared by ASEC, ASECA and future elevations.
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

;;; ---- shared by ASEC (guided) and ASECA (auto) ----
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

(defun asec:make-floor-record (i ref se ctx pws cd cw pd pw slab)
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
;;; Auto is confident only when both ends are CLOSING-LINES; otherwise ASEC asks.
(if (null *asec-proj-wall-end-search*) (setq *asec-proj-wall-end-search* 300.0))
(if (null *asec-proj-wall-end-tolerance*) (setq *asec-proj-wall-end-tolerance* 10.0))

(defun asec:project-point-wall-axis (p origin d) (asec:dot (asec:v- p origin) d))

;;; layer is on and thawed (hidden layers never contribute automatic geometry)
(defun asec:layer-visible-p (name / td)
  (and (setq td (tblsearch "LAYER" name))
       (> (asec:get 62 td) 0)                 ; negative colour = layer off
       (= 0 (logand 1 (asec:get 70 td))))     ; bit 1 = frozen
)

;;; closing-line candidates: LINEs, limited to *asec-closing-layers* when set (ASECA sets
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
;;; non-interactive Auto extent (shared with ASECA): (e0 e1 m0 m1 confident)
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
;;; Shared by guided ASEC and ASECA so both produce identical records.
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
;;; same test as asec:find-projected-host-wall (strip + finite run); used by ASECA
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
  (asec:rect (+ (car org) (car ext)) (+ (cadr org) elev (- slabT))
             (+ (car org) (cadr ext)) (+ (cadr org) elev) "ASEC-SLAB")
)

(defun asec:draw-roof-slab (org elev ext slabT)
  (asec:draw-slab org elev ext slabT)
)

;;; ground line at underside of GF slab, full master section length
(defun asec:draw-ground (org len slabT / y)
  (setq y (- (cadr org) slabT))
  (entmakex (list '(0 . "LINE") '(8 . "ASEC-GROUND")
                  (list 10 (car org) y 0.0)
                  (list 11 (+ (car org) len) y 0.0)))
)

(defun asec:draw-wall (org w bot top)
  (asec:rect (+ (car org) (asec:get 'umin w)) (+ (cadr org) bot)
             (+ (car org) (asec:get 'umax w)) (+ (cadr org) top) "ASEC-WALL")
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
      (asec:rect (+ (car org) (car pc)) (+ (cadr org) (cadr pc))
                 (+ (car org) (caddr pc)) (+ (cadr org) (cadddr pc)) "ASEC-WALL"))
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
  (asec:vis-rect x0 y0 x1 y1 "ASEC-DOOR" vis)
  (if (and (> (- x1 x0) (* 4 fr)) (> (- y1 y0) (* 2 fr)))
    (progn
      (asec:vis-poly (list (list (+ x0 fr) y0) (list (+ x0 fr) (- y1 fr))
                           (list (- x1 fr) (- y1 fr)) (list (- x1 fr) y0)) 0 "ASEC-DOOR" vis)
      (setq y1 (- y1 fr))))
  (if (= (asec:get 'type o) "DOUBLE")
    (progn (setq xm (* 0.5 (+ x0 x1)))
           (asec:vis-poly (list (list xm y0) (list xm y1)) 0 "ASEC-DOOR" vis)))
)

(defun asec:draw-projected-window (org o vis / bx x0 y0 x1 y1 fr)
  (setq bx (asec:opening-box org o) fr (asec:setting "FRAME_THK")
        x0 (car bx) y0 (cadr bx) x1 (caddr bx) y1 (cadddr bx))
  (asec:vis-rect x0 y0 x1 y1 "ASEC-WINDOW" vis)
  (if (and (> (- x1 x0) (* 2 fr)) (> (- y1 y0) (* 2 fr)))
    (asec:vis-rect (+ x0 fr) (+ y0 fr) (- x1 fr) (- y1 fr) "ASEC-WINDOW" vis))
)

;;; ---------- cut openings (drawn inside the wall void) ----------
;;; ponytail: simple frame section; richer profiles later
;;; door: head frame across wall thickness, FRAME_THK deep, below door head;
;;; closed panel FRAME_THK thick, centred in wall, floor -> frame underside
;;; ponytail: panel thickness reuses FRAME_THK; add a leaf-thickness setting if needed
(defun asec:draw-cut-door (org o / bx fr xm pw)
  (setq bx (asec:opening-box org o)
        fr (min (asec:setting "FRAME_THK") (asec:get 'height o))
        xm (* 0.5 (+ (car bx) (caddr bx)))
        pw (* 0.5 (min (asec:setting "FRAME_THK") (- (caddr bx) (car bx)))))
  (asec:rect (car bx) (- (cadddr bx) fr) (caddr bx) (cadddr bx) "ASEC-DOOR")
  (if (> (- (cadddr bx) fr (cadr bx)) asec:tol)
    (asec:rect (- xm pw) (cadr bx) (+ xm pw) (- (cadddr bx) fr) "ASEC-DOOR"))
)

;;; window: frame FRAME_THK wide centred in wall thickness, sill -> head, glazing line
(defun asec:draw-cut-window (org o / bx fr xm)
  (setq bx (asec:opening-box org o)
        xm (* 0.5 (+ (car bx) (caddr bx)))
        fr (* 0.5 (min (asec:setting "FRAME_THK") (- (caddr bx) (car bx)))))
  (asec:rect (- xm fr) (cadr bx) (+ xm fr) (cadddr bx) "ASEC-WINDOW")
  (asec:line (list xm (cadr bx)) (list xm (cadddr bx)) "ASEC-WINDOW")
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
(if (null *aseca-small-gap-tolerance*) (setq *aseca-small-gap-tolerance* 50.0))

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
(defun asec:draw-projected-wall-with-openings (org pw k ops focc pws / bot top mine vis)
  (setq bot (asec:get 'bottom pw) top (asec:get 'top pw)
        ;; the wall never hides its own edges or its hosted opening graphics
        vis (list org (asec:get 'vmid pw) focc (list (cons "PROJECTED" k))))
  (foreach o ops
    (if (= (asec:get 'phost o) k) (setq mine (cons o mine))))
  ;; only the wall's true end edges (one axis line if edge-on), clipped by nearer walls
  (foreach u (if (< (- (asec:get 'umax pw) (asec:get 'umin pw)) asec:tol)
               (list (* 0.5 (+ (asec:get 'u1 pw) (asec:get 'u2 pw))))
               (list (asec:get 'umin pw) (asec:get 'umax pw)))
    (cond
      ((asec:reveal-edge-p u pw k ops))
      ((asec:continued-edge-p u pw pws))
      (T (asec:vis-poly (list (list (+ (car org) u) (+ (cadr org) bot))
                              (list (+ (car org) u) (+ (cadr org) top))) 0 "ASEC-PROJ-WALL" vis))))
  (foreach o mine (asec:draw-projected-opening org o vis))
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
                          (cons 'v v) (cons 'v1 (asec:get 'v1 w)) (cons 'v2 (asec:get 'v2 w)))
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
(defun asec:build-floor-occluders (fl cfg / bot top ops k occ open)
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
  occ
)

(defun asec:occluder-blocks-at-z-p (oc z)
  (<= (- (asec:get 'zmin oc) *asec-occlusion-z-tolerance*) z
      (+ (asec:get 'zmax oc) *asec-occlusion-z-tolerance*))
)

;;; object at depth v is farther than the occluder (equal depth never hides)
(defun asec:nearer-occluder-p (oc v)
  (> v (+ (asec:get 'v oc) *asec-occlusion-depth-tolerance*))
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
(defun asec:axis-blockers (s lo hi v occ ign okey1 okey2 / pad out)
  (foreach oc occ
    (setq pad (if (and (= lo 'umin) (= (asec:get 'type oc) "CUT")) *asec-occlusion-u-tolerance* 0.0))
    (if (and (asec:nearer-occluder-p oc v) (not (asec:ignored-occluder-p oc ign))
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
                      (asec:axis-blockers s 'zmin 'zmax v occ ign 'umin 'umax)
                      (asec:axis-blockers s 'umin 'umax v occ ign 'zmin 'zmax))
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

(defun asec:flush-lines (/ groups g hit tl out)
  (setq tl *asec-occlusion-u-tolerance*)
  (foreach r asec:gen-lines
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
                   "  BLOCKS: " (if (and (asec:nearer-occluder-p oc v)
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

(defun asec:draw-section (sec / cfg slabT floors n i k top allw uleft org ops items pops focc)
  (setq cfg    (asec:get 'config sec)
        slabT  (caddr cfg)
        floors (asec:get 'floors sec)
        n      (length floors))
  (foreach fl floors (setq allw (append allw (asec:get 'walls fl))))
  (setq uleft (if allw (car (asec:wall-bounds allw)) 0.0)
        ;; insertion point = leftmost wall face on ground line
        org   (list (- (car (asec:get 'ins sec)) uleft)
                    (+ (cadr (asec:get 'ins sec)) slabT)))
  (setq asec:last-section sec asec:last-org org   ; for ASECTEST
        asec:gen-lines nil)
  (asec:ensure-layer "ASEC-SLAB" 1)
  (asec:ensure-layer "ASEC-WALL" 7)
  (asec:ensure-layer "ASEC-DOOR" 4)
  (asec:ensure-layer "ASEC-WINDOW" 5)
  (asec:ensure-layer "ASEC-PROJ-WALL" 8)
  (foreach fl floors
    (setq i   (asec:get 'index fl)
          ;; WALL_TOP: only TO_SLAB_UNDERSIDE exists
          top (- (asec:floor-elevation (1+ i) cfg) slabT)
          ops (append (asec:get 'cutdoors fl) (asec:get 'cutwins fl))
          k   0)
    (if (asec:get 'slab fl)
      (asec:draw-slab org (asec:get 'elev fl) (asec:get 'slab fl) slabT))
    (foreach w (asec:get 'walls fl)
      (setq k (1+ k))
      (asec:draw-wall-with-openings org w k (asec:get 'elev fl) top ops))
    (foreach o (asec:get 'cutdoors fl) (asec:draw-cut-door org o))
    (foreach o (asec:get 'cutwins fl) (asec:draw-cut-window org o))
    ;; projected items are collected here and drawn below in depth order
    (setq k 0 pops (append (asec:get 'projdoors fl) (asec:get 'projwins fl)))
    (setq focc (asec:build-floor-occluders fl cfg))
    (foreach pw (asec:get 'pwalls fl)
      (setq k (1+ k) items (cons (list (asec:get 'vmid pw) pw k pops focc (asec:get 'pwalls fl)) items)))
    (foreach o pops
      (if (null (asec:get 'phost o))
        (setq items (cons (list (asec:get 'v o) nil nil o focc) items))))
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
      ((cadr it)
       (asec:draw-projected-wall-with-openings org (cadr it) (caddr it) (cadddr it) (nth 4 it) (nth 5 it)))
      (T (asec:draw-projected-opening org (cadddr it) (list org (car it) (nth 4 it) nil)))))
  (asec:flush-lines)
  (if (asec:get 'roof sec)
    (asec:draw-roof-slab org (asec:floor-elevation n cfg) (asec:get 'roof sec) slabT))
  (if (= (asec:setting "GROUND_LINE") "YES")
    (progn (asec:ensure-layer "ASEC-GROUND" 3)
           (asec:draw-ground org (nth 3 (asec:get 'master sec)) slabT)))
)

;;; ---------- main ----------
;;; ---------- temporary wall number labels ----------
;;; TEXT entities (yellow, current layer) shown during selection, erased before
;;; the section is generated and on Esc. W = cut wall, P = projected wall.
(defun asec:label (p s se / h)
  (setq h (* 0.02 (distance (car se) (cadr se))))
  (car (setq asec:temp
         (cons (entmakex (list '(0 . "TEXT") (list 10 (car p) (cadr p) 0.0)
                               (list 11 (car p) (cadr p) 0.0) (cons 40 h) (cons 1 s)
                               '(62 . 2) '(72 . 1) '(73 . 2)))
               asec:temp)))
)

(defun asec:unlabel (e) (if (and e (entget e)) (entdel e)))

(defun asec:clear-labels ()
  (foreach e asec:temp (asec:unlabel e))
  (setq asec:temp nil)
)

(defun asec:cleanup ()
  (asec:clear-labels)
  (redraw)
  (if asec:undo-open (progn (command-s "_.UNDO" "_E") (setq asec:undo-open nil)))
  (if asec:cmdecho (setvar "CMDECHO" asec:cmdecho))
)

(defun asec:error (msg)
  (if (not (member msg '("Function cancelled" "quit / exit abort" "console break")))
    (princ (strcat "\nASEC error: " msg)))
  (asec:cleanup)
  (setq *error* asec:old-error)
  (princ)
)

(defun asec:main (/ m vd vb gfref cfg floors i)
  (setq m      (asec:get-master-section)
        vd     (asec:get-view-direction m)
        m      (asec:normalize-section-orientation m vd)
        vb     (asec:get-projection-boundary m vd)
        gfref  (asec:getpt "\nPick Ground Floor reference point: ")
        cfg    (asec:get-config)
        floors (list (asec:add-floor 0 m vd vb gfref cfg nil))
        i      0)
  (while (progn (initget "Yes No")
                (= (getkword "\nAdd upper floor? [Yes/No] <No>: ") "Yes"))
    (setq i (1+ i)
          floors (cons (asec:add-floor i m vd vb gfref cfg nil) floors))
  )
  (asec:generate m vd vb cfg floors)
)

;;; shared generation tail (ASEC + ASECA); floors newest first
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
  (asec:draw-section sec)
  (princ (strcat "\nSection generated: " (itoa (length floors)) " floor(s)."))
)

(defun c:ASEC ()
  (setq asec:old-error *error* *error* asec:error
        asec:undo-open nil asec:cmdecho nil asec:temp nil)
  (asec:main)
  (asec:cleanup)
  (setq *error* asec:old-error)
  (princ)
)

;;; ---------- ASECTEST: occlusion runtime test on the last generated section ----------
;;; Pick a plan line (two points) on a floor's plan + a height above that floor's level.
;;; V = average endpoint depth. Draws only the visible parts on ASEC-TEST (own UNDO group).
(defun c:ASECTEST (/ sec fls fl i p q s dir vd uv1 uv2 h z v occ vis org)
  (setq asec:old-error *error* *error* asec:error
        asec:undo-open nil asec:cmdecho nil asec:temp nil)
  (if (null (setq sec asec:last-section))
    (princ "\nRun ASEC or ASECA first.")
    (progn
      (setq fls (asec:get 'floors sec) i 0)
      (if (cdr fls)
        (progn (initget 4)
               (setq i (getint (strcat "\nFloor index [0-" (itoa (1- (length fls))) "] <0>: ")))
               (if (or (null i) (>= i (length fls))) (setq i 0))))
      (setq fl  (nth i fls)
            s   (asec:get 'start fl) dir (caddr (asec:get 'master sec)) vd (asec:get 'viewdir sec)
            p   (asec:getpt (strcat "\nTest line start on " (asec:floor-name i) " plan: "))
            q   (asec:getpt "\nTest line end: ")
            uv1 (asec:section-coordinate p s dir vd)
            uv2 (asec:section-coordinate q s dir vd)
            h   (getdist "\nHeight above floor level <1200>: ")
            z   (+ (asec:get 'elev fl) (if h h 1200.0))
            v   (* 0.5 (+ (cadr uv1) (cadr uv2)))
            occ (asec:build-floor-occluders fl (asec:get 'config sec))
            vis (asec:visible-u-intervals (car uv1) (car uv2) z v occ)
            org asec:last-org)
      (if (> (abs (- (cadr uv1) (cadr uv2))) *asec-occlusion-depth-tolerance*)
        (princ "\nNote: line not parallel to section, using average V."))
      (princ (strcat "\nTest line U " (rtos (min (car uv1) (car uv2)) 2 0) " -> "
                     (rtos (max (car uv1) (car uv2)) 2 0) "  Z " (rtos z 2 0) "  V " (rtos v 2 0)
                     "  Occluder pieces: " (itoa (length occ))
                     "  Visible intervals: " (itoa (length vis))))
      (foreach iv vis (princ (strcat "\n  U " (rtos (car iv) 2 0) " -> " (rtos (cadr iv) 2 0))))
      (setq asec:cmdecho (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command-s "_.UNDO" "_BE")
      (setq asec:undo-open T)
      (asec:ensure-layer "ASEC-TEST" 6)
      (foreach iv vis
        (asec:line (list (+ (car org) (car iv)) (+ (cadr org) z))
                   (list (+ (car org) (cadr iv)) (+ (cadr org) z)) "ASEC-TEST"))))
  (asec:cleanup)
  (setq *error* asec:old-error)
  (princ)
)

(princ "\nASEC loaded. Type ASEC to run.")
(princ)
