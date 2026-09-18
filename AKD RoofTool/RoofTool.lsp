;;; ==========================================================================
;;; AKD Roof V1  -  RoofTool.lsp
;;; Command: RF
;;;
;;; Footprint -> roof type -> live ghost -> commit -> optional rafters,
;;; battens and fascia, all in one RF run.
;;;
;;; Pure AutoLISP for AutoCAD for Mac: no ActiveX / VLA / VLAX / COM.
;;; Output is ordinary LINE / LWPOLYLINE entities. Drawing units = mm.
;;; ==========================================================================

;;; --------------------------------------------------------------------------
;;; CONFIGURATION  (future Settings menu edits this list)
;;; --------------------------------------------------------------------------
(setq *AKD-RF-CONFIG*
  '(
    ;; Rafters
    ("RAFTER_WIDTH"    . 50.0)
    ("RAFTER_DEPTH"    . 150.0)
    ("RAFTER_SPACING"  . 600.0)
    ;; Battens
    ("BATTEN_WIDTH"    . 50.0)
    ("BATTEN_DEPTH"    . 100.0)
    ("BATTEN_SPACING"  . 600.0)
    ;; Hip rafters
    ("HIP_RAFTER_WIDTH" . 50.0)
    ("HIP_RAFTER_DEPTH" . 150.0)
    ;; Valley rafters
    ("VALLEY_RAFTER_WIDTH" . 50.0)
    ("VALLEY_RAFTER_DEPTH" . 150.0)
    ;; Ridge beam
    ("RIDGE_BEAM_WIDTH" . 50.0)
    ("RIDGE_BEAM_DEPTH" . 200.0)
    ;; Fascia
    ("FASCIA_WIDTH"    . 25.0)
    ("FASCIA_DEPTH"    . 250.0)
    ;; Layers
    ("LAYER_ROOF"       . "A-ROOF")
    ("LAYER_RAFTER"     . "A-ROOF-RAFTER")
    ("LAYER_HIP_RAFTER" . "A-ROOF-HIP-RAFTER")
    ("LAYER_VALLEY_RAFTER" . "A-ROOF-VALLEY-RAFTER")
    ("LAYER_RIDGE_BEAM" . "A-ROOF-RIDGE-BEAM")
    ("LAYER_BATTEN"     . "A-ROOF-BATTEN")
    ("LAYER_FASCIA"     . "A-ROOF-FASCIA")
    ;; Layer colours (ACI) used when a layer is created
    ("COLOR_ROOF"       . 1)
    ("COLOR_RAFTER"     . 3)
    ("COLOR_HIP_RAFTER" . 30)
    ("COLOR_VALLEY_RAFTER" . 150)
    ("COLOR_RIDGE_BEAM" . 5)
    ("COLOR_BATTEN"     . 4)
    ("COLOR_FASCIA"     . 6)
    ;; Ghost preview colours (ACI)
    ("GHOST_PERIM"     . 8)
    ("GHOST_VALLEY"    . 5)
    ;; Framing representation: "Line" (centreline masters) or "Offset"
    ;; (actual plan-width outlines). Both use the same framing engine.
    ("FRAMING_MODE" . "Offset")
    ;; Actual-width framing (plan). Depth is not used by 2D plan geometry yet.
    ("MIN_MEMBER_LENGTH" . 50.0)   ; shorter trimmed members are skipped
    ("DEBUG_CENTRELINES" . "Off")  ; "On" also draws the centreline masters
    ;; Smallest spacing the user may request (rafters, battens)
    ("MIN_SPACING"     . 50.0)
    ;; Callouts (RF > Settings)
    ("CALLOUTS"            . "On")
    ("CALLOUT_TEXT_HEIGHT" . 250.0)
    ("LAYER_ANNO"          . "A-ROOF-ANNO")
    ("COLOR_ANNO"          . 7)
    ("GHOST_CALLOUT"       . 2)
    ("CALLOUT_ANGLE_INCREMENT" . 15.0)   ; degrees, WCS
    ("CALLOUT_LANDING_LENGTH"  . 500.0)  ; drawing units, individually placed callouts
    ("CALLOUT_MIN_LANDING_LENGTH" . 500.0) ; matched callout groups: shortest landing
    ("CALLOUT_MATCH_MAX_SHIFT"    . 2000.0) ; matched: largest allowed text Y shift
    ("CALLOUT_MIN_ROW_FACTOR"     . 1.5)    ; equal spacing: min row pitch = factor x text height
    ;; Cursor orientation hysteresis (>1.0 prevents diagonal flicker)
    ("HYSTERESIS"      . 1.15)
    ;; Geometry tolerances (drawing units)
    ("TOL"             . 0.001)
    ("MIN_SEG_LEN"     . 1.0)
   )
)

(if (not *AKD-RF-LASTTYPE*) (setq *AKD-RF-LASTTYPE* "Hip"))

(defun akd:cfg (key) (cdr (assoc key *AKD-RF-CONFIG*)))

;;; Runtime framing choices (session only). Config *_SPACING values are the
;;; DEFAULT specification; *AKD-RF-SPACING* holds the user's current request,
;;; e.g. (("RAFTER" . 450.0)). Solved (actual) spacing is never stored here.
(defun akd:rf-cur-spacing (name)
  (cond ((cdr (assoc name *AKD-RF-SPACING*)))
        ((akd:cfg (strcat name "_SPACING")))))

(defun akd:rf-set-spacing (name v / out)
  (foreach pr *AKD-RF-SPACING* (if (/= (car pr) name) (setq out (cons pr out))))
  (setq *AKD-RF-SPACING* (cons (cons name v) out))
  v)

(defun akd:num-str (v)
  (if (equal v (fix v) 1e-6) (itoa (fix v)) (rtos v 2 2)))

;;; Session settings (RF > Settings). Default from config, remembered per session.
(if (not *AKD-RF-CALLOUTS*) (setq *AKD-RF-CALLOUTS* (akd:cfg "CALLOUTS")))
(if (not *AKD-RF-FRAMING-MODE*) (setq *AKD-RF-FRAMING-MODE* (akd:cfg "FRAMING_MODE")))

;;; Framing representation for this RF run. The framing ENGINE never reads it:
;;; topology, faces, stations, spacing and the centreline masters are identical
;;; in both modes. Only the final output layer branches on it.
;;;   "Line"   -> the centreline masters are the permanent geometry (LINEs),
;;;               keeping the centreline conventions (rafters end on the ridge /
;;;               hip / valley centreline).
;;;   "Offset" -> actual-width closed outlines, face-clipped and node-mitred.
;;; This is production output, and is NOT the same thing as DEBUG_CENTRELINES,
;;; which is a developer overlay drawn on top of Offset geometry.
(defun akd:rf-offset-mode-p () (= *AKD-RF-FRAMING-MODE* "Offset"))

(defun akd:rf-ask-framing-mode (/ v)
  (initget "Line Offset")
  (setq v (getkword (strcat "\nFraming mode [Line/Offset] <" *AKD-RF-FRAMING-MODE* ">: ")))
  (if v (setq *AKD-RF-FRAMING-MODE* v))
  *AKD-RF-FRAMING-MODE*)

;;; Temporary system variables: first value saved, all restored on exit/error.
(defun akd:rf-sv-set (name val)
  (if (not (assoc name *AKD-RF-SV*))
    (setq *AKD-RF-SV* (cons (cons name (getvar name)) *AKD-RF-SV*)))
  (setvar name val))

(defun akd:rf-sv-restore ()
  (foreach pr *AKD-RF-SV* (setvar (car pr) (cdr pr)))
  (setq *AKD-RF-SV* nil))

;;; Member specification record: (name width depth spacing layer colour)
(defun akd:rf-spec (name)
  (list name
        (akd:cfg (strcat name "_WIDTH"))
        (akd:cfg (strcat name "_DEPTH"))
        (akd:cfg (strcat name "_SPACING"))
        (akd:cfg (strcat "LAYER_" name))
        (akd:cfg (strcat "COLOR_" name))))
(defun akd:spec-spacing (spec) (nth 3 spec))
(defun akd:spec-layer   (spec) (nth 4 spec))
(defun akd:spec-color   (spec) (nth 5 spec))

;;; ==========================================================================
;;; VECTOR / LIST HELPERS
;;; ==========================================================================
(defun akd:v+ (a b) (mapcar '+ a b))
(defun akd:v- (a b) (mapcar '- a b))
(defun akd:v* (a s) (mapcar '(lambda (x) (* x s)) a))
(defun akd:dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))   ; plan (XY) dot
(defun akd:len (a) (sqrt (akd:dot a a)))
(defun akd:unit (a / l)
  (setq l (akd:len a))
  (if (> l 1e-12) (list (/ (car a) l) (/ (cadr a) l) 0.0)))
(defun akd:cross2 (a b) (- (* (car a) (cadr b)) (* (cadr a) (car b))))
(defun akd:perp (a) (list (- (cadr a)) (car a) 0.0))

(defun akd:remove-nth (i lst / k out)
  (setq k 0)
  (foreach x lst (if (/= k i) (setq out (cons x out))) (setq k (1+ k)))
  (reverse out))

(defun akd:insert-num (x lst)
  (cond ((null lst) (list x))
        ((<= x (car lst)) (cons x lst))
        (T (cons (car lst) (akd:insert-num x (cdr lst))))))
(defun akd:sort-num (lst / out)
  (foreach x lst (setq out (akd:insert-num x out)))
  out)

;;; ==========================================================================
;;; FOOTPRINT ANALYSIS
;;; Footprint record (assoc list):
;;;   "ENT" "PTS" (CCW, normalised) "ELEV" "SHAPE" ("RECT" | "POLYGON")
;;;   "CENTER" (area centroid) "REFLEX" (per-vertex flags)
;;;   RECT only: "U" "V" "L" "W"  (U = unit long axis, L >= W; used by Gable)
;;; ==========================================================================
(defun akd:fp (fp key) (cdr (assoc key fp)))

;; T when every corner of the closed point list is a right angle.
(defun akd:rf-orthogonal-p (pts / n i ok e1 e2)
  (setq n (length pts) i 0 ok T)
  (while (and ok (< i n))
    (setq e1 (akd:unit (akd:v- (nth (rem (1+ i) n) pts) (nth i pts)))
          e2 (akd:unit (akd:v- (nth (rem (+ i 2) n) pts) (nth (rem (1+ i) n) pts))))
    (if (or (null e1) (null e2) (> (abs (akd:dot e1 e2)) 1e-6)) (setq ok nil))
    (setq i (1+ i)))
  ok)

;; Returns (OK footprint) or (ERR message). The source entity is never modified.
(defun akd:rf-analyze-footprint (ent / ed elev pts bulge n210 res)
  (setq ed (entget ent))
  (cond
    ((/= (cdr (assoc 0 ed)) "LWPOLYLINE")
     (list 'ERR "Selected object is not a closed LWPOLYLINE."))
    ((/= 1 (logand 1 (cdr (assoc 70 ed))))
     (list 'ERR "Selected polyline is open. Select a closed LWPOLYLINE."))
    ((and (setq n210 (cdr (assoc 210 ed)))
          (not (equal n210 '(0.0 0.0 1.0) 1e-9)))
     (list 'ERR "Polyline must lie flat in the World XY plane."))
    (T
     (setq elev (cond ((cdr (assoc 38 ed))) (0.0)))
     (foreach g ed
       (cond ((= (car g) 10)
              (setq pts (cons (list (cadr g) (caddr g) elev) pts)))
             ((and (= (car g) 42) (> (abs (cdr g)) 1e-9))
              (setq bulge T))))
     (cond
       (bulge (list 'ERR "Curved roof boundaries are not supported yet. Use a straight-segment closed polyline."))
       ((eq (car (setq res (akd:rf-normalize-polygon (reverse pts)))) 'ERR) res)
       ((and (= (length (cadr res)) 4) (akd:rf-orthogonal-p (cadr res)))
        (list 'OK (akd:rf-rect-frame ent (cadr res) elev)))
       (T
        (list 'OK (list (cons "ENT" ent) (cons "PTS" (cadr res)) (cons "ELEV" elev)
                        (cons "SHAPE" "POLYGON") (cons "CENTER" (akd:rf-centroid (cadr res)))
                        (cons "REFLEX" (akd:rf-reflex-flags (cadr res))))))))))

;; Local coordinate frame of a validated rectangle.
(defun akd:rf-rect-frame (ent pts elev / ea eb la lb c tmp)
  (setq ea (akd:v- (nth 1 pts) (nth 0 pts))
        eb (akd:v- (nth 3 pts) (nth 0 pts))
        la (akd:len ea)
        lb (akd:len eb)
        c  (akd:v* (akd:v+ (akd:v+ (nth 0 pts) (nth 1 pts))
                           (akd:v+ (nth 2 pts) (nth 3 pts))) 0.25))
  (if (< la lb)
    (setq tmp ea ea eb eb tmp
          tmp la la lb lb tmp))
  (list (cons "ENT" ent) (cons "PTS" pts) (cons "ELEV" elev)
        (cons "SHAPE" "RECT") (cons "CENTER" c) (cons "REFLEX" (akd:rf-reflex-flags pts))
        (cons "U" (akd:unit ea)) (cons "V" (akd:unit eb))
        (cons "L" la) (cons "W" lb)))

;; Local (x along U, y along V) -> WCS point at footprint elevation.
(defun akd:rf-lw (fp x y)
  (akd:v+ (akd:fp fp "CENTER")
          (akd:v+ (akd:v* (akd:fp fp "U") x) (akd:v* (akd:fp fp "V") y))))

(defun akd:rf-axis-vec (fp ax) (if (eq ax 'U) (akd:fp fp "U") (akd:fp fp "V")))

;;; ==========================================================================
;;; GENERAL FOOTPRINT  (closed straight-segment polygon, CCW internally)
;;; ==========================================================================
(defun akd:rf-area2 (pts / prev s)
  (setq prev (last pts) s 0.0)
  (foreach p pts (setq s (+ s (akd:cross2 prev p)) prev p))
  s)

(defun akd:rf-centroid (pts / prev a cx cy cr)
  (setq prev (last pts) a 0.0 cx 0.0 cy 0.0)
  (foreach p pts
    (setq cr (akd:cross2 prev p)
          a  (+ a cr)
          cx (+ cx (* cr (+ (car prev) (car p))))
          cy (+ cy (* cr (+ (cadr prev) (cadr p))))
          prev p))
  (list (/ cx (* 3.0 a)) (/ cy (* 3.0 a)) (caddr (car pts))))

;; T when segment ab touches or crosses segment cd.
(defun akd:rf-seg-hit-p (a b c d / d1 d2 d3 d4 tol)
  (setq tol (akd:cfg "TOL")
        d1 (akd:cross2 (akd:v- b a) (akd:v- c a)) d2 (akd:cross2 (akd:v- b a) (akd:v- d a))
        d3 (akd:cross2 (akd:v- d c) (akd:v- a c)) d4 (akd:cross2 (akd:v- d c) (akd:v- b c)))
  (or (and (/= (> d1 0.0) (> d2 0.0)) (/= (> d3 0.0) (> d4 0.0))
           (> (abs d1) 1e-9) (> (abs d2) 1e-9) (> (abs d3) 1e-9) (> (abs d4) 1e-9))
      (< (akd:rf-dist-to-seg c a b) tol) (< (akd:rf-dist-to-seg d a b) tol)
      (< (akd:rf-dist-to-seg a c d) tol) (< (akd:rf-dist-to-seg b c d) tol)))

;; Raw LWPOLYLINE vertices -> (OK pts) CCW, or (ERR message).
;; Removes only the duplicated closing vertex and redundant collinear vertices.
(defun akd:rf-normalize-polygon (raw / pts n i j bad changed a b c)
  (setq pts raw)
  (if (and (> (length pts) 1) (< (distance (car pts) (last pts)) (akd:cfg "TOL")))
    (setq pts (reverse (cdr (reverse pts)))))
  (setq n (length pts) i 0)
  (repeat n
    (if (< (distance (nth i pts) (nth (rem (1+ i) n) pts)) (akd:cfg "TOL"))
      (setq bad "Footprint has duplicate consecutive vertices or a zero-length edge."))
    (setq i (1+ i)))
  (if (not bad)
    (progn
      (setq changed T)
      (while (and changed (> (length pts) 3))
        (setq changed nil n (length pts) i 0)
        (while (and (not changed) (< i n))
          (setq a (nth (rem (+ i n -1) n) pts) b (nth i pts) c (nth (rem (1+ i) n) pts))
          (if (and (< (abs (akd:cross2 (akd:unit (akd:v- b a)) (akd:unit (akd:v- c b)))) 1e-9)
                   (> (akd:dot (akd:v- b a) (akd:v- c b)) 0.0))
            (setq pts (akd:remove-nth i pts) changed T))
          (setq i (1+ i))))
      (setq n (length pts))
      (cond
        ((< n 3) (setq bad "Footprint is degenerate."))
        (T
         (setq i 0)
         (while (and (not bad) (< i n))
           (setq j (1+ i))
           (while (and (not bad) (< j n))
             (if (and (/= (rem (1+ j) n) i) (/= (rem (1+ i) n) j)
                      (akd:rf-seg-hit-p (nth i pts) (nth (rem (1+ i) n) pts)
                                        (nth j pts) (nth (rem (1+ j) n) pts)))
               (setq bad "Footprint polyline is self-intersecting."))
             (setq j (1+ j)))
           (setq i (1+ i)))
         (if (and (not bad) (< (abs (akd:rf-area2 pts)) 2.0))
           (setq bad "Footprint area is near zero."))))))
  (if bad
    (list 'ERR bad)
    (list 'OK (if (< (akd:rf-area2 pts) 0.0) (reverse pts) pts))))

;; Per-vertex convex / reflex flags for a CCW polygon.
(defun akd:rf-count-true (lst / c)
  (setq c 0)
  (foreach x lst (if x (setq c (1+ c))))
  c)

(defun akd:rf-reflex-flags (pts / n i out)
  (setq n (length pts) i 0)
  (repeat n
    (setq out (cons (< (akd:cross2 (akd:v- (nth i pts) (nth (rem (+ i n -1) n) pts))
                                   (akd:v- (nth (rem (1+ i) n) pts) (nth i pts)))
                       -1e-9)
                    out)
          i   (1+ i)))
  (reverse out))

;; T when p is inside the polygon or on its boundary. Crossing parity and the
;; boundary test are kept separate: a point ON an edge (every footprint corner
;; is also a skeleton node) must never be flipped by a later crossing.
(defun akd:rf-point-in-poly-p (p pts / in edge prev tol)
  (setq prev (last pts) tol (* 10.0 (akd:cfg "TOL")))
  (foreach q pts
    (if (and (/= (> (cadr q) (cadr p)) (> (cadr prev) (cadr p)))
             (< (car p) (+ (car q) (/ (* (- (cadr p) (cadr q)) (- (car prev) (car q)))
                                     (- (cadr prev) (cadr q))))))
      (setq in (not in)))
    (if (< (akd:rf-dist-to-seg p prev q) tol) (setq edge T))
    (setq prev q))
  (or edge in))

;;; ==========================================================================
;;; EQUAL-PITCH HIP TOPOLOGY  (straight skeleton by wavefront propagation)
;;;
;;; Every eave line moves inward at unit speed. Wavefront polygons (LAVs) hold
;;; vertices (pos left-line right-line origin). A vertex moves on the bisector
;;; v with v.nL = v.nR = 1. Events are found for the whole wavefront and the
;;; earliest one is processed; all vertices advance to that time:
;;;   EDGE  : an edge shrinks to zero; its two vertices merge.
;;;   SPLIT : a reflex vertex reaches a non-adjacent edge; the LAV splits.
;;; Degenerate states are resolved immediately (coincident neighbours, and
;;; antiparallel slivers that trace level RIDGES). Each vertex leaves an arc
;;; (origin -> end) labelled with its two eave lines = the two faces it
;;; separates. Arcs become a node/edge graph; each eave's face is walked from
;;; that graph and classified RIDGE / HIP / VALLEY. The result is rejected
;;; (never approximated) if any check fails.
;;; Uses dynamic variables sP sD sN arcs nodes tol from akd:skl-solve.
;;; ==========================================================================
(defun akd:skl-vel (li ri / n1 n2 det)
  (setq n1 (nth li sN) n2 (nth ri sN) det (akd:cross2 n1 n2))
  (cond ((> (abs det) 1e-9)
         (list (/ (- (cadr n2) (cadr n1)) det) (/ (- (car n1) (car n2)) det) 0.0))
        ((> (akd:dot n1 n2) 0.0) n1)
        (T nil)))

(defun akd:skl-arc (p q fa fb)
  (if (> (distance p q) tol) (setq arcs (cons (list p q fa fb) arcs))))

(defun akd:skl-varc (v) (akd:skl-arc (nth 3 v) (car v) (cadr v) (caddr v)))

(defun akd:skl-mid (p q) (akd:v* (akd:v+ p q) 0.5))

;; cnt elements of lst starting at index start, cyclic.
(defun akd:cyc (lst start cnt / m i out)
  (setq m (length lst) i 0)
  (repeat cnt
    (setq out (cons (nth (rem (+ start i) m) lst) out) i (1+ i)))
  (reverse out))

;; Resolve degenerate wavefront states; returns the polygon or nil if consumed.
(defun akd:skl-resolve (poly / done m k hit v vp vn dp dn x w)
  (while (not done)
    (setq m (length poly) hit nil)
    (cond
      ((<= m 2)
       (if (= m 2)
         (progn
           (akd:skl-varc (car poly)) (akd:skl-varc (cadr poly))
           (akd:skl-arc (car (car poly)) (car (cadr poly)) (caddr (car poly)) (cadr (car poly)))))
       (if (= m 1) (akd:skl-varc (car poly)))
       (setq poly nil done T))
      (T
       ;; coincident neighbours -> merge
       (setq k 0)
       (while (and (not hit) (< k m))
         (if (< (distance (car (nth k poly)) (car (nth (rem (1+ k) m) poly))) tol) (setq hit k))
         (setq k (1+ k)))
       (if hit
         (progn
           (setq v (nth hit poly) vn (nth (rem (1+ hit) m) poly) x (akd:skl-mid (car v) (car vn)))
           (akd:skl-arc (nth 3 v) x (cadr v) (caddr v))
           (akd:skl-arc (nth 3 vn) x (cadr vn) (caddr vn))
           (setq poly (cons (list x (cadr v) (caddr vn) x) (akd:cyc poly (+ hit 2) (- m 2)))))
         (progn
           ;; antiparallel sliver -> level ridge to the nearer neighbour
           (setq k 0)
           (while (and (not hit) (< k m))
             (if (null (akd:skl-vel (cadr (nth k poly)) (caddr (nth k poly)))) (setq hit k))
             (setq k (1+ k)))
           (if (not hit)
             (setq done T)
             (progn
               (setq v  (nth hit poly)
                     vp (nth (rem (+ hit m -1) m) poly)
                     vn (nth (rem (1+ hit) m) poly)
                     dp (distance (car vp) (car v))
                     dn (distance (car vn) (car v)))
               (akd:skl-varc v)
               (cond
                 ((< (abs (- dp dn)) tol)
                  (setq x (akd:skl-mid (car vp) (car vn)))
                  (akd:skl-arc (car v) x (cadr v) (caddr v))
                  (akd:skl-arc (nth 3 vp) x (cadr vp) (caddr vp))
                  (akd:skl-arc (nth 3 vn) x (cadr vn) (caddr vn))
                  (setq poly (cons (list x (cadr vp) (caddr vn) x) (akd:cyc poly (+ hit 2) (- m 3)))))
                 ((< dn dp)
                  (akd:skl-arc (car v) (car vn) (cadr v) (caddr v))
                  (akd:skl-varc vn)
                  (setq poly (cons (list (car vn) (cadr v) (caddr vn) (car vn))
                                   (akd:cyc poly (+ hit 2) (- m 2)))))
                 (T
                  (akd:skl-arc (car v) (car vp) (cadr v) (caddr v))
                  (akd:skl-varc vp)
                  (setq poly (append (akd:cyc poly (+ hit 1) (- m 2))
                                     (list (list (car vp) (cadr vp) (caddr v) (car vp))))))))))))))
  poly)

;; Earliest event over all LAVs: (dt "EDGE"|"SPLIT" lav-index k m) or nil.
(defun akd:skl-next-event (lavs / best lix poly m vels k a b u rate dt v mm e ne s0 hp a2 b2 w ln)
  (setq lix 0)
  (foreach poly lavs
    (setq m (length poly)
          vels (mapcar '(lambda (x) (akd:skl-vel (cadr x) (caddr x))) poly)
          k 0)
    (repeat m
      (setq a (nth k poly) b (nth (rem (1+ k) m) poly) u (nth (caddr a) sD)
            rate (akd:dot (akd:v- (nth k vels) (nth (rem (1+ k) m) vels)) u))
      (if (> rate 1e-9)
        (progn
          (setq dt (max 0.0 (/ (akd:dot (akd:v- (car b) (car a)) u) rate)))
          (if (or (null best) (< dt (- (car best) 1e-9))
                  (and (<= (abs (- dt (car best))) 1e-9) (= (cadr best) "SPLIT")))
            (setq best (list dt "EDGE" lix k nil)))))
      (setq k (1+ k)))
    (setq k 0)
    (repeat m
      (setq v (nth k poly))
      (if (< (akd:cross2 (nth (cadr v) sD) (nth (caddr v) sD)) -1e-9)
        (progn
          (setq mm 0)
          (repeat m
            (if (not (or (= mm k) (= (rem (1+ mm) m) k)))
              (progn
                (setq a    (nth mm poly)
                      b    (nth (rem (1+ mm) m) poly)
                      e    (caddr a)
                      ne   (nth e sN)
                      s0   (akd:dot (akd:v- (car v) (car a)) ne)
                      rate (- 1.0 (akd:dot (nth k vels) ne)))
                (if (and (> rate 1e-9) (>= s0 (- tol)))
                  (progn
                    (setq dt (max 0.0 (/ s0 rate)))
                    (if (or (null best) (< dt (- (car best) 1e-9)))
                      (progn
                        (setq hp (akd:v+ (car v) (akd:v* (nth k vels) dt))
                              a2 (akd:v+ (car a) (akd:v* (nth mm vels) dt))
                              b2 (akd:v+ (car b) (akd:v* (nth (rem (1+ mm) m) vels) dt))
                              u  (nth e sD)
                              w  (akd:dot (akd:v- hp a2) u)
                              ln (akd:dot (akd:v- b2 a2) u))
                        (if (and (>= ln (- tol)) (>= w (- tol)) (<= w (+ ln tol)))
                          (setq best (list dt "SPLIT" lix k mm)))))))))
            (setq mm (1+ mm)))))
      (setq k (1+ k)))
    (setq lix (1+ lix)))
  best)

;; Solve the skeleton arcs of CCW pts. Returns arcs, or nil with *AKD-RF-SKL-REASON*.
(defun akd:skl-arcs (sP / n i sD sN arcs lavs it nl ev dt poly m k a b x v e ra rb pa pb out lix failed)
  (setq n (length sP) i 0 *AKD-RF-SKL-REASON* nil)
  (repeat n
    (setq sD (cons (akd:unit (akd:v- (nth (rem (1+ i) n) sP) (nth i sP))) sD) i (1+ i)))
  (setq sD (reverse sD) sN (mapcar 'akd:perp sD) i 0)
  (repeat n
    (setq lavs (cons (list (nth i sP) (rem (+ i n -1) n) i (nth i sP)) lavs) i (1+ i)))
  (setq lavs (list (reverse lavs)) it 0)
  (while (and lavs (not failed))
    (setq it (1+ it) nl nil)
    (foreach poly lavs (if (setq poly (akd:skl-resolve poly)) (setq nl (cons poly nl))))
    (setq lavs (reverse nl))
    (cond
      ((null lavs))
      ((> it (+ 100 (* 20 n))) (setq failed "unsupported simultaneous event (event limit)"))
      ((null (setq ev (akd:skl-next-event lavs))) (setq failed "degenerate skeleton event"))
      (T
       (setq dt (car ev)
             lavs (mapcar '(lambda (pl)
                             (mapcar '(lambda (vx)
                                        (list (akd:v+ (car vx) (akd:v* (akd:skl-vel (cadr vx) (caddr vx)) dt))
                                              (cadr vx) (caddr vx) (nth 3 vx)))
                                     pl))
                          lavs)
             poly (nth (caddr ev) lavs)
             m    (length poly)
             k    (nth 3 ev))
       (if (= (cadr ev) "EDGE")
         (progn
           (setq a (nth k poly) b (nth (rem (1+ k) m) poly) x (akd:skl-mid (car a) (car b)))
           (akd:skl-arc (nth 3 a) x (cadr a) (caddr a))
           (akd:skl-arc (nth 3 b) x (cadr b) (caddr b))
           (setq out (list (cons (list x (cadr a) (caddr b) x) (akd:cyc poly (+ k 2) (- m 2))))))
         (progn
           (setq v (nth k poly) x (car v) e (caddr (nth (nth 4 ev) poly)))
           (akd:skl-varc v)
           (setq ra (list x e (caddr v) x)
                 rb (list x (cadr v) e x)
                 pa (cons ra (akd:cyc poly (+ k 1) (rem (+ (- (nth 4 ev) k) m) m)))
                 pb (cons rb (akd:cyc poly (+ (nth 4 ev) 1) (rem (+ (- k (nth 4 ev) 1) m m) m)))
                 out (list pa pb))))
       (setq nl nil lix 0)
       (foreach pl lavs
         (setq nl (if (= lix (caddr ev)) (append (reverse out) nl) (cons pl nl)) lix (1+ lix)))
       (setq lavs (reverse nl)))))
  (if failed (progn (setq *AKD-RF-SKL-REASON* failed) nil) arcs))

;;; ---- skeleton graph -> faces ---------------------------------------------
(defun akd:skl-nid (p / i hit)
  (setq i 0)
  (foreach q nodes
    (if (and (null hit) (< (distance p q) (* 10.0 tol))) (setq hit i))
    (setq i (1+ i)))
  (if hit hit (progn (setq nodes (append nodes (list p))) (1- (length nodes)))))

(defun akd:skl-other (e c) (if (= (car e) c) (cadr e) (car e)))

;; Returns (faces nodes) with faces = ((eave-index node-ids types) ...),
;; or nil with *AKD-RF-SKL-REASON*.
(defun akd:skl-topology (sP / tol n arcs nodes edges i j fa fb c inc o1 o2 changed
                              faces etypes ids types used cur steps cand e nxt ty failed
                              sD sN p q t1 pr hi hj area)
  (setq tol (akd:cfg "TOL") n (length sP))
  (if (setq arcs (akd:skl-arcs sP))
    (progn
      (setq i 0)
      (repeat n
        (setq sD (cons (akd:unit (akd:v- (nth (rem (1+ i) n) sP) (nth i sP))) sD) i (1+ i)))
      (setq sD (reverse sD) sN (mapcar 'akd:perp sD))
      (foreach p sP (akd:skl-nid p))
      (foreach ar (reverse arcs)
        (setq i (akd:skl-nid (car ar)) j (akd:skl-nid (cadr ar))
              fa (min (caddr ar) (cadddr ar)) fb (max (caddr ar) (cadddr ar)))
        (if (and (/= i j) (/= fa fb)
                 (not (member (list (min i j) (max i j) fa fb) edges)))
          (setq edges (cons (list (min i j) (max i j) fa fb) edges))))
      ;; merge collinear chains through interior degree-2 nodes with one face pair
      (setq changed T)
      (while changed
        (setq changed nil c n)
        (while (and (not changed) (< c (length nodes)))
          (setq inc nil)
          (foreach e edges (if (or (= (car e) c) (= (cadr e) c)) (setq inc (cons e inc))))
          (if (and (= (length inc) 2) (equal (cddr (car inc)) (cddr (cadr inc))))
            (progn
              (setq o1 (akd:skl-other (car inc) c) o2 (akd:skl-other (cadr inc) c)
                    p  (akd:unit (akd:v- (nth o1 nodes) (nth c nodes)))
                    q  (akd:unit (akd:v- (nth o2 nodes) (nth c nodes))))
              (if (and (< (abs (akd:cross2 p q)) 1e-7) (< (akd:dot p q) 0.0))
                (setq edges (cons (list (min o1 o2) (max o1 o2) (caddr (car inc)) (cadddr (car inc)))
                                  (akd:list-minus edges inc))
                      changed T))))
          (setq c (1+ c))))
      ;; walk one face per eave; classify each skeleton edge once
      (setq i 0)
      (while (and (not failed) (< i n))
        (setq ids (list i (rem (1+ i) n)) types (list "EAVE") used nil cur (rem (1+ i) n) steps 0)
        (while (and (not failed) (/= cur i))
          (setq steps (1+ steps) cand nil)
          (foreach e edges
            (if (and (or (= (caddr e) i) (= (cadddr e) i))
                     (or (= (car e) cur) (= (cadr e) cur))
                     (not (member e used)))
              (setq cand (cons e cand))))
          (cond
            ((> steps (+ 2 (length edges))) (setq failed "open roof face"))
            ((/= (length cand) 1) (setq failed "ambiguous roof face"))
            (T
             (setq e (car cand) used (cons e used) nxt (akd:skl-other e cur)
                   j (if (= (caddr e) i) (cadddr e) (caddr e)))
             (if (not (setq ty (cdr (assoc e etypes))))
               (progn
                 (if (< (abs (akd:cross2 (nth i sD) (nth j sD))) 1e-6)
                   (setq ty "RIDGE")
                   (progn
                     (setq p  (nth cur nodes) q (nth nxt nodes)
                           t1 (akd:unit (akd:v- q p))
                           pr (akd:v+ (akd:skl-mid p q) (akd:v* (akd:perp t1) (min 1.0 (* 0.01 (distance p q)))))
                           hi (akd:dot (akd:v- pr (nth i sP)) (nth i sN))
                           hj (akd:dot (akd:v- pr (nth j sP)) (nth j sN))
                           ty (if (< hi hj) "HIP" "VALLEY"))))
                 (setq etypes (cons (cons e ty) etypes))))
             (setq types (append types (list ty)))
             (if (/= nxt i) (setq ids (append ids (list nxt))))
             (setq cur nxt))))
        (if (not failed) (setq faces (cons (list i ids types) faces)))
        (setq i (1+ i)))
      ;; checks: every edge typed, faces positive, full coverage, nodes inside
      (if (not failed)
        (progn
          (setq area 0.0)
          (foreach f faces
            (setq p (akd:rf-area2 (mapcar '(lambda (k) (nth k nodes)) (cadr f))))
            (if (<= p 0.0) (setq failed "inverted roof face"))
            (setq area (+ area p)))
          (foreach e edges (if (not (assoc e etypes)) (setq failed "unassigned skeleton edge")))
          (if (> (abs (- area (akd:rf-area2 sP))) (+ 2.0 (* 1e-6 (abs (akd:rf-area2 sP)))))
            (setq failed "roof faces do not cover the footprint"))
          (foreach p nodes (if (not (akd:rf-point-in-poly-p p sP)) (setq failed "skeleton node outside footprint")))))
      (if failed
        (progn (setq *AKD-RF-SKL-REASON* failed) nil)
        (list (reverse faces) nodes sD sN)))))

;; Remove every element of rm from lst (equal test).
(defun akd:list-minus (lst rm / out)
  (foreach x lst (if (not (member x rm)) (setq out (cons x out))))
  (reverse out))


;;; ==========================================================================
;;; ROOF SOLVER   (segments are lists (p1 p2) in WCS)
;;; HIP  : general equal-pitch topology (akd:skl-topology), any simple polygon.
;;; GABLE: rectangles only (irregular gable-end choice is not defined yet).
;;; FLAT : footprint is the roof region.
;;; ==========================================================================
;; axis = direction the ridge runs ('U or 'V); gable ends are the sides it meets.
(defun akd:rf-solve-gable (fp axis / hl hw)
  (setq hl (/ (akd:fp fp "L") 2.0) hw (/ (akd:fp fp "W") 2.0))
  (if (eq axis 'U)
    (list (list (akd:rf-lw fp (- hl) 0.0) (akd:rf-lw fp hl 0.0)))
    (list (list (akd:rf-lw fp 0.0 (- hw)) (akd:rf-lw fp 0.0 hw)))))


;;; ==========================================================================
;;; ROOF TOPOLOGY : faces with classified edges
;;; Roof record : "TYPE" "DATUM" "LINES" "FACES" "EDGES" "TOPH"
;;;   DATUM = footprint centroid (ridge midpoint / apex for a rectangle).
;;;   TOPH  = highest roof point as plan distance from its eave: the single
;;;           roof-level batten datum shared by every face.
;;; Face record : "POLY" "TYPES" "ALONG" "DOWN" ["EAVE" = source eave index]
;;;   POLY is a general simple polygon (any vertex count, may be non-convex);
;;;   edge 0 is always the EAVE. TYPES[i] classifies POLY[i] -> POLY[i+1]
;;;   as "EAVE" "RIDGE" "HIP" "VALLEY" "GABLE".
;;;   ALONG = unit along eave, DOWN = unit plan direction top -> eave.
;;; ==========================================================================
(defun akd:rf-make-face (pts types datum / along down mid)
  (setq along (akd:unit (akd:v- (cadr pts) (car pts)))
        down  (akd:perp along)
        mid   (akd:v* (akd:v+ (car pts) (cadr pts)) 0.5))
  (if (< (akd:dot (akd:v- mid datum) down) 0.0) (setq down (akd:v* down -1.0)))
  (list (cons "POLY" pts) (cons "TYPES" types) (cons "ALONG" along) (cons "DOWN" down)))

;; Faces from the general topology: one per eave, DOWN = -inward eave normal.
(defun akd:rf-faces-from-topology (topo / nodes sD sN)
  (setq nodes (cadr topo) sD (caddr topo) sN (cadddr topo))
  (mapcar '(lambda (f)
             (list (cons "POLY"  (mapcar '(lambda (k) (nth k nodes)) (cadr f)))
                   (cons "TYPES" (caddr f))
                   (cons "ALONG" (nth (car f) sD))
                   (cons "DOWN"  (akd:v* (nth (car f) sN) -1.0))
                   (cons "EAVE"  (car f))))
          (car topo)))

;; Faces on the eaves parallel to the ridge; other edges are gable ends.
(defun akd:rf-faces-gable (fp axis / pts c ax i a b faces)
  (setq pts (akd:fp fp "PTS") c (akd:fp fp "CENTER") ax (akd:rf-axis-vec fp axis) i 0)
  (repeat (length pts)
    (setq a (nth i pts) b (nth (rem (1+ i) (length pts)) pts))
    (if (> (abs (akd:dot (akd:unit (akd:v- b a)) ax)) 0.5)
      (setq faces
        (cons (akd:rf-make-face
                (list a b (akd:rf-project-to-line b c ax) (akd:rf-project-to-line a c ax))
                '("EAVE" "GABLE" "RIDGE" "GABLE") c)
              faces)))
    (setq i (1+ i)))
  (reverse faces))

(defun akd:rf-project-to-line (p o dir)
  (akd:v+ o (akd:v* dir (akd:dot (akd:v- p o) dir))))

;; Flat (provisional): the whole footprint is one face. Edge 0 is the longest
;; eave, so framing spans across it. Extension point: falls / drainage.
(defun akd:rf-faces-flat (fp / pts n i best bl types)
  (setq pts (akd:fp fp "PTS") n (length pts) i 0 bl 0.0 best 0)
  (repeat n
    (if (> (distance (nth i pts) (nth (rem (1+ i) n) pts)) (+ bl 1e-6))
      (setq bl (distance (nth i pts) (nth (rem (1+ i) n) pts)) best i))
    (setq i (1+ i)))
  (repeat n (setq types (cons "EAVE" types)))
  (list (akd:rf-make-face (akd:cyc pts best n) types (akd:fp fp "CENTER"))))

;;; Shared structural edges: one record per RIDGE / HIP / VALLEY, shared by the
;;; faces either side. Edge record: "TYPE" "P0" "P1" "FACES" (face indices).
;;; HIP / VALLEY: P0 = top (higher end), P1 = lower end.
;;; One geometric record per shared edge -> one shared station system.
(defun akd:rf-same-seg-p (e a b)
  (or (and (equal (akd:fp e "P0") a 1e-6) (equal (akd:fp e "P1") b 1e-6))
      (and (equal (akd:fp e "P0") b 1e-6) (equal (akd:fp e "P1") a 1e-6))))

(defun akd:rf-make-edge (ty a b face fi / down tmp)
  (setq down (akd:fp face "DOWN"))
  (if (and (member ty '("HIP" "VALLEY")) (> (akd:dot a down) (akd:dot b down)))
    (setq tmp a a b b tmp))
  (list (cons "TYPE" ty) (cons "P0" a) (cons "P1" b) (cons "FACES" (list fi))))

(defun akd:rf-build-edges (faces / fi pts types n i a b ty edges found)
  (setq fi 0)
  (foreach face faces
    (setq pts (akd:fp face "POLY") types (akd:fp face "TYPES") n (length pts) i 0)
    (repeat n
      (setq a (nth i pts) b (nth (rem (1+ i) n) pts) ty (nth i types))
      (if (member ty '("RIDGE" "HIP" "VALLEY"))
        (progn
          (setq found nil
                edges (mapcar
                        '(lambda (e)
                           (if (and (not found) (= (akd:fp e "TYPE") ty) (akd:rf-same-seg-p e a b))
                             (progn
                               (setq found T)
                               (subst (cons "FACES" (append (akd:fp e "FACES") (list fi)))
                                      (assoc "FACES" e) e))
                             e))
                        edges))
          (if (not found)
            (setq edges (append edges (list (akd:rf-make-edge ty a b face fi)))))))
      (setq i (1+ i)))
    (setq fi (1+ fi)))
  edges)

;; Roof lines (A-ROOF) = every non-zero shared structural edge.
(defun akd:rf-edge-segs (edges / out)
  (foreach e edges
    (if (> (distance (akd:fp e "P0") (akd:fp e "P1")) (akd:cfg "MIN_SEG_LEN"))
      (setq out (cons (list (akd:fp e "P0") (akd:fp e "P1")) out))))
  (reverse out))

(defun akd:rf-roof-top-height (faces / h d)
  (setq h 0.0)
  (foreach f faces
    (foreach p (akd:fp f "POLY")
      (setq d (- (akd:dot (car (akd:fp f "POLY")) (akd:fp f "DOWN")) (akd:dot p (akd:fp f "DOWN")))
            h (max h d))))
  h)

;; Returns the roof record, or nil (Hip topology unsolved: *AKD-RF-SKL-REASON*).
(defun akd:rf-build-roof (fp rtype axis / faces topo edges)
  (cond
    ((= rtype "Hip")
     (if (setq topo (akd:skl-topology (akd:fp fp "PTS")))
       (setq faces (akd:rf-faces-from-topology topo))))
    ((= rtype "Gable") (setq faces (akd:rf-faces-gable fp axis)))
    (T (setq faces (akd:rf-faces-flat fp))))
  (if faces
    (progn
      (setq edges (akd:rf-build-edges faces))
      (list (cons "TYPE"  rtype)
            (cons "DATUM" (akd:fp fp "CENTER"))
            (cons "LINES" (cond ((= rtype "Hip") (akd:rf-edge-segs edges))
                                ((= rtype "Gable") (akd:rf-solve-gable fp axis))))
            (cons "FACES" faces)
            (cons "EDGES" edges)
            (cons "TOPH"  (akd:rf-roof-top-height faces))))))

;; "6 faces, 2 ridges, 5 hips, 1 valley"
(defun akd:rf-topology-summary (roof / c)
  (setq c (mapcar '(lambda (ty) (length (akd:rf-edge-members roof ty ty))) '("RIDGE" "HIP" "VALLEY")))
  (strcat (itoa (length (akd:fp roof "FACES"))) " faces, "
          (itoa (car c)) " ridge(s), " (itoa (cadr c)) " hip(s), " (itoa (caddr c)) " valley(s)"))

;;; ==========================================================================
;;; FRAMING : face-aware rafters and battens, clipped per face
;;; ==========================================================================

;; Clip the infinite line {p : p.n = s} (direction dir) to a closed polygon.
;; One helper for triangles, rectangles, trapezoids; even-odd pairing keeps it
;; valid for future concave faces. Lines lying on a face boundary are skipped.
;; Endpoints are interpolated on the boundary edge itself, so they lie exactly
;; on the shared HIP / RIDGE / VALLEY line.
(defun akd:rf-clip-line (pts dir n s / cnt i a b da db p ts t0 t1 segs tol dmin dmax elev)
  (setq cnt (length pts) i 0 tol (akd:cfg "TOL") elev (caddr (car pts))
        dmin (akd:dot (car pts) n) dmax dmin)
  (foreach q pts
    (setq dmin (min dmin (akd:dot q n)) dmax (max dmax (akd:dot q n))))
  (if (and (> s (+ dmin tol)) (< s (- dmax tol)))
    (progn
      (while (< i cnt)
        (setq a (nth i pts) b (nth (rem (1+ i) cnt) pts)
              da (- (akd:dot a n) s) db (- (akd:dot b n) s))
        (if (/= (> da 0.0) (> db 0.0))       ; half-open rule handles vertex hits
          (setq p  (akd:v+ a (akd:v* (akd:v- b a) (/ da (- da db))))
                ts (cons (akd:dot p dir) ts)))
        (setq i (1+ i)))
      (setq ts (akd:sort-num ts))
      (while (cadr ts)
        (setq t0 (car ts) t1 (cadr ts) ts (cddr ts))
        (if (> (- t1 t0) (akd:cfg "MIN_SEG_LEN"))
          (setq segs (cons (list (akd:rf-dn-pt dir n t0 s elev)
                                 (akd:rf-dn-pt dir n t1 s elev)) segs))))))
  (reverse segs))

(defun akd:rf-dn-pt (dir n tt s elev)
  (list (+ (* (car dir) tt) (* (car n) s))
        (+ (* (cadr dir) tt) (* (cadr n) s))
        elev))

;; Lines in direction dir at stations s0 + k*spacing along n, clipped to pts.
;; kmin nil = all stations either side of s0 that fall inside the polygon.
;; Returns ((k p1 p2) ...) so callers keep the station (course) index.
(defun akd:rf-station-courses (pts dir n s0 spacing kmin / smin smax s k out tol)
  (setq tol (akd:cfg "TOL") smin (akd:dot (car pts) n) smax smin)
  (foreach q pts
    (setq smin (min smin (akd:dot q n)) smax (max smax (akd:dot q n))))
  (setq k (if kmin kmin (1- (fix (/ (- smin s0) spacing))))
        s (+ s0 (* k spacing)))
  (while (< s (- smax tol))
    (setq out (append out (mapcar '(lambda (sg) (cons k sg)) (akd:rf-clip-line pts dir n s)))
          k   (1+ k)
          s   (+ s0 (* k spacing))))
  out)

(defun akd:rf-station-lines (pts dir n s0 spacing kmin)
  (mapcar 'cdr (akd:rf-station-courses pts dir n s0 spacing kmin)))

;; Spacing measured in plan. Pitch hook: return true slope spacing projected
;; to plan here once roof pitch exists.
(defun akd:rf-plan-spacing (sp face) sp)

;;; ---- Solved rafter module ------------------------------------------------
;;; SPECIFICATION: requested spacing = maximum c/c.
;;; SOLVED LAYOUT : one module for the whole roof, taken from the governing run
;;;   run    = longest non-zero RIDGE, else longest hip/valley plan run
;;;            (apex to corner along the eave), else none (flat: requested)
;;;   bays   = ceiling(run / requested)
;;;   actual = run / bays   (never > requested, both run ends exact)
;;; Returns (actual bays run); bays/run nil when no governing run exists.
(defun akd:rf-bays (run req / n)
  (setq n (fix (/ run req)))
  (if (> run (+ (* n req) (akd:cfg "TOL"))) (setq n (1+ n)))
  (max n 1))

(defun akd:rf-hip-run (edge faces)
  (abs (akd:dot (akd:v- (akd:fp edge "P1") (akd:fp edge "P0"))
                (akd:fp (nth (car (akd:fp edge "FACES")) faces) "ALONG"))))

(defun akd:rf-rafter-module (roof req / run n d)
  (foreach e (akd:fp roof "EDGES")
    (if (and (= (akd:fp e "TYPE") "RIDGE")
             (> (setq d (distance (akd:fp e "P0") (akd:fp e "P1"))) (akd:cfg "MIN_SEG_LEN"))
             (or (null run) (> d run)))
      (setq run d)))
  (if (null run)
    (foreach e (akd:fp roof "EDGES")
      (if (and (member (akd:fp e "TYPE") '("HIP" "VALLEY"))
               (or (null run) (> (akd:rf-hip-run e (akd:fp roof "FACES")) run)))
        (setq run (akd:rf-hip-run e (akd:fp roof "FACES"))))))
  (if (and run (> run (akd:cfg "TOL")))
    (list (/ run (setq n (akd:rf-bays run req))) n run)
    (list req nil nil)))

;;; ---- Shared edge stations ------------------------------------------------
;;; Station = (kind . point). Both faces of an edge use the SAME point objects,
;;; so their rafter endpoints coincide exactly. sp = solved (actual) module.
;;;  RIDGE : m = round(length/sp) even bays, stations k = 0..m, so both ridge
;;;          ends arise naturally from the same station system.
;;;  HIP   : P0 (top) as a COMMON station, then JACK stations at
;;;          P0 + (P1-P0) * k*sp/d, d = plan offset of the hip measured
;;;          along the adjacent eave. On an equal-pitch hip d is identical for
;;;          both faces, so station k sits k*sp (plan) from the ridge end
;;;          along BOTH eaves: jacks on both slopes keep the solved module.
;;;  VALLEY: same rule from its top end; stations are the bottom ends of the
;;;          valley jacks on both faces (kind "VALLEY").
(defun akd:rf-edge-stations (edge faces sp / tol p0 p1 len m d k out)
  (setq tol (akd:cfg "TOL") p0 (akd:fp edge "P0") p1 (akd:fp edge "P1"))
  (cond
    ((= (akd:fp edge "TYPE") "RIDGE")
     (setq len (distance p0 p1))
     (if (> len tol)
       (progn
         (setq m (max 1 (fix (+ (/ len sp) 0.5))) k 0)
         (repeat (1+ m)
           (setq out (cons (cons "COMMON" (akd:v+ p0 (akd:v* (akd:v- p1 p0) (/ (float k) m)))) out)
                 k   (1+ k))))
       (setq out (list (cons "COMMON" p0))))
     out)
    ((member (akd:fp edge "TYPE") '("HIP" "VALLEY"))
     (setq d   (akd:rf-hip-run edge faces)
           out (list (cons (if (= (akd:fp edge "TYPE") "HIP") "COMMON" "VALLEY") p0))
           k   1)
     (while (< (* k sp) (- d tol))
       (setq out (cons (cons (if (= (akd:fp edge "TYPE") "HIP") "JACK" "VALLEY")
                             (akd:v+ p0 (akd:v* (akd:v- p1 p0) (/ (* k sp) d))))
                       out)
             k   (1+ k)))
     out)
    (T nil)))

(defun akd:rf-pt-in-list (p lst)
  (while (and lst (not (equal p (car lst) 1e-6))) (setq lst (cdr lst)))
  lst)

;; Rafters of one general face. Every shared RIDGE / HIP / VALLEY station of
;; the face defines a rafter line in the face's DOWN direction; the line is
;; clipped to the face and the piece that ends at the station is kept (top end
;; on ridge/hip stations, bottom end on valley stations). Other endpoints are
;; snapped to the face's shared station points, so both faces of a shared edge
;; use identical points. Lines closer than 0.35 x module are merged with
;; priority RIDGE > HIP > VALLEY.
;; V1 centreline rule: jacks end on the hip / valley centreline and commons on
;; the ridge centreline. With real timber widths these must move to the
;; member faces - do not change that here.
;; Members: (kind p-top p-bottom), kind "COMMON" (ridge/hip top -> eave) or "JACK".
(defun akd:rf-station-rank (ty) (cond ((= ty "RIDGE") 0) ((= ty "HIP") 1) (T 2)))

(defun akd:rf-near-any (s lst d)
  (while (and lst (>= (abs (- s (car lst))) d)) (setq lst (cdr lst)))
  lst)

(defun akd:rf-snap-pt (p lst tol / hit)
  (foreach q lst (if (and (null hit) (< (distance p q) tol)) (setq hit q)))
  (if hit hit p))

(defun akd:rf-face-rafters (face fi edge-stations sp / pts along down tol near sts sorted
                                 allpts lines st pt kind s top bot out)
  (setq pts   (akd:fp face "POLY")
        along (akd:fp face "ALONG")
        down  (akd:fp face "DOWN")
        tol   (akd:cfg "TOL")
        near  (* 0.35 sp))
  (foreach es edge-stations
    (if (member fi (akd:fp (car es) "FACES"))
      (foreach st (cdr es)
        (setq sts (cons (list (akd:rf-station-rank (akd:fp (car es) "TYPE")) (car st) (cdr st)) sts)))))
  (setq sts (reverse sts))
  (foreach r '(0 1 2) (foreach st sts (if (= (car st) r) (setq sorted (cons st sorted)))))
  (setq sorted (reverse sorted) allpts (mapcar 'caddr sorted))
  (foreach st sorted
    (setq pt (caddr st) kind (cadr st) s (akd:dot pt along))
    (if (not (akd:rf-near-any s lines near))
      (progn
        (setq lines (cons s lines))
        (foreach seg (akd:rf-clip-line pts down along s)
          (setq top nil)
          (cond ((< (distance (car seg) pt) tol)  (setq top pt bot (cadr seg)))
                ((< (distance (cadr seg) pt) tol) (setq top (car seg) bot pt)))
          (if top
            (setq top (akd:rf-snap-pt top allpts tol)
                  bot (akd:rf-snap-pt bot allpts tol)
                  out (cons (list (cond ((member kind '("JACK" "VALLEY")) "JACK")
                                        ((< (akd:rf-dist-to-seg bot (car pts) (cadr pts)) tol) "COMMON")
                                        (T "JACK"))
                                  top bot)
                            out)))))))
  (if (null sts)
    ;; Face without structural edges (flat roof): plain grid from the datum.
    (foreach seg (akd:rf-station-lines pts down along (akd:dot (akd:fp roof "DATUM") along)
                                       (akd:rf-plan-spacing sp face) nil)
      (setq out (cons (cons "COMMON" seg) out))))
  (reverse out))

;; Battens run ALONG (perpendicular to rafters), rows at the REQUESTED spacing
;; below ONE roof-level datum TOPH (highest ridge / apex), measured toward the
;; eave; the remainder falls at the eave (not redistributed). Equal pitch means
;; equal height at equal plan distance from every eave, so course k meets the
;; same point on every hip. Rows above a face's own top simply clip away.
;; Returns ((course p1 p2) ...): course k is the same course on every face.
(defun akd:rf-face-battens (face sp toph / pts down)
  (setq pts (akd:fp face "POLY") down (akd:fp face "DOWN"))
  (akd:rf-station-courses pts (akd:fp face "ALONG") down
                          (- (akd:dot (car pts) down) toph)
                          (akd:rf-plan-spacing sp face) 1))

;; Member = (kind p1 p2 ...); battens append (course face-index).
;; Extension point: return timber-width outlines from width/depth later.
(defun akd:rf-member-geometry (mbr spec) (list (list (cadr mbr) (caddr mbr))))

;; Face-framed members, name "RAFTER" or "BATTEN"; req = requested spacing.
;; Rafters use the solved roof module; battens use req directly.
(defun akd:rf-roof-members (roof name req / faces sp edge-stations fi out)
  (setq faces (akd:fp roof "FACES") fi 0)
  (if (= name "RAFTER")
    (setq sp (car (akd:rf-rafter-module roof req))
          edge-stations
            (mapcar '(lambda (e) (cons e (akd:rf-edge-stations e faces (akd:rf-plan-spacing sp nil))))
                    (akd:fp roof "EDGES"))))
  (foreach face faces
    (setq out (append out
                (if (= name "RAFTER")
                  (akd:rf-face-rafters face fi edge-stations sp)
                  (mapcar '(lambda (c) (list "BATTEN" (cadr c) (caddr c) (car c) fi))
                          (akd:rf-face-battens face req (akd:fp roof "TOPH")))))
          fi  (1+ fi)))
  out)

;; Members lying on shared edges of type ty (hip rafters, ridge beams).
;; Zero-length edges (pyramid apex) never produce a member.
(defun akd:rf-edge-members (roof ty kind / out)
  (foreach e (akd:fp roof "EDGES")
    (if (and (= (akd:fp e "TYPE") ty)
             (> (distance (akd:fp e "P0") (akd:fp e "P1")) (akd:cfg "MIN_SEG_LEN")))
      (setq out (cons (list kind (akd:fp e "P0") (akd:fp e "P1")) out))))
  (reverse out))

;;; ==========================================================================
;;; ACTUAL-WIDTH MEMBERS  (display / construction geometry)
;;;
;;; The centreline members stay the master geometry: topology, stations and
;;; spacing are never touched here. This layer only derives plan outlines:
;;;
;;;   MEMBER CENTRELINE -> MEMBER WIDTH -> ACTUAL MEMBER OUTLINE
;;;
;;; Rafters terminate on the FACE of the ridge / hip / valley member they run
;;; into (not its centreline), and structural members are mitred where they
;;; meet at a skeleton node. Battens and fascia are unchanged (centreline).
;;; ==========================================================================
(defun akd:rf-member-width (name) (akd:cfg (strcat name "_WIDTH")))

;; Closed plan outline of width w centred on a -> b.
(defun akd:rf-outline (a b w / d n h)
  (setq d (akd:unit (akd:v- b a)))
  (if d
    (progn
      (setq n (akd:perp d) h (* 0.5 w))
      (list (akd:v+ a (akd:v* n h)) (akd:v+ b (akd:v* n h))
            (akd:v- b (akd:v* n h)) (akd:v- a (akd:v* n h))))))

;; Keep the half-plane dot(x - p, nrm) >= 0 (Sutherland-Hodgman, convex in).
(defun akd:rf-clip-halfplane (pts p nrm / out prev dp cur dc tt)
  (setq prev (last pts) dp (akd:dot (akd:v- prev p) nrm))
  (foreach cur pts
    (setq dc (akd:dot (akd:v- cur p) nrm))
    (if (/= (>= dp 0.0) (>= dc 0.0))
      (setq tt  (/ dp (- dp dc))
            out (cons (akd:v+ prev (akd:v* (akd:v- cur prev) tt)) out)))
    (if (>= dc 0.0) (setq out (cons cur out)))
    (setq prev cur dp dc))
  (reverse out))

;; Every non-zero structural edge as (p0 p1 width type).
(defun akd:rf-struct-edges (roof / out ty w)
  (foreach e (akd:fp roof "EDGES")
    (setq ty (akd:fp e "TYPE")
          w  (cond ((= ty "RIDGE")  (akd:rf-member-width "RIDGE_BEAM"))
                   ((= ty "HIP")    (akd:rf-member-width "HIP_RAFTER"))
                   ((= ty "VALLEY") (akd:rf-member-width "VALLEY_RAFTER"))))
    (if (and w (> (distance (akd:fp e "P0") (akd:fp e "P1")) (akd:cfg "MIN_SEG_LEN")))
      (setq out (cons (list (akd:fp e "P0") (akd:fp e "P1") w ty) out))))
  (reverse out))

;; If p lies on a structural edge, pull it back along the member direction to
;; that edge's physical FACE (half width, measured on the offset line), so the
;; timber stops against the ridge / hip / valley instead of running through it.
(defun akd:rf-trim-end (p other sedges / tol dir best u n d)
  (setq tol (* 10.0 (akd:cfg "TOL")) dir (akd:unit (akd:v- other p)))
  (if dir
    (foreach e sedges
      (if (and (null best) (< (akd:rf-dist-to-seg p (car e) (cadr e)) tol))
        (progn
          (setq u (akd:unit (akd:v- (cadr e) (car e)))
                n (akd:perp u))
          (if (< (akd:dot n dir) 0.0) (setq n (akd:v* n -1.0)))
          (setq d (akd:dot dir n))
          (if (> d 1e-6) (setq best (akd:v+ p (akd:v* dir (/ (* 0.5 (caddr e)) d)))))))))
  (if best best p))

;; Cut a rafter OUTLINE against the physical faces of the structural members
;; its ends sit on. A square cut would push one corner into the hip and leave a
;; gap at the other, so the outline is clipped by the face half-plane instead,
;; giving the correct angled cut. A rafter top can land on a NODE where three
;; or more edges meet (a ridge end plus two hips), so EVERY edge containing the
;; endpoint is clipped - stopping at the first one leaves timber inside the
;; others by up to half a width.
(defun akd:rf-clip-to-faces (pts a b sedges / tol d u n)
  (setq tol (* 10.0 (akd:cfg "TOL")))
  (foreach pr (list (list a b) (list b a))
    (setq d (akd:unit (akd:v- (cadr pr) (car pr))))
    (if d
      (foreach e sedges
        (if (and pts (< (akd:rf-dist-to-seg (car pr) (car e) (cadr e)) tol))
          (progn
            (setq u (akd:unit (akd:v- (cadr e) (car e)))
                  n (akd:perp u))
            (if (< (akd:dot n d) 0.0) (setq n (akd:v* n -1.0)))
            (setq pts (akd:rf-clip-halfplane
                        pts (akd:v+ (car e) (akd:v* n (* 0.5 (caddr e)))) n)))))))
  pts)

;; Mitre one end of a structural member against its angular neighbours at that
;; node: clip by the bisector half-plane on each side. Works for 2, 3, 4 or
;; more incident edges (a pyramid apex is just the 4-edge case), so there is no
;; per-shape node code. Equal widths give a true mitre.
(defun akd:rf-miter-end (pts node far sedges / tol dir nd delta ccw cw ang m nrm)
  (setq tol (* 10.0 (akd:cfg "TOL")) dir (akd:unit (akd:v- far node)))
  (if dir
    (progn
      (foreach e sedges
        (foreach pair (list (list (car e) (cadr e)) (list (cadr e) (car e)))
          (if (< (distance node (car pair)) tol)
            (progn
              (setq nd (akd:unit (akd:v- (cadr pair) (car pair))))
              (if nd
                (progn
                  (setq delta (atan (akd:cross2 dir nd) (akd:dot dir nd)))
                  (if (> (abs delta) 1e-6)
                    (progn
                      (if (and (> delta 0.0) (< delta (- pi 1e-6))
                               (or (null ccw) (< delta ccw)))
                        (setq ccw delta ang nd))
                      (if (and (< delta 0.0) (> delta (+ (- pi) 1e-6))
                               (or (null cw) (> delta cw)))
                        (setq cw delta))))))))))
      ;; one clip per neighbouring direction actually found
      (foreach e sedges
        (foreach pair (list (list (car e) (cadr e)) (list (cadr e) (car e)))
          (if (and pts (< (distance node (car pair)) tol))
            (progn
              (setq nd (akd:unit (akd:v- (cadr pair) (car pair))))
              (if nd
                (progn
                  (setq delta (atan (akd:cross2 dir nd) (akd:dot dir nd)))
                  (if (and (> (abs delta) 1e-6) (< (abs delta) (- pi 1e-6))
                           (or (equal delta ccw 1e-9) (equal delta cw 1e-9)))
                    (progn
                      (setq m (akd:unit (akd:v+ dir nd)))
                      (if m
                        (progn
                          (setq nrm (akd:perp m))
                          (if (< (akd:dot nrm dir) 0.0) (setq nrm (akd:v* nrm -1.0)))
                          (setq pts (akd:rf-clip-halfplane pts node nrm))))))))))))))
  pts)

(defun akd:rf-struct-outline (a b w sedges / pts)
  (setq pts (akd:rf-outline a b w))
  (if pts (setq pts (akd:rf-miter-end pts a b sedges)))
  (if pts (setq pts (akd:rf-miter-end pts b a sedges)))
  pts)

;; Plan outlines for one member set, or nil when that member type is still
;; centreline-only (battens). Members must already be trimmed.
(defun akd:rf-display-outlines (roof name members / w sedges minlen a b ta tb pts out)
  (setq w      (akd:rf-member-width name)
        minlen (akd:cfg "MIN_MEMBER_LENGTH")
        sedges (akd:rf-struct-edges roof))
  ;; Line mode commits the masters themselves, so no outlines are derived.
  ;; Battens are centreline courses in both modes (unchanged this pass).
  (if (and w (/= name "BATTEN") (akd:rf-offset-mode-p))
    (progn
      (foreach m members
        (setq a (cadr m) b (caddr m) pts nil)
        (if (member name '("HIP_RAFTER" "VALLEY_RAFTER" "RIDGE_BEAM"))
          ;; structural member: rectangle mitred against its neighbours
          (if (> (distance a b) minlen)
            (setq pts (akd:rf-struct-outline a b w sedges)))
          ;; rafter: outline of the MASTER centreline, cut to the member faces.
          ;; The trimmed centreline only decides whether enough timber is left
          ;; to be worth drawing (a jack shorter than this is skipped rather
          ;; than emitted as a degenerate or self-crossing outline).
          (progn
            (setq ta (akd:rf-trim-end a b sedges)
                  tb (akd:rf-trim-end b a sedges))
            (if (> (distance ta tb) minlen)
              (setq pts (akd:rf-clip-to-faces (akd:rf-outline a b w) a b sedges)))))
        (if (and pts (> (length pts) 2)) (setq out (cons pts out))))
      (reverse out))))

;;; ==========================================================================
;;; PREVIEW  (screen-only: grread / grdraw / redraw)
;;; ==========================================================================
(defun akd:ghost-segs (segs color)
  (foreach sg segs
    (grdraw (trans (car sg) 0 1) (trans (cadr sg) 0 1) color 0)))

(defun akd:ghost-poly (pts color hl / prev)
  (setq prev (last pts))
  (foreach p pts
    (grdraw (trans prev 0 1) (trans p 0 1) color hl)
    (setq prev p)))

;; Generic cursor-driven picker, reusable for future AKD Roof features.
;;   statefn : (lambda (wcs-pt state) -> new state)
;;   drawfn  : (lambda (state) draws ghost)
;;   msg     : string, or (lambda (state) -> string) re-printed after a change
;;   keyfn   : optional (lambda (state) -> new state) run when S is typed;
;;             it may prompt normally because grread is not active then.
;; Returns accepted state. Esc raises the normal cancel error.
(defun akd:ghost-pick (msg state statefn drawfn)
  (akd:ghost-pick-keys msg state statefn drawfn nil))

(defun akd:ghost-pick-keys (msg state statefn drawfn keyfn / gr code done new)
  (princ (if (= (type msg) 'STR) msg (apply msg (list state))))
  (redraw)
  (apply drawfn (list state))
  (while (not done)
    (setq gr (grread T 15 0) code (car gr))
    (cond
      ((= code 5)
       (setq new (apply statefn (list (trans (cadr gr) 1 0) state)))
       (if (not (equal new state)) (progn (setq state new) (redraw)))
       (apply drawfn (list state)))
      ((member code '(3 11 25)) (setq done T))
      ((and (= code 2) (member (cadr gr) '(13 32))) (setq done T))
      ((and (= code 2) (= (cadr gr) 27)) (redraw) (exit))
      ((and keyfn (= code 2) (member (cadr gr) '(83 115)))
       (redraw)
       (setq state (apply keyfn (list state)))
       (princ (if (= (type msg) 'STR) msg (apply msg (list state))))
       (apply drawfn (list state)))))
  (redraw)
  state)

;; Choose 'U / 'V from cursor position in footprint-local terms.
;; Offsets are normalised by half-dimensions so the switch line is the
;; rectangle diagonal; hysteresis stops flicker near it.
(defun akd:rf-axis-from-cursor (fp pt cur / d du dv h tol)
  (setq d   (akd:v- pt (akd:fp fp "CENTER"))
        tol (akd:cfg "TOL")
        h   (akd:cfg "HYSTERESIS")
        du  (/ (abs (akd:dot d (akd:fp fp "U"))) (max (/ (akd:fp fp "L") 2.0) tol))
        dv  (/ (abs (akd:dot d (akd:fp fp "V"))) (max (/ (akd:fp fp "W") 2.0) tol)))
  (cond ((and (< du tol) (< dv tol)) cur)
        ((eq cur 'U) (if (> dv (* du h)) 'V 'U))
        ((eq cur 'V) (if (> du (* dv h)) 'U 'V))
        ((>= du dv) 'U)
        (T 'V)))

;;; ==========================================================================
;;; OUTPUT
;;; ==========================================================================
(defun akd:rf-ensure-layer (name color)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 name) '(70 . 0)
                   (cons 62 color) '(6 . "Continuous")))))

(defun akd:rf-make-lines (segs layer / cnt)
  (setq cnt 0)
  (foreach sg segs
    (if (entmake (list '(0 . "LINE") (cons 8 layer) (cons 10 (car sg)) (cons 11 (cadr sg))))
      (setq cnt (1+ cnt))))
  cnt)

(defun akd:rf-make-poly (pts elev layer closed)
  (entmake
    (append
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer)
            '(100 . "AcDbPolyline") (cons 90 (length pts))
            (cons 70 (if closed 1 0)) (cons 38 elev))
      (mapcar '(lambda (p) (list 10 (car p) (cadr p))) pts)
      (list '(210 0.0 0.0 1.0)))))

;;; ---- Batten courses ------------------------------------------------------
(defun akd:rf-dist-to-seg (p a b / ab len2 tt)
  (setq ab (akd:v- b a) len2 (akd:dot ab ab))
  (setq tt (if (> len2 0.0) (max 0.0 (min 1.0 (/ (akd:dot (akd:v- p a) ab) len2))) 0.0))
  (distance (list (car p) (cadr p)) (list (+ (car a) (* (car ab) tt)) (+ (cadr a) (* (cadr ab) tt)))))

;; T when faces fa and fb share a HIP edge that contains p.
;; Course join rule: HIP joins; RIDGE and VALLEY never join (different
;; construction conditions) - each face's course terminates there.
(defun akd:rf-course-link-p (roof fa fb p / ok)
  (foreach e (akd:fp roof "EDGES")
    (if (and (not ok) (/= fa fb)
             (= (akd:fp e "TYPE") "HIP")
             (member fa (akd:fp e "FACES"))
             (member fb (akd:fp e "FACES"))
             (< (akd:rf-dist-to-seg p (akd:fp e "P0") (akd:fp e "P1")) (akd:cfg "TOL")))
      (setq ok T)))
  ok)

;; Chain one course's segments ((p1 p2 face) ...) into ordered vertex lists.
;; Two pieces join only if they share an endpoint AND their faces are linked
;; at that point by a shared hip. Any number of pieces per chain; one course
;; may give several chains on complex roofs.
;; Returns ((closed-flag pts) ...); closed only when the chain truly returns.
(defun akd:rf-chain-course (roof segs / tol chains s pts fh ft found rest q1 q2 fc)
  (setq tol (akd:cfg "TOL"))
  (while segs
    (setq s (car segs) segs (cdr segs)
          pts (list (car s) (cadr s)) fh (caddr s) ft fh found T)
    (while found
      (setq found nil rest nil)
      (foreach c segs
        (setq q1 (car c) q2 (cadr c) fc (caddr c))
        (cond
          (found (setq rest (cons c rest)))
          ((and (equal q1 (last pts) tol) (akd:rf-course-link-p roof ft fc q1))
           (setq pts (append pts (list q2)) ft fc found T))
          ((and (equal q2 (last pts) tol) (akd:rf-course-link-p roof ft fc q2))
           (setq pts (append pts (list q1)) ft fc found T))
          ((and (equal q1 (car pts) tol) (akd:rf-course-link-p roof fh fc q1))
           (setq pts (cons q2 pts) fh fc found T))
          ((and (equal q2 (car pts) tol) (akd:rf-course-link-p roof fh fc q2))
           (setq pts (cons q1 pts) fh fc found T))
          (T (setq rest (cons c rest)))))
      (setq segs (reverse rest)))
    (setq chains
      (cons (if (and (> (length pts) 3) (equal (car pts) (last pts) tol)
                     (akd:rf-course-link-p roof fh ft (car pts)))
              (list T (reverse (cdr (reverse pts))))
              (list nil pts))
            chains)))
  (reverse chains))

;; Batten members (BATTEN p1 p2 course face) -> one LWPOLYLINE per chain.
(defun akd:rf-make-batten-courses (members layer / courses grp n)
  (foreach m members
    (setq grp (assoc (nth 3 m) courses))
    (if grp
      (setq courses (subst (append grp (list (list (nth 1 m) (nth 2 m) (nth 4 m)))) grp courses))
      (setq courses (cons (list (nth 3 m) (list (nth 1 m) (nth 2 m) (nth 4 m))) courses))))
  (setq n 0)
  (foreach grp (reverse courses)
    (foreach ch (akd:rf-chain-course roof (cdr grp))
      (if (akd:rf-make-poly (cadr ch) (akd:fp fp "ELEV") layer (car ch))
        (setq n (1+ n)))))
  n)

(defun akd:rf-undo-begin ()
  (if (not *AKD-RF-UNDO-OPEN*)
    (progn (command "_.UNDO" "_BEGIN") (setq *AKD-RF-UNDO-OPEN* T))))

(defun akd:rf-undo-end ()
  (if *AKD-RF-UNDO-OPEN*
    (progn
      (setq *AKD-RF-UNDO-OPEN* nil)
      (if command-s (command-s "_.UNDO" "_END") (command "_.UNDO" "_END")))))

(defun akd:rf-yes-p (msg / ans)
  (initget "Yes No")
  (setq ans (getkword (strcat "\n" msg " [Yes/No] <Yes>: ")))
  (/= ans "No"))

;;; ==========================================================================
;;; STAGES  (use dynamic variables fp / session from c:RF)
;;; ==========================================================================
(defun akd:rf-stage-roof (rtype / state segs col vsegs)
  (setq col (akd:cfg "COLOR_ROOF"))
  (cond
    ((= rtype "Hip")
     ;; Solve first; nothing is drawn or created unless the topology is clean.
     (if (null (setq roof (akd:rf-build-roof fp rtype nil)))
       (progn
         (princ (strcat "\nUnable to solve this roof footprint"
                        (if *AKD-RF-SKL-REASON* (strcat " (" *AKD-RF-SKL-REASON* ").") ".")))
         (exit)))
     (setq segs  (akd:fp roof "LINES")
           vsegs (akd:rf-member-segs (akd:rf-edge-members roof "VALLEY" "VALLEY") nil))
     (akd:ghost-pick
       (strcat "\nEqual-pitch hip roof preview: " (akd:rf-topology-summary roof)
               ". Click or Enter to accept, Esc to cancel.")
       'HIP
       '(lambda (p s) s)
       '(lambda (s) (akd:ghost-poly (akd:fp fp "PTS") (akd:cfg "GHOST_PERIM") 0)
                    (akd:ghost-segs segs (akd:cfg "COLOR_ROOF"))
                    (akd:ghost-segs vsegs (akd:cfg "GHOST_VALLEY")))))
    ((= rtype "Gable")
     (setq state
       (akd:ghost-pick "\nMove cursor to choose ridge direction (ridge points toward cursor). Click to accept."
         'U
         '(lambda (p s) (akd:rf-axis-from-cursor fp p s))
         '(lambda (s) (akd:ghost-poly (akd:fp fp "PTS") (akd:cfg "GHOST_PERIM") 0)
                      (akd:ghost-segs (akd:rf-solve-gable fp s) (akd:cfg "COLOR_ROOF")))))
     (setq roof (akd:rf-build-roof fp rtype state)
           segs (akd:fp roof "LINES")))
    (T
     (akd:ghost-pick "\nFlat roof: footprint is the roof perimeter. Click or Enter to accept, Esc to cancel."
       'FLAT
       '(lambda (p s) s)
       '(lambda (s) (akd:ghost-poly (akd:fp fp "PTS") (akd:cfg "COLOR_ROOF") 1)))
     (setq roof (akd:rf-build-roof fp rtype nil))))
  (akd:rf-undo-begin)
  (akd:rf-ensure-layer (akd:cfg "LAYER_ROOF") col)
  (akd:rf-make-lines segs (akd:cfg "LAYER_ROOF"))
  (setq session (append session (list (cons "TYPE" rtype) (cons "ROOF-AXIS" state)
                                      (cons "ROOF" roof))))
  (princ (strcat "\n" rtype " roof created"
                 (if (= rtype "Hip") (strcat ": " (akd:rf-topology-summary roof)) "")
                 (if segs (strcat " (" (itoa (length segs)) " lines).") "."))))

;; Spacing prompt with validation. Enter keeps cur.
(defun akd:rf-get-spacing (label cur / v ok)
  (while (not ok)
    (initget 6)
    (setq v (getdist (strcat "\n" label " spacing <" (akd:num-str cur) ">: ")))
    (cond ((null v) (setq v cur ok T))
          ((< v (akd:cfg "MIN_SPACING"))
           (princ (strcat "\n" label " spacing must be greater than or equal to "
                          (akd:num-str (akd:cfg "MIN_SPACING")) " mm.")))
          (T (setq ok T))))
  v)

(defun akd:rf-spec-text (spec)
  (strcat (akd:num-str (nth 1 spec)) " x " (akd:num-str (nth 2 spec))))

;; One-line spacing summary: SPECIFICATION vs SOLVED LAYOUT.
(defun akd:rf-spacing-info (name sp / spec mod)
  (setq spec (akd:rf-spec name))
  (if (= name "RAFTER")
    (progn
      (setq mod (akd:rf-rafter-module roof sp))
      (strcat "RAFTERS " (akd:rf-spec-text spec)
              "  Requested: max " (akd:num-str sp) " c/c"
              "  Actual: " (akd:num-str (car mod)) " c/c"
              (if (cadr mod) (strcat " (" (itoa (cadr mod)) " bays)") "")))
    (strcat "BATTENS " (akd:rf-spec-text spec)
            "  Spacing: " (akd:num-str sp) " c/c from ridge/top")))

;; Preview + commit one framing member set.
;;   name  : spec key ("RAFTER" "HIP_RAFTER" "VALLEY_RAFTER" "RIDGE_BEAM" "BATTEN")
;;   genfn : (lambda (state) -> members)
;;   state : requested spacing for spaced members (S adjusts it in preview),
;;           nil for hip rafters / valley rafters / ridge beams.
;; Orientation comes from the roof topology, so there is no cursor choice.
(defun akd:rf-stage-members (name label genfn state / spec cache-ok cache-key mems segs outs n)
  (setq spec (akd:rf-spec name))
  (setq state
    (akd:ghost-pick-keys
      '(lambda (s)
         (strcat "\n" (itoa (length (akd:fp roof "FACES"))) " roof face(s): "
                 (if s (akd:rf-spacing-info name s) (strcase label))
                 (if s "\nClick/Enter to accept or [Spacing]: "
                       "\nClick/Enter to accept, Esc to cancel.")))
      state
      '(lambda (p s) s)
      '(lambda (s)
         (if (not (and cache-ok (equal s cache-key)))
           (setq cache-ok T cache-key s
                 mems (apply genfn (list s))
                 outs (akd:rf-display-outlines roof name mems)
                 segs (akd:rf-member-segs mems spec)))
         (foreach f (akd:fp roof "FACES")
           (akd:ghost-poly (akd:fp f "POLY") (akd:cfg "GHOST_PERIM") 0))
         ;; Preview shows actual plan width where the member type has it.
         (if outs
           (foreach o outs (akd:ghost-poly o (akd:spec-color spec) 0))
           (akd:ghost-segs segs (akd:spec-color spec))))
      (if state
        '(lambda (s)
           (akd:rf-set-spacing name
             (akd:rf-get-spacing (strcat (strcase (substr label 1 1)) (substr label 2)) s))))))
  (if (not (and cache-ok (equal state cache-key)))
    (setq mems (apply genfn (list state))
          outs (akd:rf-display-outlines roof name mems)
          segs (akd:rf-member-segs mems spec)))
  (akd:rf-undo-begin)
  (akd:rf-ensure-layer (akd:spec-layer spec) (akd:spec-color spec))
  ;; Battens commit as joined course polylines; width members commit as closed
  ;; plan outlines; anything else stays a centreline LINE.
  (setq n (cond ((= name "BATTEN") (akd:rf-make-batten-courses mems (akd:spec-layer spec)))
                (outs (akd:rf-make-outlines outs (akd:fp fp "ELEV") (akd:spec-layer spec)))
                (T (akd:rf-make-lines segs (akd:spec-layer spec)))))
  (if (and outs (= (akd:cfg "DEBUG_CENTRELINES") "On"))
    (akd:rf-make-lines segs (akd:spec-layer spec)))
  (setq session (append session (list (cons name (list (cons "REQUESTED" state) (cons "SEGS" segs))))))
  (princ (strcat "\n" (itoa n) " " label
                 (cond (outs (strcat " outline(s) created at "
                                     (akd:num-str (akd:rf-member-width name)) " wide (Offset mode)."))
                       ((= name "BATTEN")
                        (strcat " course polyline(s) created from " (itoa (length segs)) " segments."))
                       (T "(s) created."))
                 (if state (strcat " " (akd:rf-spacing-info name state)) ""))))

(defun akd:rf-member-segs (members spec / segs)
  (foreach m members (setq segs (append segs (akd:rf-member-geometry m spec))))
  segs)

(defun akd:rf-make-outlines (outs elev layer / cnt)
  (setq cnt 0)
  (foreach pts outs
    (if (akd:rf-make-poly pts elev layer T) (setq cnt (1+ cnt))))
  cnt)

;; V1 fascia is a single line on the perimeter. Extension point: inside/outside
;; offset by FASCIA_WIDTH will use the cursor state here.
(defun akd:rf-stage-fascia ()
  (akd:ghost-pick "\nFascia follows the roof perimeter. Click or Enter to accept, Esc to cancel."
    'PERIM
    '(lambda (p s) s)
    '(lambda (s) (akd:ghost-poly (akd:fp fp "PTS") (akd:cfg "COLOR_FASCIA") 1)))
  (akd:rf-undo-begin)
  (akd:rf-ensure-layer (akd:cfg "LAYER_FASCIA") (akd:cfg "COLOR_FASCIA"))
  (akd:rf-make-poly (akd:fp fp "PTS") (akd:fp fp "ELEV") (akd:cfg "LAYER_FASCIA") T)
  (setq session (append session
                  (list (cons "FASCIA" (list (cons "SEGS" (akd:rf-poly-edges (akd:fp fp "PTS"))))))))
  (princ "\nFascia created."))

(defun akd:rf-poly-edges (pts / prev out)
  (setq prev (last pts))
  (foreach p pts (setq out (cons (list prev p) out) prev p))
  (reverse out))

;;; ==========================================================================
;;; CALLOUTS  (one per member type created; kept out of the framing solver)
;;; Leader = native LEADER command with command-line MTEXT annotation
;;; (no MLEADER entmake / ActiveX needed; editable on AutoCAD for Mac).
;;; ==========================================================================
(setq *AKD-RF-CALLOUT-ORDER*
  '(("RAFTER" . "RAFTER") ("HIP_RAFTER" . "HIP RAFTER") ("VALLEY_RAFTER" . "VALLEY RAFTER")
    ("RIDGE_BEAM" . "RIDGE BEAM")
    ("BATTEN" . "BATTEN") ("FASCIA" . "FASCIA")))

;; Specification text, from the current settings of this run.
;; Rafters are evenly redistributed, so the spacing is labelled as MAX.
(defun akd:rf-callout-text (name rec / spec wd sp)
  (setq spec (akd:rf-spec name)
        wd   (strcat (akd:num-str (nth 1 spec)) "x" (akd:num-str (nth 2 spec)))
        sp   (akd:fp rec "REQUESTED"))
  (cond
    ((= name "RAFTER")     (strcat wd " TIMBER RAFTERS @ " (akd:num-str sp) " C/C MAX"))
    ((= name "BATTEN")     (strcat wd " TIMBER BATTENS @ " (akd:num-str sp) " C/C"))
    ((= name "HIP_RAFTER") (strcat wd " HIP RAFTER"))
    ((= name "VALLEY_RAFTER") (strcat wd " VALLEY RAFTER"))
    ((= name "RIDGE_BEAM") (strcat wd " RIDGE BEAM"))
    (T                     (strcat wd " FASCIA"))))

;; Representative member: the segment whose midpoint is nearest the roof datum
;; (central common rafter / inner batten course / long fascia edge).
;; Returns (seg . leader-target-point).
(defun akd:rf-callout-target (segs / c best bd mid d)
  (setq c (akd:fp roof "DATUM"))
  (foreach sg segs
    (setq mid (akd:v* (akd:v+ (car sg) (cadr sg)) 0.5) d (distance mid c))
    (if (or (null bd) (< d (- bd 1e-6))) (setq bd d best (cons sg mid))))
  best)

;; Snap the target->cursor direction to CALLOUT_ANGLE_INCREMENT (WCS angles,
;; independent of roof axes and of POLARANG / SNAPANG / ORTHOMODE), keep the
;; cursor distance, then add a horizontal landing on the text side.
;; Returns WCS leader vertices (target elbow landing-end), or nil at zero length.
(defun akd:rf-callout-geometry (tgt cur / dx dy d inc a elbow side)
  (setq dx (- (car cur) (car tgt)) dy (- (cadr cur) (cadr tgt))
        d  (sqrt (+ (* dx dx) (* dy dy))))
  (if (> d (akd:cfg "MIN_SEG_LEN"))
    (progn
      (setq inc (* pi (/ (akd:cfg "CALLOUT_ANGLE_INCREMENT") 180.0))
            a   (atan dy dx))
      (if (< a 0.0) (setq a (+ a (* 2.0 pi))))
      (setq a     (* inc (fix (+ (/ a inc) 0.5)))
            elbow (list (+ (car tgt) (* d (cos a))) (+ (cadr tgt) (* d (sin a))) (caddr tgt))
            side  (if (< (cos a) -1e-9) -1.0 1.0))   ; vertical leaders land right
      (list tgt elbow
            (list (+ (car elbow) (* side (akd:cfg "CALLOUT_LANDING_LENGTH"))) (cadr elbow) (caddr tgt))))))

(defun akd:rf-ghost-xor-path (pts / prev)
  (setq prev (car pts))
  (foreach p (cdr pts)
    (grdraw (trans prev 0 1) (trans p 0 1) -1 1)
    (setq prev p)))

;; Leader target fixed, cursor chooses the snapped text point. XOR ghost of
;; the SNAPPED leader + landing, screen only. The click commits exactly the
;; last previewed geometry, so there is no jump.
;; Returns leader vertex list (WCS), or nil for Skip (S / Enter / Space / right-click).
(defun akd:rf-callout-preview (msg tgt / gr code done shown res geo)
  (princ msg)
  (redraw)
  (akd:ghost-segs (list (car tgt)) (akd:cfg "GHOST_CALLOUT"))
  (while (not done)
    (setq gr (grread T 15 0) code (car gr))
    (cond
      ((= code 5)
       (setq geo (akd:rf-callout-geometry (cdr tgt) (trans (cadr gr) 1 0)))
       (if (not (equal geo shown 1e-9))
         (progn
           (if shown (akd:rf-ghost-xor-path shown))
           (if geo (akd:rf-ghost-xor-path geo))
           (setq shown geo))))
      ((= code 3)
       (setq res (cond (shown)
                       ((akd:rf-callout-geometry (cdr tgt) (trans (cadr gr) 1 0))))
             done (if res T)))
      ((and (= code 2) (= (cadr gr) 27)) (redraw) (exit))
      ((or (member code '(11 25))
           (and (= code 2) (member (cadr gr) '(13 32 83 115))))
       (setq done T))))
  (redraw)
  res)

;; THE callout backend abstraction: RF solves target / 15-degree leg / elbow /
;; landing / text side, and this is the only place that turns that into
;; entities. Swapping LEADER+MTEXT for one native MLEADER means changing this
;; function (and the record's ENTS list) only. Native MLEADER was probed on
;; AutoCAD for Mac and is DEFERRED (annotation work is frozen) - see the
;; MLEADER section of ROOF_TESTS.md for the findings.
;; Native LEADER: vertices, Enter (end points -> annotation), text line,
;; Enter (end annotation). MTEXT side follows the landing direction.
;; Returns the list of entities the command actually created (LEADER + its
;; annotation), found by walking entnext from the previous entlast, so no
;; assumption is made about LEADER internals.
(defun akd:rf-callout-create (path txt / ds before en ents)
  (akd:rf-undo-begin)
  (akd:rf-ensure-layer (akd:cfg "LAYER_ANNO") (akd:cfg "COLOR_ANNO"))
  (akd:rf-sv-set "CLAYER" (akd:cfg "LAYER_ANNO"))
  (akd:rf-sv-set "OSMODE" 0)
  (setq ds (getvar "DIMSCALE"))
  (if (<= ds 0.0) (setq ds 1.0))
  (akd:rf-sv-set "DIMTXT" (/ (akd:cfg "CALLOUT_TEXT_HEIGHT") ds))
  (setq before (entlast))
  (command "_.LEADER")
  (foreach p path (command (trans p 0 1)))
  (command "" txt "")
  (setq en (if before (entnext before) (entnext)))
  (while en (setq ents (cons en ents) en (entnext en)))
  (reverse ents))

;;; Callout record (session only, no XData):
;;;   "ID" "NAME" "TEXT" "PATH" (target elbow landing-end, WCS) "JUST" "ENTS"
;;; Target = PATH[0] is fixed forever; matching only rewrites elbow/landing/text.
(defun akd:rf-callout-place (name label / rec tgt txt path ents)
  (setq rec (cdr (assoc name session))
        tgt (akd:rf-callout-target (akd:fp rec "SEGS"))
        txt (akd:rf-callout-text name rec))
  (if tgt
    (progn
      (setq path (akd:rf-callout-preview
                   (strcat "\n" txt "\nPlace " label " callout or [Skip] <Skip>: ") tgt))
      (if (and path (setq ents (akd:rf-callout-create path txt)))
        (progn
          (setq callouts (append callouts
                           (list (list (cons "ID" (1+ (length callouts))) (cons "NAME" name)
                                       (cons "TEXT" txt) (cons "PATH" path)
                                       (cons "JUST" (akd:rf-callout-just path)) (cons "ENTS" ents)))))
          (princ (strcat "\nCallout: " txt)))))))

;;; ---- Callout matching ----------------------------------------------------
;;; Matched callout group: selected RF callouts share one MATCH DATUM, a WCS
;;; vertical line forming the common LEFT or RIGHT text edge.
;;;   Left  : landing runs rightward and ends on the datum; LEADER attaches
;;;           the text left-justified, starting at the datum.
;;;   Right : landing runs leftward and ends on the datum; text is
;;;           right-justified, ending at the datum.
;;; Member targets never move. Each leg is re-solved on the angle grid, and
;;; landing length is free (>= CALLOUT_MIN_LANDING_LENGTH).

(defun akd:rf-ask-no-p (msg / ans)
  (initget "Yes No")
  (setq ans (getkword (strcat "\n" msg " [Yes/No] <No>: ")))
  (= ans "Yes"))

(defun akd:rf-unhilite ()
  (foreach e *AKD-RF-HILITE* (if (entget e) (redraw e 4)))
  (setq *AKD-RF-HILITE* nil))

(defun akd:rf-callout-of (en / hit)
  (foreach r callouts (if (and (not hit) (member en (akd:fp r "ENTS"))) (setq hit r)))
  hit)

;; Text side of a callout path: "Left" = landing runs right (text starts at the
;; landing end), "Right" = landing runs left (text ends at the landing end).
(defun akd:rf-callout-just (path)
  (if (< (car (caddr path)) (car (cadr path))) "Right" "Left"))

;; Returns the distinct RF callout records in the selection.
(defun akd:rf-select-callouts (/ ss i en rec recs ignored)
  (princ "\nSelect callouts to match: ")
  (setq ss (ssget) i 0 ignored 0)
  (if ss
    (repeat (sslength ss)
      (setq en (ssname ss i) rec (akd:rf-callout-of en))
      (cond ((null rec) (setq ignored (1+ ignored)))
            ((not (member rec recs)) (setq recs (cons rec recs))))
      (setq i (1+ i))))
  (princ (strcat "\n" (itoa (length recs)) " RF callout(s) selected."
                 (if (> ignored 0) (strcat " " (itoa ignored) " other object(s) ignored.") "")))
  (reverse recs))

;; Solve one callout for a match datum (WCS X). Right mode is solved in an
;; X-mirrored frame so one routine serves both.
;; In that frame the elbow must satisfy X <= datum - min landing. For every
;; allowed leg angle the leg length is clamped to its valid range, choosing
;; the length that puts the elbow nearest the callout's previous Y.
;; Best = smallest Y shift, then closest to the previous leg direction.
;; Returns new (target elbow landing-end), or nil when no clean solution
;; exists within CALLOUT_MATCH_MAX_SHIFT. ywant (equal spacing) replaces the
;; previous Y with an assigned row Y that must be met exactly.
(defun akd:rf-match-path (rec just datumx ywant / path tg el m z tx ty y0 lim minseg inc a0
                              k a c sn lo hi ok s ey d best bc ba ex)
  (setq path   (akd:fp rec "PATH")
        tg     (car path)
        el     (cadr path)
        m      (if (= just "Right") -1.0 1.0)
        z      (caddr tg)
        tx     (* m (car tg))
        ty     (cadr tg)
        y0     (if ywant ywant (cadr el))
        lim    (- (* m datumx) (akd:cfg "CALLOUT_MIN_LANDING_LENGTH"))
        minseg (akd:cfg "MIN_SEG_LEN")
        inc    (* pi (/ (akd:cfg "CALLOUT_ANGLE_INCREMENT") 180.0))
        a0     (if (equal el tg 1e-9) 0.0 (atan (- (cadr el) ty) (- (* m (car el)) tx)))
        k      0)
  (if (< a0 0.0) (setq a0 (+ a0 (* 2.0 pi))))
  (repeat (fix (+ (/ (* 2.0 pi) inc) 0.5))
    (setq a  (* k inc) c (cos a) sn (sin a)
          lo minseg hi nil ok T)
    (cond ((> c 1e-9)  (setq hi (/ (- lim tx) c)))
          ((< c -1e-9) (setq lo (max lo (/ (- tx lim) (- c)))))
          ((> tx lim)  (setq ok nil)))
    (if (and ok hi (< hi lo)) (setq ok nil))
    (if ok
      (progn
        (setq s (cond ((> (abs sn) 1e-9) (/ (- y0 ty) sn))
                      (hi hi)
                      (T lo)))
        (if (< s lo) (setq s lo))
        (if (and hi (> s hi)) (setq s hi))
        (setq ey (+ ty (* s sn))
              ex (+ tx (* s c))
              bc (if bc bc 1e99)
              d  (abs (- a a0)))
        (if (> d pi) (setq d (- (* 2.0 pi) d)))
        (if (or (null best)
                (< (abs (- ey y0)) (- bc 1.0))
                (and (<= (abs (- (abs (- ey y0)) bc)) 1.0) (< d ba)))
          (setq best (list ex ey) bc (abs (- ey y0)) ba d))))
    (setq k (1+ k)))
  ;; Equal-spacing rows must be hit exactly; otherwise the Y shift is capped.
  (if (and best (<= bc (if ywant 1.0 (akd:cfg "CALLOUT_MATCH_MAX_SHIFT"))))
    (list tg
          (list (* m (car best)) (cadr best) z)
          (list datumx (cadr best) z))))

;; All-or-nothing solve for a group; nil if any callout has no clean solution.
;; rows = assigned text Y per callout (equal spacing) or nil.
(defun akd:rf-match-solve (group just datumx rows / out bad p i)
  (setq i 0)
  (foreach r group
    (if (setq p (akd:rf-match-path r just datumx (if rows (nth i rows))))
      (setq out (cons p out))
      (setq bad T))
    (setq i (1+ i)))
  (if (not bad) (reverse out)))

;; Group sorted top -> bottom by current text (elbow) Y; drawing order kept.
(defun akd:rf-callout-y (r) (cadr (cadr (akd:fp r "PATH"))))

(defun akd:rf-insert-by-y (r lst)
  (cond ((null lst) (list r))
        ((> (akd:rf-callout-y r) (akd:rf-callout-y (car lst))) (cons r lst))
        (T (cons (car lst) (akd:rf-insert-by-y r (cdr lst))))))

(defun akd:rf-sort-callouts-by-y (group / out)
  (foreach r group (setq out (akd:rf-insert-by-y r out)))
  out)

;; Equal spacing: rows keep the group's top and bottom text Y. If that pitch is
;; below CALLOUT_MIN_ROW_FACTOR x text height, the group expands about its
;; middle so text never overlaps.
(defun akd:rf-equal-rows (group / ys n top bot step mn mid k)
  (setq ys  (mapcar '(lambda (r) (cadr (cadr (akd:fp r "PATH")))) group)
        n   (length ys)
        top (car ys)
        bot (last ys)
        step (if (> n 1) (/ (- top bot) (1- n)) 0.0)
        mn  (* (akd:cfg "CALLOUT_MIN_ROW_FACTOR") (akd:cfg "CALLOUT_TEXT_HEIGHT")))
  (if (and (> n 1) (< step mn))
    (setq mid (/ (+ top bot) 2.0) step mn top (+ mid (/ (* step (1- n)) 2.0))))
  (setq k -1)
  (mapcar '(lambda (r) (setq k (1+ k)) (- top (* k step))) group))

;; Screen-only ghost primitives: the solved callouts, an estimated text box
;; on the text side of the datum, and the vertical match-datum guide.
(defun akd:rf-match-ghost (group paths just datumx / h m ylo yhi out i x1 y)
  (setq h (akd:cfg "CALLOUT_TEXT_HEIGHT") m (if (= just "Right") -1.0 1.0) i 0)
  (foreach p paths
    (setq y   (cadr (caddr p))
          x1  (+ datumx (* m (* 0.8 h (strlen (akd:fp (nth i group) "TEXT")))))
          ylo (if ylo (min ylo y) y)
          yhi (if yhi (max yhi y) y)
          out (cons p out)
          out (cons (list (list datumx (+ y (* 0.5 h)) 0.0) (list x1 (+ y (* 0.5 h)) 0.0)
                          (list x1 (+ y (* 1.5 h)) 0.0) (list datumx (+ y (* 1.5 h)) 0.0))
                    out)
          i   (1+ i)))
  (if paths
    (setq out (cons (list (list datumx (- ylo (* 2.0 h)) 0.0) (list datumx (+ yhi (* 3.0 h)) 0.0)) out)))
  out)

;; Ghost the matched group; click commits exactly the ghosted paths.
;; Enter / right-click abandons this round without changes.
(defun akd:rf-match-group (group just rows / gr code done ghost paths new-paths i ents new)
  (setq *AKD-RF-HILITE* nil)
  (foreach r group (setq *AKD-RF-HILITE* (append *AKD-RF-HILITE* (akd:fp r "ENTS"))))
  (foreach e *AKD-RF-HILITE* (redraw e 3))
  (princ "\nSpecify callout match position: ")
  (while (not done)
    (setq gr (grread T 15 0) code (car gr))
    (cond
      ((= code 5)
       (setq new-paths (akd:rf-match-solve group just (car (trans (cadr gr) 1 0)) rows))
       (if (not (equal new-paths paths 1e-9))
         (progn
           (foreach p ghost (akd:rf-ghost-xor-path p))
           (setq paths new-paths
                 ghost (akd:rf-match-ghost group paths just (car (trans (cadr gr) 1 0))))
           (foreach p ghost (akd:rf-ghost-xor-path p)))))
      ((= code 3)
       (if paths
         (setq done 'COMMIT)
         (princ (if rows "\nUnable to create a clean matched group at this position."
                         "\nUnable to create a clean matched callout at this position."))))
      ((and (= code 2) (= (cadr gr) 27)) (akd:rf-unhilite) (redraw) (exit))
      ((or (member code '(11 25)) (and (= code 2) (member (cadr gr) '(13 32))))
       (setq done 'ABANDON))))
  (akd:rf-unhilite)
  (redraw)
  (if (eq done 'COMMIT)
    (progn
      (setq i 0)
      (foreach rec group
        ;; Build the replacement first; erase the old callout only if that worked.
        (if (setq ents (akd:rf-callout-create (nth i paths) (akd:fp rec "TEXT")))
          (progn
            (foreach e (akd:fp rec "ENTS") (if (entget e) (entdel e)))
            (setq new (subst (cons "ENTS" ents) (assoc "ENTS" rec)
                        (subst (cons "JUST" just) (assoc "JUST" rec)
                          (subst (cons "PATH" (nth i paths)) (assoc "PATH" rec) rec)))
                  callouts (subst new rec callouts))))
        (setq i (1+ i)))
      (princ (strcat "\n" (itoa (length group)) " callout(s) matched, "
                     (strcase just T) " text alignment" (if rows ", equal spacing." "."))))
    (princ "\nCallout match abandoned.")))

(defun akd:rf-match-round (/ group done just rows)
  (while (not done)
    (setq group (akd:rf-select-callouts))
    (if (>= (length group) 2)
      (setq done T)
      (progn
        (princ "\nAt least 2 RF callouts are required.")
        (initget "Reselect Cancel")
        (if (= (getkword "\nMatch group [Reselect/Cancel] <Reselect>: ") "Cancel")
          (setq done T group nil)))))
  (if group
    (progn
      (initget "Left Right")
      (setq just (cond ((getkword "\nText alignment [Left/Right] <Left>: ")) ("Left")))
      (setq group (akd:rf-sort-callouts-by-y group))
      (if (akd:rf-yes-p "Equal spacing?")
        (setq rows (akd:rf-equal-rows group)))
      (akd:rf-match-group group just rows))))

(defun akd:rf-match-stage ()
  (if (and (>= (length callouts) 2) (akd:rf-yes-p "Match callouts?"))
    (progn
      (akd:rf-match-round)
      (while (akd:rf-ask-no-p "Match another callout group?")
        (akd:rf-match-round)))))

;; RF > Settings. Command-line menu; add future settings as keywords here.
(defun akd:rf-settings (/ opt v)
  (while (progn
           (initget "Callouts Framing Exit")
           (setq opt (getkword (strcat "\nRF SETTINGS  Callouts=" *AKD-RF-CALLOUTS*
                                       "  Framing=" *AKD-RF-FRAMING-MODE*
                                       "  [Callouts/Framing/Exit] <Exit>: ")))
           (member opt '("Callouts" "Framing")))
    (if (= opt "Framing")
      (akd:rf-ask-framing-mode)
      (progn
        (initget "On Off")
        (setq v (getkword (strcat "\nCallouts [On/Off] <" *AKD-RF-CALLOUTS* ">: ")))
        (if v (setq *AKD-RF-CALLOUTS* v))))))

;;; ==========================================================================
;;; RF COMMAND
;;; ==========================================================================
(defun c:RF (/ *error* fp roof session callouts ent res rtype members)
  (setq *AKD-RF-UNDO-OPEN* nil
        *AKD-RF-SV* nil
        *AKD-RF-HILITE* nil)
  ;; Allows (command) from *error* (AutoCAD refuses it otherwise).
  (if *push-error-using-command* (*push-error-using-command*))

  (defun *error* (msg)
    (akd:rf-unhilite)
    (redraw)
    (akd:rf-undo-end)
    (akd:rf-sv-restore)
    (if (not (member (strcase msg T) '("function cancelled" "quit / exit abort" "console break")))
      (princ (strcat "\nRF error: " msg)))
    (princ "\nRF cancelled. Committed stages remain (one Undo removes them).")
    (princ))

  (akd:rf-sv-set "CMDECHO" 0)

  ;; INPUT  (S = Settings here only)
  (while (not fp)
    (setvar "ERRNO" 0)
    (initget "Settings")
    (setq ent (entsel "\nSelect closed roof boundary polyline or [Settings]: "))
    (cond
      ((= ent "Settings") (akd:rf-settings))
      ((null ent)
       (if (= (getvar "ERRNO") 7) (princ "\nNothing selected.") (exit)))
      (T
       (setq res (akd:rf-analyze-footprint (car ent)))
       (if (eq (car res) 'OK)
         (progn
           (setq fp (cadr res))
           (princ (strcat "\nFootprint: " (itoa (length (akd:fp fp "PTS"))) " vertices, "
                          (if (member T (akd:fp fp "REFLEX"))
                            (strcat "concave (" (itoa (akd:rf-count-true (akd:fp fp "REFLEX"))) " reflex corner(s))")
                            "convex")
                          (if (= (akd:fp fp "SHAPE") "RECT") ", rectangle." "."))))
         (princ (strcat "\n" (cadr res)))))))
  (setq session (list (cons "FOOTPRINT" fp)))

  ;; ROOF  (Gable only for rectangles in this version)
  (while (null rtype)
    (initget "Hip Gable Flat")
    (setq rtype (cond ((getkword (strcat "\nRoof type [Hip/Gable/Flat] <"
                                        (if (and (= *AKD-RF-LASTTYPE* "Gable") (/= (akd:fp fp "SHAPE") "RECT"))
                                          "Hip" *AKD-RF-LASTTYPE*)
                                        ">: ")))
                      ((and (= *AKD-RF-LASTTYPE* "Gable") (/= (akd:fp fp "SHAPE") "RECT")) "Hip")
                      (*AKD-RF-LASTTYPE*)))
    (if (and (= rtype "Gable") (/= (akd:fp fp "SHAPE") "RECT"))
      (progn
        (princ "\nIrregular Gable roofs are not supported yet. Use Hip or Flat.")
        (setq rtype nil))))
  (setq *AKD-RF-LASTTYPE* rtype)
  (akd:rf-stage-roof rtype)

  ;; FRAMING MODE: asked once; applies to rafters, hip/valley rafters and
  ;; ridge beams for this whole run (battens and fascia are unaffected).
  (akd:rf-ask-framing-mode)

  ;; RAFTERS (common + jack, coordinated on shared ridge / hip stations)
  (if (akd:rf-yes-p "Add rafters?")
    (akd:rf-stage-members "RAFTER" "rafter"
      '(lambda (sp) (akd:rf-roof-members roof "RAFTER" sp))
      (akd:rf-set-spacing "RAFTER"
        (akd:rf-get-spacing "Rafter" (akd:rf-cur-spacing "RAFTER")))))

  ;; HIP RAFTERS (only when the roof has hips)
  (if (and (setq members (akd:rf-edge-members roof "HIP" "HIP"))
           (akd:rf-yes-p "Add hip rafters?"))
    (akd:rf-stage-members "HIP_RAFTER" "hip rafter" '(lambda (s) members) nil))

  ;; VALLEY RAFTERS (only when the roof has valleys)
  (if (and (setq members (akd:rf-edge-members roof "VALLEY" "VALLEY"))
           (akd:rf-yes-p "Add valley rafters?"))
    (akd:rf-stage-members "VALLEY_RAFTER" "valley rafter" '(lambda (s) members) nil))

  ;; RIDGE BEAMS (0..N: one per non-zero ridge; none on a pyramid)
  (if (and (setq members (akd:rf-edge-members roof "RIDGE" "RIDGE"))
           (akd:rf-yes-p "Add ridge beam?"))
    (akd:rf-stage-members "RIDGE_BEAM" "ridge beam" '(lambda (s) members) nil))

  ;; BATTENS (per face, top-down datum, requested spacing)
  (if (akd:rf-yes-p "Add battens?")
    (akd:rf-stage-members "BATTEN" "batten"
      '(lambda (sp) (akd:rf-roof-members roof "BATTEN" sp))
      (akd:rf-set-spacing "BATTEN"
        (akd:rf-get-spacing "Batten" (akd:rf-cur-spacing "BATTEN")))))

  ;; FASCIA
  (if (akd:rf-yes-p "Add fascia?")
    (akd:rf-stage-fascia))

  ;; CALLOUTS (one per member type actually created, inside the undo group)
  (if (= *AKD-RF-CALLOUTS* "On")
    (foreach pr *AKD-RF-CALLOUT-ORDER*
      (if (assoc (car pr) session)
        (akd:rf-callout-place (car pr) (cdr pr)))))

  ;; CALLOUT MATCHING (rounds; only callouts of this run; same undo group)
  (akd:rf-match-stage)

  ;; OUTPUT / CLEANUP
  (redraw)
  (akd:rf-undo-end)
  (akd:rf-sv-restore)
  (princ "\nRF complete.")
  (princ))

(princ "\nAKD Roof V1 loaded. Command: RF")
(princ)
