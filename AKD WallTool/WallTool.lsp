;;; WallTool.lsp -- AKD WallTool v0.1.0 (Stage 1: 2D Wall Core)
;;; Commands: AX (axis), ZXW (grid axis), WW (wall), XW (axis to wall), EW (erase wall), WWF (wall from wall), WWD (wall to distance), WWE (wall connect), TW (connection repair), TX (line junction cleanup), WWR (wall/axis repair)
;;; Plain AutoLISP + DCL only (no VL/VLA/VLAX, no XData). AutoCAD Mac + Windows.
;;;
;;; Model:  MASTER AXIS -> THICKNESS + POSITION -> THEORETICAL STRIP
;;;         -> TOPOLOGY (nodes: L/T/X/collinear) -> UNION OUTLINE -> A-WALL LINEs

;;; ===================================================================
;;; 0. Globals, tolerances, debug
;;; ===================================================================

(setq *wt-debug*        nil)    ; T = print topology diagnostics
(setq *wt:tol*          1e-3)   ; point equality / on-segment distance (drawing units)
(setq *wt:tol-par*      1e-6)   ; |cross| of unit vectors below this = parallel
(setq *wt:tol-side*     1e-2)   ; probe offset used by the outline visibility test
(setq *wt:miter-limit*  10.0)   ; miter point farther than this x max thickness -> square end
(setq *wt:recon-max*    1000.0) ; max face offset searched when reconstructing old walls

(defun wt:dbg (lst)
  (if *wt-debug*
    (progn (princ "\n[WT]") (foreach x lst (princ " ") (princ x))))
  nil)

;;; ===================================================================
;;; 1. Generic utilities
;;; ===================================================================

(defun wt:take (l n / r) (repeat n (setq r (cons (car l) r) l (cdr l))) (reverse r))
(defun wt:drop (l n) (repeat n (setq l (cdr l))) l)

(defun wt:merge (a b less / out)
  (while (and a b)
    (if (apply less (list (car b) (car a)))
      (setq out (cons (car b) out) b (cdr b))
      (setq out (cons (car a) out) a (cdr a))))
  (append (reverse out) a b))

(defun wt:sort (lst less / h)   ; stable merge sort
  (if (cdr lst)
    (progn
      (setq h (/ (length lst) 2))
      (wt:merge (wt:sort (wt:take lst h) less) (wt:sort (wt:drop lst h) less) less))
    lst))

(defun wt:setnth (l i v / k)
  (setq k -1)
  (mapcar '(lambda (x) (if (= (setq k (1+ k)) i) v x)) l))

(defun wt:strpos (s ch / i r)
  (setq i 1)
  (while (and (not r) (<= i (strlen s)))
    (if (= (substr s i 1) ch) (setq r i))
    (setq i (1+ i)))
  r)

(defun wt:trim (s)
  (while (and (> (strlen s) 0) (member (substr s 1 1) '(" " "\t")))
    (setq s (substr s 2)))
  (while (and (> (strlen s) 0) (member (substr s (strlen s) 1) '(" " "\t")))
    (setq s (substr s 1 (1- (strlen s)))))
  s)

(defun wt:split (s ch / out i)
  (while (setq i (wt:strpos s ch))
    (setq out (cons (substr s 1 (1- i)) out) s (substr s (1+ i))))
  (reverse (cons s out)))

;; "3000, 4000,3000" -> (3000.0 4000.0 3000.0); nil if any item is not a positive number
(defun wt:numlist (s / out ok v)
  (setq ok t)
  (foreach x (wt:split s ",")
    (if (and (setq v (distof (wt:trim x) 2)) (> v 0))
      (setq out (cons v out))
      (setq ok nil)))
  (if ok (reverse out)))

;; 150.0 -> "150", 12.5 -> "12.5"
(defun wt:fmt (x / s)
  (setq s (rtos x 2 4))
  (if (wt:strpos s ".")
    (progn
      (while (= (substr s (strlen s) 1) "0") (setq s (substr s 1 (1- (strlen s)))))
      (if (= (substr s (strlen s) 1) ".") (setq s (substr s 1 (1- (strlen s)))))))
  s)

;;; ===================================================================
;;; 2. Vector math (2D points as (x y))
;;; ===================================================================

(defun wt:pt2 (p) (list (float (car p)) (float (cadr p))))
(defun wt:3d (p) (list (car p) (cadr p) 0.0))
(defun wt:v+ (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b))))
(defun wt:v- (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun wt:v* (a s) (list (* (car a) s) (* (cadr a) s)))
(defun wt:dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun wt:cross (a b) (- (* (car a) (cadr b)) (* (cadr a) (car b))))
(defun wt:len (a) (sqrt (wt:dot a a)))
(defun wt:dist (a b) (wt:len (wt:v- b a)))
(defun wt:perp (a) (list (- (cadr a)) (car a)))          ; left normal
(defun wt:ang (a) (atan (cadr a) (car a)))
(defun wt:unit (a / l)
  (setq l (wt:len a))
  (if (> l 1e-12) (wt:v* a (/ 1.0 l)) (list 0.0 0.0)))

(defun wt:peq (a b) (< (wt:dist a b) *wt:tol*))
(defun wt:par (u v) (< (abs (wt:cross u v)) *wt:tol-par*))  ; u v unit

;; intersection of infinite lines p+su and q+sv (u v unit); nil if parallel
(defun wt:xline (p u q v / d)
  (setq d (wt:cross u v))
  (if (> (abs d) *wt:tol-par*)
    (wt:v+ p (wt:v* u (/ (wt:cross (wt:v- q p) v) d)))))

;; p lies on closed segment a-b (within tolerance)
(defun wt:on-seg (p a b / u s)
  (setq u (wt:unit (wt:v- b a)) s (wt:dot (wt:v- p a) u))
  (and (< (abs (wt:cross u (wt:v- p a))) *wt:tol*)
       (> s (- *wt:tol*))
       (< s (+ (wt:dist a b) *wt:tol*))))

;; p lies strictly inside segment a-b (not near either end)
(defun wt:in-seg (p a b / u s)
  (setq u (wt:unit (wt:v- b a)) s (wt:dot (wt:v- p a) u))
  (and (< (abs (wt:cross u (wt:v- p a))) *wt:tol*)
       (> s *wt:tol*)
       (< s (- (wt:dist a b) *wt:tol*))))

;; closed segments a-b and c-d share at least one point
(defun wt:seg-touch (a b c d / x)
  (or (wt:on-seg a c d) (wt:on-seg b c d) (wt:on-seg c a b) (wt:on-seg d a b)
      (and (setq x (wt:xline a (wt:unit (wt:v- b a)) c (wt:unit (wt:v- d c))))
           (wt:on-seg x a b) (wt:on-seg x c d))))

;; segment r=(a b) contained in some segment of segs
(defun wt:seg-covered (r segs / hit)
  (while (and segs (not hit))
    (if (and (wt:on-seg (car r) (car (car segs)) (cadr (car segs)))
             (wt:on-seg (cadr r) (car (car segs)) (cadr (car segs))))
      (setq hit t))
    (setq segs (cdr segs)))
  hit)

;;; ===================================================================
;;; 3. Configuration (WallTool.txt, KEY=VALUE, # comments, [sections] ignored)
;;; ===================================================================

(setq *wt:cfg-defaults*
  (list
    (cons "DEFAULT_THICKNESS" 150.0)
    (cons "THICKNESSES" '(75.0 100.0 125.0 150.0 200.0 250.0 300.0))
    (cons "DEFAULT_POSITION" "CENTER")
    (cons "AXIS_LAYER" "X-AXIS") (cons "AXIS_COLOR" 1) (cons "AXIS_LINETYPE" "DASHDOT") (cons "AXIS_PLOT" 0)
    (cons "GRID_LAYER" "X-GRID") (cons "GRID_COLOR" 8) (cons "GRID_LINETYPE" "CONTINUOUS") (cons "GRID_PLOT" 1)
    (cons "WALL_LAYER" "A-WALL") (cons "WALL_COLOR" 7) (cons "WALL_LINETYPE" "CONTINUOUS") (cons "WALL_PLOT" 1)
    (cons "BUBBLE_DIAMETER" 800.0) (cons "BUBBLE_OFFSET" 500.0) (cons "TEXT_HEIGHT" 350.0)
    (cons "DEFAULT_OFFSET" 1500.0) (cons "TW_CONNECT_DISTANCE" 150.0)
    (cons "TX_CONNECT_DISTANCE" 150.0) (cons "TX_WALL_MAX" 600.0)
    (cons "WWE_CORNER_DISTANCE" 300.0)))

(setq *wt:cfg* nil)

(defun wt:cfg (key) (cdr (assoc key *wt:cfg*)))

;; raw string -> validated value, or nil
(defun wt:cfg-parse (key raw def / v)
  (cond
    ((= key "DEFAULT_POSITION")
     (if (member (setq v (strcase raw)) '("CENTER" "LEFT" "RIGHT")) v))
    ((wcmatch key "*_PLOT")
     (cond ((member (strcase raw) '("YES" "Y" "1" "TRUE")) 1)
           ((member (strcase raw) '("NO" "N" "0" "FALSE")) 0)))
    ((= (type def) 'INT)
     (if (and (setq v (distof raw 2)) (= v (fix v)) (<= 1 v 255)) (fix v)))
    ((= (type def) 'REAL)
     (if (and (setq v (distof raw 2)) (> v 0)) v))
    ((listp def) (wt:numlist raw))
    (t (if (snvalid raw) raw))))

;; WallTool.txt is the standard name; legacy WallTool.cfg still accepted
(defun wt:cfg-path () (cond ((findfile "WallTool.txt")) ((findfile "WallTool.cfg"))))

(defun wt:cfg-load (/ path f line i raw)
  (if (setq path (wt:cfg-path))
    (progn
      (setq f (open path "r"))
      (while (setq line (read-line f))
        (setq line (wt:trim line))
        (if (and (> (strlen line) 0)
                 (not (member (substr line 1 1) '("#" ";" "[")))
                 (setq i (wt:strpos line "=")))
          (setq raw (cons (cons (strcase (wt:trim (substr line 1 (1- i))))
                                (wt:trim (substr line (1+ i))))
                          raw))))
      (close f)
      (foreach r raw
        (if (not (assoc (car r) *wt:cfg-defaults*))
          (princ (strcat "\nWarning: Unknown setting " (car r) " ignored.")))))
    (princ "\nWallTool.txt not found. Using default settings."))
  (setq *wt:cfg*
    (mapcar
      '(lambda (d / r v)
         (cond
           ((not (setq r (assoc (car d) raw))) d)
           ((setq v (wt:cfg-parse (car d) (cdr r) (cdr d))) (cons (car d) v))
           (t (princ (strcat "\nWarning: Invalid " (car d) ". Using default: "
                             (wt:cfg-str (cdr d)) "."))
              d)))
      *wt:cfg-defaults*))
  path)

(defun wt:cfg-str (v)
  (cond ((= (type v) 'STR) v)
        ((= (type v) 'INT) (itoa v))
        ((numberp v) (wt:fmt v))
        ((listp v) (if v (apply 'strcat (cons (wt:fmt (car v))
                                              (mapcar '(lambda (x) (strcat "," (wt:fmt x))) (cdr v)))) ""))
        (t "")))

;; session state: config loaded once, current thickness/position remembered
(defun wt:init ()
  (if (not *wt:cfg*) (wt:cfg-load))
  (if (not *wt:thk*) (setq *wt:thk* (wt:cfg "DEFAULT_THICKNESS")))
  (if (not *wt:pos*) (setq *wt:pos* (wt:cfg "DEFAULT_POSITION"))))

;;; ===================================================================
;;; 4. Layers, linetypes, entity creation
;;; ===================================================================

(defun wt:ltype (lt / file)
  (cond
    ((tblsearch "LTYPE" lt) lt)
    ((and (setq file (cond ((findfile "acadiso.lin")) ((findfile "acad.lin"))))
          (progn (command-s "_.-LINETYPE" "_Load" lt file "") (tblsearch "LTYPE" lt)))
     lt)
    (t (princ (strcat "\nWarning: Linetype " lt " unavailable. Using Continuous."))
       "Continuous")))

;; pfx = "AXIS" | "GRID" | "WALL". Creates the layer from config if missing;
;; an existing layer keeps the user's properties. Returns the layer name.
(defun wt:layer (pfx / name)
  (setq name (wt:cfg (strcat pfx "_LAYER")))
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0)
                   (cons 62 (wt:cfg (strcat pfx "_COLOR")))
                   (cons 6 (wt:ltype (wt:cfg (strcat pfx "_LINETYPE"))))
                   (cons 290 (wt:cfg (strcat pfx "_PLOT"))))))
  name)

;; entities carry no color/linetype/lineweight codes -> BYLAYER
(defun wt:mk-line (a b lyr)
  (if (entmake (list '(0 . "LINE") (cons 8 lyr) (cons 10 (wt:3d a)) (cons 11 (wt:3d b))))
    (entlast)))

(defun wt:mk-circle (c r lyr)
  (entmake (list '(0 . "CIRCLE") (cons 8 lyr) (cons 10 (wt:3d c)) (cons 40 r))))

(defun wt:mk-text (c h s lyr)
  (entmake (list '(0 . "TEXT") (cons 8 lyr) (cons 10 (wt:3d c)) (cons 11 (wt:3d c))
                 (cons 40 h) (cons 1 s) '(72 . 1) '(73 . 2))))

;;; ===================================================================
;;; 5. Command frame: undo group, CMDECHO, error handler
;;; ===================================================================

(defun wt:begin ()
  (setq *wt:cmdecho* (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command-s "_.UNDO" "_BEgin")
  (setq *wt:open* t)
  (wt:init))

(defun wt:end ()
  (if *wt:open*
    (progn (command-s "_.UNDO" "_End")
           (setvar "CMDECHO" *wt:cmdecho*)
           (setq *wt:open* nil)))
  (princ))

(defun wt:error (msg)
  (if *wt:pending* (wt:seg-undo *wt:pending*))
  (setq *wt:pending* nil *wt:tx-field* nil *wt:tx-nested* nil *wt:op-sus* nil *wt:op-amb* nil *wt:op-cache* nil)
  (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*"))
    (princ (strcat "\nError: " msg)))
  (wt:end))

;; UCS-aware point input. Returns WCS (x y), keyword string, or nil on Enter.
(defun wt:getpt (base msg kw / p)
  (if kw (initget kw))
  (setq p (if base (getpoint (trans (wt:3d base) 0 1) msg) (getpoint msg)))
  (cond ((= (type p) 'STR) p)
        (p (wt:pt2 (trans p 1 0)))))

;;; ===================================================================
;;; 6. AX -- draw axis
;;; ===================================================================

(defun c:AX (/ *error* lyr p q segs)
  (setq *error* wt:error)
  (wt:begin)
  (setq lyr (wt:layer "AXIS"))
  (if (setq p (wt:getpt nil "\nSpecify first point: " nil))
    (while (setq q (wt:getpt p (if segs "\nSpecify next point or [Undo]: " "\nSpecify next point: ")
                             (if segs "Undo")))
      (cond
        ((= q "Undo")
         (entdel (car (car segs)))
         (setq p (cadr (car segs)) segs (cdr segs)))
        ((wt:peq p q) (princ "\nZero-length segment ignored."))
        (t (setq segs (cons (list (wt:mk-line p q lyr) p) segs) p q)))))
  (wt:end))

;;; ===================================================================
;;; 7. ZXW -- grid axes (topology) + bubbles (annotation, replaceable)
;;; ===================================================================

;; 1 -> "A", 26 -> "Z", 27 -> "AA"
(defun wt:grid-alpha (n / s)
  (setq s "")
  (while (> n 0)
    (setq n (1- n) s (strcat (chr (+ 65 (rem n 26))) s) n (/ n 26)))
  s)

(defun wt:grid-cumulative (spacings / acc out)
  (setq acc 0.0 out (list 0.0))
  (foreach s spacings (setq acc (+ acc s) out (cons acc out)))
  (reverse out))

;; Pure grid topology. Returns list of (p1 p2 label), p1/p2 are axis ends
;; in grid-local coordinates. Numbers run along X, letters along Y.
(defun wt:grid-axes (xspacings yspacings ext / xs ys w h i out)
  (setq xs (wt:grid-cumulative xspacings) ys (wt:grid-cumulative yspacings)
        w (last xs) h (last ys) i 0)
  (foreach x xs
    (setq out (cons (list (list x (- ext)) (list x (+ h ext)) (itoa (setq i (1+ i)))) out)))
  (setq i 0)
  (foreach y ys
    (setq out (cons (list (list (- ext) y) (list (+ w ext) y) (wt:grid-alpha (setq i (1+ i)))) out)))
  (reverse out))

;; Annotation style: circle tangent to the axis end + centered label.
(defun wt:grid-bubble (end dir label lyr / r c)
  (setq r (/ (wt:cfg "BUBBLE_DIAMETER") 2.0)
        c (wt:v+ end (wt:v* dir r)))
  (wt:mk-circle c r lyr)
  (wt:mk-text c (wt:cfg "TEXT_HEIGHT") label lyr))

(defun wt:get-spacings (msg / s l done)
  (while (not done)
    (setq s (wt:trim (getstring t msg)))
    (cond ((= s "") (setq done t l nil))
          ((setq l (wt:numlist s)) (setq done t))
          (t (princ "\nInvalid spacing. Enter positive numbers separated by commas, e.g. 3000,4000,3000."))))
  l)

(defun c:ZXW (/ *error* lyr sx sy ins u)
  (setq *error* wt:error)
  (wt:begin)
  (setq lyr (wt:layer "GRID"))
  (if (and (setq sx (wt:get-spacings "\nEnter horizontal grid spacings (numbered axes, left to right): "))
           (setq sy (wt:get-spacings "\nEnter vertical grid spacings (lettered axes, bottom to top): "))
           (setq ins (wt:getpt nil "\nSpecify grid insertion point (axis 1 / A): " nil)))
    (foreach ax (wt:grid-axes sx sy (wt:cfg "BUBBLE_OFFSET"))
      (setq u (wt:unit (wt:v- (cadr ax) (car ax))))
      (wt:mk-line (wt:v+ ins (car ax)) (wt:v+ ins (cadr ax)) lyr)
      (wt:grid-bubble (wt:v+ ins (car ax)) (wt:v* u -1.0) (caddr ax) lyr)
      (wt:grid-bubble (wt:v+ ins (cadr ax)) u (caddr ax) lyr)))
  (wt:end))

;;; ===================================================================
;;; 8. Wall representation
;;; wall  = (ename p1 p2 thickness position). Persistent walls are always "CENTER"
;;;         (master = geometric centerline); the solver still accepts LEFT/RIGHT records.
;;; strip = (L0 R0 L1 R1)  face end points; L/R relative to p1->p2
;;; ===================================================================

(defun wt:w-p1 (w) (cadr w))
(defun wt:w-p2 (w) (caddr w))
(defun wt:w-thk (w) (cadddr w))
(defun wt:w-pos (w) (nth 4 w))
(defun wt:w-u (w) (wt:unit (wt:v- (wt:w-p2 w) (wt:w-p1 w))))

;; signed offsets (left right) of the two faces along the left normal
;; LEFT  = master is the right face, wall extends left
;; RIGHT = master is the left face, wall extends right
(defun wt:w-offs (w / th)
  (setq th (wt:w-thk w))
  (cond ((= (wt:w-pos w) "LEFT") (list th 0.0))
        ((= (wt:w-pos w) "RIGHT") (list 0.0 (- th)))
        (t (list (/ th 2.0) (/ th -2.0)))))

(defun wt:strip-default (w / n o)
  (setq n (wt:perp (wt:w-u w)) o (wt:w-offs w))
  (list (wt:v+ (wt:w-p1 w) (wt:v* n (car o))) (wt:v+ (wt:w-p1 w) (wt:v* n (cadr o)))
        (wt:v+ (wt:w-p2 w) (wt:v* n (car o))) (wt:v+ (wt:w-p2 w) (wt:v* n (cadr o)))))

(defun wt:strip-set-end (strips wi side lp rp / s)
  (setq s (nth wi strips))
  (wt:setnth strips wi (if (= side 0) (list lp rp (caddr s) (cadddr s))
                                      (list (car s) (cadr s) lp rp))))

(defun wt:strip-poly (s) (list (car s) (caddr s) (cadddr s) (cadr s)))  ; L0 L1 R1 R0

;;; ===================================================================
;;; 9. Topology: discover nodes on master axes, classify, solve wall ends
;;; end = (wall-index side point)   side 0 = p1, 1 = p2
;;; ===================================================================

;; T host: a non-parallel wall whose master interior contains the end point
(defun wt:topo-host (e walls / j r w)
  (setq j 0)
  (foreach w walls
    (if (and (not r) (/= j (car e))
             (wt:in-seg (caddr e) (wt:w-p1 w) (wt:w-p2 w))
             (not (wt:par (wt:w-u w) (wt:w-u (nth (car e) walls)))))
      (setq r j))
    (setq j (1+ j)))
  r)

;; T junction: extend the terminating wall's faces to the host's far face
(defun wt:topo-tee (strips e walls hi / w h hu o ho far fp u n base lp rp lim)
  (setq w (nth (car e) walls) h (nth hi walls)
        hu (wt:w-u h) ho (wt:w-offs h)
        u (wt:w-u w) n (wt:perp u) o (wt:w-offs w) base (caddr e)
        ;; far face = host face on the side away from the terminating wall body
        far (if (> (wt:cross hu (wt:v- (if (= (cadr e) 0) (wt:w-p2 w) (wt:w-p1 w)) (wt:w-p1 h))) 0)
              (cadr ho) (car ho))
        fp (wt:v+ (wt:w-p1 h) (wt:v* (wt:perp hu) far))
        lp (wt:xline (wt:v+ base (wt:v* n (car o))) u fp hu)
        rp (wt:xline (wt:v+ base (wt:v* n (cadr o))) u fp hu)
        lim (* *wt:miter-limit* (max (wt:w-thk w) (wt:w-thk h))))
  (wt:dbg (list "T end" e "host" hi "far" far "L" lp "R" rp))
  (if (and lp rp (< (wt:dist lp base) lim) (< (wt:dist rp base) lim))
    (wt:strip-set-end strips (car e) (cadr e) lp rp)
    strips))

;; L / collinear / multi-way node where 2+ wall ends meet.
;; Ends sorted by outward angle; each angular gap is closed by intersecting
;; the left face of one end with the right face of the next. Parallel or
;; runaway intersections fall back to the square end (bevel filled by hub).
;; Returns (strips hub), hub = polygon closing the node core.
(defun wt:topo-node (strips grp walls / node recs n lim pairs i ri rj lp rp p hub)
  (setq node (caddr (car grp)))
  ;; rec = (end dir left-off right-off), offsets along left normal of outward dir
  (setq recs
    (mapcar
      '(lambda (e / w u o)
         (setq w (nth (car e) walls) u (wt:w-u w) o (wt:w-offs w))
         (if (= (cadr e) 0)
           (list e u (car o) (cadr o))
           (list e (wt:v* u -1.0) (- (cadr o)) (- (car o)))))
      grp))
  (setq recs (wt:sort recs '(lambda (a b) (< (wt:ang (cadr a)) (wt:ang (cadr b)))))
        n (length recs)
        lim (* *wt:miter-limit* (apply 'max (mapcar '(lambda (e) (wt:w-thk (nth (car e) walls))) grp))))
  ;; pairs[i] = (left point of i . right point of i+1)
  (setq i 0)
  (repeat n
    (setq ri (nth i recs) rj (nth (rem (1+ i) n) recs)
          lp (wt:v+ node (wt:v* (wt:perp (cadr ri)) (caddr ri)))
          rp (wt:v+ node (wt:v* (wt:perp (cadr rj)) (cadddr rj)))
          p (wt:xline lp (cadr ri) rp (cadr rj)))
    (if (and p (< (wt:dist p node) lim))
      (setq lp p rp p))
    (setq pairs (cons (list lp rp) pairs) i (1+ i)))
  (setq pairs (reverse pairs) i 0)
  (foreach r recs
    (setq lp (car (nth i pairs))
          rp (cadr (nth (rem (+ i n -1) n) pairs))
          hub (append hub (list rp lp)))
    (wt:dbg (list "node" node "end" (car r) "dir" (cadr r) "L" lp "R" rp))
    (setq strips
      (if (= (cadr (car r)) 0)
        (wt:strip-set-end strips (car (car r)) 0 lp rp)
        (wt:strip-set-end strips (car (car r)) 1 rp lp)))   ; outward left = original right
    (setq i (1+ i)))
  (list strips hub))

;; Solve all ends. Returns (strips hubs), hubs = list of (polygon wall-indices).
(defun wt:topo-solve (walls / strips ends i used hi grp r hubs)
  (setq strips (mapcar 'wt:strip-default walls) i 0)
  (foreach w walls
    (setq ends (cons (list i 1 (wt:w-p2 w)) (cons (list i 0 (wt:w-p1 w)) ends)) i (1+ i)))
  (setq ends (reverse ends))
  (foreach e ends
    (if (not (member (list (car e) (cadr e)) used))
      (cond
        ((setq hi (wt:topo-host e walls))
         (setq used (cons (list (car e) (cadr e)) used)
               strips (wt:topo-tee strips e walls hi)))
        (t
         (setq grp nil)
         (foreach f ends
           (if (and (not (member (list (car f) (cadr f)) used)) (wt:peq (caddr e) (caddr f)))
             (setq grp (cons f grp))))
         (foreach f grp (setq used (cons (list (car f) (cadr f)) used)))
         (if (cdr grp)
           (progn
             (wt:dbg (list "node" (caddr e) (if (cddr grp) "MULTI" "L/COLLINEAR") (length grp)))
             (setq r (wt:topo-node strips (reverse grp) walls)
                   strips (car r)
                   hubs (cons (list (cadr r) (mapcar 'car grp)) hubs)))
           (wt:dbg (list "free end" e)))))))
  (list strips hubs))

;;; ===================================================================
;;; 10. Outline: visible linework = boundary of the union of solved strips
;;; ===================================================================

(defun wt:pip (p poly / c a)   ; even-odd point in polygon
  (setq a (last poly))
  (foreach b poly
    (if (and (not (eq (> (cadr a) (cadr p)) (> (cadr b) (cadr p))))
             (< (car p) (+ (car a) (/ (* (- (cadr p) (cadr a)) (- (car b) (car a)))
                                      (- (cadr b) (cadr a))))))
      (setq c (not c)))
    (setq a b))
  c)

(defun wt:inside-any (p polys / r)
  (while (and polys (not r)) (setq r (wt:pip p (car polys)) polys (cdr polys)))
  r)

(defun wt:poly-edges (poly / out a)
  (setq a (last poly))
  (foreach b poly
    (if (> (wt:dist a b) *wt:tol*) (setq out (cons (list a b) out)))
    (setq a b))
  (reverse out))

;; sorted distinct split distances along edge a-b
(defun wt:edge-cuts (a b cutters / u l ts x out)
  (setq l (wt:dist a b) u (wt:unit (wt:v- b a)) ts (list 0.0 l))
  (foreach c cutters
    (foreach q c
      (if (wt:on-seg q a b) (setq ts (cons (wt:dot (wt:v- q a) u) ts))))
    (if (and (setq x (wt:xline a u (car c) (wt:unit (wt:v- (cadr c) (car c)))))
             (wt:on-seg x a b) (wt:on-seg x (car c) (cadr c)))
      (setq ts (cons (wt:dot (wt:v- x a) u) ts))))
  (foreach s (wt:sort (mapcar '(lambda (s) (max 0.0 (min l s))) ts) '<)
    (if (or (not out) (> (- s (car out)) *wt:tol*)) (setq out (cons s out))))
  (reverse out))

(defun wt:solid-p (p polys voids) (and (wt:inside-any p polys) (not (wt:inside-any p voids))))

;; visible runs of an edge: a piece is visible when exactly one side is solid (in the union, not in a void)
(defun wt:edge-visible (a b polys voids cutters / u nn ts s0 m run out)
  (setq u (wt:unit (wt:v- b a)) nn (wt:v* (wt:perp u) *wt:tol-side*)
        ts (wt:edge-cuts a b cutters) s0 (car ts))
  (foreach s1 (cdr ts)
    (setq m (wt:v+ a (wt:v* u (/ (+ s0 s1) 2.0))))
    (if (not (eq (wt:solid-p (wt:v+ m nn) polys voids) (wt:solid-p (wt:v- m nn) polys voids)))
      (setq run (list (if run (car run) s0) s1))
      (if run (setq out (cons run out) run nil)))
    (setq s0 s1))
  (if run (setq out (cons run out)))
  (mapcar '(lambda (r) (list (wt:v+ a (wt:v* u (car r))) (wt:v+ a (wt:v* u (cadr r))))) out))

;; walls: all participating walls; regen: indices whose linework is emitted.
;; Returns list of segments (a b).
;;; --- Openings: registered architectural voids (doors/windows owned by other tools) ---
;;; WallTool reads no XData and knows no other tool. Tools register provider SYMBOLS
;;; (idempotently, in any load order) in these lists:
;;;   *wt:opening-fns*          (fn)          -> ((mid dir width id) ...) for the current space;
;;;                                              mid on the wall centerline, dir along the wall,
;;;                                              width along the wall, id opaque (may be nil)
;;;   *wt:wall-moved-fns*       (fn ids vec)  move those openings by vec (a WWD wall move)
;;;   *wt:opening-removed-fns*  (fn ids)      delete those openings (their wall is gone)
;;;   *wt:erase-fns*            (fn ids)      EW: erase the objects of that tool among the
;;;                                           selected enames; returns the enames it consumed
;;; Event functions return their drawing changes as (created erased modified-old-data)
;;; (or nil) so WallTool's own rollback covers them; the AutoCAD undo group covers the rest.
;;; Association is geometric and recomputed every time (survives splits, joins, Undo):
;;; an opening belongs to a wall when exactly one logical wall (collinear spans sharing
;;; one band) contains its midpoint, the whole opening lies inside that wall and no other
;;; wall body reaches it. None -> ignored. Several / junction / past the wall end ->
;;; AMBIG: not preserved, reported. Malformed records are skipped; a provider that
;;; raises an error aborts the command through wt:error (transaction rolled back).

(setq *wt:op-tol* 0.01)
(setq *wt:op-cache* nil *wt:op-sus* nil *wt:op-amb* nil *wt:op-net* nil *wt:op-skip* nil *wt:op-extra* nil)

(defun wt:num-p (x) (member (type x) '(INT REAL)))
(defun wt:pt-p (p)
  (and (= (type p) 'LIST) (wt:num-p (car p)) (= (type (cdr p)) 'LIST) (wt:num-p (cadr p))))

(defun wt:op-valid-p (r)
  (and (= (type r) 'LIST) (wt:pt-p (car r))
       (= (type (cdr r)) 'LIST) (wt:pt-p (cadr r))
       (> (wt:len (wt:pt2 (cadr r))) 1e-9)
       (= (type (cddr r)) 'LIST) (wt:num-p (caddr r)) (> (caddr r) *wt:tol*)
       (or (not (cdddr r)) (= (type (cdddr r)) 'LIST))))

;; internal opening = (mid2 unit-dir width id)
(defun wt:op-norm (r)
  (list (wt:pt2 (car r)) (wt:unit (wt:pt2 (cadr r))) (float (caddr r)) (if (cdddr r) (cadddr r))))

;; registered, currently defined hook functions (bad entries and duplicates skipped)
(defun wt:hook-fns (lst / f out)
  (while (and (= (type lst) 'LIST) lst)
    (setq f (car lst) lst (cdr lst))
    (if (and (= (type f) 'SYM) (not (member f out))
             (member (type (eval f)) '(SUBR USUBR EXRXSUBR)))
      (setq out (cons f out))))
  (reverse out))

;; all valid openings; collected once per transaction (cache dropped by events)
(defun wt:openings (/ res x out)
  (if (and *wt:pending* *wt:op-cache*)
    (cdr *wt:op-cache*)
    (progn
      (foreach f (wt:hook-fns *wt:opening-fns*)
        (setq res (apply f nil))
        (while (and (= (type res) 'LIST) res)
          (setq x (car res) res (cdr res))
          (if (wt:op-valid-p x) (setq out (cons (wt:op-norm x) out)))))
      (setq out (reverse out))
      (if *wt:pending* (setq *wt:op-cache* (cons t out)))
      out)))

;; openings within reach of the walls' bounding box
(defun wt:op-near (walls ops / x0 y0 x1 y1 d out)
  (foreach w walls
    (foreach p (list (wt:w-p1 w) (wt:w-p2 w))
      (setq x0 (if x0 (min x0 (car p)) (car p)) x1 (if x1 (max x1 (car p)) (car p))
            y0 (if y0 (min y0 (cadr p)) (cadr p)) y1 (if y1 (max y1 (cadr p)) (cadr p)))))
  (if x0
    (foreach op ops
      (setq d (+ *wt:recon-max* (caddr op)))
      (if (and (<= (- x0 d) (car (car op)) (+ x1 d)) (<= (- y0 d) (cadr (car op)) (+ y1 d)))
        (setq out (cons op out)))))
  (reverse out))

;; faces of w as sorted signed offsets from mid along n; station span of w along u
(defun wt:op-band (w mid n / s b o)
  (setq s (wt:dot (wt:perp (wt:w-u w)) n) b (wt:dot (wt:v- (wt:w-p1 w) mid) n) o (wt:w-offs w))
  (list (min (+ b (* s (car o))) (+ b (* s (cadr o)))) (max (+ b (* s (car o))) (+ b (* s (cadr o))))))
(defun wt:op-span (w mid u / a b)
  (setq a (wt:dot (wt:v- (wt:w-p1 w) mid) u) b (wt:dot (wt:v- (wt:w-p2 w) mid) u))
  (list (min a b) (max a b)))

;; candidate walls for op: `walls` plus drawing masters within reach (*wt:op-net*,
;; bound by the caller), minus *wt:op-skip* (masters being removed)
(defun wt:op-pool (op walls / w out)
  (setq out walls)
  (foreach m (car *wt:op-net*)
    (if (and (not (assoc (car m) out)) (not (member (car m) *wt:op-skip*))
             (not (wt:peq (cadr m) (caddr m)))
             (<= (wt:seg-dist (car op) (cadr m) (caddr m)) (+ (/ (caddr op) 2.0) *wt:recon-max*))
             (setq w (wt:wall-from-master m (cadr *wt:op-net*))))
      (setq out (cons w out))))
  out)

;; -> ("OK" void-rect walls) | ("AMBIG" nil walls) | ("NONE")
(defun wt:op-assoc (op pool / mid u n hw cands b s ref lo hi more c half sn fp x r)
  (setq mid (car op) u (cadr op) n (wt:perp u) hw (/ (caddr op) 2.0))
  (foreach w pool
    (if (and (wt:par u (wt:w-u w))
             (setq b (wt:op-band w mid n))
             (< (car b) (- *wt:op-tol*)) (> (cadr b) *wt:op-tol*)
             (setq s (wt:op-span w mid u))
             (<= (car s) *wt:tol*) (>= (cadr s) (- *wt:tol*)))
      (setq cands (cons w cands) ref (if ref ref b))))
  (foreach w cands (if (not (equal (wt:op-band w mid n) ref *wt:op-tol*)) (setq r t)))
  (cond
    ((not cands) (list "NONE"))
    (r (list "AMBIG" nil cands))                          ; several different walls
    (t
     ;; extent of the logical wall: chained collinear spans with the same band
     (setq lo 0.0 hi 0.0 more t)
     (foreach w cands (setq s (wt:op-span w mid u) lo (min lo (car s)) hi (max hi (cadr s))))
     (while more
       (setq more nil)
       (foreach w pool
         (if (and (wt:par u (wt:w-u w))
                  (equal (wt:op-band w mid n) ref *wt:op-tol*)
                  (setq s (wt:op-span w mid u))
                  (<= (car s) (+ hi *wt:tol*)) (>= (cadr s) (- lo *wt:tol*))
                  (or (< (car s) (- lo *wt:tol*)) (> (cadr s) (+ hi *wt:tol*))))
           (setq lo (min lo (car s)) hi (max hi (cadr s)) more t))))
     (setq c (/ (+ (car ref) (cadr ref)) 2.0) half (/ (- (cadr ref) (car ref)) 2.0))
     ;; a non-parallel wall whose centerline reaches this band and whose body
     ;; overlaps the opening interval (L/T/X at the opening)
     (foreach w pool
       (if (and (not r) (not (wt:par u (wt:w-u w)))
                (setq x (wt:xline (wt:v+ mid (wt:v* n c)) u (wt:w-p1 w) (wt:w-u w))))
         (progn
           (setq sn (abs (wt:cross u (wt:w-u w)))
                 fp (/ (+ (/ (wt:w-thk w) 2.0) (* half (abs (wt:dot u (wt:w-u w))))) sn))
           (if (and (<= (wt:seg-dist x (wt:w-p1 w) (wt:w-p2 w)) (+ (/ half sn) *wt:tol*))
                    (< (abs (wt:dot (wt:v- x mid) u)) (- (+ hw fp) *wt:op-tol*)))
             (setq r t)))))
     (cond
       ((or r (> lo (- (+ hw *wt:op-tol*))) (< hi (+ hw *wt:op-tol*))) (list "AMBIG" nil cands))
       (t
        (list "OK"
              (mapcar '(lambda (a o) (wt:v+ mid (wt:v+ (wt:v* u a) (wt:v* n o))))
                      (list (- hw) hw hw (- hw))
                      (list (- (car ref) 1.0) (- (car ref) 1.0) (+ (cadr ref) 1.0) (+ (cadr ref) 1.0)))
              cands))))))

(defun wt:op-note (op) (if (not (member op *wt:op-amb*)) (setq *wt:op-amb* (cons op *wt:op-amb*))))
(defun wt:op-suspect (op)
  (if (and (cadddr op) (not (member (cadddr op) *wt:op-sus*))) (setq *wt:op-sus* (cons (cadddr op) *wt:op-sus*))))

;; -> ((void-rect wall-indices op) ...) for the openings of `walls`
(defun wt:opening-voids (walls / r i idx out)
  (if walls
    (foreach op (wt:op-near walls (wt:openings))
      (setq r (wt:op-assoc op (wt:op-pool op walls)))
      (cond
        ((= (car r) "OK")
         (setq i 0 idx nil)
         (foreach w walls (if (member w (caddr r)) (setq idx (cons i idx))) (setq i (1+ i)))
         (setq out (cons (list (cadr r) idx op) out)))
        ((and (= (car r) "AMBIG") (wt:any-member walls (caddr r))) (wt:op-note op)))))
  out)

;; void rects of openings (e.g. no longer registered) held by one of `walls`: their old jambs get erased
(defun wt:op-rects (ops walls / r out)
  (foreach op ops
    (if (and (= (car (setq r (wt:op-assoc op (wt:op-pool op walls)))) "OK") (wt:any-member walls (caddr r)))
      (setq out (cons (cadr r) out))))
  out)

;; A-WALL line f lies on an edge of one of the void rects (a jamb)
(defun wt:void-cap-p (f rects / r)
  (foreach v rects
    (foreach e (wt:poly-edges v)
      (if (and (wt:on-seg (cadr f) (car e) (cadr e)) (wt:on-seg (caddr f) (car e) (cadr e))) (setq r t))))
  r)

;; segment p-q is a jamb of a registered opening (perpendicular at +/- width/2, centred)
(defun wt:op-jamb-p (p q / r sa sb)
  (foreach op (wt:openings)
    (setq sa (wt:dot (wt:v- p (car op)) (cadr op)) sb (wt:dot (wt:v- q (car op)) (cadr op)))
    (if (and (< (abs (- sa sb)) *wt:op-tol*)
             (< (abs (- (abs sa) (/ (caddr op) 2.0))) *wt:op-tol*)
             (< (abs (+ (wt:dot (wt:v- p (car op)) (wt:perp (cadr op))) (wt:dot (wt:v- q (car op)) (wt:perp (cadr op))))) *wt:op-tol*))
      (setq r t)))
  r)

;; ids of openings lying wholly on wall w (WWD moves them with it)
(defun wt:op-ids-on (w / *wt:op-net* r s hw out)
  (setq *wt:op-net* (wt:net-scan))
  (foreach op (wt:op-near (list w) (wt:openings))
    (setq hw (/ (caddr op) 2.0))
    (if (and (cadddr op)
             (= (car (setq r (wt:op-assoc op (wt:op-pool op (list w))))) "OK")
             (member w (caddr r))
             (setq s (wt:op-span w (car op) (cadr op)))
             (<= (car s) (- *wt:tol* hw)) (>= (cadr s) (- hw *wt:tol*)))
      (setq out (cons (cadddr op) out))))
  out)

;; hook event: call every registered fn, absorb its changes into the transaction
(defun wt:op-event (fns args)
  (foreach f (wt:hook-fns fns) (wt:pend-absorb (apply f args)))
  (setq *wt:op-cache* nil))

;; --- Public API for opening providers (safe to call only while WallTool is loaded) ---

;; (mid dir width) -> "OK" (a WallTool wall owns it) | "AMBIG" | "NONE"
(defun wt:api-opening-status (rec / *wt:op-net* op)
  (wt:init)
  (if (wt:op-valid-p rec)
    (progn
      (setq *wt:op-net* (wt:net-scan) op (wt:op-norm rec))
      (car (wt:op-assoc op (wt:op-pool op nil))))
    "NONE"))

;; Openings (mid dir width) were added, removed or resized: regenerate the WallTool
;; walls holding them from the current registrations (old jambs erased). Runs as
;; its own WallTool transaction inside the caller's undo group. T if a wall changed.
(defun wt:api-openings-changed (recs / *wt:op-net* *wt:op-extra* ws op r)
  (wt:init)
  (setq *wt:op-net* (wt:net-scan))
  (foreach x recs
    (if (and (wt:op-valid-p x)
             (setq op (wt:op-norm x))
             (= (car (setq r (wt:op-assoc op (wt:op-pool op nil)))) "OK"))
      (progn
        (setq *wt:op-extra* (cons op *wt:op-extra*))
        (foreach w (caddr r) (if (not (assoc (car w) ws)) (setq ws (cons w ws)))))))
  (if ws
    (progn (wt:pend-begin) (wt:rebuild (reverse ws) nil) (wt:pend-end) t)))

(defun wt:topo-linework (walls regen / res strips hubs polys voids vs cutters cand i runs out)
  (setq res (wt:topo-solve walls) strips (car res) hubs (cadr res)
        polys (append (mapcar 'wt:strip-poly strips) (mapcar 'car hubs))
        vs (wt:opening-voids walls) voids (mapcar 'car vs)
        cutters (apply 'append (mapcar 'wt:poly-edges (append polys voids)))
        i 0)
  (foreach v vs
    (if (wt:any-member (cadr v) regen) (setq cand (append cand (wt:poly-edges (car v))))))
  (foreach s strips
    (if (member i regen) (setq cand (append cand (wt:poly-edges (wt:strip-poly s)))))
    (setq i (1+ i)))
  (foreach h hubs
    (if (wt:any-member (cadr h) regen) (setq cand (append cand (wt:poly-edges (car h))))))
  (foreach e cand
    (setq runs (append runs (wt:edge-visible (car e) (cadr e) polys voids cutters))))
  ;; drop runs contained in a longer one (coincident edges of adjacent strips)
  (foreach r (wt:sort runs '(lambda (a b) (> (wt:dist (car a) (cadr a)) (wt:dist (car b) (cadr b)))))
    (if (not (wt:seg-covered r out)) (setq out (cons r out))))
  (reverse out))

(defun wt:any-member (a b / r)
  (foreach x a (if (member x b) (setq r t)))
  r)

;;; ===================================================================
;;; 11. Wall identification layer
;;; ===================================================================
;;; Stage 1 has no persistent wall metadata. Everything that decides "is this a
;;; wall, and which one" lives here, so a Stage 2 persistence layer only has to
;;; replace these functions:
;;;   wt:net-scan             drawing snapshot (masters faces)
;;;   wt:wall-from-master     X-AXIS LINE -> wall
;;;   wt:find-master-for-face A-WALL LINE -> owning wall
;;;   wt:wall-from-entity     any LINE ename -> wall
;;;   wt:pick-to-master       picked point -> connection point on a master
;;; Walls drawn in this session are remembered by master ename (*wt:reg*).
;;; Other walls are reconstructed from parallel A-WALL faces (wt:recon).

(defun wt:reg-add (en th pos) (setq *wt:reg* (cons (list en th pos) *wt:reg*)))

;; list of (ename p1 p2) for LINEs on a layer in the current space
(defun wt:scan (lyr / ss i d out)
  (if (setq ss (ssget "_X" (list '(0 . "LINE") (cons 8 lyr) (cons 410 (getvar "CTAB")))))
    (repeat (setq i (sslength ss))
      (setq d (entget (ssname ss (setq i (1- i))))
            out (cons (list (cdr (assoc -1 d)) (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d)))) out))))
  out)

(defun wt:net-scan () (list (wt:scan (wt:cfg "AXIS_LAYER")) (wt:scan (wt:cfg "WALL_LAYER"))))

;; Thickness of an unregistered master from parallel A-WALL lines overlapping its
;; span. Masters are wall centerlines, so the faces must sit symmetrically at
;; +/- thickness/2. nil otherwise (plain axis, off-centre legacy master -> see WWR).
(defun wt:recon (m faces / p1 p2 u l d sa sb pmin nmax zero)
  (setq p1 (cadr m) p2 (caddr m) u (wt:unit (wt:v- p2 p1)) l (wt:dist p1 p2))
  (foreach f faces
    (setq d (wt:cross u (wt:v- (cadr f) p1))
          sa (wt:dot (wt:v- (cadr f) p1) u) sb (wt:dot (wt:v- (caddr f) p1) u))
    (if (and (wt:par u (wt:unit (wt:v- (caddr f) (cadr f))))
             (<= (abs d) *wt:recon-max*)
             (> (max sa sb) *wt:tol*) (< (min sa sb) (- l *wt:tol*)))
      (cond ((< (abs d) *wt:tol*) (setq zero t))
            ((> d 0) (if (or (not pmin) (< d pmin)) (setq pmin d)))
            (t (if (or (not nmax) (> d nmax)) (setq nmax d))))))
  (cond
    ((and pmin nmax (< (abs (+ pmin nmax)) *wt:tol*)) (list (car m) p1 p2 (- pmin nmax) "CENTER"))))

;; A-WALL lines generated by wall w: face pieces overlapping its span, or a square cap at p1/p2
;; spanning exactly its two faces. Ownership of visible output, used to erase/identify.
(defun wt:owned-line-p (f w / p1 u l o da db sa sb)
  (setq p1 (wt:w-p1 w) u (wt:w-u w) l (wt:dist p1 (wt:w-p2 w)) o (wt:w-offs w)
        da (wt:cross u (wt:v- (cadr f) p1)) db (wt:cross u (wt:v- (caddr f) p1))
        sa (wt:dot (wt:v- (cadr f) p1) u) sb (wt:dot (wt:v- (caddr f) p1) u))
  (or
    (and (< (abs (- da db)) *wt:tol*)
         (or (< (abs (- da (car o))) *wt:tol*) (< (abs (- da (cadr o))) *wt:tol*))
         (> (max sa sb) *wt:tol*) (< (min sa sb) (- l *wt:tol*)))
    ;; square cap: across THIS wall's own faces (not merely the same length on
    ;; the same perpendicular -- that claimed caps of unrelated aligned walls)
    (and (< (abs (- sa sb)) *wt:tol*)
         (or (< (abs sa) *wt:tol*) (< (abs (- sa l)) *wt:tol*))
         (< (abs (- (max da db) (car o))) *wt:tol*)
         (< (abs (- (min da db) (cadr o))) *wt:tol*))))

(defun wt:any-owned (f ws / r)
  (foreach w ws (if (and (not r) (wt:owned-line-p f w)) (setq r t)))
  r)

;; registry is trusted only while the wall still has a face on A-WALL
;; (guards against an undone XW line later reappearing on X-AXIS)
(defun wt:wall-from-master (m faces / r w)
  (if (and (setq r (assoc (car m) *wt:reg*))
           (setq w (list (car m) (cadr m) (caddr m) (cadr r) (caddr r)))
           (wt:has-owned-face w faces))
    w
    (wt:recon m faces)))

(defun wt:has-owned-face (w faces / r)   ; any face of `faces` owned by w
  (foreach f faces (if (and (not r) (wt:owned-line-p f w)) (setq r t)))
  r)

;; TW: masters may have been moved by hand, so a session registry entry is trusted
;; without requiring faces at the current position; otherwise reconstruct.
(defun wt:wall-for-repair (m faces / r)
  (if (setq r (assoc (car m) *wt:reg*))
    (list (car m) (cadr m) (caddr m) (cadr r) (caddr r))
    (wt:recon m faces)))

;; closest point on segment a-b
(defun wt:proj-seg (p a b / u s)
  (setq u (wt:unit (wt:v- b a)) s (max 0.0 (min (wt:dist a b) (wt:dot (wt:v- p a) u))))
  (wt:v+ a (wt:v* u s)))

;; A-WALL line f=(ename p1 p2) -> every wall whose derived outline claims it
(defun wt:face-owners (f net / fu mid mu w out)
  (setq fu (wt:unit (wt:v- (caddr f) (cadr f)))
        mid (wt:v* (wt:v+ (cadr f) (caddr f)) 0.5))
  (foreach m (car net)
    (setq mu (wt:unit (wt:v- (caddr m) (cadr m))))
    (if (and (or (wt:par fu mu) (< (abs (wt:dot fu mu)) *wt:tol-par*))   ; face or cap direction
             (<= (wt:dist mid (wt:proj-seg mid (cadr m) (caddr m))) *wt:recon-max*)
             (setq w (wt:wall-from-master m (cadr net)))
             (wt:owned-line-p f w))
      (setq out (cons w out))))
  out)

;; owners of f; with a pick point q, only walls whose master span contains q's station
;; (a merged face across collinear spans belongs to the span under the pick)
(defun wt:owners-at (f q net / o s r)
  (setq o (wt:face-owners f net))
  (if (and q (cdr o))
    (progn
      (foreach w o
        (setq s (wt:dot (wt:v- q (wt:w-p1 w)) (wt:w-u w)))
        (if (and (> s (- *wt:tol*)) (< s (+ (wt:dist (wt:w-p1 w) (wt:w-p2 w)) *wt:tol*)))
          (setq r (cons w r))))
      r)
    o))

;; A-WALL line -> its single owning wall; nil when none or more than one wall claims it
(defun wt:find-master-for-face (f net / o)
  (if (= (length (setq o (wt:face-owners f net))) 1) (car o)))

;; any LINE ename (master or face) -> wall, or nil
(defun wt:wall-from-entity (en net / d lyr m)
  (if (and (setq d (entget en)) (= (cdr (assoc 0 d)) "LINE"))
    (progn
      (setq lyr (strcase (cdr (assoc 8 d)))
            m (list en (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d)))))
      (cond ((= lyr (strcase (wt:cfg "AXIS_LAYER"))) (wt:wall-from-master m (cadr net)))
            ((= lyr (strcase (wt:cfg "WALL_LAYER"))) (wt:find-master-for-face m net))))))

;; Picked point -> point used for master topology.
;; On a master or in free space: unchanged. On a recognised wall line: the
;; closest point of the owning master (face endpoint -> master endpoint, face
;; midspan -> perpendicular foot). Only on unrecognised A-WALL lines, or on
;; lines of walls that disagree: nil.
(defun wt:pick-to-master (q net / hit w pts ok)
  (cond
    ((wt:on-any q (car net)) q)
    (t
     (foreach f (cadr net)
       (if (wt:on-seg q (cadr f) (caddr f))
         (progn
           (setq hit t)
           (foreach w (wt:owners-at f q net)
             (setq pts (cons (wt:proj-seg q (wt:w-p1 w) (wt:w-p2 w)) pts))))))
     (setq ok pts)
     (foreach p pts (if (not (wt:peq p (car pts))) (setq ok nil)))
     (cond ((not hit) q)
           (ok (car pts))))))

(defun wt:on-any (q segs / r)
  (foreach s segs (if (and (not r) (wt:on-seg q (cadr s) (caddr s))) (setq r t)))
  r)

;; existing (ename p1 p2 ...) whose endpoints match a-b in either order
(defun wt:master-at (a b ms / r)
  (foreach m ms
    (if (and (not r) (or (and (wt:peq a (cadr m)) (wt:peq b (caddr m)))
                         (and (wt:peq a (caddr m)) (wt:peq b (cadr m)))))
      (setq r m)))
  r)

;;; ===================================================================
;;; 12. Transactions and local rebuild (shared by WW, XW, EW)
;;; ===================================================================

;; *wt:pending* = (created erased modified-old-data); reverted by wt:seg-undo
(defun wt:pend-begin ()
  (setq *wt:pending* (list nil nil nil) *wt:op-cache* nil *wt:op-sus* nil *wt:op-amb* nil))
;; Close the transaction: openings whose wall was removed and not replaced are
;; deleted by their providers (recorded), skipped openings reported. -> record
(defun wt:pend-end (/ *wt:op-net* ops op gone rec)
  (if *wt:op-sus*
    (progn
      (setq *wt:op-net* (wt:net-scan) ops (wt:openings))
      (foreach id *wt:op-sus*
        (setq op nil)
        (foreach o ops (if (equal (cadddr o) id) (setq op o)))
        (if (and op (= (car (wt:op-assoc op (wt:op-pool op nil))) "NONE")) (setq gone (cons id gone))))
      (if gone
        (progn
          (wt:op-event *wt:opening-removed-fns* (list gone))
          (princ (strcat "\n" (itoa (length gone)) " door/window opening(s) removed with their wall."))))))
  (if *wt:op-amb*
    (princ (strcat "\n" (itoa (length *wt:op-amb*))
                   " door/window opening(s) overlap a wall junction, leave their wall or match several walls: hole not kept.")))
  (setq rec *wt:pending* *wt:pending* nil *wt:op-sus* nil *wt:op-amb* nil *wt:op-cache* nil)
  rec)
;; provider changes (created erased modified-old-data) -> current transaction
(defun wt:pend-absorb (rec / k l x)
  (if *wt:pending*
    (progn
      (setq k 0)
      (while (and (< k 3) (= (type rec) 'LIST) rec)
        (setq l (car rec) rec (cdr rec))
        (while (and (= (type l) 'LIST) l)
          (setq x (car l) l (cdr l))
          (if (if (= k 2)
                (and (= (type x) 'LIST) (= (type (car x)) 'LIST) (= (type (cdr (assoc -1 x))) 'ENAME))
                (= (type x) 'ENAME))
            (wt:pend-push k x)))
        (setq k (1+ k))))))
(defun wt:pend-push (k x) (setq *wt:pending* (wt:setnth *wt:pending* k (cons x (nth k *wt:pending*)))))
(defun wt:pend-make (en) (if en (wt:pend-push 0 en)) en)
(defun wt:pend-erase (en) (entdel en) (wt:pend-push 1 en))
(defun wt:pend-modify (data / old)
  (setq old (entget (cdr (assoc -1 data))))
  (if (entmod data) (wt:pend-push 2 old)))
(defun wt:seg-undo (rec)
  (foreach e (car rec) (entdel e))      ; delete created
  (foreach e (cadr rec) (entdel e))     ; entdel on an erased entity restores it
  (foreach d (caddr rec) (entmod d)))   ; restore modified

(defun wt:touch-any (m ws / r)
  (foreach w ws (if (and (not r) (wt:seg-touch (cadr m) (caddr m) (wt:w-p1 w) (wt:w-p2 w))) (setq r t)))
  r)

;; Join two collinear segments sharing an endpoint; nil otherwise.
(defun wt:seg-join (a b / u p ts)
  (setq p (car a) u (wt:unit (wt:v- (cadr a) p)))
  (if (and (< (abs (wt:cross u (wt:v- (car b) p))) *wt:tol*)
           (< (abs (wt:cross u (wt:v- (cadr b) p))) *wt:tol*)
           (or (wt:peq (car a) (car b)) (wt:peq (car a) (cadr b))
               (wt:peq (cadr a) (car b)) (wt:peq (cadr a) (cadr b))))
    (progn
      (setq ts (mapcar '(lambda (x) (wt:dot (wt:v- x p) u)) (list (car a) (cadr a) (car b) (cadr b))))
      (list (wt:v+ p (wt:v* u (apply 'min ts))) (wt:v+ p (wt:v* u (apply 'max ts)))))))

;; Generic merge of collinear contiguous segments ((a b) ...).
;; okfn: (lambda (merged-seg) ...) may veto a merge (nil = refuse).
(defun wt:seg-merge (segs okfn / out s hit m rest)
  (while segs
    (setq s (car segs) segs (cdr segs) hit t)
    (while hit
      (setq hit nil rest nil)
      (foreach o segs
        (if (and (not hit) (setq m (wt:seg-join s o)) (apply okfn (list m)))
          (setq s m hit t)
          (setq rest (cons o rest))))
      (setq segs (reverse rest)))
    (setq out (cons s out)))
  (reverse out))

;; Solver output with collinear contiguous pieces merged. A merged face may span
;; several collinear walls; wt:rebuild regenerates every wall sharing it.
(defun wt:linework-merged (walls idx)
  (wt:seg-merge (wt:topo-linework walls idx) '(lambda (s) t)))

;; --- Master normalization: ONE MASTER SPAN BETWEEN TOPOLOGY NODES ---

;; Points strictly inside wall w's master where another master of `others`
;; ends or crosses (non-parallel only), sorted along w, duplicates removed.
(defun wt:master-nodes (w others / mn-a mn-b mn-u pts x out)
  (setq mn-a (wt:w-p1 w) mn-b (wt:w-p2 w) mn-u (wt:w-u w))
  (foreach o others
    (if (and (not (eq (car o) (car w)))
             (not (wt:par mn-u (wt:unit (wt:v- (caddr o) (cadr o))))))
      (progn
        (foreach p (list (cadr o) (caddr o))
          (if (wt:in-seg p mn-a mn-b) (setq pts (cons p pts))))
        (if (and (setq x (wt:xline mn-a mn-u (cadr o) (wt:unit (wt:v- (caddr o) (cadr o)))))
                 (wt:in-seg x mn-a mn-b) (wt:on-seg x (cadr o) (caddr o)))
          (setq pts (cons x pts))))))
  (foreach p (wt:sort pts '(lambda (p q) (< (wt:dot (wt:v- p mn-a) mn-u) (wt:dot (wt:v- q mn-a) mn-u))))
    (if (or (not out) (not (wt:peq p (car out)))) (setq out (cons p out))))
  (reverse out))

;; Replace wall w's master by pieces split at pts (same direction, thickness,
;; position). Recorded in the transaction; pieces registered. Returns piece walls.
(defun wt:master-split-at (w pts / lyr prev en out)
  (setq lyr (wt:cfg "AXIS_LAYER") prev (wt:w-p1 w))
  (wt:pend-erase (car w))
  (foreach p (append pts (list (wt:w-p2 w)))
    (if (setq en (wt:pend-make (wt:mk-line prev p lyr)))
      (progn
        (wt:reg-add en (wt:w-thk w) "CENTER")
        (setq out (cons (list en prev p (wt:w-thk w) "CENTER") out))))
    (setq prev p))
  (reverse out))

;; Split existing walls touched by `new` and the new walls themselves at every
;; topology node the new walls introduce. Returns the normalized new walls.
(defun wt:normalize-masters (new / net exist w pts out)
  (setq net (wt:net-scan))
  (foreach m (car net)
    (if (and (not (assoc (car m) new)) (wt:touch-any m new)
             (setq w (wt:wall-from-master m (cadr net))))
      (setq exist (cons w exist))))
  (foreach w exist
    (if (setq pts (wt:master-nodes w new)) (wt:master-split-at w pts)))
  (foreach w new
    (setq out (append out (if (setq pts (wt:master-nodes w (append exist new)))
                            (wt:master-split-at w pts)
                            (list w)))))
  out)

;; Local rebuild after adding walls `new` (masters already drawn) and/or
;; removing walls `removed` (masters and linework still drawn). Records into
;; *wt:pending* (call wt:pend-begin first). Returns the normalized new walls.
;; regen = new + existing walls touching new/removed, plus any wall sharing a
;;         (merged) line with them (linework erased and redrawn)
;; ctx   = existing walls touching regen walls (topology only)
(defun wt:rebuild (new removed / lyr net faces changed rest regen ctx w walls idx keep grow vr n0
                                  *wt:op-net* *wt:op-skip*)
  (if new (setq new (wt:normalize-masters new)))
  (setq lyr (wt:cfg "WALL_LAYER") net (wt:net-scan) faces (cadr net) changed (append new removed)
        *wt:op-net* net)
  (foreach m (car net)
    (cond ((assoc (car m) changed))
          ((and (wt:touch-any m changed) (setq w (wt:wall-from-master m faces)))
           (setq regen (cons w regen)))
          (t (setq rest (cons m rest)))))
  (setq grow t)
  (while grow
    (setq grow nil ctx nil)
    (foreach m rest
      (if (and (not (assoc (car m) regen)) (wt:touch-any m regen)
               (setq w (wt:wall-from-master m faces)))
        (setq ctx (cons w ctx))))
    (foreach c ctx
      (foreach f faces
        (if (and (not (assoc (car c) regen))
                 (wt:owned-line-p f c)
                 (or (wt:any-owned f regen) (wt:any-owned f removed)))
          (setq regen (cons c regen) grow t)))))
  (setq walls (append new regen ctx))
  (repeat (+ (length new) (length regen)) (setq idx (cons (length idx) idx)))
  (wt:dbg (list "rebuild new" (length new) "removed" (length removed) "regen" (length regen) "ctx" (length ctx)))
  ;; registered openings: their old jambs are erased and redrawn; an opening on a removed
  ;; wall is a suspect, deleted at wt:pend-end unless a wall still holds it then
  (setq n0 (+ (length new) (length regen)))
  (foreach v (wt:opening-voids (append new regen removed))
    (if (cadr v) (setq vr (cons (car v) vr)))   ; only openings of walls redrawn here
    (foreach k (cadr v) (if (>= k n0) (wt:op-suspect (caddr v)))))
  (setq vr (append vr (wt:op-rects *wt:op-extra* (append new regen removed)))
        *wt:op-skip* (mapcar 'car removed))
  (foreach f faces
    (if (or (wt:any-owned f new) (wt:any-owned f regen) (wt:any-owned f removed) (wt:void-cap-p f vr))
      (wt:pend-erase (car f))
      (setq keep (cons (list (cadr f) (caddr f)) keep))))
  (foreach w removed (if (entget (car w)) (wt:pend-erase (car w))))
  (foreach s (wt:linework-merged walls idx)
    (if (not (wt:seg-covered s keep))
      (wt:pend-make (wt:mk-line (car s) (cadr s) lyr))))
  new)

;; --- Local axis healing (shared by WW and WWE; WWR stays the user-requested audit) ---
;; Only the given node points are inspected. At each point a master node is removed
;; only when it no longer represents topology:
;;   - zero-length masters there are erased
;;   - an exact duplicate of another master there is erased (one copy kept)
;;   - exactly two recognised collinear spans of one thickness end there and nothing
;;     else touches the point -> joined into one master (WWR's wt:wr-join rule)
;; A T, X, L, a width step or any other wall at the point keeps the split. Records
;; into the caller's transaction. Returns (gone-ename . kept-ename) or nil.
(defun wt:axis-heal-node (p / net ms seen w2 seg a b d fld)
  (setq net (wt:net-scan))
  (foreach mm (car net)
    (if (wt:on-seg p (cadr mm) (caddr mm))
      (cond
        ((wt:peq (cadr mm) (caddr mm)) (wt:pend-erase (car mm)))
        ((wt:master-at (cadr mm) (caddr mm) seen) (wt:pend-erase (car mm)))
        (t (setq seen (cons mm seen))))))
  (setq net (wt:net-scan)
        fld (list (wt:v+ p '(-1.0 -1.0)) (wt:v+ p '(1.0 -1.0)) (wt:v+ p '(1.0 1.0)) (wt:v+ p '(-1.0 1.0))))
  (foreach mm seen
    (setq ms (cons (if (setq w2 (wt:wall-from-master mm (cadr net))) w2 mm) ms)))
  (if (and (= (length ms) 2) (= (length (car ms)) 5) (= (length (cadr ms)) 5)
           (setq seg (wt:wr-join (car ms) (cadr ms) (car net) fld)))
    (progn
      (setq a (car ms) b (cadr ms) d (entget (car a))
            d (subst (cons 10 (wt:3d (car seg))) (assoc 10 d) d)
            d (subst (cons 11 (wt:3d (cadr seg))) (assoc 11 d) d))
      (wt:pend-modify d)
      (wt:rebuild (list (list (car a) (car seg) (cadr seg) (wt:w-thk a) "CENTER")) (list b))
      (wt:reg-add (car a) (wt:w-thk a) "CENTER")
      (wt:dbg (list "AXIS HEAL joined" (car b) "into" (car a) "at" p))
      (cons (car b) (car a)))))

;; heal every given node point once. -> ((gone-ename . kept-ename) ...)
(defun wt:axis-heal-local (pts / done out r)
  (foreach q pts
    (if (not (wt:tw-has-pt q done))
      (progn
        (setq done (cons q done))
        (if (setq r (wt:axis-heal-node q)) (setq out (cons r out))))))
  out)

;; endpoints of wall records / (ename p1 p2) masters
(defun wt:heal-pts (ws / out)
  (foreach w ws (setq out (cons (cadr w) (cons (caddr w) out))))
  out)

;; --- Creation alignment -> centerline. Masters are always wall centerlines. ---

;; Pure. Centerline of a wall of thickness th placed pos ("LEFT" body on the left of
;; travel, "RIGHT" on the right, "CENTER" on the line) relative to drawn p1->p2.
(defun wt:placement-to-centerline (p1 p2 th pos / off n)
  (setq off (cond ((= pos "LEFT") (/ th 2.0)) ((= pos "RIGHT") (/ th -2.0)) (t 0.0))
        n (wt:v* (wt:perp (wt:unit (wt:v- p2 p1))) off))
  (list (wt:v+ p1 n) (wt:v+ p2 n)))

;; WW chain context (only while c:WW binds *wt:chain-on*): (drawn-point ename end-point).
;; An entry is used only while its end-point is still an endpoint of that master, so
;; entries left behind by an undone segment (which had moved the master) are skipped.
(defun wt:chain-entry (dpt en / r d)
  (foreach c *wt:chain*
    (if (and (not r) (eq (cadr c) en) (wt:peq (car c) dpt) (setq d (entget en))
             (or (wt:peq (caddr c) (wt:pt2 (cdr (assoc 10 d))))
                 (wt:peq (caddr c) (wt:pt2 (cdr (assoc 11 d))))))
      (setq r c)))
  r)

;; Centerlines for drawn segments with thickness th and alignment pos.
;; An offset end slides along its own direction until it meets the centerline of
;; whatever the drawn point landed on (an existing wall master, a previous WW
;; segment, another segment of the same batch), so faces meet exactly where the
;; drawn alignment put them. A single existing wall ENDING there follows the new
;; corner (host move). Returns (centerlines host-moves), host-moves = ((m k point) ...).
(defun wt:centerlines (segs th pos net / cls i cl u dpt pt xs hs j x ok moves out k m e)
  (setq cls (mapcar '(lambda (sg) (wt:placement-to-centerline (car sg) (cadr sg) th pos)) segs) i 0)
  (foreach sg segs
    (setq cl (nth i cls) u (wt:unit (wt:v- (cadr cl) (car cl))))
    (foreach side '(0 1)
      (setq dpt (if (= side 0) (car sg) (cadr sg)) pt (if (= side 0) (car cl) (cadr cl)) xs nil hs nil)
      (foreach m (car net)
        (setq e (if *wt:chain-on* (wt:chain-entry dpt (car m))))
        (if (and (or e (wt:on-seg dpt (cadr m) (caddr m)))
                 (wt:wall-from-master m (cadr net))
                 (setq x (wt:xline pt u (cadr m) (wt:unit (wt:v- (caddr m) (cadr m))))))
          (setq xs (cons x xs) hs (cons (list m x (if e (caddr e) dpt)) hs))))
      (setq j 0)
      (foreach s2 segs
        (if (and (/= j i) (wt:on-seg dpt (car s2) (cadr s2))
                 (setq x (wt:xline pt u (car (nth j cls)) (wt:unit (wt:v- (cadr (nth j cls)) (car (nth j cls)))))))
          (setq xs (cons x xs)))
        (setq j (1+ j)))
      (setq ok (and xs t))
      (foreach x xs (if (not (wt:peq x (car xs))) (setq ok nil)))
      (if ok (setq pt (car xs)))
      ;; one existing wall whose END is at the corner follows it (free end only)
      (if (and ok (= (length hs) 1))
        (progn
          (setq m (car (car hs)) e (caddr (car hs))
                k (cond ((wt:peq e (cadr m)) 10) ((wt:peq e (caddr m)) 11)))
          (if (and k (not (wt:peq pt e)) (<= (wt:dist pt e) *wt:recon-max*)
                   (= (wt:tw-touch-count-m e (car m) (car net)) 0))
            (setq moves (cons (list m k pt) moves)))))
      (setq out (cons pt out)))
    (setq i (1+ i)))
  (setq out (reverse out) cls nil)
  (while out (setq cls (cons (list (car out) (cadr out)) cls) out (cddr out)))
  (list (reverse cls) moves))

;; masters other than en touching point p
(defun wt:tw-touch-count-m (p en ms / n)
  (setq n 0)
  (foreach m ms (if (and (not (eq (car m) en)) (wt:on-seg p (cadr m) (caddr m))) (setq n (1+ n))))
  n)

;; Move host master ends (recorded). Their old lines are erased first, so the moved
;; walls are returned as records and must be passed to wt:rebuild as walls to
;; regenerate (with no lines left they would no longer be recognised).
(defun wt:apply-host-moves (moves net / w d out)
  (foreach mv moves
    (if (setq w (wt:wall-from-master (car mv) (cadr net)))
      (progn
        (foreach f (cadr net)
          (if (and (entget (car f)) (wt:owned-line-p f w)) (wt:pend-erase (car f))))
        (setq d (entget (car (car mv))))
        (wt:pend-modify (subst (cons (cadr mv) (wt:3d (caddr mv))) (assoc (cadr mv) d) d))
        (setq out (cons (if (= (cadr mv) 10)
                          (list (car w) (caddr mv) (wt:w-p2 w) (wt:w-thk w) "CENTER")
                          (list (car w) (wt:w-p1 w) (caddr mv) (wt:w-thk w) "CENTER"))
                        out)))))
  out)

;; Add walls drawn along segments ((a b) ...) with the current width and creation
;; alignment as ONE transaction. Stored masters are centerlines. Returns the
;; (created erased modified) record, or nil if rejected.
(defun wt:walls-add (segs / net new en rec bad res cls i moved hl pr)
  (setq net (wt:net-scan))
  (foreach sg segs
    (if (wt:peq (car sg) (cadr sg)) (setq bad "\nZero-length wall ignored.")))
  (if (not bad)
    (progn
      (setq res (wt:centerlines segs *wt:thk* *wt:pos* net) cls (car res))
      (foreach c cls
        (cond ((wt:peq (car c) (cadr c)) (setq bad "\nZero-length wall ignored."))
              ((and (setq en (wt:master-at (car c) (cadr c) (car net)))
                    (wt:wall-from-master en (cadr net)))
               (setq bad "\nA wall already exists on that axis. Ignored."))))))
  (if bad
    (progn (princ bad) nil)
    (progn
      (wt:pend-begin)
      (setq moved (wt:apply-host-moves (cadr res) net))
      (foreach c cls
        (if (setq en (wt:pend-make (wt:mk-line (car c) (cadr c) (wt:cfg "AXIS_LAYER"))))
          (setq new (cons (list en (car c) (cadr c) *wt:thk* "CENTER") new))))
      (setq new (wt:rebuild (append (reverse new) moved) nil))
      (foreach w new (wt:reg-add (car w) (wt:w-thk w) "CENTER"))
      ;; WW only (c:WW binds *wt:ww-heal*): nodes of the new spans and the corners hosts left
      (if *wt:ww-heal* (setq hl (wt:axis-heal-local
                 (append (wt:heal-pts new)
                         (mapcar '(lambda (mv) (if (= (cadr mv) 10) (cadr (car mv)) (caddr (car mv)))) (cadr res))))))
      (foreach pr hl
        (setq new (mapcar '(lambda (w) (if (eq (car w) (car pr)) (cons (cdr pr) (cdr w)) w)) new))
        (setq *wt:chain* (mapcar '(lambda (c) (if (eq (cadr c) (car pr)) (list (car c) (cdr pr) (caddr c)) c)) *wt:chain*)))
      (if *wt:chain-on*
        (progn
          (setq i 0)
          (foreach sg segs
            (foreach side '(0 1)
              (setq en (if (= side 0) (car (nth i cls)) (cadr (nth i cls))))
              (foreach w new
                (if (or (wt:peq en (wt:w-p1 w)) (wt:peq en (wt:w-p2 w)))
                  (setq *wt:chain* (cons (list (if (= side 0) (car sg) (cadr sg)) (car w) en) *wt:chain*)))))
            (setq i (1+ i)))))
      (setq rec (wt:pend-end))
      rec)))

;;; ===================================================================
;;; 13. Settings UI shared by WW and XW
;;; ===================================================================

(defun wt:status ()
  (princ (strcat "\nCurrent wall: " (wt:fmt *wt:thk*) " mm | " *wt:pos*)))

(defun wt:thk-accept (lst / i v)
  (setq i (atoi (get_tile "list"))
        v (if (< i (length lst)) (nth i lst) (distof (get_tile "custom") 2)))
  (if (and v (> v 0))
    (progn (setq *wt:dlg-val* v) (done_dialog 1))
    (set_tile "error" "Thickness must be a positive number.")))

;; number, 'CANCEL, or nil when the dialog is unavailable
(defun wt:thk-dialog (/ f id lst sel res i)
  (if (and (setq f (findfile "WallTool.dcl")) (>= (setq id (load_dialog f)) 0))
    (if (new_dialog "wt_thickness" id)
      (progn
        (setq lst (wt:cfg "THICKNESSES") i 0)
        (foreach v lst (if (equal v *wt:thk* *wt:tol*) (setq sel i)) (setq i (1+ i)))
        (start_list "list")
        (foreach v lst (add_list (strcat (wt:fmt v) " mm")))
        (add_list "Custom...")
        (end_list)
        (set_tile "list" (itoa (if sel sel (length lst))))
        (set_tile "custom" (wt:fmt *wt:thk*))
        (mode_tile "custom" (if sel 1 0))
        (action_tile "list" "(mode_tile \"custom\" (if (= (atoi $value) (length lst)) 0 1))")
        (action_tile "accept" "(wt:thk-accept lst)")
        (setq res (start_dialog))
        (unload_dialog id)
        (if (= res 1) *wt:dlg-val* 'CANCEL))
      (progn (unload_dialog id) nil))))

(defun wt:thk-cmdline (/ v)
  (initget 6 "Custom")
  (setq v (getreal (strcat "\nWall thickness ["
                           (apply 'strcat (mapcar '(lambda (x) (strcat (wt:fmt x) "/")) (wt:cfg "THICKNESSES")))
                           "Custom] <" (wt:fmt *wt:thk*) ">: ")))
  (if (= v "Custom")
    (progn (initget 6) (setq v (getreal "\nCustom wall thickness: "))))
  v)

(defun wt:ww-thickness (/ v)
  (setq v (wt:thk-dialog))
  (if (not v) (setq v (wt:thk-cmdline)))
  (if (numberp v) (setq *wt:thk* (float v)))
  (wt:status))

;; Alignment submenu (the one current creation alignment, *wt:pos*). Left/Center/Right,
;; with left-hand aliases Q = LEFT, W = CENTER, E = RIGHT. Enter keeps the current
;; value; anything unrecognised leaves it unchanged.
(defun wt:ww-position (/ k v)
  (initget "Left Center Right Q W E")
  (setq k (getkword (strcat "\nAlignment [Left/Center/Right] <"
                            (substr *wt:pos* 1 1) (strcase (substr *wt:pos* 2) t) ">: ")))
  (if (and (= (type k) 'STR) (> (strlen k) 0)
           (setq v (cdr (assoc (strcase (substr k 1 1)) '(("Q" . "LEFT") ("W" . "CENTER") ("E" . "RIGHT")
                                                          ("L" . "LEFT") ("C" . "CENTER") ("R" . "RIGHT"))))))
    (setq *wt:pos* v))
  (wt:status))

(defun wt:ww-settings (/ k)
  (initget "List Reload")
  (setq k (getkword "\nSettings [List/Reload] <List>: "))
  (if (= k "Reload")
    (progn (setq *wt:cfg* nil) (wt:cfg-load)
           (setq *wt:thk* (wt:cfg "DEFAULT_THICKNESS") *wt:pos* (wt:cfg "DEFAULT_POSITION"))))
  (princ (strcat "\nConfig file: " (cond ((wt:cfg-path)) ("<not found, defaults>"))))
  (foreach c *wt:cfg* (princ (strcat "\n  " (car c) "=" (wt:cfg-str (cdr c)))))
  (wt:status))

;;; ===================================================================
;;; 14. WW -- draw wall
;;; ===================================================================

;; Rectangle = four masters fed to the normal engine as one transaction.
;; Traversal is counter-clockwise in the current UCS, so LEFT puts the wall
;; body inside the rectangle and RIGHT outside, on every side.
(defun wt:ww-rect (/ c1 c2 x0 x1 y0 y1 z pts)
  (if (and (setq c1 (getpoint "\nSpecify first corner: "))
           (setq c2 (getcorner c1 "\nSpecify opposite corner: ")))
    (progn
      (setq x0 (min (car c1) (car c2)) x1 (max (car c1) (car c2))
            y0 (min (cadr c1) (cadr c2)) y1 (max (cadr c1) (cadr c2)) z (caddr c1))
      (if (or (< (- x1 x0) *wt:tol*) (< (- y1 y0) *wt:tol*))
        (progn (princ "\nRectangle has zero width or height. Ignored.") nil)
        (progn
          (setq pts (mapcar '(lambda (p) (wt:pt2 (trans (list (car p) (cadr p) z) 1 0)))
                            (list (list x0 y0) (list x1 y0) (list x1 y1) (list x0 y1))))
          (wt:walls-add (list (list (nth 0 pts) (nth 1 pts)) (list (nth 1 pts) (nth 2 pts))
                              (list (nth 2 pts) (nth 3 pts)) (list (nth 3 pts) (nth 0 pts)))))))))

;; hist items: (transaction-record points-stack-before)
(defun c:WW (/ *error* p0 p q pts hist r done *wt:chain* *wt:chain-on* *wt:ww-heal*)
  (setq *error* wt:error *wt:chain-on* t *wt:ww-heal* t)
  (wt:begin)
  (wt:layer "AXIS")
  (wt:layer "WALL")
  (wt:status)
  (while (not done)
    ;; Width and Alignment are current creation settings: each segment uses the
    ;; values active when it is committed; earlier segments are never re-placed.
    (setq q (if p
              (wt:getpt p "\nSpecify next point or [Width/Alignment/Rectangle/Undo/Close/Settings]: "
                        "Width Alignment Rectangle Undo Close Settings")
              (wt:getpt nil (if hist "\nSpecify start point or [Width/Alignment/Rectangle/Undo/Settings]: "
                                     "\nSpecify start point or [Width/Alignment/Rectangle/Settings]: ")
                        "Width Alignment Rectangle Undo Settings")))
    (cond
      ((not q) (setq done t))
      ((= q "Width") (wt:ww-thickness))
      ((= q "Alignment") (wt:ww-position))
      ((= q "Settings") (wt:ww-settings))
      ((= q "Rectangle")
       (setq p nil p0 nil pts nil *wt:chain* nil)
       (if (setq r (wt:ww-rect)) (setq hist (cons (list r nil) hist))))
      ((= q "Undo")
       (cond ((and p (not (cdr pts))) (setq p nil p0 nil pts nil))   ; drop a lone start point
             (hist (wt:seg-undo (car (car hist)))
                   (setq pts (cadr (car hist)) p (car pts) p0 (last pts) hist (cdr hist)))
             (t (princ "\nNothing to undo."))))
      ((= q "Close")
       (cond ((< (length pts) 3) (princ "\nAt least two segments are required to close."))
             ((wt:walls-add (list (list p p0))) (setq done t))))
      ((= (type q) 'STR))
      ((not (setq q (wt:pick-to-master q (wt:net-scan))))
       (princ "\nCould not identify an AKD WallTool wall from this line. Pick a wall face or master axis."))
      ((not p) (setq p0 q p q pts (list q)))
      ((setq r (wt:walls-add (list (list p q))))
       (setq hist (cons (list r pts) hist) pts (cons q pts) p q))))
  (wt:end))

;;; ===================================================================
;;; 15. XW -- axis to wall (input adapter into wt:rebuild)
;;; ===================================================================

;; Point pick or implied window/crossing selection of LINEs, with keywords
;; (ssget cannot show keywords portably). kwfn is called with a keyword.
(defun wt:select-lines (msg kw kwfn / sel p c s i e done)
  (while (not done)
    (initget kw)
    (setq p (getpoint msg))
    (cond
      ((not p) (setq done t))
      ((= (type p) 'STR) (apply kwfn (list p)))
      (t
       (if (not (setq s (ssget p '((0 . "LINE")))))
         (if (setq c (getcorner p "\nSpecify opposite corner: "))
           (setq s (ssget (if (< (car p) (car c)) "_W" "_C") p c '((0 . "LINE"))))))
       (if s
         (repeat (setq i (sslength s))
           (setq e (ssname s (setq i (1- i))))
           (if (not (member e sel)) (progn (setq sel (cons e sel)) (redraw e 3)))))
       (princ (strcat "\n" (itoa (length sel)) " line(s) selected.")))))
  (foreach e sel (redraw e 4))
  sel)

(defun wt:xw-kw (k) (if (= k "Width") (wt:ww-thickness) (wt:ww-position)))

(defun wt:bylayer (d lyr)
  (setq d (subst (cons 8 lyr) (assoc 8 d) d))
  (foreach c (list (cons 62 256) (cons 6 "BYLAYER") (cons 370 -1))
    (if (assoc (car c) d) (setq d (subst c (assoc (car c) d) d))))
  d)

;; Convert LINE enames to walls as one network + one transaction.
;; Each source is the drawn reference line: the width and creation alignment place
;; the wall, and the source entity itself becomes the wall's centerline master
;; (moved to X-AXIS, BYLAYER, endpoints shifted to the centerline; Undo restores it).
;; A source duplicating an existing plain X-AXIS axis is erased and that axis is used.
(defun wt:xw-convert (sel / net axis srcs new skipped d a b ex res i cl moved)
  (setq net (wt:net-scan) axis (wt:cfg "AXIS_LAYER") skipped 0)
  (foreach en sel
    (setq d (entget en) a (wt:pt2 (cdr (assoc 10 d))) b (wt:pt2 (cdr (assoc 11 d))))
    (cond
      ((or (wt:peq a b)
           (= (strcase (cdr (assoc 8 d))) (strcase (wt:cfg "WALL_LAYER")))
           (wt:master-at a b srcs))
       (setq skipped (1+ skipped)))
      ((setq ex (wt:master-at a b (car net)))
       (if (wt:wall-from-master ex (cadr net))
         (setq skipped (1+ skipped))
         (setq srcs (cons (list (car ex) a b (if (eq (car ex) en) nil en)) srcs))))
      (t (setq srcs (cons (list en a b nil) srcs)))))
  (setq srcs (reverse srcs))
  (wt:pend-begin)
  (if srcs
    (progn
      (setq res (wt:centerlines (mapcar '(lambda (x) (list (cadr x) (caddr x))) srcs) *wt:thk* *wt:pos* net) i 0)
      (setq moved (wt:apply-host-moves (cadr res) net))
      (foreach x srcs
        (setq cl (nth i (car res)) d (wt:bylayer (entget (car x)) axis))
        (if (cadddr x) (wt:pend-erase (cadddr x)))
        (setq d (subst (cons 10 (wt:3d (car cl))) (assoc 10 d) d)
              d (subst (cons 11 (wt:3d (cadr cl))) (assoc 11 d) d))
        (wt:pend-modify d)
        (setq new (cons (list (car x) (car cl) (cadr cl) *wt:thk* "CENTER") new) i (1+ i)))))
  (setq new (reverse new) ex (length new))
  (if new (setq new (wt:rebuild (append new moved) nil)))
  (foreach w new (wt:reg-add (car w) (wt:w-thk w) "CENTER"))
  (wt:pend-end)
  (if (> skipped 0)
    (princ (strcat "\n" (itoa skipped) " line(s) skipped (zero length, wall face, duplicate, or already a wall).")))
  (princ (strcat "\n" (itoa ex) " axis line(s) converted."))
  new)

(defun c:XW (/ *error* ss sel other)
  (setq *error* wt:error)
  (if (setq ss (ssget "_I")) (sssetfirst nil nil))   ; before any command clears PickFirst
  (wt:begin)
  (wt:layer "AXIS")
  (wt:layer "WALL")
  (wt:status)
  (if ss
    (progn
      (setq other 0)
      (foreach it (wt:ss-items ss)
        (if (= (caddr it) "LINE") (setq sel (cons (car it) sel)) (setq other (1+ other))))
      (if (> other 0)
        (princ (strcat "\n" (itoa other) " unsupported object(s) ignored."))))
    (setq sel (wt:select-lines "\nSelect axis lines or [Width/posiTion]: "
                               "Width posiTion" 'wt:xw-kw)))
  (if sel (wt:xw-convert sel))
  (wt:end))

;;; ===================================================================
;;; 16. EW -- smart erase wall (remove masters -> wt:rebuild)
;;; ===================================================================
;;; ONE MASTER LINE = ONE DELETABLE WALL SEGMENT. Selection resolves to exact
;;; master enames first; the drawing is only touched after all are resolved.

(defun wt:seg-dist (p a b) (wt:dist p (wt:proj-seg p a b)))

;; selection set -> list of (ename pick-point-or-nil type); pick point only
;; for point picks (ssnamex method 1), nil for windows / PickFirst
(defun wt:ss-items (ss / i e pt out)
  (repeat (setq i (sslength ss))
    (setq e (ssname ss (setq i (1- i))) pt nil)
    (foreach x (ssnamex ss i)
      (if (and (= (car x) 1) (listp (cadddr x)) (listp (cadr (cadddr x))))
        (setq pt (wt:pt2 (cadr (cadddr x))))))
    (setq out (cons (list e pt (cdr (assoc 0 (entget e)))) out)))
  out)

;; about one screen pixel in drawing units: picks closer than this are ties
(defun wt:pick-margin (/ sz)
  (setq sz (getvar "SCREENSIZE"))
  (if (and (listp sz) (> (cadr sz) 0))
    (max *wt:tol* (/ (getvar "VIEWSIZE") (cadr sz)))
    *wt:tol*))

;; one selected LINE -> (status wall), status "OK" | "AMBIG" | "NONE"
;;  master line          -> that master (if it is a wall)
;;  face + pick point    -> the A-WALL lines nearest the pick (within margin)
;;                          must all belong to the same single master, else AMBIG
;;  face, no pick point  -> exactly one master must claim the line, else AMBIG
(defun wt:ew-resolve (en q net margin / d lyr m w o dmin owner r)
  (setq d (entget en) lyr (strcase (cdr (assoc 8 d)))
        m (list en (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d)))))
  (cond
    ((= lyr (strcase (wt:cfg "AXIS_LAYER")))
     (if (setq w (wt:wall-from-master m (cadr net))) (list "OK" w) (list "NONE" nil)))
    ((/= lyr (strcase (wt:cfg "WALL_LAYER"))) (list "NONE" nil))
    ((not q)
     (setq o (wt:face-owners m net))
     (cond ((= (length o) 1) (list "OK" (car o)))
           (o (list "AMBIG" nil))
           (t (list "NONE" nil))))
    (t
     ;; nearest visible wall lines to the pick decide, whichever line AutoCAD returned
     (foreach f (cadr net)
       (setq w (wt:seg-dist q (cadr f) (caddr f)))
       (if (or (not dmin) (< w dmin)) (setq dmin w)))
     (setq r "OK")
     (foreach f (cadr net)
       (if (<= (wt:seg-dist q (cadr f) (caddr f)) (+ dmin margin))
         (progn
           (setq o (wt:owners-at f q net))
           (cond ((/= (length o) 1) (setq r (if o "AMBIG" (if (= r "OK") "NONE" r))))
                 ((not owner) (setq owner (car o)))
                 ((not (eq (car owner) (car (car o)))) (setq owner (car o) r "AMBIG"))))))
     (cond ((= r "AMBIG") (list "AMBIG" nil))
           ((and owner (= r "OK")) (list "OK" owner))
           (owner (list "AMBIG" nil))   ; an owned line ties with an unidentified one
           (t (list "NONE" nil))))))

;; items from wt:ss-items -> erase the selected master segments, one transaction
;; Items this command could not attribute to a wall. While *wt:ew-defer* is on
;; (c:EW) they are collected in *wt:ew-unclaimed* for the erase hook instead of
;; being reported here.
(setq *wt:ew-unclaimed* nil)
(defun wt:ew-unclaim (en)
  (if *wt:ew-defer* (setq *wt:ew-unclaimed* (cons en *wt:ew-unclaimed*))))

(defun wt:ew-erase (items margin / net walls amb none other r)
  (setq net (wt:net-scan) amb 0 none 0 other 0 *wt:ew-unclaimed* nil)
  (foreach it items
    (if (/= (caddr it) "LINE")
      (progn (setq other (1+ other)) (wt:ew-unclaim (car it)))
      (progn
        (setq r (wt:ew-resolve (car it) (cadr it) net margin))
        (cond ((= (car r) "OK")
               (if (not (assoc (car (cadr r)) walls)) (setq walls (cons (cadr r) walls))))
              ((= (car r) "AMBIG") (setq amb (1+ amb)))
              (t (setq none (1+ none)) (wt:ew-unclaim (car it)))))))
  (if walls
    (progn
      (wt:pend-begin)
      (wt:rebuild nil walls)
      (wt:pend-end)
      (princ (strcat "\n" (itoa (length walls)) " wall(s) erased."))))
  (if (> amb 0)
    (princ (strcat "\n" (itoa amb) " ambiguous wall line(s) skipped. Select closer to the wall segment to erase.")))
  (if (and (> none 0) (not *wt:ew-defer*))
    (princ (strcat "\n" (itoa none) " line(s) not identified as AKD WallTool walls, skipped.")))
  (if (and (> other 0) (not *wt:ew-defer*))
    (princ (strcat "\n" (itoa other) " unsupported object(s) ignored.")))
  walls)

;; Objects of other tools (AKD WinDoor doors/windows) that EW could not attribute
;; to a wall: each provider erases its own and returns the enames it took. Called
;; after the wall transaction, so a provider may run WallTool rebuilds of its own
;; (a door on a wall erased just now is already gone with it).
(defun wt:erase-hook (ids / r out)
  (foreach f (wt:hook-fns *wt:erase-fns*)
    (setq r (apply f (list ids)))
    (while (and (= (type r) 'LIST) r)
      (if (= (type (car r)) 'ENAME) (setq out (cons (car r) out)))
      (setq r (cdr r))))
  out)

(defun wt:ew-left (ens done / out)
  (foreach e ens (if (not (member e done)) (setq out (cons e out))))
  (reverse out))

(defun wt:ew-alive (ens / out)
  (foreach e ens (if (entget e) (setq out (cons e out))))
  (reverse out))

;; EW erases walls, and doors/windows of any registered tool, in one undo step.
(defun c:EW (/ *error* ss rest left *wt:ew-defer*)
  (setq *error* wt:error)
  (if (setq ss (ssget "_I")) (sssetfirst nil nil))   ; before any command clears PickFirst
  (wt:begin)
  (if (not ss)
    (progn (princ (if *wt:erase-fns* "\nSelect wall, door or window: " "\nSelect wall: "))
           (setq ss (ssget))))
  (if ss
    (progn
      (setq *wt:ew-defer* (and *wt:erase-fns* t))
      (wt:ew-erase (wt:ss-items ss) (wt:pick-margin))
      (if *wt:ew-defer*
        (progn
          (setq rest (wt:ew-alive *wt:ew-unclaimed*)         ; erased with their wall already
                left (wt:ew-left rest (wt:erase-hook rest)))
          (if left
            (princ (strcat "\n" (itoa (length left))
                           " object(s) not identified as walls, doors or windows, skipped.")))))
      (setq *wt:ew-unclaimed* nil)))
  (wt:end))

;;; ===================================================================
;;; 17. WWF -- wall from wall (input adapter: face pick -> new master -> wt:walls-add)
;;; ===================================================================

;; Pure. New master for a parallel copy of wall w.
;; side +1 = the left face of p1->p2 was picked, -1 = the right face.
;; dist = clear distance from that face to the NEAREST face of the new wall.
;; The new wall keeps w's thickness and direction and is CENTER, so its
;; faces sit at +/- thickness/2 whatever w's position was.
;; Returns (p1 p2 thickness "CENTER").
(defun wt:wwf-offset-master (w side dist / o off n)
  (setq o (wt:w-offs w)
        off (+ (if (> side 0) (car o) (cadr o)) (* side (+ dist (/ (wt:w-thk w) 2.0))))
        n (wt:perp (wt:w-u w)))
  (list (wt:v+ (wt:w-p1 w) (wt:v* n off)) (wt:v+ (wt:w-p2 w) (wt:v* n off)) (wt:w-thk w) "CENTER"))

;; Picked A-WALL line + WCS pick point -> side face (+1 left / -1 right) of the
;; resolved span, or a message string.
(defun wt:wwf-side (en q w / d fu sp o)
  (setq d (entget en)
        fu (wt:unit (wt:v- (wt:pt2 (cdr (assoc 11 d))) (wt:pt2 (cdr (assoc 10 d)))))
        sp (wt:cross (wt:w-u w) (wt:v- q (wt:w-p1 w)))
        o (wt:w-offs w))
  (cond ((not (wt:par fu (wt:w-u w))) "\nSelect a wall side face, not an end cap.")
        ((< (abs (- sp (car o))) (abs (- sp (cadr o)))) 1)
        (t -1)))

;; One offset wall from a picked face. Returns the transaction record or nil.
(defun wt:wwf-add (en q dist / net r w side nm *wt:thk* *wt:pos*)
  (setq net (wt:net-scan))
  (cond
    ((/= (strcase (cdr (assoc 8 (entget en)))) (strcase (wt:cfg "WALL_LAYER")))
     (princ "\nSelect a wall face.") nil)
    ((= (car (setq r (wt:ew-resolve en q net (wt:pick-margin)))) "AMBIG")
     (princ "\nAmbiguous wall junction. Select closer to the wall segment.") nil)
    ((/= (car r) "OK")
     (princ "\nCould not identify an AKD WallTool wall from this line.") nil)
    ((= (type (setq w (cadr r) side (wt:wwf-side en q w))) 'STR)
     (princ side) nil)
    (t
     (setq nm (wt:wwf-offset-master w side dist)
           *wt:thk* (caddr nm) *wt:pos* (cadddr nm))   ; local bindings: session settings untouched
     (wt:walls-add (list (list (car nm) (cadr nm)))))))

(defun wt:wwf-distance (def / v)
  (initget 6)
  (setq v (getdist (strcat "\nSpecify offset distance <" (wt:fmt def) ">: ")))
  (if v (float v) def))

(defun c:WWF (/ *error* dist e hist done r)
  (setq *error* wt:error)
  (wt:begin)
  (wt:layer "AXIS")
  (wt:layer "WALL")
  (if (not *wt:wwf-dist*) (setq *wt:wwf-dist* (wt:cfg "DEFAULT_OFFSET")))
  (setq dist (wt:wwf-distance *wt:wwf-dist*))
  (while (not done)
    (setvar "ERRNO" 0)
    (initget "Distance Undo")
    (setq e (entsel (if hist "\nSelect wall face or [Distance/Undo] <Exit>: " "\nSelect wall face: ")))
    (cond
      ((= e "Distance") (setq dist (wt:wwf-distance dist)))
      ((= e "Undo")
       (if hist (progn (wt:seg-undo (car hist)) (setq hist (cdr hist)) (princ "\nWall removed."))
                (princ "\nNothing to undo.")))
      ((not e) (if (/= (getvar "ERRNO") 7) (setq done t)))   ; 7 = missed pick, keep asking
      ((setq r (wt:wwf-add (car e) (wt:pt2 (trans (cadr e) 1 0)) dist))
       (setq hist (cons r hist) *wt:wwf-dist* dist)
       (princ "\nWall created."))))
  (wt:end))

;;; ===================================================================
;;; 18. TW -- wall repair in a field: AKD masters (then wt:rebuild) + ordinary double-line walls (TX cleanup)
;;; ===================================================================

;; field = 4 WCS corners of the picked UCS rectangle
(defun wt:tw-field (c1 c2 / z)
  (setq z (caddr c1))
  (mapcar '(lambda (p) (wt:pt2 (trans (list (car p) (cadr p) z) 1 0)))
          (list (list (min (car c1) (car c2)) (min (cadr c1) (cadr c2)))
                (list (max (car c1) (car c2)) (min (cadr c1) (cadr c2)))
                (list (max (car c1) (car c2)) (max (cadr c1) (cadr c2)))
                (list (min (car c1) (car c2)) (max (cadr c1) (cadr c2))))))

(defun wt:tw-seg-in-field (a b field / r e)
  (if (or (wt:pip a field) (wt:pip b field))
    t
    (progn
      (setq e (last field))
      (foreach c field (if (wt:seg-touch a b e c) (setq r t)) (setq e c))
      r)))

;; segment within tol of the field (read scope)
(defun wt:tw-seg-near-field (a b field tol / r e)
  (if (wt:tw-seg-in-field a b field)
    t
    (progn
      (setq e (last field))
      (foreach c field
        (if (or (<= (wt:seg-dist a e c) tol) (<= (wt:seg-dist b e c) tol)
                (<= (wt:seg-dist e a b) tol) (<= (wt:seg-dist c a b) tol))
          (setq r t))
        (setq e c))
      r)))

(defun wt:tw-end-pt (w side) (if (= side 0) (wt:w-p1 w) (wt:w-p2 w)))
(defun wt:tw-end-dir (w side) (if (= side 0) (wt:v* (wt:w-u w) -1.0) (wt:w-u w)))  ; outward

;; number of other walls whose master touches p
(defun wt:tw-touch-count (p wi walls / j n)
  (setq j 0 n 0)
  (foreach w walls
    (if (and (/= j wi) (wt:on-seg p (wt:w-p1 w) (wt:w-p2 w))) (setq n (1+ n)))
    (setq j (1+ j)))
  n)

;; Short span left by an overshoot that normalization already split off:
;; length <= tol, one free end in the field, the other end on a node where a
;; collinear wall continues it and at least one more wall meets.
(defun wt:tw-stub-p (w wi walls field tol / r p q j)
  (if (<= (wt:dist (wt:w-p1 w) (wt:w-p2 w)) tol)
    (foreach side '(0 1)
      (setq p (wt:tw-end-pt w side) q (wt:tw-end-pt w (- 1 side)))
      (if (and (not r) (wt:pip p field)
               (= (wt:tw-touch-count p wi walls) 0)
               (>= (wt:tw-touch-count q wi walls) 2))
        (progn
          (setq j 0)
          (foreach o walls
            (if (and (/= j wi) (wt:par (wt:w-u w) (wt:w-u o))
                     (or (and (wt:peq q (wt:w-p1 o)) (< (wt:dot (wt:v- (wt:w-p2 o) q) (wt:v- p q)) 0))
                         (and (wt:peq q (wt:w-p2 o)) (< (wt:dot (wt:v- (wt:w-p1 o) q) (wt:v- p q)) 0))))
              (setq r t))
            (setq j (1+ j)))))))
  r)

;; Pure repair analysis. walls = scope walls, field = WCS polygon, tol = connect distance.
;; Returns (moves stubs ambiguous-count)
;;   moves = ((wall-index side new-point) ...)  free endpoint extended/shortened
;;   stubs = (wall-index ...)                    overshoot spans to remove
;; A free end (no other wall master touches it) inside the field is extended or
;; shortened ALONG ITS OWN DIRECTION by at most tol to where it meets another
;; master (T), or to where it meets another free end's extension (L). The meeting
;; point must be inside the field. Several distinct meeting points = ambiguous.
(defun wt:tw-candidates (walls field tol / wi p d len j x tt q cands pts moves stubs amb keys bad out)
  (setq wi 0 amb 0)
  (foreach w walls
    (if (wt:tw-stub-p w wi walls field tol)
      (setq stubs (cons wi stubs))
      (foreach side '(0 1)
        (setq p (wt:tw-end-pt w side) d (wt:tw-end-dir w side)
              len (wt:dist (wt:w-p1 w) (wt:w-p2 w)) cands nil pts nil)
        (if (and (wt:pip p field) (= (wt:tw-touch-count p wi walls) 0))
          (progn
            (setq j 0)
            (foreach o walls
              (if (and (/= j wi) (not (wt:par d (wt:w-u o)))
                       (setq x (wt:xline p d (wt:w-p1 o) (wt:w-u o)))
                       (<= (abs (setq tt (wt:dot (wt:v- x p) d))) tol)
                       (> tt (- *wt:tol* len))          ; shortening keeps a real wall
                       (wt:pip x field))
                (if (wt:on-seg x (wt:w-p1 o) (wt:w-p2 o))
                  (setq cands (cons (list x nil) cands))
                  (foreach qs '(0 1)
                    (setq q (wt:tw-end-pt o qs))
                    (if (and (<= (wt:dist q x) tol)
                             (> (wt:dot (wt:v- x q) (wt:tw-end-dir o qs)) 0)
                             (wt:pip q field)
                             (= (wt:tw-touch-count q j walls) 0))
                      (setq cands (cons (list x (list j qs x)) cands))))))
              (setq j (1+ j)))
            (foreach c cands
              (if (not (wt:tw-has-pt (car c) pts)) (setq pts (cons (car c) pts))))
            (cond
              ((not cands))
              ((cdr pts) (setq amb (1+ amb)))
              (t
               (if (not (wt:peq p (car pts))) (setq moves (cons (list wi side (car pts)) moves)))
               (foreach c cands (if (cadr c) (setq moves (cons (cadr c) moves))))))))))
    (setq wi (1+ wi)))
  ;; one end, one destination: conflicting requests for the same end are dropped
  (foreach m moves
    (foreach m2 moves
      (if (and (= (car m) (car m2)) (= (cadr m) (cadr m2)) (not (wt:peq (caddr m) (caddr m2)))
               (not (member (list (car m) (cadr m)) bad)))
        (setq bad (cons (list (car m) (cadr m)) bad) amb (1+ amb)))))
  (foreach m moves
    (if (and (not (member (list (car m) (cadr m)) bad)) (not (member (list (car m) (cadr m)) keys))
             (not (member (car m) stubs)))
      (setq keys (cons (list (car m) (cadr m)) keys) out (cons m out))))
  (list out stubs amb))

(defun wt:tw-has-pt (p pts / r) (foreach x pts (if (wt:peq p x) (setq r t))) r)

;; apply one endpoint move to the master entity (recorded) and the wall record
(defun wt:tw-apply-move (walls mv / w d k)
  (setq w (nth (car mv) walls) d (entget (car w)) k (if (= (cadr mv) 0) 10 11))
  (wt:pend-modify (subst (cons k (wt:3d (caddr mv))) (assoc k d) d))
  (wt:setnth walls (car mv)
    (if (= (cadr mv) 0)
      (list (car w) (caddr mv) (caddr w) (wt:w-thk w) "CENTER")
      (list (car w) (cadr w) (caddr mv) (wt:w-thk w) "CENTER"))))

;; A-WALL line parallel/perpendicular to, and within reach of, one of walls
(defun wt:tw-near-master-p (f walls / fu mid r mu)
  (setq fu (wt:unit (wt:v- (caddr f) (cadr f))) mid (wt:v* (wt:v+ (cadr f) (caddr f)) 0.5))
  (foreach w walls
    (setq mu (wt:w-u w))
    (if (and (or (wt:par fu mu) (< (abs (wt:dot fu mu)) *wt:tol-par*))
             (<= (wt:seg-dist mid (wt:w-p1 w) (wt:w-p2 w)) *wt:recon-max*))
      (setq r t)))
  r)

;; Repair walls in field. Returns the transaction record, or nil if no walls.
(defun wt:tw-repair (field tol / net walls w r moves stubs amb nodes keepws stubws n rec lw)
  (setq net (wt:net-scan))
  (foreach m (car net)
    (if (and (wt:tw-seg-near-field (cadr m) (caddr m) field tol)
             (setq w (wt:wall-for-repair m (cadr net))))
      (setq walls (cons w walls))))
  (setq walls (reverse walls))
  (if (not walls)
    (progn (princ "\nTW: No AKD WallTool walls found in the repair area.") nil)
    (progn
      (setq r (wt:tw-candidates walls field tol) moves (car r) stubs (cadr r) amb (caddr r))
      (wt:pend-begin)
      (foreach mv moves
        (setq walls (wt:tw-apply-move walls mv))
        (if (not (wt:tw-has-pt (caddr mv) nodes)) (setq nodes (cons (caddr mv) nodes))))
      (setq n 0)
      (foreach w walls
        (if (member n stubs) (setq stubws (cons w stubws)) (setq keepws (cons w keepws)))
        (setq n (1+ n)))
      ;; stale A-WALL pieces beside the walls being rebuilt that no recognised wall
      ;; claims any more (e.g. faces/caps left where a moved master used to be)
      (setq net (wt:net-scan) lw (wt:legacy-walls net))   ; legacy walls are WWR's job
      (foreach f (cadr net)
        (if (and (not (wt:any-owned f walls))
                 (not (wt:face-owners f net))
                 (not (wt:any-owned f lw))
                 (wt:tw-near-master-p f walls))
          (wt:pend-erase (car f))))
      (wt:rebuild (reverse keepws) stubws)
      (setq rec (wt:pend-end))
      (setq n (+ (length nodes) (length stubs)))
      (princ (strcat "\nTW: "
                     (if (> n 0) (strcat (itoa n) " wall junction(s) repaired.") "Wall geometry rebuilt.")
                     (if (> amb 0) (strcat " " (itoa amb) " ambiguous connection(s) skipped.") "")))
      rec)))

(setq *wt:tw-net* nil)

;; any AKD wall (registered or reconstructable master) near the field
(defun wt:tw-akd-present (field tol / net r)
  (setq net (wt:net-scan))
  (foreach m (car net)
    (if (and (not r) (wt:tw-seg-near-field (cadr m) (caddr m) field tol) (wt:wall-for-repair m (cadr net)))
      (setq r t)))
  r)

;; collection filter: A-WALL lines owned by an AKD wall stay with the AKD path
(defun wt:tw-akd-line-p (e d a b)
  (and (= (strcase (cdr (assoc 8 d))) (strcase (wt:cfg "WALL_LAYER")))
       (or (wt:face-owners (list e a b) *wt:tw-net*) (wt:op-jamb-p a b))))

;; mutual-pair components of *wt:tx-segs* -> generic wall records
(defun wt:tw-gen-walls (/ seen comp queue x recs ref u o2 lo hi s1 s2 ok iv off rec base)
  (foreach s *wt:tx-segs*
    (if (and (not (member (car s) seen)) (wt:tx-paired-p (car s)))
      (progn
        (setq comp (list (car s)) queue (list (car s)) seen (cons (car s) seen))
        (while queue
          (setq x (car queue) queue (cdr queue))
          (foreach j (cdr (assoc x *wt:tx-ptab*))
            (if (and (not (member j seen)) (wt:tx-pairp x j))
              (setq comp (cons j comp) queue (append queue (list j)) seen (cons j seen)))))
        (setq comp (wt:sort comp '<) ref (wt:tx-seg (car comp)) u (wt:tx-u ref)
              o2 nil s1 nil s2 nil lo nil hi nil ok t)
        (foreach id comp
          (setq off (wt:tx-off (cadr (wt:tx-seg id)) ref) iv (wt:tx-iv (wt:tx-seg id) ref))
          (if (or (not lo) (< (car iv) lo)) (setq lo (car iv)))
          (if (or (not hi) (> (cadr iv) hi)) (setq hi (cadr iv)))
          (cond ((<= (abs off) *wt:tx-tol-col*) (setq s1 (cons id s1)))
                ((not o2) (setq o2 off s2 (cons id s2)))
                ((<= (abs (- off o2)) *wt:tx-tol-col*) (setq s2 (cons id s2)))
                (t (setq ok nil))))
        (if (and ok o2)
          (progn
            (setq base (wt:v+ (cadr ref) (wt:v* (wt:perp u) (/ o2 2.0)))
                  rec (list comp (wt:v+ base (wt:v* u lo)) (wt:v+ base (wt:v* u hi)) (abs o2) "CENTER"
                            (reverse s1) (reverse s2) (car ref) o2 lo hi))
            (wt:dbg (list "TW GENERIC WALL faces" (reverse s1) "/" (reverse s2) "width" (wt:fmt (abs o2))
                          "inferred centerline" (cadr rec) "->" (caddr rec) "source GENERIC"))
            (setq recs (cons rec recs)))
          (wt:dbg (list "TW GENERIC faces" comp "not a consistent two-face wall, ignored"))))))
  (reverse recs))

(defun wt:tw-rec-u (r) (wt:unit (wt:v- (caddr r) (cadr r))))

;; p inside record r's wall strip (faces + extent, tol-col margin)
(defun wt:tw-in-strip (p r / ref st off)
  (setq ref (wt:tx-seg (nth 7 r)) st (wt:tx-sta p ref) off (wt:tx-off p ref))
  (and (>= st (- (nth 9 r) *wt:tx-tol-col*)) (<= st (+ (nth 10 r) *wt:tx-tol-col*))
       (>= off (- (min 0.0 (nth 8 r)) *wt:tx-tol-col*)) (<= off (+ (max 0.0 (nth 8 r)) *wt:tx-tol-col*))))

;; records -> TW wall list (id p1 p2 width pos). An axis end already inside another
;; wall's strip is connected: it is snapped (in memory) onto that wall's axis so the
;; AKD topology rules see a touching end and leave it to TX's visible cleanup.
(defun wt:tw-snap-ends (recs / out p q x ends)
  (foreach r recs
    (setq ends nil)
    (foreach side '(0 1)
      (setq p (if (= side 0) (cadr r) (caddr r)) q p)
      (foreach o recs
        (if (and (not (eq o r)) (wt:peq q p)
                 (>= (abs (wt:cross (wt:tw-rec-u r) (wt:tw-rec-u o))) *wt:tx-min-sin*)
                 (wt:tw-in-strip p o)
                 (setq x (wt:xline p (wt:tw-rec-u r) (cadr o) (wt:tw-rec-u o))))
          (setq q x)))
      (setq ends (cons q ends)))
    (setq ends (reverse ends))
    (setq out (cons (list (car r) (car ends) (cadr ends) (cadddr r) "CENTER") out)))
  (reverse out))

;; collinear runs: parallel, same width, same axis line, facing free ends, gap in (tol, maxgap]
;; with its midpoint in the field. -> ((i si j sj gap) ...)
(defun wt:tw-gen-collinear (recs walls field maxgap / out i j ri rj ui uj pe qe oi oj g)
  (setq i 0)
  (foreach ri recs
    (setq j 0)
    (foreach rj recs
      (if (> j i)
        (progn
          (setq ui (wt:tw-rec-u ri) uj (wt:tw-rec-u rj))
          (if (and (< (abs (wt:cross ui uj)) *wt:tx-tol-par*)
                   (<= (abs (- (cadddr ri) (cadddr rj))) *wt:tx-tol-col*)
                   (<= (abs (wt:cross ui (wt:v- (cadr rj) (cadr ri)))) *wt:tx-tol-col*))
            (foreach si '(0 1)
              (foreach sj '(0 1)
                (setq pe (wt:tw-end-pt (nth i walls) si) qe (wt:tw-end-pt (nth j walls) sj)
                      oi (if (= si 1) ui (wt:v* ui -1.0)) oj (if (= sj 1) uj (wt:v* uj -1.0))
                      g (wt:dot (wt:v- qe pe) oi))
                (if (and (< (wt:dot oi oj) 0.0) (> g *wt:tol*) (<= g maxgap)
                         (= (wt:tw-touch-count pe i walls) 0) (= (wt:tw-touch-count qe j walls) 0)
                         (wt:pip (wt:v* (wt:v+ pe qe) 0.5) field))
                  (setq out (cons (list i si j sj g) out))))))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  (reverse out))

;; move every face of record rec at end side by d along the wall (entities, recorded)
(defun wt:tw-gen-move (rec side d / u o best bk bp dd p)
  (setq u (wt:tw-rec-u rec) o (if (= side 1) u (wt:v* u -1.0)))
  (foreach grp (list (nth 5 rec) (nth 6 rec))
    (setq best nil)
    (foreach id grp
      (foreach en (cadddr (wt:tx-seg id))
        (setq dd (entget en))
        (foreach k '(10 11)
          (setq p (wt:pt2 (cdr (assoc k dd))))
          (if (or (not best) (> (wt:dot p o) (+ (wt:dot best o) *wt:tol*)))
            (setq best p bk k bp en)))))
    (if best
      (progn
        (setq dd (entget bp) p (cdr (assoc bk dd)))
        (wt:pend-modify (subst (cons bk (list (+ (car p) (* d (car o))) (+ (cadr p) (* d (cadr o)))
                                              (if (caddr p) (caddr p) 0.0)))
                               (assoc bk dd) dd))))))

(defun wt:tw-count (k keys / n) (setq n 0) (foreach x keys (if (equal x k) (setq n (1+ n)))) n)
(defun wt:tw-move-at (wi side moves / r)
  (foreach m moves (if (and (= (car m) wi) (= (cadr m) side)) (setq r t))) r)
(defun wt:tw-pt-member (x pts / r) (foreach p pts (if (wt:peq x p) (setq r t))) r)
(defun wt:tw-shared-pt (mv moves / r)
  (foreach m moves (if (and (/= (car m) (car mv)) (wt:peq (caddr m) (caddr mv))) (setq r t))) r)

;; Topology repair of ordinary double-line walls in the field (records into *wt:pending*).
;; Returns (junctions ambiguous generic-wall-count).
(defun wt:tw-generic (field tol / ens recs walls r moves amb col keys bad drop acc n x d w pts)
  (setq *wt:tx-field* nil n 0 amb 0
        ens (wt:tx-collect field (+ tol (wt:cfg "TX_WALL_MAX")) 'wt:tw-akd-line-p))
  (if ens (progn (wt:tx-prepare ens *wt:tol*) (setq recs (wt:tw-gen-walls))))
  (if recs
    (progn
      (setq walls (wt:tw-snap-ends recs)
            r (wt:tw-candidates walls field tol) moves (car r) amb (caddr r)
            col (wt:tw-gen-collinear recs walls field tol))
      (foreach c col (setq keys (cons (list (car c) (cadr c)) (cons (list (caddr c) (cadddr c)) keys))))
      ;; a continuation must be unique and not compete with an L/T connection of the same end
      (foreach c col
        (if (or (/= (wt:tw-count (list (car c) (cadr c)) keys) 1)
                (/= (wt:tw-count (list (caddr c) (cadddr c)) keys) 1)
                (wt:tw-move-at (car c) (cadr c) moves) (wt:tw-move-at (caddr c) (cadddr c) moves))
          (progn
            (setq bad (cons c bad) amb (1+ amb))
            (wt:dbg (list "TW TOPOLOGY collinear walls" (car (nth (car c) recs)) (car (nth (caddr c) recs))
                          "classification COLLINEAR accepted NO (ambiguous)")))))
      (foreach mv moves (if (member (list (car mv) (cadr mv)) keys) (setq drop (cons (caddr mv) drop))))
      (foreach mv moves (if (not (wt:tw-pt-member (caddr mv) drop)) (setq acc (cons mv acc))))
      (setq acc (reverse acc))
      (foreach mv acc
        (setq w (nth (car mv) walls) x (caddr mv)
              d (wt:dot (wt:v- x (wt:tw-end-pt w (cadr mv))) (wt:tw-end-dir w (cadr mv))))
        (if (not (wt:tw-pt-member x pts)) (setq pts (cons x pts) n (1+ n)))
        (wt:dbg (list "TW TOPOLOGY wall" (car (nth (car mv) recs)) "end P" (1+ (cadr mv))
                      "classification" (if (wt:tw-shared-pt mv acc) "L" "T")
                      "axis intersection" x "required movement" (wt:fmt (abs d)) "accepted YES"))
        (wt:tw-gen-move (nth (car mv) recs) (cadr mv) d))
      (foreach c col
        (if (not (member c bad))
          (progn
            (wt:dbg (list "TW TOPOLOGY collinear walls" (car (nth (car c) recs)) (car (nth (caddr c) recs))
                          "classification COLLINEAR gap" (wt:fmt (nth 4 c)) "accepted YES"))
            (wt:tw-gen-move (nth (car c) recs) (cadr c) (/ (nth 4 c) 2.0))
            (wt:tw-gen-move (nth (caddr c) recs) (cadddr c) (/ (nth 4 c) 2.0))
            (setq n (1+ n)))))))
  (setq *wt:tx-segs* nil *wt:tx-ptab* nil)
  (list n amb (length recs)))

;; TW: AKD walls (authoritative masters, unchanged path), then ordinary double-line
;; walls, then the shared TX visible cleanup inside the field. One undo group.
(defun wt:tw-run (field tol / akd g cl)
  (setq *wt:tx-field* nil *wt:tx-nested* nil cl 0)
  (if (setq akd (wt:tw-akd-present field tol)) (wt:tw-repair field tol))
  (setq *wt:tw-net* (wt:net-scan))
  (wt:pend-begin)
  (setq *wt:tx-nested* t g (wt:tw-generic field tol))
  (if (> (caddr g) 0)
    (progn
      (setq *wt:tx-field* field
            cl (wt:tx-collect field (+ (wt:cfg "TX_CONNECT_DISTANCE") (wt:cfg "TX_WALL_MAX")) 'wt:tw-akd-line-p)
            cl (if cl (wt:tx-run cl) 0))))
  (setq *wt:tx-nested* nil *wt:tx-field* nil *wt:pending* nil *wt:tw-net* nil)
  (cond
    ((> (caddr g) 0)
     (princ (strcat "\nTW: " (itoa (car g)) " generic wall junction(s) repaired, "
                    (itoa cl) " visible junction(s) cleaned."
                    (if (> (cadr g) 0) (strcat " " (itoa (cadr g)) " ambiguous connection(s) skipped.") ""))))
    ((not akd) (princ "\nTW: No walls found in the repair area.")))
  g)

(defun c:TW (/ *error* c1 c2)
  (setq *error* wt:error)
  (wt:begin)
  (wt:layer "AXIS")
  (wt:layer "WALL")
  (if (and (setq c1 (getpoint "\nSpecify first corner of wall repair area: "))
           (setq c2 (getcorner c1 "\nSpecify opposite corner: ")))
    (progn
      (princ "\nRepairing wall network...")
      (wt:tw-run (wt:tw-field c1 c2) (wt:cfg "TW_CONNECT_DISTANCE"))))
  (wt:end))

;;; ===================================================================
;;; 19. WWR -- wall repair: audit / repair centerline masters in a field
;;; ===================================================================
;;; WWR repairs what the walls ARE (centerline, thickness, missing masters).
;;; TW repairs how nearby walls CONNECT. Evidence, strongest first:
;;;   1 recognised wall master   2 A-WALL faces   3 junctions with known walls
;;;   4 reconstruction (only when both ends are proven)

(defun wt:wr-has-num (x l / r) (foreach y l (if (< (abs (- x y)) *wt:tol*) (setq r t))) r)

;; sorted distinct signed offsets of A-WALL lines parallel to a-b overlapping its span
(defun wt:wr-offsets (a b faces / u l d sa sb out)
  (setq u (wt:unit (wt:v- b a)) l (wt:dist a b))
  (foreach f faces
    (setq d (wt:cross u (wt:v- (cadr f) a))
          sa (wt:dot (wt:v- (cadr f) a) u) sb (wt:dot (wt:v- (caddr f) a) u))
    (if (and (wt:par u (wt:unit (wt:v- (caddr f) (cadr f))))
             (<= (abs d) *wt:recon-max*)
             (> (max sa sb) *wt:tol*) (< (min sa sb) (- l *wt:tol*))
             (not (wt:wr-has-num d out)))
      (setq out (cons d out))))
  (wt:sort out '<))

;; faces minus the lines (faces, caps) owned by recognised walls that cross the
;; direction u of master en: e.g. a branch's cap lying inside this wall's band must
;; not split the band. Lines of parallel walls (collinear spans share faces) stay.
(defun wt:wr-free-faces (faces known en u / others out)
  (foreach w known
    (if (and (not (eq (car w) en)) (not (wt:par u (wt:w-u w)))) (setq others (cons w others))))
  (foreach f faces (if (not (wt:any-owned f others)) (setq out (cons f out))))
  out)

;; Pure. The wall band around a master = the single pair of adjacent face offsets
;; bracketing offset 0. Returns (mid thickness), "AMBIG" (several), or nil (none).
(defun wt:wr-band (offs / r n)
  (setq n 0)
  (while (cdr offs)
    (if (and (<= (car offs) *wt:tol*) (>= (cadr offs) (- *wt:tol*)))
      (setq r (list (/ (+ (car offs) (cadr offs)) 2.0) (- (cadr offs) (car offs))) n (1+ n)))
    (setq offs (cdr offs)))
  (cond ((= n 1) r) ((> n 1) "AMBIG")))

;; Proven end of a candidate band midline near station st: a junction with a known
;; wall (crossing its segment, or continuing a collinear one) within th, else a cap
;; line across the band exactly at the face end. nil = unproven.
(defun wt:wr-end (m u st th faces walls / q r x)
  (setq q (wt:v+ m (wt:v* u st)))
  (foreach w walls
    (if (not r)
      (if (wt:par u (wt:w-u w))
        (foreach k (list (wt:w-p1 w) (wt:w-p2 w))
          (if (and (not r) (< (abs (wt:cross u (wt:v- k m))) *wt:tol*) (<= (wt:dist k q) th))
            (setq r k)))
        (if (and (setq x (wt:xline m u (wt:w-p1 w) (wt:w-u w)))
                 (<= (wt:dist x q) th)
                 (wt:on-seg x (wt:w-p1 w) (wt:w-p2 w)))
          (setq r x)))))
  (if (not r)
    (foreach c faces
      (if (and (not r)
               (< (abs (wt:dot u (wt:unit (wt:v- (caddr c) (cadr c))))) *wt:tol-par*)
               (< (abs (wt:dot (wt:v- (cadr c) q) u)) *wt:tol*)
               (< (abs (wt:dot (wt:v- (caddr c) q) u)) *wt:tol*)
               (< (abs (- (wt:dist (cadr c) (caddr c)) th)) *wt:tol*)
               (<= (wt:dist (cadr c) q) (+ (/ th 2.0) *wt:tol*))
               (<= (wt:dist (caddr c) q) (+ (/ th 2.0) *wt:tol*)))
        (setq r q))))
  r)

;; Missing-master candidate from an unclaimed face line f: the nearest overlapping
;; parallel A-WALL line on each side forms a band; the band's midline between two
;; proven ends is the centerline. Returns ("OK" (p1 p2) th), ("AMBIG"), or nil.
(defun wt:wr-candidate (f faces walls / a u l d sa sb bl br cands m e0 e1)
  (setq a (cadr f) u (wt:unit (wt:v- (caddr f) a)) l (wt:dist a (caddr f)))
  (foreach g faces
    (setq d (wt:cross u (wt:v- (cadr g) a))
          sa (wt:dot (wt:v- (cadr g) a) u) sb (wt:dot (wt:v- (caddr g) a) u))
    (if (and (not (eq (car g) (car f)))
             (wt:par u (wt:unit (wt:v- (caddr g) (cadr g))))
             (> (abs d) *wt:tol*) (<= (abs d) *wt:recon-max*)
             (> (max sa sb) *wt:tol*) (< (min sa sb) (- l *wt:tol*)))
      (if (> d 0)
        (if (or (not bl) (< d bl)) (setq bl d))
        (if (or (not br) (> d br)) (setq br d)))))
  (foreach d (list bl br)
    (if d
      (progn
        (setq m (wt:v+ a (wt:v* (wt:perp u) (/ d 2.0)))
              e0 (wt:wr-end m u 0.0 (abs d) faces walls)
              e1 (wt:wr-end m u l (abs d) faces walls))
        (if (and e0 e1 (> (wt:dist e0 e1) *wt:tol*))
          (setq cands (cons (list "OK" (list e0 e1) (abs d)) cands))))))
  (cond ((cdr cands) (list "AMBIG")) (cands (car cands))))

;; Faces for inference: jambs of registered openings dropped, and collinear face
;; pieces interrupted exactly by an opening joined across it (first piece's ename),
;; so an intentional hole is neither a wall end, a band break nor a contradiction.
(defun wt:wr-open-faces (faces / mid u n hw keep ls rs sa sb da db lo hi hit rest)
  (foreach op (wt:openings)
    (setq mid (car op) u (cadr op) n (wt:perp u) hw (/ (caddr op) 2.0) keep nil ls nil rs nil)
    (foreach f faces
      (setq sa (wt:dot (wt:v- (cadr f) mid) u) sb (wt:dot (wt:v- (caddr f) mid) u)
            da (wt:dot (wt:v- (cadr f) mid) n) db (wt:dot (wt:v- (caddr f) mid) n)
            lo (min sa sb) hi (max sa sb))
      (cond
        ((and (< (abs (- sa sb)) *wt:op-tol*) (< (abs (- (abs sa) hw)) *wt:op-tol*)
              (< (abs (+ da db)) *wt:op-tol*)))                          ; jamb: dropped
        ((or (> (abs (- da db)) *wt:op-tol*) (> (abs da) *wt:recon-max*)) (setq keep (cons f keep)))
        ((and (< (abs (+ hi hw)) *wt:op-tol*) (< lo (- (+ hw *wt:op-tol*)))) (setq ls (cons (list f lo da) ls)))
        ((and (< (abs (- lo hw)) *wt:op-tol*) (> hi (+ hw *wt:op-tol*))) (setq rs (cons (list f hi da) rs)))
        (t (setq keep (cons f keep)))))
    (foreach a ls
      (setq hit nil rest nil)
      (foreach b rs
        (if (and (not hit) (< (abs (- (caddr a) (caddr b))) *wt:op-tol*)) (setq hit b) (setq rest (cons b rest))))
      (setq rs rest)
      (setq keep (cons (if hit
                         (list (car (car a))
                               (wt:v+ mid (wt:v+ (wt:v* u (cadr a)) (wt:v* n (caddr a))))
                               (wt:v+ mid (wt:v+ (wt:v* u (cadr hit)) (wt:v* n (caddr a)))))
                         (car a))
                       keep)))
    (foreach b rs (setq keep (cons (car b) keep)))
    (setq faces (reverse keep)))
  faces)

;; a wall line close to master w that w does not own: the master no longer matches
;; its faces (e.g. the axis was stretched at one end only)
(defun wt:wr-contradicted-p (w faces / r)
  (foreach f faces
    (if (and (not r) (not (wt:owned-line-p f w))
             (<= (wt:seg-dist (wt:v* (wt:v+ (cadr f) (caddr f)) 0.5) (wt:w-p1 w) (wt:w-p2 w)) (wt:w-thk w)))
      (setq r t)))
  r)

;; an unrecognised X-AXIS line inside the candidate band (e.g. a master stretched off its
;; faces): which line is the wall's axis cannot be proven -> the candidate is skipped
(defun wt:wr-in-band (p seg u th / s)
  (setq s (wt:dot (wt:v- p (car seg)) u))
  (and (> s (- *wt:tol*)) (< s (+ (wt:dist (car seg) (cadr seg)) *wt:tol*))
       (<= (abs (wt:cross u (wt:v- p (car seg)))) (/ th 2.0))))

(defun wt:wr-foreign-axis-p (seg th net walls / u r)
  (setq u (wt:unit (wt:v- (cadr seg) (car seg))))
  (foreach mm (car net)
    (if (and (not r) (not (assoc (car mm) walls))
             (or (wt:wr-in-band (cadr mm) seg u th) (wt:wr-in-band (caddr mm) seg u th))
             (not (wt:wall-from-master mm (cadr net))))
      (setq r t)))
  r)

;; candidate band overlaps a known wall's body (would duplicate/nest a wall)
(defun wt:wr-overlaps-p (seg th walls / u r ta tb)
  (setq u (wt:unit (wt:v- (cadr seg) (car seg))))
  (foreach w walls
    (if (and (not r) (wt:par u (wt:w-u w))
             (< (abs (wt:cross (wt:w-u w) (wt:v- (car seg) (wt:w-p1 w)))) (- (/ (+ th (wt:w-thk w)) 2.0) *wt:tol*)))
      (progn
        (setq ta (wt:dot (wt:v- (car seg) (wt:w-p1 w)) (wt:w-u w))
              tb (wt:dot (wt:v- (cadr seg) (wt:w-p1 w)) (wt:w-u w)))
        (if (and (> (max ta tb) *wt:tol*) (< (min ta tb) (- (wt:dist (wt:w-p1 w) (wt:w-p2 w)) *wt:tol*)))
          (setq r t)))))
  r)

;; masters of ms other than enames e1 e2 touching q
(defun wt:wr-others-at (q e1 e2 ms / n)
  (setq n 0)
  (foreach m ms
    (if (and (not (eq (car m) e1)) (not (eq (car m) e2)) (wt:on-seg q (cadr m) (caddr m)))
      (setq n (1+ n))))
  n)

;; Broken pieces of one wall: collinear, same thickness, touching or overlapping,
;; with no other master at the joint (a real junction keeps its spans).
;; Returns the joined segment in a's direction, or nil.
(defun wt:wr-join (a b ms field / u q ts)
  (setq u (wt:w-u a))
  (if (and (wt:par u (wt:w-u b))
           (< (abs (wt:cross u (wt:v- (wt:w-p1 b) (wt:w-p1 a)))) *wt:tol*)
           (< (abs (- (wt:w-thk a) (wt:w-thk b))) *wt:tol*))
    (progn
      (foreach p (list (wt:w-p1 b) (wt:w-p2 b))
        (if (and (not q) (wt:on-seg p (wt:w-p1 a) (wt:w-p2 a))) (setq q p)))
      (if (and q (wt:pip q field) (= (wt:wr-others-at q (car a) (car b) ms) 0))
        (progn
          (setq ts (mapcar '(lambda (p) (wt:dot (wt:v- p (wt:w-p1 a)) u))
                           (list (wt:w-p1 a) (wt:w-p2 a) (wt:w-p1 b) (wt:w-p2 b))))
          (list (wt:v+ (wt:w-p1 a) (wt:v* u (apply 'min ts)))
                (wt:v+ (wt:w-p1 a) (wt:v* u (apply 'max ts)))))))))

;; Audit and repair wall masters in field. Returns the transaction record (or nil).
(defun wt:wr-repair (field / net recs band reg adj amb ambs created walls i r w m p xs x nw k d
                            pass more c seg en rec checked msg joined pair keep a b known ff of)
  (setq net (wt:net-scan) adj 0 created 0 joined 0 of (wt:wr-open-faces (cadr net)))
  (foreach m (car net) (if (setq w (wt:wall-from-master m (cadr net))) (setq known (cons w known))))
  ;; 1. audit existing masters: faces decide the centerline and thickness
  (foreach m (car net)
    (if (wt:tw-seg-in-field (cadr m) (caddr m) field)
      (progn
        (setq ff (wt:wr-free-faces of known (car m) (wt:unit (wt:v- (caddr m) (cadr m))))
              band (wt:wr-band (wt:wr-offsets (cadr m) (caddr m) ff))
              reg (assoc (car m) *wt:reg*))
        (cond
          ((= (type band) 'LIST)
           (setq d (wt:v* (wt:perp (wt:unit (wt:v- (caddr m) (cadr m)))) (car band)))
           (setq recs (cons (list m (list (car m) (wt:v+ (cadr m) d) (wt:v+ (caddr m) d) (cadr band) "CENTER")) recs)))
          ((and reg                             ; known wall whose faces are gone: master wins,
                (not (wt:wr-contradicted-p (list (car m) (cadr m) (caddr m) (cadr reg) "CENTER") ff)))
           (setq recs (cons (list m (list (car m) (cadr m) (caddr m) (cadr reg) "CENTER")) recs)))
          ((or reg band) (setq ambs (cons (car m) ambs)))))))   ; faces disagree (e.g. partial STRETCH)
  (setq recs (reverse recs))
  ;; 2. an end that touched another wall master slides along its corrected line
  ;;    onto that master's corrected line (keeps L/T/X after re-centering)
  (setq i 0)
  (foreach r recs
    (setq m (car r) w (cadr r) nw w)
    (foreach side '(0 1)
      (setq p (if (= side 0) (cadr m) (caddr m)) xs nil)
      (foreach r2 recs
        (if (and (not (eq (car (car r2)) (car m)))
                 (wt:on-seg p (cadr (car r2)) (caddr (car r2)))
                 (setq x (wt:xline (wt:tw-end-pt w side) (wt:w-u w) (wt:w-p1 (cadr r2)) (wt:w-u (cadr r2)))))
          (setq xs (cons x xs))))
      (if xs
        (progn
          (setq k t)
          (foreach x xs (if (not (wt:peq x (car xs))) (setq k nil)))
          (if (and k (<= (wt:dist (car xs) (wt:tw-end-pt w side)) *wt:recon-max*))
            (setq nw (if (= side 0)
                       (list (car nw) (car xs) (caddr nw) (wt:w-thk nw) "CENTER")
                       (list (car nw) (cadr nw) (car xs) (wt:w-thk nw) "CENTER")))))))
    (setq recs (wt:setnth recs i (list m nw)) i (1+ i)))
  ;; 3. apply master corrections
  (wt:pend-begin)
  (foreach r recs
    (setq m (car r) w (cadr r))
    (if (or (not (wt:peq (cadr m) (wt:w-p1 w))) (not (wt:peq (caddr m) (wt:w-p2 w))))
      (progn
        (setq d (entget (car m)) adj (1+ adj)
              d (subst (cons 10 (wt:3d (wt:w-p1 w))) (assoc 10 d) d)
              d (subst (cons 11 (wt:3d (wt:w-p2 w))) (assoc 11 d) d))
        (wt:pend-modify d)))
    (wt:reg-add (car w) (wt:w-thk w) "CENTER")
    (setq walls (cons w walls)))
  ;; 4. rebuild missing masters from proven wall bands (later passes may use
  ;;    masters rebuilt by earlier ones as junction evidence)
  (setq pass 0 more t)
  (while (and more (< pass 3))
    (setq more nil pass (1+ pass) net (wt:net-scan) of (wt:wr-open-faces (cadr net)))
    (foreach f of
      (if (and (wt:tw-seg-in-field (cadr f) (caddr f) field)
               (> (wt:dist (cadr f) (caddr f)) *wt:tol*)
               (not (wt:any-owned f walls))
               (not (wt:face-owners f net)))
        (progn
          (setq c (wt:wr-candidate f of walls))
          (cond
            ((not c))
            ((= (car c) "AMBIG") (if (not (member (car f) ambs)) (setq ambs (cons (car f) ambs))))
            ((wt:wr-foreign-axis-p (cadr c) (caddr c) net walls)
             (if (not (member (car f) ambs)) (setq ambs (cons (car f) ambs))))
            (t
             (setq seg (cadr c))
             (if (and (wt:pip (wt:v* (wt:v+ (car seg) (cadr seg)) 0.5) field)
                      (not (wt:wr-overlaps-p seg (caddr c) walls))
                      (setq en (wt:pend-make (wt:mk-line (car seg) (cadr seg) (wt:cfg "AXIS_LAYER")))))
               (progn
                 (wt:reg-add en (caddr c) "CENTER")
                 (setq walls (cons (list en (car seg) (cadr seg) (caddr c) "CENTER") walls)
                       created (1+ created) more t)))))))))
  ;; 5. join broken collinear pieces of the same wall
  (setq more t)
  (while more
    (setq more nil pair nil net (wt:net-scan))
    (foreach a walls
      (foreach b walls
        (if (and (not pair) (not (eq (car a) (car b))))
          (if (setq seg (wt:wr-join a b (car net) field)) (setq pair (list a b seg))))))
    (if pair
      (progn
        (setq a (car pair) b (cadr pair) seg (caddr pair)
              d (entget (car a))
              d (subst (cons 10 (wt:3d (car seg))) (assoc 10 d) d)
              d (subst (cons 11 (wt:3d (cadr seg))) (assoc 11 d) d))
        (wt:pend-modify d)
        (wt:pend-erase (car b))
        (setq keep nil)
        (foreach w walls
          (cond ((eq (car w) (car b)))
                ((eq (car w) (car a)) (setq keep (cons (list (car a) (car seg) (cadr seg) (wt:w-thk a) "CENTER") keep)))
                (t (setq keep (cons w keep)))))
        (setq walls (reverse keep) joined (1+ joined) more t))))
  ;; 6. stale unclaimed A-WALL in the field beside the walls, then normalize + rebuild
  (setq net (wt:net-scan))
  (foreach f (cadr net)
    (if (and (wt:tw-seg-in-field (cadr f) (caddr f) field)
             (not (wt:any-owned f walls))
             (not (wt:face-owners f net))
             (not (member (car f) ambs))
             (wt:tw-near-master-p f walls))
      (wt:pend-erase (car f))))
  (if walls (wt:rebuild (reverse walls) nil))
  (setq rec (wt:pend-end)
        checked (length walls) amb (length ambs))
  (setq msg (strcat "\nWR: " (itoa checked) " wall(s) checked."))
  (if (and (= adj 0) (= created 0) (= joined 0))
    (setq msg (strcat msg " No axis repairs required."))
    (setq msg (strcat msg (if (> adj 0) (strcat " " (itoa adj) " axis/axes adjusted.") "")
                          (if (> created 0) (strcat " " (itoa created) " missing axis/axes rebuilt.") "")
                          (if (> joined 0) (strcat " " (itoa joined) " broken axis segment(s) joined.") ""))))
  (if (> amb 0) (setq msg (strcat msg " " (itoa amb) " ambiguous wall(s) skipped.")))
  (princ msg)
  rec)

(defun c:WWR (/ *error* c1 c2)
  (setq *error* wt:error)
  (wt:begin)
  (wt:layer "AXIS")
  (wt:layer "WALL")
  (if (and (setq c1 (getpoint "\nSpecify first corner of wall repair area: "))
           (setq c2 (getcorner c1 "\nSpecify opposite corner: ")))
    (wt:wr-repair (wt:tw-field c1 c2)))
  (wt:end))


;; Off-centre legacy AKD walls: X-AXIS masters whose two faces form a band that
;; does not centre on the master (older eccentric drawings, or a master moved by
;; hand) that are not recognised as centred walls. Returned as the centred
;; records WWR would create. Such walls are left to
;; WWR: TX/TW/WWD/WWE never treat their faces as ordinary geometry.
(defun wt:legacy-wall (m faces / band d)
  (setq band (wt:wr-band (wt:wr-offsets (cadr m) (caddr m) faces)))
  (if (and (= (type band) 'LIST) (> (abs (car band)) *wt:tol*))
    (progn
      (setq d (wt:v* (wt:perp (wt:unit (wt:v- (caddr m) (cadr m)))) (car band)))
      (list (car m) (wt:v+ (cadr m) d) (wt:v+ (caddr m) d) (cadr band) "CENTER"))))

;; Recognised centred walls are never legacy, and their own lines (faces, caps)
;; are ignored when looking for an off-centre wall's face pair.
(defun wt:legacy-walls (net / rec w out)
  (foreach m (car net) (if (setq w (wt:wall-from-master m (cadr net))) (setq rec (cons w rec))))
  (foreach m (car net)
    (if (and (not (assoc (car m) rec))
             (setq w (wt:legacy-wall m (wt:wr-free-faces (cadr net) rec (car m) (wt:unit (wt:v- (caddr m) (cadr m)))))))
      (setq out (cons w out))))
  out)

;;; ===================================================================
;;; 20. TX -- junction cleanup for ordinary LINE geometry (supertrim)
;;; ===================================================================
;;; Geometry-first: works on any selected LINEs (any layer, no AKD data).
;;; Masters on AXIS_LAYER are skipped. Nothing is modified until the plan is
;;; complete, and every decision reads the original snapshot only:
;;;   wt:tx-snapshot   READ     selected LINEs, sorted by geometry (order-free)
;;;   wt:tx-merge      PLAN 1   collinear duplicates / overlaps / small gaps -> one segment
;;;   wt:tx-partners   ANALYZE  parallel face pairs (wall-like double lines)
;;;   wt:tx-wall-phase PLAN 2   wall-like L (face miters) and T (branch into host)
;;;   wt:tx-line-phase PLAN 3   single-line endpoints: L (fillet 0) and T (trim/extend to host)
;;;   wt:tx-apply      MODIFY   entmod / entdel / entmake, recorded in *wt:pending*
;;; Plan state: *wt:tx-moves* ((id side) . point), *wt:tx-erase* (id ...),
;;; *wt:tx-cuts* ((id pa pb) ...) = remove the part of segment id between pa and pb.
;;; A segment is (id p1 p2 enames): p1 lexicographically before p2, enames = the
;;; surviving entity first, then collinear pieces merged into it.

(setq *wt:tx-nprot* 0)
(setq *wt:tx-tol-col* 0.5)    ; max offset between lines treated as collinear (drawing units)
(setq *wt:tx-tol-par* 1e-4)   ; |sin| below this = parallel
(setq *wt:tx-min-sin* 0.02)   ; |sin| below this (~1.1 deg) never forms a corner

;; --- snapshot and geometry on segment records ---

(defun wt:tx-lt (a b)
  (cond ((< (car a) (- (car b) *wt:tol*)) t)
        ((> (car a) (+ (car b) *wt:tol*)) nil)
        (t (< (cadr a) (- (cadr b) *wt:tol*)))))

(defun wt:tx-seg-lt (a b)
  (cond ((wt:tx-lt (cadr a) (cadr b)) t)
        ((wt:tx-lt (cadr b) (cadr a)) nil)
        (t (wt:tx-lt (caddr a) (caddr b)))))

(defun wt:tx-u (s) (wt:unit (wt:v- (caddr s) (cadr s))))
(defun wt:tx-len (s) (wt:dist (cadr s) (caddr s)))
(defun wt:tx-end (s side) (if (= side 0) (cadr s) (caddr s)))
(defun wt:tx-out (s side) (if (= side 0) (wt:v* (wt:tx-u s) -1.0) (wt:tx-u s)))
(defun wt:tx-sin (a b) (abs (wt:cross (wt:tx-u a) (wt:tx-u b))))
(defun wt:tx-inter (a b) (wt:xline (cadr a) (wt:tx-u a) (cadr b) (wt:tx-u b)))
(defun wt:tx-sta (p s) (wt:dot (wt:v- p (cadr s)) (wt:tx-u s)))
(defun wt:tx-off (p s) (wt:cross (wt:tx-u s) (wt:v- p (cadr s))))
(defun wt:tx-at (s st) (wt:v+ (cadr s) (wt:v* (wt:tx-u s) st)))
(defun wt:tx-seg (id) (nth id *wt:tx-segs*))
(defun wt:tx-iv (b a / s1 s2)            ; station interval of b on a's line
  (setq s1 (wt:tx-sta (cadr b) a) s2 (wt:tx-sta (caddr b) a))
  (list (min s1 s2) (max s1 s2)))

;; enames -> (raw-segments axis-count unsupported-count); raw = (ename p1 p2)
;; AKD wall geometry is protected from generic line repair: X-AXIS masters, and
;; A-WALL lines owned by a recognised AKD wall or by an off-centre legacy wall
;; (see wt:legacy-walls). Ownership is decided geometrically, not by layer.
(defun wt:tx-protected-p (e p q ctx / f)
  (setq f (list e p q))
  (or (wt:face-owners f (car ctx)) (wt:any-owned f (cadr ctx)) (wt:op-jamb-p p q)))

(defun wt:tx-snapshot (ens / d p q out nax nother ctx)
  (setq nax 0 nother 0 *wt:tx-nprot* 0
        ctx (list (wt:net-scan) nil))
  (setq ctx (list (car ctx) (wt:legacy-walls (car ctx))))
  (foreach e ens
    (setq d (entget e))
    (cond ((/= (cdr (assoc 0 d)) "LINE") (setq nother (1+ nother)))
          ((= (strcase (cdr (assoc 8 d))) (strcase (wt:cfg "AXIS_LAYER"))) (setq nax (1+ nax)))
          (t
           (setq p (wt:pt2 (cdr (assoc 10 d))) q (wt:pt2 (cdr (assoc 11 d))))
           (cond
             ((wt:peq p q))
             ((wt:tx-protected-p e p q ctx) (setq *wt:tx-nprot* (1+ *wt:tx-nprot*)))
             (t (setq out (cons (if (wt:tx-lt q p) (list e q p) (list e p q)) out)))))))
  (list (wt:sort (reverse out) 'wt:tx-seg-lt) nax nother))

;; --- PLAN 1: collinear merge ---

(defun wt:tx-colp (a b)
  (and (< (wt:tx-sin a b) *wt:tx-tol-par*)
       (<= (abs (wt:tx-off (cadr b) a)) *wt:tx-tol-col*)
       (<= (abs (wt:tx-off (caddr b) a)) *wt:tx-tol-col*)))

;; Collinear a b merge when they overlap/touch, or the gap is <= D and no
;; non-parallel line ends inside the gap within the gap width (an opening kept
;; by a branch, e.g. the inner host face of a double-line T).
(defun wt:tx-mergeable (a b raw / ib g0 g1 x st r)
  (if (wt:tx-colp a b)
    (progn
      (setq ib (wt:tx-iv b a))
      (cond ((> (car ib) (wt:tx-len a)) (setq g0 (wt:tx-len a) g1 (car ib)))
            ((< (cadr ib) 0.0) (setq g0 (cadr ib) g1 0.0)))
      (cond
        ((not g0)   ; touching / overlapping: locus = the shared interval
         (wt:tx-seg-in-field (wt:tx-at a (max 0.0 (car ib))) (wt:tx-at a (min (wt:tx-len a) (cadr ib)))))
        ((> (- g1 g0) *wt:tx-D*) nil)
        ((not (wt:tx-seg-in-field (wt:tx-at a g0) (wt:tx-at a g1))) nil)   ; gap outside the repair field
        ((<= (- g1 g0) *wt:tol*) t)
        (t
         (setq r t)
         (foreach k raw
           (if (and r (>= (wt:tx-sin a k) *wt:tx-min-sin*)
                    (setq x (wt:tx-inter a k))
                    (setq st (wt:tx-sta x a))
                    (>= st (- g0 *wt:tx-tol-col*)) (<= st (+ g1 *wt:tx-tol-col*))
                    (<= (min (wt:dist x (cadr k)) (wt:dist x (caddr k))) (- g1 g0)))
             (setq r nil)))
         r)))))

;; raw -> (segments merged-group-count). Groups are connected components over
;; the geometry-sorted snapshot, so the result does not depend on selection order.
(defun wt:tx-merge (raw / rest grp queue x keep id out merged surv lo hi st u ents)
  (setq rest raw id 0 merged 0)
  (while rest
    (setq grp (list (car rest)) queue (list (car rest)) rest (cdr rest))
    (while queue
      (setq x (car queue) queue (cdr queue) keep nil)
      (foreach o rest
        (if (wt:tx-mergeable x o raw)
          (setq grp (cons o grp) queue (append queue (list o)))
          (setq keep (cons o keep))))
      (setq rest (reverse keep)))
    (setq grp (reverse grp) surv nil lo nil hi nil ents nil)
    (foreach g grp
      (if (or (not surv) (> (wt:tx-len g) (+ (wt:tx-len surv) *wt:tol*))) (setq surv g)))
    (foreach g grp
      (foreach p (list (cadr g) (caddr g))
        (setq st (wt:tx-sta p surv))
        (if (or (not lo) (< st lo)) (setq lo st))
        (if (or (not hi) (> st hi)) (setq hi st)))
      (if (not (eq (car g) (car surv))) (setq ents (cons (car g) ents))))
    (setq u (wt:tx-u surv))
    (if (cdr grp)
      (progn
        (setq merged (1+ merged))
        (wt:dbg (list "TX COLLINEAR" (length grp) "lines -> segment" id "survivor" (car surv)))))
    (setq out (cons (list id (wt:v+ (cadr surv) (wt:v* u lo)) (wt:v+ (cadr surv) (wt:v* u hi))
                          (cons (car surv) (reverse ents)))
                    out)
          id (1+ id)))
  (list (reverse out) merged))

;; --- ANALYZE: connections and parallel pairs ---

;; ids of non-parallel segments passing through endpoint (id side)
(defun wt:tx-conn (ek / s p out)
  (setq s (wt:tx-seg (car ek)) p (wt:tx-end s (cadr ek)))
  (foreach k *wt:tx-segs*
    (if (and (/= (car k) (car s)) (not (member (car k) *wt:tx-erase*))
             (>= (wt:tx-sin s k) *wt:tx-min-sin*) (wt:on-seg p (cadr k) (caddr k)))
      (setq out (cons (car k) out))))
  out)

(defun wt:tx-conn-ok (ek allowed / r)
  (setq r t)
  (foreach c (wt:tx-conn ek) (if (not (member c allowed)) (setq r nil)))
  r)

;; Equal spacing on both sides of s: prefer the side whose line matches s's own
;; extent (overlap / longer length) by more than 0.25; similar extents stay unpaired.
(defun wt:tx-tie-side (s cands pos neg / k iv ov sc sp sn)
  (setq sp 0.0 sn 0.0)
  (foreach c cands
    (setq k (wt:tx-seg (cdr c)) iv (wt:tx-iv k s)
          ov (- (min (cadr iv) (wt:tx-len s)) (max (car iv) 0.0))
          sc (/ ov (max (wt:tx-len s) (wt:tx-len k))))
    (cond ((<= (abs (- (car c) pos)) *wt:tx-tol-col*) (setq sp (max sp sc)))
          ((<= (abs (- (car c) neg)) *wt:tx-tol-col*) (setq sn (max sn sc)))))
  (cond ((> sp (+ sn 0.25)) pos)
        ((> sn (+ sp 0.25)) neg)
        (t (wt:dbg (list "TX PAIR TIE segment" (car s) "equal spacing both sides, similar extents -> no wall partner"))
           nil)))

;; Parallel segments on the nearer side of s at spacing (tol-col, W], overlapping
;; at least half the shorter line. Collinear pieces at the same spacing all count
;; (a split host face). Equal spacing on both sides = no partner.
(defun wt:tx-partners (s / o iv ov cands pos neg side out)
  (foreach k (if (member (car s) *wt:tx-nopair*) nil *wt:tx-segs*)   ; cap lines never pair
    (if (and (/= (car k) (car s)) (< (wt:tx-sin s k) *wt:tx-tol-par*) (not (member (car k) *wt:tx-nopair*)))
      (progn
        (setq o (wt:tx-off (cadr k) s) iv (wt:tx-iv k s)
              ov (- (min (cadr iv) (wt:tx-len s)) (max (car iv) 0.0)))
        (if (and (> (abs o) *wt:tx-tol-col*) (<= (abs o) *wt:tx-W*)
                 (>= ov (* 0.5 (min (wt:tx-len s) (wt:tx-len k)))))
          (setq cands (cons (cons o (car k)) cands))))))
  (foreach c cands
    (if (> (car c) 0)
      (if (or (not pos) (< (car c) pos)) (setq pos (car c)))
      (if (or (not neg) (> (car c) neg)) (setq neg (car c)))))
  (setq side (cond ((and pos neg)
                    (cond ((< pos (- (- neg) *wt:tx-tol-col*)) pos)
                          ((< (- neg) (- pos *wt:tx-tol-col*)) neg)
                          (t (wt:tx-tie-side s cands pos neg))))
                   (pos) (neg)))
  (if side
    (foreach c cands (if (<= (abs (- (car c) side)) *wt:tx-tol-col*) (setq out (cons (cdr c) out)))))
  (reverse out))

;; Cap-like lines: both ends exactly on endpoints of two parallel lines f g, length =
;; their spacing -> ((k f g) ...). Such a line is excluded from wall pairing when f g
;; are a mutual pair without it (it closes a wall end); otherwise it pairs normally.
(setq *wt:tx-nopair* nil)
(defun wt:tx-cap-like (/ out f g pa pb)
  (foreach k *wt:tx-segs*
    (setq pa (cadr k) pb (caddr k) f nil g nil)
    (foreach x *wt:tx-segs*
      (if (and (/= (car x) (car k)) (>= (wt:tx-sin x k) *wt:tx-min-sin*))
        (progn
          (if (or (wt:peq pa (cadr x)) (wt:peq pa (caddr x))) (setq f (if f -1 (car x))))
          (if (or (wt:peq pb (cadr x)) (wt:peq pb (caddr x))) (setq g (if g -1 (car x)))))))
    (if (and f g (>= f 0) (>= g 0) (/= f g)
             (< (wt:tx-sin (wt:tx-seg f) (wt:tx-seg g)) *wt:tx-tol-par*)
             (<= (abs (- (wt:tx-len k) (abs (wt:tx-off (cadr (wt:tx-seg g)) (wt:tx-seg f))))) *wt:tx-tol-col*))
      (setq out (cons (list (car k) f g) out))))
  (reverse out))

;; partner table with cap lines resolved
(defun wt:tx-build-ptab (/ caps keep)
  (setq caps (wt:tx-cap-like) *wt:tx-nopair* (mapcar 'car caps))
  (setq *wt:tx-ptab* (mapcar '(lambda (s) (cons (car s) (wt:tx-partners s))) *wt:tx-segs*))
  (foreach c caps (if (wt:tx-pairp (cadr c) (caddr c)) (setq keep (cons (car c) keep))))
  (if (/= (length keep) (length caps))
    (setq *wt:tx-nopair* keep
          *wt:tx-ptab* (mapcar '(lambda (s) (cons (car s) (wt:tx-partners s))) *wt:tx-segs*)))
  (foreach k keep (wt:dbg (list "TX CAP LINE segment" k "closes a wall end (not a wall face)")))
  *wt:tx-ptab*)

(defun wt:tx-pairp (i j)
  (and (member j (cdr (assoc i *wt:tx-ptab*))) (member i (cdr (assoc j *wt:tx-ptab*)))))

(defun wt:tx-paired-p (i / r)
  (foreach j (cdr (assoc i *wt:tx-ptab*)) (if (wt:tx-pairp i j) (setq r t)))
  r)

;; all segments collinear with s (its line family), ids
(defun wt:tx-family (s / out)
  (foreach k *wt:tx-segs* (if (or (= (car k) (car s)) (wt:tx-colp s k)) (setq out (cons (car k) out))))
  (reverse out))

;; --- PLAN 2: wall-like junctions ---

;; Wall ends: for each mutual pair, the two face endpoints on the same side whose
;; stations differ by <= D + spacing. End = (key outward spacing cap-id)
;; key = (ia sa ib sb). cap = a selected line joining exactly those two endpoints.
(defun wt:tx-wall-ends (/ out b u sp sa sb pa pb cap o)
  (foreach a *wt:tx-segs*
    (foreach j (cdr (assoc (car a) *wt:tx-ptab*))
      (if (and (< (car a) j) (wt:tx-pairp (car a) j))
        (progn
          (setq b (wt:tx-seg j) u (wt:tx-u a) sp (abs (wt:tx-off (cadr b) a)))
          (foreach sa '(0 1)
            (setq o (wt:tx-out a sa) pa (wt:tx-end a sa)
                  sb (if (> (wt:dot (cadr b) o) (wt:dot (caddr b) o)) 0 1)
                  pb (wt:tx-end b sb) cap nil)
            (if (<= (abs (wt:dot (wt:v- pa pb) u)) (+ *wt:tx-D* sp))
              (progn
                (foreach c *wt:tx-segs*
                  (if (or (and (wt:peq (cadr c) pa) (wt:peq (caddr c) pb))
                          (and (wt:peq (cadr c) pb) (wt:peq (caddr c) pa)))
                    (setq cap (car c))))
                (setq out (cons (list (list (car a) sa j sb) o sp cap) out)))))))))
  (reverse out))

(defun wt:tx-end-keys (e) (list (list (car (car e)) (cadr (car e))) (list (caddr (car e)) (cadddr (car e)))))
(defun wt:tx-end-mid (e / k)
  (setq k (wt:tx-end-keys e))
  (wt:v* (wt:v+ (wt:tx-end (wt:tx-seg (car (car k))) (cadr (car k)))
                (wt:tx-end (wt:tx-seg (car (cadr k))) (cadr (cadr k)))) 0.5))

;; end e's face endpoint keys as (inner outer); inner = face nearer body direction bdir
(defun wt:tx-in-out (e bdir / k m fa fb qa qb)
  (setq k (wt:tx-end-keys e) m (wt:tx-end-mid e)
        fa (wt:tx-seg (car (car k))) fb (wt:tx-seg (car (cadr k)))
        qa (wt:tx-at fa (wt:tx-sta m fa)) qb (wt:tx-at fb (wt:tx-sta m fb)))
  (if (> (wt:dot qa bdir) (wt:dot qb bdir)) k (list (cadr k) (car k))))

;; signed move of endpoint ek to x along its own outward direction, or nil when
;; outside [lo hi] or the line would collapse
(defun wt:tx-mv (ek x lo hi / s m)
  (setq s (wt:tx-seg (car ek)) m (wt:dot (wt:v- x (wt:tx-end s (cadr ek))) (wt:tx-out s (cadr ek))))
  (if (and (>= m lo) (<= m hi) (> m (- *wt:tol* (wt:tx-len s)))) m))

(defun wt:tx-ids (e) (list (car (car e)) (caddr (car e))))

;; candidate = (type score moves cuts erases partner-key label)
;; Wall L: inner face meets inner face, outer meets outer; all four face ends
;; move by at most D + the other wall's spacing.
(defun wt:tx-wall-L (ea eb / ioa iob xin xout lim-a lim-b m1 m2 m3 m4 ka kb)
  (if (and (not (wt:any-member (wt:tx-ids ea) (wt:tx-ids eb)))
           (>= (wt:tx-sin (wt:tx-seg (car (car ea))) (wt:tx-seg (car (car eb)))) *wt:tx-min-sin*))
    (progn
      (setq ioa (wt:tx-in-out ea (wt:v* (cadr eb) -1.0)) iob (wt:tx-in-out eb (wt:v* (cadr ea) -1.0))
            xin (wt:tx-inter (wt:tx-seg (car (car ioa))) (wt:tx-seg (car (car iob))))
            xout (wt:tx-inter (wt:tx-seg (car (cadr ioa))) (wt:tx-seg (car (cadr iob))))
            lim-a (+ *wt:tx-D* (caddr eb)) lim-b (+ *wt:tx-D* (caddr ea))
            ka (append (wt:tx-ids eb) (if (cadddr ea) (list (cadddr ea))))
            kb (append (wt:tx-ids ea) (if (cadddr eb) (list (cadddr eb)))))
      (if (and xin xout
               (setq m1 (wt:tx-mv (car ioa) xin (- lim-a) lim-a))
               (setq m2 (wt:tx-mv (cadr ioa) xout (- lim-a) lim-a))
               (setq m3 (wt:tx-mv (car iob) xin (- lim-b) lim-b))
               (setq m4 (wt:tx-mv (cadr iob) xout (- lim-b) lim-b))
               (wt:tx-conn-ok (car ioa) ka) (wt:tx-conn-ok (cadr ioa) ka)
               (wt:tx-conn-ok (car iob) kb) (wt:tx-conn-ok (cadr iob) kb))
        (list "WALL-L" (max (abs m1) (abs m2))
              (list (cons (car ioa) xin) (cons (cadr ioa) xout) (cons (car iob) xin) (cons (cadr iob) xout))
              nil
              (append (if (cadddr ea) (list (cadddr ea))) (if (cadddr eb) (list (cadddr eb))))
              (car eb)
              (list "faces" (car (car ioa)) "<->" (car (car iob)) "/" (car (cadr ioa)) "<->" (car (cadr iob))
                    "spacing" (caddr ea) (caddr eb)))))))

;; Wall T: branch end eb into host pair (h k). The host face nearer the branch body
;; is the near face; branch faces terminate on it (extend <= D, trim <= D + host
;; spacing). The far face must continue D beyond the junction on both sides.
;; Near-face pieces lose the part between the branch faces.
(defun wt:tx-wall-T (eb h k / m o xh xk near far sph kb x1 x2 m1 m2 s1 s2 t1 t2 cov famn famf
                        allowed moves cuts erases iv sa hit local)
  (setq m (wt:tx-end-mid eb) o (cadr eb) kb (wt:tx-end-keys eb))
  (if (and (not (member (car h) (wt:tx-ids eb))) (not (member (car k) (wt:tx-ids eb)))
           (>= (wt:tx-sin h (wt:tx-seg (car (car kb)))) *wt:tx-min-sin*)
           (setq xh (wt:xline m o (cadr h) (wt:tx-u h)))
           (setq xk (wt:xline m o (cadr k) (wt:tx-u k))))
    (progn
      (if (< (wt:dot (wt:v- xh m) o) (wt:dot (wt:v- xk m) o)) (setq near h far k) (setq near k far h))
      (setq sph (abs (wt:tx-off (cadr k) h))
            x1 (wt:tx-inter (wt:tx-seg (car (car kb))) near)
            x2 (wt:tx-inter (wt:tx-seg (car (cadr kb))) near)
            famn (wt:tx-family near) famf (wt:tx-family far)
            allowed (append famn famf (if (cadddr eb) (list (cadddr eb)))))
      (setq t1 (min (wt:tx-sta x1 far) (wt:tx-sta x2 far)) t2 (max (wt:tx-sta x1 far) (wt:tx-sta x2 far)))
      (foreach f famf
        (setq iv (wt:tx-iv (wt:tx-seg f) far))
        (if (and (<= (car iv) (- t1 *wt:tx-D*)) (>= (cadr iv) (+ t2 *wt:tx-D*))) (setq cov t)))
      (setq s1 (min (wt:tx-sta x1 near) (wt:tx-sta x2 near)) s2 (max (wt:tx-sta x1 near) (wt:tx-sta x2 near)))
      ;; debug only: host near face within reach of this branch end
      (setq local (<= (abs (wt:dot (wt:v- (if (eq near h) xh xk) m) o)) (+ *wt:tx-D* sph)))
      (if (and cov
               (setq m1 (wt:tx-mv (car kb) x1 (- (+ *wt:tx-D* sph)) *wt:tx-D*))
               (setq m2 (wt:tx-mv (cadr kb) x2 (- (+ *wt:tx-D* sph)) *wt:tx-D*))
               (wt:tx-conn-ok (car kb) allowed) (wt:tx-conn-ok (cadr kb) allowed))
        (progn
          (setq moves (list (cons (car kb) x1) (cons (cadr kb) x2)))
          (if (cadddr eb) (setq erases (list (cadddr eb))))
          (foreach f famn
            (setq iv (wt:tx-iv (wt:tx-seg f) near)
                  sa (if (<= (wt:tx-sta (cadr (wt:tx-seg f)) near) (wt:tx-sta (caddr (wt:tx-seg f)) near)) 0 1))
            (if (and (>= (cadr iv) (- s1 *wt:tx-D*)) (<= (car iv) (+ s2 *wt:tx-D*)))
              (progn
                (setq hit t)
                (cond
                  ((and (>= (car iv) (- s1 *wt:tol*)) (<= (cadr iv) (+ s2 *wt:tol*)))
                   (setq erases (cons f erases)))
                  ((and (< (car iv) (- s1 *wt:tol*)) (> (cadr iv) (+ s2 *wt:tol*)))
                   (setq cuts (cons (list f (wt:tx-at near s1) (wt:tx-at near s2)) cuts)))
                  ((< (car iv) (- s1 *wt:tol*))
                   (if (> (abs (- (cadr iv) s1)) *wt:tol*)
                     (setq moves (cons (cons (list f (- 1 sa)) (wt:tx-at near s1)) moves))))
                  ((> (abs (- (car iv) s2)) *wt:tol*)
                   (setq moves (cons (cons (list f sa) (wt:tx-at near s2)) moves)))))))
          (if hit
            (list "WALL-T" (max (abs m1) (abs m2)) moves cuts erases nil
                  (list "host near" (car near) "far" (car far) "branch" (wt:tx-ids eb)
                        "spacing" (caddr eb) sph)
                  ;; debug detail: targets, movements, branch endpoints, host faces, opening
                  (list x1 x2 m1 m2 (wt:tx-end (wt:tx-seg (car (car kb))) (cadr (car kb)))
                        (wt:tx-end (wt:tx-seg (car (cadr kb))) (cadr (cadr kb)))
                        near far (wt:tx-at near s1) (wt:tx-at near s2)))
            (if local (wt:dbg (list "TX NOT T: branch" (wt:tx-ids eb) "host near" (car near) "far" (car far)
                                    "- no host near-face piece at the junction")))))
        (if local
          (wt:dbg (list "TX NOT T: branch" (wt:tx-ids eb) "host near" (car near) "far" (car far) "-"
                        (cond ((not cov) "host far face does not continue D beyond the junction on both sides")
                              ((not (and m1 m2)) "branch face end farther from host near face than allowed")
                              (t "branch face end already joined to a line outside the host")))))))))

;; candidates sorted by score -> (status candidate); unique only when the next
;; different candidate scores more than twice the best (+ tol-col)
(defun wt:tx-choose (cands / s)
  (setq s (wt:sort cands '(lambda (a b) (< (cadr a) (cadr b)))))
  (cond ((not s) nil)
        ((and (cdr s) (<= (cadr (cadr s)) (+ (* 2.0 (cadr (car s))) *wt:tx-tol-col*)))
         (list "AMBIG" s))
        (t (list "OK" (car s)))))

;; a candidate with any real geometry change
(defun wt:tx-changes-p (c / r)
  (if (or (cadddr c) (nth 4 c)) (setq r t))
  (foreach mv (caddr c)
    (if (not (wt:peq (cdr mv) (wt:tx-end (wt:tx-seg (car (car mv))) (cadr (car mv))))) (setq r t)))
  r)

;; add a candidate to the plan unless it moves an endpoint already planned elsewhere
(defun wt:tx-plan-add (c / ok old)
  (setq ok t)
  (foreach mv (caddr c)
    (if (and (setq old (assoc (car mv) *wt:tx-moves*)) (not (wt:peq (cdr old) (cdr mv)))) (setq ok nil)))
  (if ok
    (progn
      (foreach mv (caddr c) (if (not (assoc (car mv) *wt:tx-moves*)) (setq *wt:tx-moves* (cons mv *wt:tx-moves*))))
      (setq *wt:tx-cuts* (append *wt:tx-cuts* (cadddr c)))
      (foreach e (nth 4 c) (if (not (member e *wt:tx-erase*)) (setq *wt:tx-erase* (cons e *wt:tx-erase*))))))
  ok)

;; debug report of a WALL-T candidate: branch movement and host near-face opening
;; are separate questions (movement 0 can still need an opening)
(defun wt:tx-dbg-T (c / d lb nm)
  (if (and *wt-debug* (= (car c) "WALL-T") (setq d (nth 7 c)))
    (progn
      (setq lb (nth 6 c) nm (- (length (caddr c)) 2))
      (wt:dbg (list "TX WALL T  HOST: near face" (car (nth 6 d)) "far face" (car (nth 7 d)) "spacing" (wt:fmt (nth 8 lb))))
      (wt:dbg (list "  BRANCH: faces" (nth 5 lb) "spacing" (wt:fmt (nth 7 lb))))
      (wt:dbg (list "  BRANCH FACE B1 endpoint" (nth 4 d) "target" (car d) "movement" (wt:fmt (caddr d))))
      (wt:dbg (list "  BRANCH FACE B2 endpoint" (nth 5 d) "target" (cadr d) "movement" (wt:fmt (cadddr d))))
      (wt:dbg (list "  HOST NEAR FACE: segment" (car (nth 6 d)) (cadr (nth 6 d)) "->" (caddr (nth 6 d))))
      (wt:dbg (list "  REQUIRED OPENING:" (nth 8 d) "->" (nth 9 d)))
      (wt:dbg (list "  CURRENT OPENING:"
                    (cond ((cadddr c) "NO (near face continuous)")
                          ((or (> nm 0) (nth 4 c)) "PARTIAL (near-face pieces need normalizing)")
                          (t "YES"))))
      (wt:dbg (list "  ACTION:"
                    (if (cadddr c) "SPLIT HOST NEAR FACE" "")
                    (if (> nm 0) "NORMALIZE NEAR-FACE PIECES" "")
                    (if (nth 4 c) "ERASE PIECES/CAP" "")
                    (if (or (> (abs (caddr d)) *wt:tol*) (> (abs (cadddr d)) *wt:tol*)) "MOVE BRANCH FACES" "")
                    (if (wt:tx-changes-p c) "" "NONE (already clean)"))))))

;; returns (repaired ambiguous)
(defun wt:tx-wall-phase (/ ends choices cands ch pc done n amb hosts key)
  (setq ends (wt:tx-unclaimed (wt:tx-wall-ends)) n 0 amb 0)   ; cross nodes excluded
  (foreach e ends
    (setq cands nil hosts nil)
    (foreach e2 ends (if (and (not (equal e e2)) (setq ch (wt:tx-wall-L e e2))) (setq cands (cons ch cands))))
    (foreach a *wt:tx-segs*
      (foreach j (cdr (assoc (car a) *wt:tx-ptab*))
        (if (and (< (car a) j) (wt:tx-pairp (car a) j) (setq ch (wt:tx-wall-T e a (wt:tx-seg j))))
          (progn
            ;; host identity = its two line families, in either order (a split face is reached via each piece)
            (setq key (list (min (car (wt:tx-family a)) (car (wt:tx-family (wt:tx-seg j))))
                            (max (car (wt:tx-family a)) (car (wt:tx-family (wt:tx-seg j))))))
            (if (not (member key hosts)) (setq hosts (cons key hosts) cands (cons ch cands)))))))
    (setq choices (cons (cons (car e) (wt:tx-choose cands)) choices)))
  (foreach e ends
    (setq ch (cdr (assoc (car e) choices)))
    (cond
      ((not ch))
      ((= (car ch) "AMBIG")
       (if (wt:tx-pt-in-field (wt:tx-end-mid e)) (setq amb (1+ amb)))
       (wt:dbg (list "TX AMBIGUOUS WALL END" (car e)))
       (foreach c (cadr ch) (wt:dbg (list "  candidate" (car c) "score" (wt:fmt (cadr c)) (nth 6 c)))))
      ((member (car e) done))
      ((and (= (car (cadr ch)) "WALL-L")
            (not (and (setq pc (cdr (assoc (nth 5 (cadr ch)) choices))) (= (car pc) "OK")
                      (= (car (cadr pc)) "WALL-L") (equal (nth 5 (cadr pc)) (car e)))))
       (wt:dbg (list "TX WALL END" (car e) "L partner does not agree, unchanged")))
      ((not (wt:tx-changes-p (cadr ch)))
       (wt:tx-dbg-T (cadr ch))
       (setq done (cons (car e) done)))
      ((not (wt:tx-cand-in-field (cadr ch)))
       (wt:dbg (list "TX WALL END" (car e) "junction outside repair field, unchanged")))
      ((wt:tx-plan-add (cadr ch))
       (wt:tx-dbg-T (cadr ch))
       (setq n (1+ n) done (cons (car e) done))
       (if (nth 5 (cadr ch)) (setq done (cons (nth 5 (cadr ch)) done)))
       (wt:dbg (list "TX WALL PAIR end" (car e) "junction" (car (cadr ch)) (nth 6 (cadr ch))))
       (foreach mv (caddr (cadr ch)) (wt:dbg (list "  ACTION segment" (car (car mv)) "P" (1+ (cadr (car mv))) "->" (cdr mv)))))
      (t (setq amb (1+ amb)) (wt:dbg (list "TX WALL END" (car e) "conflicts with an earlier repair, unchanged")))))
  (list n amb))

;; --- PLAN 2a: double-line CROSS (+), planned before wall L/T ---
;; *wt:tx-claims* ((family-ids ref-seg s1 s2) ...): face stretches owned by a
;; recognised cross. Endpoints inside [s1-D, s2+D] are not reinterpreted by the
;; wall L/T or single-line rules.

;; distinct wall pairs: (key face1 face2 family1 family2), key = sorted family ids
(defun wt:tx-walls (/ out fa fb key)
  (foreach a *wt:tx-segs*
    (foreach j (cdr (assoc (car a) *wt:tx-ptab*))
      (if (and (< (car a) j) (wt:tx-pairp (car a) j))
        (progn
          (setq fa (wt:tx-family a) fb (wt:tx-family (wt:tx-seg j))
                key (list (min (car fa) (car fb)) (max (car fa) (car fb))))
          (if (not (assoc key out)) (setq out (cons (list key a (wt:tx-seg j) fa fb) out)))))))
  (reverse out))

;; station extent (lo hi) of a line family on ref
(defun wt:tx-fam-ext (fam ref / iv lo hi)
  (foreach f fam
    (setq iv (wt:tx-iv (wt:tx-seg f) ref))
    (if (or (not lo) (< (car iv) lo)) (setq lo (car iv)))
    (if (or (not hi) (> (cadr iv) hi)) (setq hi (cadr iv))))
  (list lo hi))

;; some piece of the family reaches [s1-D, s2+D]
(defun wt:tx-fam-near (fam ref s1 s2 / r iv)
  (foreach f fam
    (setq iv (wt:tx-iv (wt:tx-seg f) ref))
    (if (and (>= (cadr iv) (- s1 *wt:tx-D*)) (<= (car iv) (+ s2 *wt:tx-D*))) (setq r t)))
  r)

;; Interval clipping of a collinear family against the opening (s1 s2) on ref:
;; inside -> erase, spans -> cut, crosses/stops short of s1 or s2 (within D) -> end moved there.
;; Returns (moves cuts erases).
(defun wt:tx-clip-family (fam ref s1 s2 / iv sa moves cuts erases)
  (foreach f fam
    (setq iv (wt:tx-iv (wt:tx-seg f) ref)
          sa (if (<= (wt:tx-sta (cadr (wt:tx-seg f)) ref) (wt:tx-sta (caddr (wt:tx-seg f)) ref)) 0 1))
    (if (and (>= (cadr iv) (- s1 *wt:tx-D*)) (<= (car iv) (+ s2 *wt:tx-D*)))
      (cond
        ((and (>= (car iv) (- s1 *wt:tol*)) (<= (cadr iv) (+ s2 *wt:tol*)))
         (setq erases (cons f erases)))
        ((and (< (car iv) (- s1 *wt:tol*)) (> (cadr iv) (+ s2 *wt:tol*)))
         (setq cuts (cons (list f (wt:tx-at ref s1) (wt:tx-at ref s2)) cuts)))
        ((< (car iv) (- s1 *wt:tol*))
         (if (> (abs (- (cadr iv) s1)) *wt:tol*)
           (setq moves (cons (cons (list f (- 1 sa)) (wt:tx-at ref s1)) moves))))
        ((> (abs (- (car iv) s2)) *wt:tol*)
         (setq moves (cons (cons (list f sa) (wt:tx-at ref s2)) moves))))))
  (list moves cuts erases))

;; Wall wa x wall wb -> (candidate claims center) or nil.
;; CROSS: non-parallel, all four face families reach the overlap, and every face
;; of BOTH walls extends more than D beyond the overlap on BOTH sides.
(defun wt:tx-wall-cross (wa wb / faces rows ok lo-ok hi-ok x1 x2 st1 st2 ext row moves cuts erases r claims cen why)
  (if (and (>= (wt:tx-sin (cadr wa) (cadr wb)) *wt:tx-min-sin*)
           (not (wt:any-member (append (cadddr wa) (nth 4 wa)) (append (cadddr wb) (nth 4 wb)))))
    (progn
      (setq faces (list (list (cadr wa) (cadddr wa) (cadr wb) (caddr wb) "A")
                        (list (caddr wa) (nth 4 wa) (cadr wb) (caddr wb) "A")
                        (list (cadr wb) (cadddr wb) (cadr wa) (caddr wa) "B")
                        (list (caddr wb) (nth 4 wb) (cadr wa) (caddr wa) "B"))
            ok t cen '(0.0 0.0))
      ;; row = (face fam s1 s2 lo hi wall p1 p2)
      (foreach f faces
        (setq x1 (wt:tx-inter (car f) (caddr f)) x2 (wt:tx-inter (car f) (cadddr f))
              st1 (wt:tx-sta x1 (car f)) st2 (wt:tx-sta x2 (car f))
              ext (wt:tx-fam-ext (cadr f) (car f))
              cen (wt:v+ cen (wt:v* x1 0.125)) cen (wt:v+ cen (wt:v* x2 0.125)))
        (if (> st1 st2) (setq r st1 st1 st2 st2 r r x1 x1 x2 x2 r))
        (if (not (wt:tx-fam-near (cadr f) (car f) st1 st2)) (setq ok nil))
        (setq rows (cons (list (car f) (cadr f) st1 st2 (car ext) (cadr ext) (nth 4 f) x1 x2) rows)))
      (setq rows (reverse rows))
      (if ok
        (progn
          (foreach w '("A" "B")
            (setq lo-ok t hi-ok t)
            (foreach row rows
              (if (= (nth 6 row) w)
                (progn
                  (if (not (< (nth 4 row) (- (nth 2 row) *wt:tx-D*))) (setq lo-ok nil))
                  (if (not (> (nth 5 row) (+ (nth 3 row) *wt:tx-D*))) (setq hi-ok nil)))))
            (wt:dbg (list "TX CROSS CHECK wall" w "faces" (car (car (car (if (= w "A") rows (cddr rows)))))
                          (car (car (car (if (= w "A") (cdr rows) (cdddr rows)))))
                          "before =" (if lo-ok "YES" "NO") "after =" (if hi-ok "YES" "NO")))
            (if (not (and lo-ok hi-ok))
              (setq why (strcat "NOT CROSS: Wall " w " does not continue beyond intersection on both sides."))))
          (if why
            (progn (wt:dbg (list "TX" why)) nil)
            (progn
              (wt:dbg (list "TX WALL CROSS  A faces" (car (cadr wa)) (car (caddr wa))
                            "spacing" (wt:fmt (abs (wt:tx-off (cadr (caddr wa)) (cadr wa))))
                            "  B faces" (car (cadr wb)) (car (caddr wb))
                            "spacing" (wt:fmt (abs (wt:tx-off (cadr (caddr wb)) (cadr wb))))))
              (foreach row rows
                (setq r (wt:tx-clip-family (cadr row) (car row) (nth 2 row) (nth 3 row))
                      moves (append moves (car r)) cuts (append cuts (cadr r)) erases (append erases (caddr r))
                      claims (cons (list (cadr row) (car row) (nth 2 row) (nth 3 row)) claims))
                (wt:dbg (list "  face" (car (car row)) "intersections" (nth 7 row) (nth 8 row)
                              "remove interval" (nth 7 row) "->" (nth 8 row))))
              (list (list "WALL-CROSS" 0.0 moves cuts erases nil nil) claims cen))))))))

;; endpoint inside a claimed cross stretch
(defun wt:tx-claimed-p (ek / s p r st)
  (setq s (wt:tx-seg (car ek)) p (wt:tx-end s (cadr ek)))
  (foreach c *wt:tx-claims*
    (if (member (car ek) (car c))
      (progn
        (setq st (wt:tx-sta p (cadr c)))
        (if (and (>= st (- (caddr c) *wt:tx-D*)) (<= st (+ (cadddr c) *wt:tx-D*))) (setq r t)))))
  r)

(defun wt:tx-unclaimed (ends / out)
  (foreach e ends
    (if (not (or (wt:tx-claimed-p (car (wt:tx-end-keys e))) (wt:tx-claimed-p (cadr (wt:tx-end-keys e)))))
      (setq out (cons e out))))
  (reverse out))

;; returns (repaired ambiguous). Two crosses sharing a wall whose centres are
;; closer than D + both spacings compete for one node (3+ walls) -> ambiguous.
(defun wt:tx-cross-phase (/ walls rest c cands bad n amb lim)
  (setq walls (wt:tx-walls) n 0 amb 0)
  (while walls
    (foreach wb (cdr walls)
      (if (setq c (wt:tx-wall-cross (car walls) wb))
        (setq cands (cons (append c (list (car (car walls)) (car wb)
                                          (+ (abs (wt:tx-off (cadr (caddr (car walls))) (cadr (car walls))))
                                             (abs (wt:tx-off (cadr (caddr wb)) (cadr wb))))))
                          cands))))
    (setq walls (cdr walls)))
  (setq cands (reverse cands))
  (foreach c cands
    (foreach c2 cands
      (if (and (not (eq c c2))
               (or (equal (nth 3 c) (nth 3 c2)) (equal (nth 3 c) (nth 4 c2))
                   (equal (nth 4 c) (nth 3 c2)) (equal (nth 4 c) (nth 4 c2)))
               (< (wt:dist (caddr c) (caddr c2)) (+ *wt:tx-D* (max (nth 5 c) (nth 5 c2)))))
        (setq bad (cons c bad)))))
  (foreach c cands
    (cond
      ((member c bad)
       (if (wt:tx-pt-in-field (caddr c)) (setq amb (1+ amb)))
       (wt:dbg (list "TX AMBIGUOUS CROSS at" (caddr c) "more than two wall families meet, unchanged")))
      ((not (wt:tx-changes-p (car c)))
       (setq *wt:tx-claims* (append *wt:tx-claims* (cadr c)))
       (wt:dbg (list "TX CROSS at" (caddr c) "already clean")))
      ((not (wt:tx-pt-in-field (caddr c)))
       (setq *wt:tx-claims* (append *wt:tx-claims* (cadr c)))
       (wt:dbg (list "TX CROSS at" (caddr c) "outside repair field, unchanged")))
      ((wt:tx-plan-add (car c))
       (setq *wt:tx-claims* (append *wt:tx-claims* (cadr c)) n (1+ n))
       (wt:dbg (list "TX CROSS at" (caddr c) "CLASSIFICATION CROSS, repaired")))
      (t (setq amb (1+ amb)) (wt:dbg (list "TX CROSS at" (caddr c) "conflicts with an earlier repair, unchanged")))))
  (list n amb))

;; --- PLAN 3: single-line endpoints ---

;; candidates for free endpoint ek of an unpaired segment:
;;  L = (x within D of both free ends; both move), T = endpoint moves onto host line
(defun wt:tx-line-cands (ek / s p o len x m st lj sj dj jk out same)
  (setq s (wt:tx-seg (car ek)) p (wt:tx-end s (cadr ek)) o (wt:tx-out s (cadr ek)) len (wt:tx-len s))
  (foreach j *wt:tx-segs*
    (if (and (/= (car j) (car s)) (not (member (car j) *wt:tx-erase*))
             (>= (wt:tx-sin s j) *wt:tx-min-sin*)
             (setq x (wt:tx-inter s j))
             (setq m (wt:dot (wt:v- x p) o))
             (<= (abs m) *wt:tx-D*) (> m (- *wt:tol* len)))
      (progn
        (setq st (wt:tx-sta x j) lj (wt:tx-len j) sj (if (< st (/ lj 2.0)) 0 1)
              dj (if (= sj 0) (- st) (- st lj)) jk (list (car j) sj))
        (cond
          ((and (<= (abs dj) *wt:tx-D*) (> dj (- *wt:tol* lj))
                (not (wt:tx-paired-p (car j))) (not (assoc jk *wt:tx-moves*)) (not (wt:tx-conn jk)))
           (setq out (cons (list "L" (abs m) x jk) out)))
          ((and (>= st (- *wt:tol*)) (<= st (+ lj *wt:tol*)))
           (setq out (cons (list "T" (abs m) x (car j)) out)))))))
  ;; candidates at the same point are one junction (T wins; several L partners = ambiguous)
  (setq same nil)
  (foreach c out
    (if (not (wt:tx-point-in (caddr c) same))
      (setq same (cons (wt:tx-collapse (caddr c) out) same))))
  same)

(defun wt:tx-point-in (x cands / r) (foreach c cands (if (wt:peq x (caddr c)) (setq r t))) r)

;; all candidates at x -> one: a T if any, else the L if exactly one, else "LL" (ambiguous)
(defun wt:tx-collapse (x cands / ts ls)
  (foreach c cands
    (if (wt:peq x (caddr c)) (if (= (car c) "T") (setq ts (cons c ts)) (setq ls (cons c ls)))))
  (cond (ts (car ts)) ((cdr ls) (list "LL" (cadr (car ls)) x nil)) (t (car ls))))

;; debug: an unpaired line ending on a wall face is handled as a single line,
;; where touching = already connected = unchanged
(defun wt:tx-dbg-touch (ek / hits)
  (if *wt-debug*
    (progn
      (foreach c (wt:tx-conn ek) (if (wt:tx-paired-p c) (setq hits (cons c hits))))
      (if hits
        (wt:dbg (list "TX NOTE segment" (car ek) "P" (1+ (cadr ek)) "ends on wall face" hits
                      "but has no wall partner (candidates" (cdr (assoc (car ek) *wt:tx-ptab*))
                      ") -> single-line rules: touching = unchanged"))))))

;; returns (repaired ambiguous)
(defun wt:tx-line-phase (/ ek cands ch choices ts n amb pc s p q st host r)
  (setq n 0 amb 0)
  (foreach s *wt:tx-segs*
    (if (and (not (member (car s) *wt:tx-erase*)) (not (wt:tx-paired-p (car s))))
      (foreach side '(0 1)
        (setq ek (list (car s) side))
        (wt:tx-dbg-touch ek)
        (if (and (not (assoc ek *wt:tx-moves*)) (not (wt:tx-conn ek)) (not (wt:tx-claimed-p ek))
                 (setq cands (wt:tx-line-cands ek)))
          (progn
            (setq ch (wt:tx-choose cands))
            (if (or (= (car ch) "AMBIG") (= (car (cadr ch)) "LL"))
              (progn
                (if (wt:tx-pt-in-field (wt:tx-end s side)) (setq amb (1+ amb)))
                (wt:dbg (list "TX AMBIGUOUS ENDPOINT segment" (car s) "P" (1+ side) (wt:tx-end s side)))
                (foreach c (if (= (car ch) "AMBIG") (cadr ch) (list (cadr ch)))
                  (wt:dbg (list "  candidate" (car c) "point" (caddr c) "distance" (wt:fmt (cadr c)) "with" (cadddr c)))))
              (setq choices (cons (cons ek (cadr ch)) choices))))))))
  (setq choices (reverse choices))
  (foreach c choices
    (setq ch (cdr c))
    (cond
      ((assoc (car c) *wt:tx-moves*))
      ((= (car ch) "L")
       (if (and (wt:tx-pt-in-field (caddr ch))
                (setq pc (cdr (assoc (cadddr ch) choices))) (= (car pc) "L")
                (equal (cadddr pc) (car c)) (wt:peq (caddr pc) (caddr ch)))
         (progn
           (setq *wt:tx-moves* (cons (cons (car c) (caddr ch)) (cons (cons (cadddr ch) (caddr ch)) *wt:tx-moves*))
                 n (1+ n))
           (wt:dbg (list "TX L segment" (car (car c)) "P" (1+ (cadr (car c))) "+ segment" (car (cadddr ch))
                         "P" (1+ (cadr (cadddr ch))) "-> intersection" (caddr ch))))
         (wt:dbg (list "TX L endpoint" (car c) "partner does not agree, unchanged"))))
      (t (setq ts (cons c ts)))))
  ;; T: host must still contain the point after all planned moves
  (foreach c (reverse ts)
    (setq ch (cdr c) host (wt:tx-seg (cadddr ch))
          p (cond ((cdr (assoc (list (car host) 0) *wt:tx-moves*))) ((cadr host)))
          q (cond ((cdr (assoc (list (car host) 1) *wt:tx-moves*))) ((caddr host))))
    (if (and (not (member (car host) *wt:tx-erase*)) (not (assoc (car c) *wt:tx-moves*))
             (wt:tx-pt-in-field (caddr ch))
             (wt:on-seg (caddr ch) p q))
      (progn
        (setq *wt:tx-moves* (cons (cons (car c) (caddr ch)) *wt:tx-moves*) n (1+ n))
        (wt:dbg (list "TX T segment" (car (car c)) "P" (1+ (cadr (car c))) "-> host" (car host) "at" (caddr ch))))
      (wt:dbg (list "TX T endpoint" (car c) "host changed by another repair, unchanged"))))
  (list n amb))

;; --- repair field (TX / TW window). nil = selection mode: everything eligible ---
;; The field decides WHICH junctions may be repaired, never which part of a line exists.

(setq *wt:tx-field* nil *wt:tx-nested* nil *wt:tx-adds* nil)

(defun wt:tx-pt-in-field (p / r e)
  (cond ((not *wt:tx-field*) t)
        ((wt:pip p *wt:tx-field*) t)
        (t
         (setq e (last *wt:tx-field*))
         (foreach c *wt:tx-field*
           (if (<= (wt:seg-dist p e c) *wt:tx-tol-col*) (setq r t))
           (setq e c))
         r)))

(defun wt:tx-seg-in-field (a b)
  (cond ((not *wt:tx-field*) t)
        ((wt:peq a b) (wt:tx-pt-in-field a))
        (t (wt:tw-seg-near-field a b *wt:tx-field* *wt:tx-tol-col*))))

(defun wt:tx-sum (pts / r) (setq r '(0.0 0.0)) (foreach p pts (setq r (wt:v+ r p))) r)

;; junction locus of a planned candidate = centroid of its target and cut points
(defun wt:tx-cand-in-field (c / pts)
  (foreach mv (caddr c) (setq pts (cons (cdr mv) pts)))
  (foreach ct (cadddr c) (setq pts (cons (cadr ct) (cons (caddr ct) pts))))
  (if pts (wt:tx-pt-in-field (wt:v* (wt:tx-sum pts) (/ 1.0 (length pts)))) t))

;; --- PLAN 4: wall-end caps (field mode only, after ALL junction planning) ---
;; *wt:tx-adds* ((pa pb source-ename) ...) = new cap LINEs copying the source face's properties.

;; final (planned) endpoint
(defun wt:tx-fp (id side)
  (cond ((cdr (assoc (list id side) *wt:tx-moves*))) ((wt:tx-end (wt:tx-seg id) side))))

;; why face endpoint ek is NOT a free wall end in the final topology (nil = free).
;; "LINE" = touches only unpaired lines (end left alone, an existing cap is kept).
(defun wt:tx-end-reason (ek skip / p s r)
  (setq s (wt:tx-seg (car ek)) p (wt:tx-fp (car ek) (cadr ek)))
  (cond
    ((assoc ek *wt:tx-moves*) "JUNCTION REPAIRED IN THIS RUN")
    ((wt:tx-claimed-p ek) "CROSS JUNCTION")
    (t
     (foreach k *wt:tx-segs*
       (if (and (not r) (/= (car k) (car s)) (not (member (car k) *wt:tx-erase*)) (not (member (car k) skip))
                (>= (wt:tx-sin s k) *wt:tx-min-sin*)
                (wt:on-seg p (wt:tx-fp (car k) 0) (wt:tx-fp (car k) 1)))
         (setq r (if (wt:tx-paired-p (car k)) "WALL JUNCTION" "LINE"))))
     r)))

;; existing cap of the end pa-pb: ("EXACT" id) | ("NEAR" id side-at-pa side-at-pb) | "AMBIG" | nil.
;; NEAR = an unpaired line on the pa-pb line, no longer than spacing + 2D, each end within D.
(defun wt:tx-find-cap (pa pb faces sp / out a b uk ka)
  (foreach k *wt:tx-segs*
    (if (and (not (member (car k) faces)) (not (member (car k) *wt:tx-erase*)) (not (wt:tx-paired-p (car k))))
      (progn
        (setq a (wt:tx-fp (car k) 0) b (wt:tx-fp (car k) 1) uk (wt:tx-u k))
        (cond
          ((or (and (wt:peq a pa) (wt:peq b pb)) (and (wt:peq a pb) (wt:peq b pa)))
           (setq out (cons (list "EXACT" (car k)) out)))
          ((and (<= (abs (wt:cross uk (wt:v- pa a))) *wt:tx-tol-col*)
                (<= (abs (wt:cross uk (wt:v- pb a))) *wt:tx-tol-col*)
                (<= (wt:dist a b) (+ sp (* 2.0 *wt:tx-D*))))
           (setq ka (if (<= (wt:dist a pa) (wt:dist b pa)) 0 1))
           (if (and (<= (wt:dist (wt:tx-fp (car k) ka) pa) *wt:tx-D*)
                    (<= (wt:dist (wt:tx-fp (car k) (- 1 ka)) pb) *wt:tx-D*))
             (setq out (cons (list "NEAR" (car k) ka (- 1 ka)) out))))))))
  (cond ((not out) nil)
        ((not (cdr out)) (car out))
        ((assoc "EXACT" out))
        (t "AMBIG")))

(defun wt:tx-dbg-cap (key free why cap act)
  (wt:dbg (list "TX WALL END  wall faces" (car key) "/" (caddr key) "end P" (1+ (cadr key))
                "free:" free (if why (strcat "reason: " why) "") "cap:" cap "action:" act)))

;; Free end = both faces of a mutual pair end at the same station (within tol-col),
;; neither end is part of a junction, and the end lies in the repair field.
;; Missing cap -> create; misconnected cap -> normalize; cap at a junction end -> erase.
;; Returns the number of cap changes.
(defun wt:tx-cap-phase (/ n b u sp o sb pa pb faces c capid ra rb why key)
  (setq n 0)
  (foreach a *wt:tx-segs*
    (foreach j (cdr (assoc (car a) *wt:tx-ptab*))
      (if (and (< (car a) j) (wt:tx-pairp (car a) j)
               (not (member (car a) *wt:tx-erase*)) (not (member j *wt:tx-erase*)))
        (progn
          (setq b (wt:tx-seg j) u (wt:tx-u a) sp (abs (wt:tx-off (cadr b) a)) faces (list (car a) j))
          (foreach sa '(0 1)
            (setq o (wt:tx-out a sa)
                  sb (if (> (wt:dot (wt:tx-fp j 0) o) (wt:dot (wt:tx-fp j 1) o)) 0 1)
                  pa (wt:tx-fp (car a) sa) pb (wt:tx-fp j sb) key (list (car a) sa j sb))
            (if (and (<= (abs (wt:dot (wt:v- pa pb) u)) *wt:tx-tol-col*)
                     (<= (abs (- (abs (wt:cross u (wt:v- pb pa))) sp)) *wt:tx-tol-col*)
                     (wt:tx-pt-in-field (wt:v* (wt:v+ pa pb) 0.5)))
              (progn
                (setq c (wt:tx-find-cap pa pb faces sp)
                      capid (if (listp c) (cadr c))
                      ra (wt:tx-end-reason (list (car a) sa) (if capid (list capid)))
                      rb (wt:tx-end-reason (list j sb) (if capid (list capid)))
                      why (if ra ra rb))
                (cond
                  (why
                   (if (and capid (/= why "LINE"))
                     (progn
                       (setq *wt:tx-erase* (cons capid *wt:tx-erase*) n (1+ n))
                       (wt:tx-dbg-cap key "NO" why "YES" "ERASE STALE CAP"))
                     (wt:tx-dbg-cap key "NO" why (if capid "YES" "NO") "NONE")))
                  ((= c "AMBIG") (wt:tx-dbg-cap key "YES" nil "AMBIGUOUS" "NONE"))
                  ((and c (= (car c) "EXACT")) (wt:tx-dbg-cap key "YES" nil "EXISTS" "NONE"))
                  (c
                   (if (wt:tx-plan-add (list "CAP" 0.0 (list (cons (list capid (caddr c)) pa) (cons (list capid (cadddr c)) pb))
                                             nil nil))
                     (progn (setq n (1+ n)) (wt:tx-dbg-cap key "YES" nil "MISCONNECTED" "NORMALIZE"))
                     (wt:tx-dbg-cap key "YES" nil "MISCONNECTED" "NONE (conflict)")))
                  (t
                   (setq *wt:tx-adds* (cons (list pa pb (car (cadddr a))) *wt:tx-adds*) n (1+ n))
                   (wt:tx-dbg-cap key "YES" nil "MISSING" "CREATE"))))))))))
  n)

;; --- MODIFY ---

;; new LINE copying display properties of en, recorded
(defun wt:tx-copy (en a b z / out)
  (foreach g (entget en)
    (if (member (car g) '(0 8 6 62 48 370 39 420 430 440 284 60 67 210)) (setq out (cons g out))))
  (if (entmake (append (reverse out) (list (cons 10 (list (car a) (cadr a) z)) (cons 11 (list (car b) (cadr b) z)))))
    (wt:pend-make (entlast))))

;; apply the plan; returns number of entities changed/erased/created
(defun wt:tx-apply (/ n p q u len pieces nw c1 c2 d o10 z a b sv)
  (setq n 0)
  (foreach s *wt:tx-segs*
    (setq p (cond ((cdr (assoc (list (car s) 0) *wt:tx-moves*))) ((cadr s)))
          q (cond ((cdr (assoc (list (car s) 1) *wt:tx-moves*))) ((caddr s)))
          pieces nil)
    (if (and (not (member (car s) *wt:tx-erase*)) (not (wt:peq p q)))
      (progn
        (setq u (wt:unit (wt:v- q p)) len (wt:dist p q) pieces (list (list 0.0 len)))
        (foreach c *wt:tx-cuts*
          (if (= (car c) (car s))
            (progn
              (setq c1 (wt:dot (wt:v- (cadr c) p) u) c2 (wt:dot (wt:v- (caddr c) p) u) nw nil)
              (foreach pc pieces
                (if (< (car pc) (- (min c1 c2) *wt:tol*)) (setq nw (cons (list (car pc) (min (cadr pc) (min c1 c2))) nw)))
                (if (> (cadr pc) (+ (max c1 c2) *wt:tol*)) (setq nw (cons (list (max (car pc) (max c1 c2)) (cadr pc)) nw))))
              (setq pieces (reverse nw)))))))
    (setq pieces (mapcar '(lambda (pc) (list (wt:v+ p (wt:v* u (car pc))) (wt:v+ p (wt:v* u (cadr pc))))) pieces))
    (setq sv (car (cadddr s)))
    (foreach e (cdr (cadddr s)) (wt:pend-erase e) (setq n (1+ n)))
    (if (not pieces)
      (progn (wt:pend-erase sv) (setq n (1+ n)))
      (progn
        (setq d (entget sv) o10 (cdr (assoc 10 d)) z (if (caddr o10) (caddr o10) 0.0)
              a (car (car pieces)) b (cadr (car pieces)))
        (if (> (wt:dist (wt:pt2 o10) a) (wt:dist (wt:pt2 o10) b)) (setq a (cadr (car pieces)) b (car (car pieces))))
        (if (not (and (wt:peq (wt:pt2 o10) a) (wt:peq (wt:pt2 (cdr (assoc 11 d))) b)))
          (progn
            (wt:pend-modify (subst (cons 11 (list (car b) (cadr b) z)) (assoc 11 d)
                                   (subst (cons 10 (list (car a) (cadr a) z)) (assoc 10 d) d)))
            (setq n (1+ n))))
        (foreach pc (cdr pieces) (wt:tx-copy sv (car pc) (cadr pc) z) (setq n (1+ n))))))
  (foreach ad *wt:tx-adds*
    (setq d (entget (caddr ad)) o10 (cdr (assoc 10 d)) z (if (caddr o10) (caddr o10) 0.0))
    (wt:tx-copy (caddr ad) (car ad) (cadr ad) z)
    (setq n (1+ n)))
  n)

;; --- command driver ---

;; READ + PLAN 1 + wall-pair analysis (no drawing change). gap = largest collinear
;; gap merged (TX: D; TW analysis: touching only). Returns (snapshot merged-count).
(defun wt:tx-prepare (ens gap / snap r)
  (setq snap (wt:tx-snapshot ens)
        *wt:tx-D* gap *wt:tx-W* (wt:cfg "TX_WALL_MAX")
        *wt:tx-moves* nil *wt:tx-erase* nil *wt:tx-cuts* nil *wt:tx-claims* nil *wt:tx-adds* nil)
  (setq r (wt:tx-merge (car snap)) *wt:tx-segs* (car r) *wt:tx-D* (wt:cfg "TX_CONNECT_DISTANCE"))
  (wt:tx-build-ptab)
  (list snap (cadr r)))

;; enames -> repairs (one transaction unless *wt:tx-nested*); returns repaired junctions.
;; With *wt:tx-field*: repairs only junctions in the field, and caps free wall ends there.
(defun wt:tx-run (ens / pr snap r0 r1 r2 r3 merged nj amb nent quiet)
  (setq quiet *wt:tx-nested* nj 0 r3 0
        pr (wt:tx-prepare ens (wt:cfg "TX_CONNECT_DISTANCE")) snap (car pr) merged (cadr pr))
  (wt:dbg (list "TX INPUT" (length (car snap)) "LINEs, D =" (wt:fmt *wt:tx-D*) "wall max =" (wt:fmt *wt:tx-W*)))
  (if (and (> *wt:tx-nprot* 0) (not quiet))
    (princ (strcat "\nTX: " (itoa *wt:tx-nprot*) " AKD wall line(s) left to the wall tools.")))
  (if (and (> (cadr snap) 0) (not quiet))
    (princ (strcat "\nTX: " (if (> (cadr snap) 1) (strcat (itoa (cadr snap)) " X-AXIS master lines") "X-AXIS master line")
                   " skipped.")))
  (if (car snap)
    (progn
      (if (not quiet) (princ "\nAnalyzing junctions..."))
      (foreach s *wt:tx-segs*
        (foreach j (cdr (assoc (car s) *wt:tx-ptab*))
          (if (and (< (car s) j) (wt:tx-pairp (car s) j))
            (wt:dbg (list "TX WALL PAIR" (car s) "/" j "spacing" (wt:fmt (abs (wt:tx-off (cadr (wt:tx-seg j)) s))))))))
      (setq r0 (wt:tx-cross-phase) r1 (wt:tx-wall-phase) r2 (wt:tx-line-phase))
      (if *wt:tx-field* (setq r3 (wt:tx-cap-phase)))
      (setq nj (+ merged (car r0) (car r1) (car r2)) amb (+ (cadr r0) (cadr r1) (cadr r2)))
      (if (not quiet) (wt:pend-begin))
      (setq nent (wt:tx-apply))
      (if (not quiet) (setq *wt:pending* nil))
      (wt:dbg (list "TX RESULT collinear" merged "cross" (car r0) "wall" (car r1) "line" (car r2)
                    "caps" r3 "ambiguous" amb "entities changed" nent))
      (if *wt:tx-field* (wt:dbg (list "TX FIELD candidate junctions in field" (+ nj amb) "cap changes" r3)))
      (if (not quiet)
        (progn
          (princ (strcat "\nTX: " (itoa nj) " junction(s) repaired."))
          (if (> r3 0) (princ (strcat "\n" (itoa r3) " wall end cap(s) updated.")))
          (if (> amb 0) (princ (strcat "\n" (itoa amb) " ambiguous junction(s) left unchanged.")))))))
  (if (and (> (caddr snap) 0) (not quiet))
    (princ (strcat "\n" (itoa (caddr snap)) " unsupported object(s) ignored.")))
  (setq *wt:tx-segs* nil *wt:tx-ptab* nil *wt:tx-adds* nil)
  nj)

;; LINEs of the current space (not on AXIS_LAYER) within tol of the field polygon.
;; Whole entities are collected; skipfn (e data p1 p2) may exclude lines.
(defun wt:tx-collect (field tol skipfn / ss i e d a b out ax)
  (setq ax (strcase (wt:cfg "AXIS_LAYER")))
  (if (setq ss (ssget "_X" (list '(0 . "LINE") (cons 410 (getvar "CTAB")))))
    (repeat (setq i (sslength ss))
      (setq e (ssname ss (setq i (1- i))) d (entget e)
            a (wt:pt2 (cdr (assoc 10 d))) b (wt:pt2 (cdr (assoc 11 d))))
      (if (and (/= (strcase (cdr (assoc 8 d))) ax)
               (wt:tw-seg-near-field a b field tol)
               (not (and skipfn (apply skipfn (list e d a b)))))
        (setq out (cons e out)))))
  out)

;; TX in a repair field: collect lines (context reach D + wall max), repair junctions in the field
(defun wt:tx-field-run (field / ens n)
  (setq *wt:tx-field* field
        ens (wt:tx-collect field (+ (wt:cfg "TX_CONNECT_DISTANCE") (wt:cfg "TX_WALL_MAX")) nil))
  (wt:dbg (list "TX FIELD corners" (car field) (caddr field) "LINEs collected" (length ens)))
  (setq n (if ens (wt:tx-run ens) (progn (princ "\nTX: No lines in the repair area.") 0))
        *wt:tx-field* nil)
  n)

;; TX: repair window (preselected LINEs are still accepted: selection mode, no capping)
(defun c:TX (/ *error* ss i ens c1 c2)
  (setq *error* wt:error *wt:tx-field* nil *wt:tx-nested* nil)
  (if (setq ss (ssget "_I")) (sssetfirst nil nil))   ; before any command clears PickFirst
  (wt:begin)
  (cond
    (ss
     (repeat (setq i (sslength ss)) (setq ens (cons (ssname ss (setq i (1- i))) ens)))
     (wt:tx-run ens))
    ((and (setq c1 (getpoint "\nSpecify first corner of repair area: "))
          (setq c2 (getcorner c1 "\nSpecify opposite corner: ")))
     (wt:tx-field-run (wt:tw-field c1 c2))))
  (setq *wt:tx-field* nil)
  (wt:end))

;;; ===================================================================
;;; 21. WWD -- wall to distance (move ONE wall so a picked face is D from a picked face)
;;; ===================================================================
;;; Pick record (read-only, built before any change):
;;;   (src id face-pt u other-pt owned-ents info jobs label)
;;;   src    "AKD" | "GENERIC"
;;;   id     AKD: wall (master p1 p2 thk pos); GENERIC: owned entity list
;;;   face-pt / u   a point on / the direction of the PICKED face line
;;;   other-pt      a point on the wall's other face (body side)
;;;   info   (base u lo hi omin omax): footprint relative to the picked face line
;;;   jobs   GENERIC: wall ends in a junction with the moving wall; squared after the
;;;          move if they became free, so the TX cap phase can close them
;;; Mutation: AKD -> the centerline master moves by the same vector as its faces, so it
;;;           stays centred (stored as CENTER), through the EW/WW rebuild path.
;;;           GENERIC -> owned faces + caps translated, then the shared TX solver
;;;           cleans the old and the new footprint (nested: no prompts, one undo).

(setq *wt:wwd-dist* nil)

;; Pure. Translation of the moving wall: picked moving face through pm (direction u,
;; other face through pb), picked reference face through pr, clear distance d >= 0.
;; The moving face stays on its current side of the reference face (no mirroring);
;; coincident faces separate away from the moving wall's body.
(defun wt:wwd-vector (pm pr u pb d / n delta sg)
  (setq n (wt:perp u) delta (- (wt:dot pr n) (wt:dot pm n))
        sg (cond ((> delta *wt:tol*) 1.0)
                 ((< delta (- *wt:tol*)) -1.0)
                 ((> (wt:dot (wt:v- pb pm) n) 0.0) -1.0)
                 (t 1.0)))
  (wt:v* n (- delta (* sg d))))

;; footprint polygon of info (base u lo hi omin omax), grown by m, shifted by v
(defun wt:wwd-field (info m v / b u n lo hi o1 o2)
  (setq b (wt:v+ (car info) v) u (cadr info) n (wt:perp u)
        lo (- (caddr info) m) hi (+ (cadddr info) m) o1 (- (nth 4 info) m) o2 (+ (nth 5 info) m))
  (list (wt:v+ b (wt:v+ (wt:v* u lo) (wt:v* n o1)))
        (wt:v+ b (wt:v+ (wt:v* u hi) (wt:v* n o1)))
        (wt:v+ b (wt:v+ (wt:v* u hi) (wt:v* n o2)))
        (wt:v+ b (wt:v+ (wt:v* u lo) (wt:v* n o2)))))

(defun wt:wwd-box (p m)
  (list (wt:v+ p (list (- m) (- m))) (wt:v+ p (list m (- m))) (wt:v+ p (list m m)) (wt:v+ p (list (- m) m))))

(defun wt:wwd-reach () (+ (wt:cfg "TX_CONNECT_DISTANCE") (wt:cfg "TX_WALL_MAX")))

;; --- AKD face pick (existing EW / WWF ownership and side rules) ---
;; -> record, message string, or nil when the line is not an AKD wall face
(defun wt:wwd-akd (en q net / r w side o n p1 fo oo)
  (setq r (wt:ew-resolve en q net (wt:pick-margin)))
  (cond
    ((= (car r) "AMBIG") "\nAmbiguous wall junction. Select closer to the wall segment.")
    ((/= (car r) "OK") nil)
    ((= (type (setq w (cadr r) side (wt:wwf-side en q w))) 'STR) side)
    (t
     (setq o (wt:w-offs w) n (wt:perp (wt:w-u w)) p1 (wt:w-p1 w)
           fo (if (> side 0) (car o) (cadr o)) oo (if (> side 0) (cadr o) (car o)))
     (list "AKD" w
           (wt:v+ p1 (wt:v* n fo)) (wt:w-u w) (wt:v+ p1 (wt:v* n oo))
           (list en)
           (list (wt:v+ p1 (wt:v* n fo)) (wt:w-u w) 0.0 (wt:dist p1 (wt:w-p2 w))
                 (min 0.0 (- oo fo)) (max 0.0 (- oo fo)))
           nil
           (list "master" (car w) "width" (wt:fmt (wt:w-thk w)) "position" (wt:w-pos w)
                 "face" (if (> side 0) "LEFT" "RIGHT") "direction" (wt:w-u w))))))

;; --- generic face pick (TX pairing + TW generic wall records) ---

;; a parallel line at wall spacing overlaps s (pairing failed on a tie, not for lack of a partner)
(defun wt:wwd-has-parallel (s / r iv o)
  (foreach k *wt:tx-segs*
    (if (and (not r) (/= (car k) (car s)) (< (wt:tx-sin s k) *wt:tx-tol-par*))
      (progn
        (setq o (abs (wt:tx-off (cadr k) s)) iv (wt:tx-iv k s))
        (if (and (> o *wt:tx-tol-col*) (<= o *wt:tx-W*)
                 (> (- (min (cadr iv) (wt:tx-len s)) (max (car iv) 0.0)) *wt:tol*))
          (setq r t)))))
  r)

;; lines in field -> (seg rec recs lines) for the picked entity, or a message.
;; Leaves the TX analysis state loaded; the caller clears it.
(defun wt:wwd-gen-find (en field / ens s rec recs)
  (setq ens (wt:tx-collect field (wt:wwd-reach) 'wt:tw-akd-line-p))
  (wt:tx-prepare ens *wt:tol*)
  (foreach k *wt:tx-segs* (if (member en (cadddr k)) (setq s k)))
  (cond
    ((not s) "\nCould not identify a wall from this line.")
    ((member (car s) *wt:tx-nopair*) "\nSelect a wall side face, not an end cap.")
    ((not (wt:tx-paired-p (car s)))
     (if (wt:wwd-has-parallel s)
       "\nAmbiguous wall face. Select a clearer wall face."
       "\nCould not identify a double-line wall from this line."))
    (t
     (setq recs (wt:tw-gen-walls))
     (foreach x recs (if (member (car s) (car x)) (setq rec x)))
     (if rec
       (list s rec recs
             (mapcar '(lambda (e / d) (setq d (entget e))
                        (list e (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d)))))
                     ens))
       "\nAmbiguous wall face. Select a clearer wall face."))))

;; p lies across another wall: between its faces, within reach of its extent
;; (a crossing wall's own opening leaves p just beyond its face pieces)
(defun wt:wwd-some-strip (p skip recs / r ref st off)
  (foreach x recs
    (if (and (not r) (not (member x skip)))
      (progn
        (setq ref (wt:tx-seg (nth 7 x)) st (wt:tx-sta p ref) off (wt:tx-off p ref))
        (if (and (>= st (- (nth 9 x) (wt:cfg "TX_CONNECT_DISTANCE")))
                 (<= st (+ (nth 10 x) (wt:cfg "TX_CONNECT_DISTANCE")))
                 (>= off (- (min 0.0 (nth 8 x)) *wt:tx-tol-col*))
                 (<= off (+ (max 0.0 (nth 8 x)) *wt:tx-tol-col*)))
          (setq r t)))))
  r)

;; rec plus collinear same-width records continuing it through a junction (the gap
;; between their ends lies inside a third wall), transitively
(defun wt:wwd-continuations (rec recs / out grow ua lo1 hi1 lo2 hi2 mid)
  (setq out (list rec) grow t)
  (while grow
    (setq grow nil)
    (foreach r2 recs
      (foreach r1 out
        (setq ua (wt:tw-rec-u r1) mid nil)
        (if (and (not (member r2 out))
                 (< (abs (wt:cross ua (wt:tw-rec-u r2))) *wt:tx-tol-par*)
                 (<= (abs (- (cadddr r1) (cadddr r2))) *wt:tx-tol-col*)
                 (<= (abs (wt:cross ua (wt:v- (cadr r2) (cadr r1)))) *wt:tx-tol-col*))
          (progn
            (setq lo1 (wt:dot (cadr r1) ua) hi1 (wt:dot (caddr r1) ua)
                  lo2 (min (wt:dot (cadr r2) ua) (wt:dot (caddr r2) ua))
                  hi2 (max (wt:dot (cadr r2) ua) (wt:dot (caddr r2) ua)))
            (cond ((> lo2 hi1) (setq mid (wt:v+ (caddr r1) (wt:v* ua (/ (- lo2 hi1) 2.0)))))
                  ((> lo1 hi2) (setq mid (wt:v+ (cadr r1) (wt:v* ua (/ (- hi2 lo1) 2.0))))))
            (if (and mid (wt:wwd-some-strip mid (list r1 r2) recs))
              (setq out (append out (list r2)) grow t)))))))
  out)

;; extreme endpoint (point code ename) of entities along o
(defun wt:wwd-extreme (ents o / best bk bp p d)
  (foreach e ents
    (if (setq d (entget e))
      (foreach k '(10 11)
        (setq p (wt:pt2 (cdr (assoc k d))))
        (if (or (not best) (> (wt:dot p o) (+ (wt:dot best o) *wt:tol*)))
          (setq best p bk k bp e)))))
  (if best (list best bk bp)))

;; pt lies on one of lines (ename p1 p2) whose entity is not in excl
(defun wt:wwd-touch (pt lines excl / r)
  (foreach l lines
    (if (and (not r) (not (member (car l) excl)) (wt:on-seg pt (cadr l) (caddr l))) (setq r t)))
  r)

(defun wt:wwd-seg-ents (ids / out)
  (foreach id ids (setq out (append out (cadddr (wt:tx-seg id)))))
  out)

(defun wt:wwd-not-in (a b / out) (foreach x a (if (not (member x b)) (setq out (cons x out)))) out)
(defun wt:wwd-only (lines ents / out) (foreach l lines (if (member (car l) ents) (setq out (cons l out)))) out)

;; footprint (lo hi omin omax) of segment ids relative to seg s
(defun wt:wwd-extent (ids s / iv off lo hi om ox)
  (foreach id ids
    (setq iv (wt:tx-iv (wt:tx-seg id) s) off (wt:tx-off (cadr (wt:tx-seg id)) s))
    (if (or (not lo) (< (car iv) lo)) (setq lo (car iv)))
    (if (or (not hi) (> (cadr iv) hi)) (setq hi (cadr iv)))
    (if (or (not om) (< off om)) (setq om off))
    (if (or (not ox) (> off ox)) (setq ox off)))
  (list lo hi (min 0.0 om) (max 0.0 ox)))

(defun wt:wwd-group-ids (grp / ids) (foreach x grp (setq ids (append ids (car x)))) ids)

;; -> record or message
(defun wt:wwd-generic (en q / r s rec recs lines grp ids caps ents ext other res jobs xents mv e1 e2 sl)
  (setq r (wt:wwd-gen-find en (wt:wwd-box q (wt:wwd-reach))))
  (if (/= (type r) 'STR)
    ;; pass 2: the whole wall footprint (faces may run far beyond the pick)
    (progn
      (setq s (car r) ext (wt:wwd-extent (wt:wwd-group-ids (wt:wwd-continuations (cadr r) (caddr r))) s))
      (setq r (wt:wwd-gen-find en (wt:wwd-field (cons (cadr s) (cons (wt:tx-u s) ext)) (wt:wwd-reach) '(0.0 0.0))))))
  (if (/= (type r) 'STR)
    (progn
      (setq s (car r) rec (cadr r) recs (caddr r) lines (cadddr r)
            grp (wt:wwd-continuations rec recs) ids (wt:wwd-group-ids grp))
      (foreach c (wt:tx-cap-like)
        (if (and (member (cadr c) ids) (member (caddr c) ids)) (setq caps (cons (car c) caps))))
      (setq ents (wt:wwd-seg-ents (append ids caps))
            ext (wt:wwd-extent ids s)
            other (if (member (car s) (nth 5 rec)) (nth 6 rec) (nth 5 rec)))
      ;; wall ends in a junction with the moving wall
      (foreach x recs
        (setq xents (wt:wwd-seg-ents (car x)) mv (not (wt:wwd-not-in xents ents)))
        (foreach o (list (wt:tw-rec-u x) (wt:v* (wt:tw-rec-u x) -1.0))
          (setq e1 (wt:wwd-extreme (wt:wwd-seg-ents (nth 5 x)) o)
                e2 (wt:wwd-extreme (wt:wwd-seg-ents (nth 6 x)) o)
                sl (if mv lines (wt:wwd-only lines ents)))
          (if (and e1 e2
                   (or (wt:wwd-touch (car e1) sl (if mv ents xents))
                       (wt:wwd-touch (car e2) sl (if mv ents xents))))
            (setq jobs (cons (list (wt:wwd-seg-ents (nth 5 x)) (wt:wwd-seg-ents (nth 6 x)) o (if mv ents xents))
                             jobs)))))
      (setq res (list "GENERIC" ents (cadr s) (wt:tx-u s) (cadr (wt:tx-seg (car other)))
                      ents (cons (cadr s) (cons (wt:tx-u s) ext)) jobs
                      (list "faces" (car rec) "picked" en "width" (wt:fmt (cadddr rec))
                            "direction" (wt:tx-u s) "caps" (length caps)))))
    (setq res r))
  (setq *wt:tx-segs* nil *wt:tx-ptab* nil)
  res)

;; any LINE pick -> record or message
(defun wt:wwd-resolve (en q / d lyr r)
  (setq d (entget en))
  (cond
    ((or (not d) (/= (cdr (assoc 0 d)) "LINE")) "\nSelect a wall face.")
    ((= (setq lyr (strcase (cdr (assoc 8 d)))) (strcase (wt:cfg "AXIS_LAYER")))
     "\nSelect a wall face, not the wall axis.")
    (t
     (setq *wt:tw-net* (wt:net-scan))
     (if (wt:any-owned (list en (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d)))) (wt:legacy-walls *wt:tw-net*))
       (setq r "\nLegacy wall axis detected. Run WWR first."))
     (if (and (not r) (= lyr (strcase (wt:cfg "WALL_LAYER")))) (setq r (wt:wwd-akd en q *wt:tw-net*)))
     (if (not r) (setq r (wt:wwd-generic en q)))
     (setq *wt:tw-net* nil)
     r)))

(defun wt:wwd-same-p (m r)
  (if (= (car m) (car r))
    (if (= (car m) "AKD")
      (eq (car (cadr m)) (car (cadr r)))
      (wt:any-member (cadr m) (cadr r)))))

(defun wt:wwd-dbg-rec (title rec)
  (wt:dbg (list title "source" (car rec) (nth 8 rec))))

;; after a generic move: a junction end that now touches nothing is squared (the
;; shorter face extended to the longer one) so the TX cap phase can close it
(defun wt:wwd-square (job lines / a b sa sb lo d)
  (setq a (wt:wwd-extreme (car job) (caddr job)) b (wt:wwd-extreme (cadr job) (caddr job)))
  (if (and a b
           (not (wt:wwd-touch (car a) lines (cadddr job)))
           (not (wt:wwd-touch (car b) lines (cadddr job))))
    (progn
      (setq sa (wt:dot (car a) (caddr job)) sb (wt:dot (car b) (caddr job)))
      (if (and (> (abs (- sa sb)) *wt:tol*) (<= (abs (- sa sb)) (wt:wwd-reach)))
        (progn
          (setq lo (if (< sa sb) a b) d (entget (caddr lo)))
          (wt:pend-modify
            (subst (cons (cadr lo) (wt:tx-z3 (wt:v+ (car lo) (wt:v* (caddr job) (abs (- sa sb))))
                                              (cdr (assoc (cadr lo) d))))
                   (assoc (cadr lo) d) d))
          (wt:dbg (list "WWD SQUARE freed wall end, face" (caddr lo) "extended" (wt:fmt (abs (- sa sb))))))))))

;; 2D point p with the z of old 3D point
(defun wt:tx-z3 (p old) (list (car p) (cadr p) (if (caddr old) (caddr old) 0.0)))

(defun wt:wwd-lines (field / out d)
  (foreach e (wt:tx-collect field (wt:wwd-reach) nil)
    (setq d (entget e) out (cons (list e (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d)))) out)))
  out)

;; shared TX cleanup (nested) in one field; AKD-owned linework excluded
(defun wt:wwd-clean (field / ens n)
  (setq *wt:tx-field* field
        ens (wt:tx-collect field (wt:wwd-reach) 'wt:tw-akd-line-p)
        n (if ens (wt:tx-run ens) 0)
        *wt:tx-field* nil)
  n)

;; Validated records + distance -> transaction record, or a message (no change)
(defun wt:wwd-execute (m r d / v w p1 p2 net en new rec f0 f1 lines n0 n1 dl ids)
  (cond
    ((>= (abs (wt:cross (cadddr m) (cadddr r))) *wt:tx-tol-par*)
     (wt:dbg (list "WWD REJECT reason SELECTED WALLS NOT PARALLEL"))
     "\nSelected walls are not parallel.\nWWD currently requires parallel walls.")
    ((progn
       (setq v (wt:wwd-vector (caddr m) (caddr r) (cadddr m) (nth 4 m) d))
       (wt:wwd-dbg-rec "WWD MOVING WALL" m)
       (wt:wwd-dbg-rec "WWD REFERENCE WALL" r)
       (wt:dbg (list "WWD DISTANCE current signed separation"
                     (wt:fmt (wt:dot (wt:v- (caddr r) (caddr m)) (wt:perp (cadddr m))))
                     "requested" (wt:fmt d) "move amount" (wt:fmt (wt:len v)) "move vector" v))
       (< (wt:len v) *wt:tol*))
     "\nWall is already at that distance.")
    ((= (car m) "AKD")
     (setq w (cadr m) p1 (wt:v+ (wt:w-p1 w) v) p2 (wt:v+ (wt:w-p2 w) v) net (wt:net-scan))
     (if (and (setq en (wt:master-at p1 p2 (car net))) (wt:wall-from-master en (cadr net)))
       "\nA wall already exists at that position."
       (progn
         (setq ids (wt:op-ids-on w))                      ; openings travel with the wall
         (wt:pend-begin)
         (wt:rebuild nil (list w))                       ; old location: EW path
         (if ids (wt:op-event *wt:wall-moved-fns* (list ids (wt:3d v))))
         (setq en (wt:pend-make (wt:mk-line p1 p2 (wt:cfg "AXIS_LAYER"))))
         (setq new (wt:rebuild (list (list en p1 p2 (wt:w-thk w) "CENTER")) nil))   ; new location: WW path
         (foreach nw new (wt:reg-add (car nw) (wt:w-thk nw) "CENTER"))
         (wt:dbg (list "WWD AKD master moved" (car w) "->" en "spans" (length new) "openings" (length ids)))
         (setq rec (wt:pend-end))
         rec)))
    (t
     (setq f0 (wt:wwd-field (nth 6 m) (wt:cfg "TX_CONNECT_DISTANCE") '(0.0 0.0))
           f1 (wt:wwd-field (nth 6 m) (wt:cfg "TX_CONNECT_DISTANCE") v))
     (wt:pend-begin)
     (setq *wt:tx-nested* t *wt:tw-net* (wt:net-scan))
     (foreach e (cadr m)
       (setq dl (entget e))
       (wt:pend-modify
         (subst (cons 11 (wt:tx-z3 (wt:v+ (wt:pt2 (cdr (assoc 11 dl))) v) (cdr (assoc 11 dl)))) (assoc 11 dl)
                (subst (cons 10 (wt:tx-z3 (wt:v+ (wt:pt2 (cdr (assoc 10 dl))) v) (cdr (assoc 10 dl)))) (assoc 10 dl) dl))))
     (setq lines (append (wt:wwd-lines f0) (wt:wwd-lines f1)))
     (foreach job (nth 7 m) (wt:wwd-square job lines))
     (setq n0 (wt:wwd-clean f0) n1 (wt:wwd-clean f1))
     (wt:dbg (list "WWD CLEANUP old field" (car f0) (caddr f0) "new field" (car f1) (caddr f1)
                   "old junctions repaired" n0 "new junctions repaired" n1))
     (setq *wt:tx-nested* nil *wt:tw-net* nil rec *wt:pending* *wt:pending* nil)
     rec)))

;; non-interactive driver (also used by tests): picks + distance -> record or message
(defun wt:wwd-run (e1 q1 e2 q2 d / m r)
  (setq m (wt:wwd-resolve e1 q1))
  (cond ((= (type m) 'STR) m)
        ((= (type (setq r (wt:wwd-resolve e2 q2))) 'STR) r)
        ((wt:wwd-same-p m r) "\nMoving wall and reference wall must be different walls.")
        ((< d 0) "\nDistance must be zero or greater.")
        (t (wt:wwd-execute m r d))))

;; face pick loop -> record, or nil on Enter/Esc
(defun wt:wwd-select (msg moving / e res out done)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq e (entsel msg))
    (cond
      ((not e) (if (/= (getvar "ERRNO") 7) (setq done t)))
      ((= (type (setq res (wt:wwd-resolve (car e) (wt:pt2 (trans (cadr e) 1 0))))) 'STR)
       (wt:dbg (list "WWD REJECT" res))
       (princ res))
      ((and moving (wt:wwd-same-p moving res))
       (princ "\nMoving wall and reference wall must be different walls."))
      (t (setq out res done t))))
  out)

(defun wt:wwd-distance (def / v done)
  (while (not done)
    (setq v (getdist (strcat "\nEnter distance <" (wt:fmt def) ">: ")))
    (cond ((not v) (setq v def done t))
          ((< v 0) (princ "\nDistance must be zero or greater."))
          (t (setq done t))))
  (float v))

(defun c:WWD (/ *error* m r d res)
  (setq *error* wt:error)
  (wt:begin)
  (if (not *wt:wwd-dist*) (setq *wt:wwd-dist* 1200.0))
  (if (and (setq m (wt:wwd-select "\nSelect wall face to move: " nil))
           (setq r (wt:wwd-select "\nSelect reference wall face: " m))
           (setq d (wt:wwd-distance *wt:wwd-dist*)))
    (if (= (type (setq res (wt:wwd-execute m r d))) 'STR)
      (princ res)
      (progn
        (setq *wt:wwd-dist* d)
        (princ (strcat "\nWall adjusted to " (wt:fmt d) ".")))))
  (wt:end))

;;; ===================================================================
;;; 22. WWE -- wall connect (connect ONE wall end to ONE picked target wall)
;;; ===================================================================
;;; Moving record = assoc list ("KEY" . value):
;;;   SRC "AKD"|"GENERIC", END 0|1, PEXT / PFIX axis ends, O outward unit,
;;;   OWNED entities, CONN connected-to-other-wall flag, LABEL
;;;   AKD:     W wall
;;;   GENERIC: SP1 SU (picked face line), LO HI OMIN OMAX (footprint on it),
;;;            SIDEA SIDEB face entities, ENDCAPS caps at the selected end
;;; Target = WWD pick record (wt:wwd-resolve).
;;; AKD -> AKD (wt:wwe-plan / wt:wwe-akd-connect): intelligent connection. The logical
;;;   corner is centerline x centerline; the plan decides extend / trim / detach of the
;;;   selected end, T or L at the target (a free target end may be extended to the
;;;   corner; WWE_CORNER_DISTANCE only decides L vs T near an end), removes overshoot
;;;   pieces, absorbs collinear spans the end passes. Nothing changes until the plan is
;;;   complete; the old spans are then removed (EW path), the new centred masters added
;;;   (WW path) and the touched nodes healed (wt:axis-heal-local), in one transaction.
;;; GENERIC: extend only (never shortens, never moves the target); mixed AKD / ordinary
;;;   junctions are refused.

(defun wt:wwe-g (rec k) (cdr (assoc k rec)))

;; --- moving wall: AKD (side faces and caps both resolve through EW ownership) ---
(defun wt:wwe-akd (en q net / r w u len st end pe pf o conn)
  (setq r (wt:ew-resolve en q net (wt:pick-margin)))
  (cond
    ((= (car r) "AMBIG") "\nAmbiguous wall junction. Select farther from the junction.")
    ((/= (car r) "OK") nil)
    (t
     (setq w (cadr r) u (wt:w-u w) len (wt:dist (wt:w-p1 w) (wt:w-p2 w))
           st (wt:dot (wt:v- q (wt:w-p1 w)) u)
           end (if (< st (/ len 2.0)) 0 1)
           pe (if (= end 0) (wt:w-p1 w) (wt:w-p2 w))
           pf (if (= end 0) (wt:w-p2 w) (wt:w-p1 w))
           o (wt:unit (wt:v- pe pf)))
     (foreach m (car net)
       (if (and (not (eq (car m) (car w))) (wt:on-seg pe (cadr m) (caddr m))) (setq conn (cons (car m) conn))))
     (list (cons "SRC" "AKD") (cons "W" w) (cons "END" end) (cons "PEXT" pe) (cons "PFIX" pf)
           (cons "O" o) (cons "OWNED" (list (car w))) (cons "CONN" conn)
           (cons "LABEL" (list "master" (car w) "width" (wt:fmt (wt:w-thk w)) "pick station" (wt:fmt st)
                               "selected end" (if (= end 0) "END A" "END B") "current end" pe "direction" o))))))

;; --- moving wall: GENERIC (a cap pick resolves to the face it closes; its end is selected) ---
(defun wt:wwe-generic (en q / r s rec recs lines ids caps ext ob sa sb st end o u base pe pf e1 e2
                          owned endcaps res face k mid)
  (setq r (wt:wwd-gen-find en (wt:wwd-box q (wt:wwd-reach))))
  (if (equal r "\nSelect a wall side face, not an end cap.")   ; cap pick -> the wall it closes
    (progn
      (foreach c (wt:tx-cap-like)
        (if (and (not face) (member en (cadddr (wt:tx-seg (car c)))))
          (setq face (car (cadddr (wt:tx-seg (cadr c)))))))
      (setq *wt:tx-segs* nil *wt:tx-ptab* nil)
      (if face (setq r (wt:wwd-gen-find face (wt:wwd-box q (wt:wwd-reach))) en face)
               (setq r "\nCould not identify the wall closed by this end cap."))))
  (if (/= (type r) 'STR)
    (progn   ; pass 2: whole wall footprint
      (setq s (car r) ext (wt:wwd-extent (wt:wwd-group-ids (wt:wwd-continuations (cadr r) (caddr r))) s))
      (setq r (wt:wwd-gen-find en (wt:wwd-field (cons (cadr s) (cons (wt:tx-u s) ext)) (wt:wwd-reach) '(0.0 0.0))))))
  (if (/= (type r) 'STR)
    (progn
      (setq s (car r) rec (cadr r) recs (caddr r) lines (cadddr r) u (wt:tx-u s)
            ids (wt:wwd-group-ids (wt:wwd-continuations rec recs))
            ext (wt:wwd-extent ids s)
            ob (if (< (caddr ext) (- *wt:tx-tol-col*)) (caddr ext) (cadddr ext)))
      (foreach id ids
        (if (<= (abs (wt:tx-off (cadr (wt:tx-seg id)) s)) *wt:tx-tol-col*)
          (setq sa (append sa (cadddr (wt:tx-seg id))))
          (setq sb (append sb (cadddr (wt:tx-seg id))))))
      (foreach c (wt:tx-cap-like)
        (if (and (member (cadr c) ids) (member (caddr c) ids)) (setq caps (cons (car c) caps))))
      (setq st (wt:tx-sta q s)
            end (if (< (- st (car ext)) (- (cadr ext) st)) 0 1)
            o (if (= end 1) u (wt:v* u -1.0))
            base (wt:v+ (cadr s) (wt:v* (wt:perp u) (/ ob 2.0)))
            pe (wt:v+ base (wt:v* u (if (= end 1) (cadr ext) (car ext))))
            pf (wt:v+ base (wt:v* u (if (= end 1) (car ext) (cadr ext))))
            owned (append sa sb (wt:wwd-seg-ents caps)))
      (foreach k caps
        (setq mid (wt:v* (wt:v+ (cadr (wt:tx-seg k)) (caddr (wt:tx-seg k))) 0.5))
        (if (<= (abs (wt:dot (wt:v- mid pe) o)) (wt:cfg "TX_CONNECT_DISTANCE"))
          (setq endcaps (append endcaps (cadddr (wt:tx-seg k))))))
      (setq e1 (wt:wwd-extreme sa o) e2 (wt:wwd-extreme sb o))
      (setq res (list (cons "SRC" "GENERIC") (cons "END" end) (cons "PEXT" pe) (cons "PFIX" pf) (cons "O" o)
                      (cons "OWNED" owned)
                      (cons "CONN" (or (wt:wwd-touch (car e1) lines owned) (wt:wwd-touch (car e2) lines owned)))
                      (cons "SP1" (cadr s)) (cons "SU" u)
                      (cons "LO" (car ext)) (cons "HI" (cadr ext)) (cons "OMIN" (caddr ext)) (cons "OMAX" (cadddr ext))
                      (cons "SIDEA" sa) (cons "SIDEB" sb) (cons "ENDCAPS" endcaps)
                      (cons "LABEL" (list "faces" (car rec) "picked" en "width" (wt:fmt (abs ob))
                                          "pick station" (wt:fmt st) "selected end" (if (= end 0) "END A" "END B")
                                          "current end" pe "direction" o "end caps" (length endcaps))))))
    (setq res r))
  (setq *wt:tx-segs* nil *wt:tx-ptab* nil)
  res)

(defun wt:wwe-resolve (en q / d lyr r)
  (setq d (entget en))
  (cond
    ((or (not d) (/= (cdr (assoc 0 d)) "LINE")) "\nSelect a wall.")
    ((= (setq lyr (strcase (cdr (assoc 8 d)))) (strcase (wt:cfg "AXIS_LAYER")))
     "\nSelect a wall face, not the wall axis.")
    (t
     (setq *wt:tw-net* (wt:net-scan))
     (if (wt:any-owned (list en (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d)))) (wt:legacy-walls *wt:tw-net*))
       (setq r "\nLegacy wall axis detected. Run WWR first."))
     (if (and (not r) (= lyr (strcase (wt:cfg "WALL_LAYER")))) (setq r (wt:wwe-akd en q *wt:tw-net*)))
     (if (not r) (setq r (wt:wwe-generic en q)))
     (setq *wt:tw-net* nil)
     r)))

(defun wt:wwe-same-p (m tg)
  (if (= (wt:wwe-g m "SRC") (car tg))
    (if (= (car tg) "AKD")
      (eq (car (wt:wwe-g m "W")) (car (cadr tg)))
      (wt:any-member (wt:wwe-g m "OWNED") (cadr tg)))))

(defun wt:wwe-reject (why msg) (wt:dbg (list "WWE REJECT reason" why)) msg)

;; Pure classification. -> (x d) or a message.
;; AKD -> AKD: the wall-level target is the target MASTER (AKD junctions are master to
;; master; the rebuild terminates the faces on the picked side). Otherwise the picked face line.
(defun wt:wwe-target (m tg / o pe akd tw lp lu x d len st lo hi)
  (setq o (wt:wwe-g m "O") pe (wt:wwe-g m "PEXT")
        akd (and (= (wt:wwe-g m "SRC") "AKD") (= (car tg) "AKD")) tw (cadr tg))
  (if akd
    (setq lp (wt:w-p1 tw) lu (wt:w-u tw) lo 0.0 hi (wt:dist (wt:w-p1 tw) (wt:w-p2 tw)))
    (setq lp (car (nth 6 tg)) lu (cadr (nth 6 tg)) lo (caddr (nth 6 tg)) hi (cadddr (nth 6 tg))))
  (setq len (wt:dist pe (wt:wwe-g m "PFIX")))
  (cond
    ((or (< (abs (wt:cross o lu)) *wt:tx-min-sin*) (not (setq x (wt:xline pe o lp lu))))
     (wt:wwe-reject "TARGET PARALLEL" "\nTarget face is parallel to the wall.\nCannot determine an extension point."))
    ((progn (setq d (wt:dot (wt:v- x pe) o) st (wt:dot (wt:v- x lp) lu)) nil))
    ((<= d (- *wt:tol* len))
     (wt:wwe-reject "TARGET BEHIND SELECTED END" "\nTarget is behind the selected wall end.\nUse EW to shorten the wall."))
    ((< d (- *wt:tol*))
     (wt:wwe-reject "WALL ALREADY BEYOND TARGET" "\nWall already extends beyond target.\nUse EW to shorten the wall."))
    ((or (< st (- lo (wt:cfg "TX_CONNECT_DISTANCE"))) (> st (+ hi (wt:cfg "TX_CONNECT_DISTANCE"))))
     (wt:wwe-reject "TARGET EXTENT NOT REACHED" "\nExtension does not reach the selected target wall."))
    (t (list x d))))

;; axis path pe -> x crosses another wall (lines / masters not owned by either wall)
;; every master (AKD) / line (GENERIC) the axis or face paths pe -> x cross, excluding both walls
(defun wt:wwe-path-hits (m tg x / o a b r excl)
  (setq o (wt:wwe-g m "O"))
  (if (= (wt:wwe-g m "SRC") "AKD")
    (progn
      (setq a (wt:v+ (wt:wwe-g m "PEXT") (wt:v* o *wt:tx-tol-col*)) b (wt:v- x (wt:v* o *wt:tx-tol-col*)))
      (if (> (wt:dot (wt:v- b a) o) 0.0)
        (foreach mm (car (wt:net-scan))
          (if (and (not (member (car mm) r)) (not (eq (car mm) (car (wt:wwe-g m "W"))))
                   (not (and (= (car tg) "AKD") (eq (car mm) (car (cadr tg)))))
                   (wt:seg-touch a b (cadr mm) (caddr mm)))
            (setq r (cons (car mm) r))))))
    (progn
      (setq excl (append (wt:wwe-g m "OWNED") (cadr tg)))
      (foreach side (list (wt:wwe-g m "SIDEA") (wt:wwe-g m "SIDEB"))
        (setq a (car (wt:wwd-extreme side o))
              b (wt:v+ a (wt:v* o (- (wt:dot (wt:v- x a) o) *wt:tx-tol-col*)))
              a (wt:v+ a (wt:v* o *wt:tx-tol-col*)))
        (if (> (wt:dot (wt:v- b a) o) 0.0)
          (foreach l (wt:wwd-lines (list a b (wt:v+ b (wt:v* (wt:perp o) 0.001)) (wt:v+ a (wt:v* (wt:perp o) 0.001))))
            (if (and (not (member (car l) r)) (not (member (car l) excl)) (wt:seg-touch a b (cadr l) (caddr l)))
              (setq r (cons (car l) r))))))))
  (reverse r))

;; first entity crossing the extension path (kept for callers of the V1 rule)
(defun wt:wwe-obstructed (m tg x) (car (wt:wwe-path-hits m tg x)))

;;; --- connected start end: old-node classification and extension corridor ---
;;; Old node (before any change): FREE | CAP | L | T, or a refusal for COLLINEAR,
;;; COMPLEX (2+ walls / ambiguous) and UNSUPPORTED (unrecognised linework).
;;; Corridor: every wall crossed between the end and the target must be a recognised,
;;; non-parallel wall the moving wall passes completely (more than D beyond its far
;;; face; an intermediate wall must also continue D past both moving faces). The final
;;; L / T / CROSS geometry is left to the rebuild (AKD) or the TX solver (GENERIC).

(defun wt:wwe-topo-reject (why)
  (wt:wwe-reject (strcat "OLD JUNCTION " why)
    (cond ((= why "COLLINEAR")
           "\nSelected wall end is part of a collinear continuation.\nConnected collinear extension is not supported yet.")
          ((= why "COMPLEX") "\nSelected wall end is part of a complex or ambiguous junction. No change.")
          (t "\nSelected wall end touches unrecognised linework. No change."))))

;; AKD: masters touching the selected master end
;; AKD: masters touching the selected master end -> (kind partners collinear)
;; kind FREE | L | T | COLLINEAR | COMPLEX | UNSUPPORTED (classification only; the
;; connection planner decides what is allowed)
(defun wt:wwe-topo-akd (m / w pe net ts mm w2 bad col)
  (setq w (wt:wwe-g m "W") pe (wt:wwe-g m "PEXT") net *wt:tw-net*)
  (foreach e (wt:wwe-g m "CONN")
    (setq mm (assoc e (car net)))
    (cond ((or (not mm) (not (setq w2 (wt:wall-from-master mm (cadr net))))) (setq bad "UNSUPPORTED"))
          ((< (abs (wt:cross (wt:w-u w) (wt:w-u w2))) *wt:tx-min-sin*) (setq col (cons w2 col)))
          (t (setq ts (cons w2 ts)))))
  (foreach a ts
    (foreach b ts
      (if (or (>= (abs (wt:cross (wt:w-u a) (wt:w-u b))) *wt:tx-tol-par*)
              (> (abs (wt:cross (wt:w-u a) (wt:v- (wt:w-p1 b) (wt:w-p1 a)))) *wt:tol*))
        (if (not bad) (setq bad "COMPLEX")))))
  (cond (bad (list bad ts col))
        (col (list "COLLINEAR" ts col))
        ((not ts) (list "FREE" nil nil))
        ((cdr (cdr ts)) (list "COMPLEX" ts nil))
        ((cdr ts) (list "T" ts nil))                                ; host split at the node
        ((or (wt:peq pe (wt:w-p1 (car ts))) (wt:peq pe (wt:w-p2 (car ts)))) (list "L" ts nil))
        (t (list "T" ts nil))))

;; generic walls and all lines in a field -> ((ents u face1-pt face2-pt axis-p1 axis-p2 width) ...) lines
(defun wt:wwe-scan (field / ens out lines d)
  (setq ens (wt:tx-collect field (wt:wwd-reach) 'wt:tw-akd-line-p))
  (if ens
    (progn
      (wt:tx-prepare ens *wt:tol*)
      (foreach r (wt:tw-gen-walls)
        (setq out (cons (list (wt:wwd-seg-ents (car r)) (wt:tw-rec-u r)
                              (cadr (wt:tx-seg (car (nth 5 r)))) (cadr (wt:tx-seg (car (nth 6 r))))
                              (cadr r) (caddr r) (cadddr r))
                        out)))
      (setq *wt:tx-segs* nil *wt:tx-ptab* nil)))
  (foreach e (wt:tx-collect field (wt:wwd-reach) nil)
    (setq d (entget e) lines (cons (list e (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d)))) lines)))
  (list (reverse out) lines))

(defun wt:wwe-rec-of (e recs / r) (foreach x recs (if (and (not r) (member e (car x))) (setq r x))) r)

;; the moving wall's two face lines: a point on each (direction O)
(defun wt:wwe-faces (m)
  (list (car (wt:wwd-extreme (wt:wwe-g m "SIDEA") (wt:wwe-g m "O")))
        (car (wt:wwd-extreme (wt:wwe-g m "SIDEB") (wt:wwe-g m "O")))))

;; scan wall r continues more than D beyond both moving face lines
(defun wt:wwe-through-p (m r / o fs len s1 s2)
  (setq o (wt:wwe-g m "O") fs (wt:wwe-faces m) len (wt:dist (nth 4 r) (nth 5 r)))
  (if (>= (abs (wt:cross o (cadr r))) *wt:tx-min-sin*)
    (progn
      (setq s1 (wt:dot (wt:v- (wt:xline (car fs) o (nth 4 r) (cadr r)) (nth 4 r)) (cadr r))
            s2 (wt:dot (wt:v- (wt:xline (cadr fs) o (nth 4 r) (cadr r)) (nth 4 r)) (cadr r)))
      (and (> (- (min s1 s2) (wt:cfg "TX_CONNECT_DISTANCE")) 0.0)
           (< (+ (max s1 s2) (wt:cfg "TX_CONNECT_DISTANCE")) len)))))

;; station (along O) of scan wall r's farther face on the moving axis
(defun wt:wwe-far (m r / o pe)
  (setq o (wt:wwe-g m "O") pe (wt:wwe-g m "PEXT"))
  (max (wt:dot (wt:xline pe o (caddr r) (cadr r)) o) (wt:dot (wt:xline pe o (cadddr r) (cadr r)) o)))

;; GENERIC: walls whose lines touch the selected face ends
(defun wt:wwe-topo-generic (m / sc recs lines o e1 e2 owned ps bad r)
  (setq sc (wt:wwe-scan (wt:wwd-box (wt:wwe-g m "PEXT") (wt:wwd-reach))) recs (car sc) lines (cadr sc)
        o (wt:wwe-g m "O") owned (wt:wwe-g m "OWNED")
        e1 (car (wt:wwe-faces m)) e2 (cadr (wt:wwe-faces m)))
  (foreach l lines
    (if (and (not (member (car l) owned))
             (or (wt:on-seg e1 (cadr l) (caddr l)) (wt:on-seg e2 (cadr l) (caddr l))))
      (if (setq r (wt:wwe-rec-of (car l) recs))
        (if (not (member r ps)) (setq ps (cons r ps)))
        (setq bad "UNSUPPORTED"))))
  (cond (bad (wt:wwe-topo-reject bad))
        ((not ps) (list (if (wt:wwe-g m "ENDCAPS") "CAP" "FREE")))
        ((cdr ps) (wt:wwe-topo-reject "COMPLEX"))
        ((< (abs (wt:cross o (cadr (car ps)))) *wt:tx-min-sin*) (wt:wwe-topo-reject "COLLINEAR"))
        ((wt:wwe-through-p m (car ps)) (list "T" ps))
        (t (list "L" ps))))

;; -> (type partners) or a refusal message. No drawing change.
(defun wt:wwe-old-topology (m / res)
  (setq *wt:tw-net* (wt:net-scan)
        res (wt:wwe-topo-generic m)
        *wt:tw-net* nil)
  (if (/= (type res) 'STR)
    (wt:dbg (list "WWE START TOPOLOGY selected end" (if (= (wt:wwe-g m "END") 0) "END A" "END B")
                  "old topology" (car res) "old partners" (length (cadr res))
                  "moving role" (cond ((= (car res) "T") "BRANCH") ((= (car res) "L") "CORNER") (t "FREE END")))))
  res)

(defun wt:wwe-partner-p (r topo / hit)
  (foreach p (cadr topo) (if (and (listp (car p)) (wt:any-member (car r) (car p))) (setq hit t)))
  hit)

;; AKD: ordinary (non-AKD) linework across the axis path
(defun wt:wwe-akd-path-generic (m x / o a b r d n)
  (setq o (wt:wwe-g m "O") n (wt:perp o)
        a (wt:v+ (wt:wwe-g m "PEXT") (wt:v* o *wt:tx-tol-col*)) b (wt:v- x (wt:v* o *wt:tx-tol-col*)))
  (if (> (wt:dot (wt:v- b a) o) 0.0)
    (foreach e (wt:tx-collect (list a b (wt:v+ b (wt:v* n 0.001)) (wt:v+ a (wt:v* n 0.001))) (wt:wwd-reach) 'wt:tw-akd-line-p)
      (setq d (entget e))
      (if (and (not r) (wt:seg-touch a b (wt:pt2 (cdr (assoc 10 d))) (wt:pt2 (cdr (assoc 11 d))))) (setq r e))))
  r)

;; -> ("OK" intermediate-count) or a refusal message. No drawing change.
(defun wt:wwe-corridor (m tg x topo / o hits n bad net mm w2 xm st len sc recs r parts dd lp)
  (setq *wt:tw-net* (wt:net-scan) o (wt:wwe-g m "O") n 0 hits (wt:wwe-path-hits m tg x))
  (if (= (wt:wwe-g m "SRC") "AKD")
    (progn
      (setq net *wt:tw-net*)
      (foreach e hits
        (setq mm (assoc e (car net)))
        (cond
          (bad)
          ((or (not (setq w2 (wt:wall-from-master mm (cadr net))))
               (< (abs (wt:cross o (wt:w-u w2))) *wt:tx-min-sin*))
           (setq bad "UNSUPPORTED"))
          (t
           (setq xm (wt:xline (wt:wwe-g m "PEXT") o (wt:w-p1 w2) (wt:w-u w2))
                 st (wt:dot (wt:v- xm (wt:w-p1 w2)) (wt:w-u w2)) len (wt:dist (wt:w-p1 w2) (wt:w-p2 w2)))
           (if (or (< st (wt:cfg "TX_CONNECT_DISTANCE")) (> st (- len (wt:cfg "TX_CONNECT_DISTANCE")))
                   (<= (wt:dot (wt:v- x xm) o) (+ (wt:cfg "TX_CONNECT_DISTANCE") (/ (wt:w-thk w2) 2.0))))
             (setq bad "PARTIAL")
             (progn
               (setq n (1+ n))
               (wt:dbg (list "WWE EXTENSION PATH intermediate master" e "VALID CROSS")))))))
      ;; the old L / T partner sits at the start of the path: the wall must pass it by more than D
      (if (and (not bad) (member (car topo) '("L" "T"))
               (<= (wt:dot (wt:v- x (wt:wwe-g m "PEXT")) o)
                   (+ (wt:cfg "TX_CONNECT_DISTANCE") (/ (wt:w-thk (car (cadr topo))) 2.0))))
        (setq bad "PARTIAL"))
      (if (and (not bad) (wt:wwe-akd-path-generic m x)) (setq bad "MIXED")))
    (progn
      (setq sc (wt:wwe-scan (wt:wwe-field m x)) recs (car sc))
      (foreach e hits
        (setq r (wt:wwe-rec-of e recs))
        (cond
          (bad)
          ((not r)
           (setq dd (entget e) lp (list e (wt:pt2 (cdr (assoc 10 dd))) (wt:pt2 (cdr (assoc 11 dd)))))
           (setq bad (if (and (= (strcase (cdr (assoc 8 dd))) (strcase (wt:cfg "WALL_LAYER")))
                              (wt:face-owners lp *wt:tw-net*))
                       "MIXED" "UNSUPPORTED")))
          ((< (abs (wt:cross o (cadr r))) *wt:tx-min-sin*) (setq bad "UNSUPPORTED"))
          ((member r parts))
          ((<= (- (wt:dot x o) (wt:wwe-far m r)) (wt:cfg "TX_CONNECT_DISTANCE")) (setq bad "PARTIAL"))
          ((wt:wwe-partner-p r topo) (setq parts (cons r parts)))
          ((not (wt:wwe-through-p m r)) (setq bad "PARTIAL"))
          (t (setq parts (cons r parts) n (1+ n))
             (wt:dbg (list "WWE EXTENSION PATH intermediate wall" (car r) "VALID CROSS")))))))
  (setq *wt:tw-net* nil)
  (cond
    ((= bad "MIXED")
     (wt:wwe-reject "MIXED AKD / GENERIC JUNCTION" "\nMixed AKD / ordinary wall junctions are not supported yet. No change."))
    ((= bad "PARTIAL")
     (wt:wwe-reject "UNSUPPORTED INTERMEDIATE RELATIONSHIP"
                    "\nThe extension would end inside or too close to another wall. No change."))
    (bad
     (wt:wwe-reject "UNSUPPORTED INTERMEDIATE OBSTRUCTION"
                    "\nAnother wall lies between the wall end and the target. No change."))
    (t
     (wt:dbg (list "WWE EXTENSION PATH old station" (wt:fmt (wt:dot (wt:wwe-g m "PEXT") o))
                   "new station" (wt:fmt (wt:dot x o)) "distance" (wt:fmt (wt:dot (wt:v- x (wt:wwe-g m "PEXT")) o))
                   "intermediate walls" n))
     (list "OK" n))))

;; local cleanup field: selected end .. target point, across the wall, grown by D
(defun wt:wwe-field (m x / u s0 s1 sx)
  (setq u (wt:wwe-g m "SU")
        s0 (wt:dot (wt:v- (wt:wwe-g m "PEXT") (wt:wwe-g m "SP1")) u)
        sx (wt:dot (wt:v- x (wt:wwe-g m "SP1")) u))
  (wt:wwd-field (list (wt:wwe-g m "SP1") u (min s0 sx) (max s0 sx) (wt:wwe-g m "OMIN") (wt:wwe-g m "OMAX"))
                (wt:cfg "TX_CONNECT_DISTANCE") '(0.0 0.0)))

;;; --- AKD -> AKD: intelligent connection (ANALYZE FIRST, MODIFY SECOND) ---

(defun wt:wwe-set (m k v) (cons (cons k v) m))      ; assoc record: newest value wins

;; masters touching p, excluding enames in excl
(defun wt:wwe-conn-at (p excl net / out)
  (foreach mm (car net)
    (if (and (not (member (car mm) excl)) (wt:on-seg p (cadr mm) (caddr mm))) (setq out (cons (car mm) out))))
  out)

;; recognised wall spans ENDING at p, parallel to u, excluding enames in excl
(defun wt:wwe-line-spans-at (p u excl net / out w2)
  (foreach mm (car net)
    (if (and (not (member (car mm) excl))
             (or (wt:peq p (cadr mm)) (wt:peq p (caddr mm)))
             (setq w2 (wt:wall-from-master mm (cadr net)))
             (wt:par u (wt:w-u w2)))
      (setq out (cons w2 out))))
  out)

(defun wt:wwe-far-end (s p) (if (wt:peq p (wt:w-p1 s)) (wt:w-p2 s) (wt:w-p1 s)))

;; wall w with its end p moved to x (direction kept) -> (p1 p2)
(defun wt:wwe-moved (w p x) (if (wt:peq p (wt:w-p1 w)) (list x (wt:w-p2 w)) (list (wt:w-p1 w) x)))

;; short overshoot pieces among spans: far end free and no more than lim from x
(defun wt:wwe-overshoots (spans x lim net / out f)
  (foreach sp spans
    (setq f (wt:wwe-far-end sp x))
    (if (and (<= (wt:dist x f) lim) (not (wt:wwe-conn-at f (list (car sp)) net)))
      (setq out (cons sp out))))
  out)

;; Pure plan (reads the drawing only). -> plan assoc, or a refusal message.
;; Rules:
;;  corner X      centerline x centerline (parallel: collinear -> STRAIGHT, else refused)
;;  source end    moves along its own line: EXTEND (forward, any distance, corridor
;;                checked), TRIM (back, never past the fixed end), REMOVE when the
;;                selected span itself starts at X (an overshoot / detached piece)
;;  target        X inside the span -> T, unless X is within WWE_CORNER_DISTANCE of a
;;                FREE target end and the source does not already touch the target ->
;;                L (target end trimmed); X at a target end -> L (a short free overshoot
;;                piece beyond it is removed), T if the target line continues;
;;                X beyond a FREE target end -> L (target end extended, any distance:
;;                the user picked this target); a connected target end is refused.
;;                WWE_CORNER_DISTANCE only classifies (L vs T near an end, overshoot
;;                pieces); it never limits an explicit extension.
;;  collinear     spans the extended end passes on its own line are absorbed (the
;;                extended span replaces them; their branches stay T); a continuation
;;                past X is refused
;;  refused       parallel, behind the fixed end, target out of reach, target end
;;                connected elsewhere, X already a junction of other walls, ambiguous
;;                continuation / overshoot, corridor obstructions (existing rules)
(defun wt:wwe-plan (m tg / w tw net pe pf o tu l0 lt lim excl x sx tx typ sop top rems tnew ne d sa sb
                         msg cur sp nx absorbed m2 topo cor more mm)
  (setq w (wt:wwe-g m "W") tw (cadr tg) net (wt:net-scan) *wt:tw-net* net
        pe (wt:wwe-g m "PEXT") pf (wt:wwe-g m "PFIX") o (wt:wwe-g m "O")
        tu (wt:w-u tw) l0 (wt:dist pe pf) lt (wt:dist (wt:w-p1 tw) (wt:w-p2 tw))
        lim (wt:cfg "WWE_CORNER_DISTANCE") excl (list (car w) (car tw)) sop "NONE" top "NONE")
  ;; 1. logical corner
  (if (< (abs (wt:cross o tu)) *wt:tx-min-sin*)
    (if (> (abs (wt:cross o (wt:v- (wt:w-p1 tw) pf))) *wt:tol*)
      (setq msg "\nSelected walls are parallel and cannot form a corner.")
      (setq sa (wt:dot (wt:v- (wt:w-p1 tw) pf) o) sb (wt:dot (wt:v- (wt:w-p2 tw) pf) o)
            x (if (< sa sb) (wt:w-p1 tw) (wt:w-p2 tw)) sx (min sa sb) typ "STRAIGHT"
            msg (if (< sx (- l0 *wt:tol*)) "\nTarget overlaps or lies behind the selected wall end. No change.")))
    (setq x (wt:xline pf o (wt:w-p1 tw) tu) sx (wt:dot (wt:v- x pf) o)
          tx (wt:dot (wt:v- x (wt:w-p1 tw)) tu)))
  ;; 2. source end
  (if (not msg)
    (cond
      ((< sx (- *wt:tol*))
       (setq msg "\nTarget is behind the selected wall end.\nThe wall cannot pass its other end."))
      ((<= sx *wt:tol*)
       (if (wt:on-seg pf (wt:w-p1 tw) (wt:w-p2 tw))
         (setq sop "REMOVE" rems (list w))
         (setq msg "\nTarget is behind the selected wall end.\nThe wall cannot pass its other end.")))
      ((> sx (+ l0 *wt:tol*)) (setq sop "EXTEND"))
      ((< sx (- l0 *wt:tol*)) (setq sop "TRIM"))))
  ;; 3. target relationship
  (if (and (not msg) (/= typ "STRAIGHT"))
    (cond
      ((and (> tx *wt:tol*) (< tx (- lt *wt:tol*)))
       (setq ne (if (< tx (/ lt 2.0)) (wt:w-p1 tw) (wt:w-p2 tw)) d (wt:dist ne x) typ "T")
       (if (and (<= d lim) (/= sop "REMOVE")
                (not (wt:on-seg pe (wt:w-p1 tw) (wt:w-p2 tw)))
                (not (wt:wwe-conn-at ne excl net)))
         (setq typ "L" top "TRIM" tnew (wt:wwe-moved tw ne x))))
      ((or (<= (abs tx) *wt:tol*) (<= (abs (- tx lt)) *wt:tol*))
       (setq sp (wt:wwe-line-spans-at x tu excl net) typ (if sp "T" "L")
             nx (wt:wwe-overshoots sp x lim net))
       (cond ((cdr nx) (setq msg "\nAmbiguous target junction. No change."))
             ((and nx (not (cdr sp))) (setq top "REMOVE" typ "L" rems (append rems nx)))))
      (t
       (setq ne (if (< tx 0.0) (wt:w-p1 tw) (wt:w-p2 tw)) d (wt:dist ne x) typ "L")
       (cond ((wt:wwe-conn-at ne excl net)
              (setq msg "\nThe target wall end is already connected elsewhere. No change."))
             (t (setq top "EXTEND" tnew (wt:wwe-moved tw ne x)))))))
  ;; 4. the selected end already sits on the corner: a short overshoot beyond it goes
  (if (and (not msg) (= sop "NONE") (/= typ "STRAIGHT"))
    (progn
      (setq nx (wt:wwe-overshoots (wt:wwe-line-spans-at x o excl net) x lim net))
      (cond ((cdr nx) (setq msg "\nAmbiguous wall continuation at the connection point. No change."))
            (nx (setq sop "REMOVE" rems (append rems nx))))))
  ;; 5. X must not already be a junction of other (crossing) walls
  (if (not msg)
    (foreach e (wt:wwe-conn-at x (append excl (mapcar 'car rems)) net)
      (setq mm (assoc e (car net)))
      (if (and (not msg)
               (not (wt:par o (wt:unit (wt:v- (caddr mm) (cadr mm)))))
               (not (wt:par tu (wt:unit (wt:v- (caddr mm) (cadr mm))))))
        (setq msg "\nThe connection point is already a junction of other walls. No change."))))
  ;; 6. forward: absorb collinear spans on the way, then the existing corridor rules
  (if (and (not msg) (= sop "EXTEND"))
    (progn
      (setq cur pe more t)
      (while (and more (not msg))
        (setq sp (wt:wwe-line-spans-at cur o (append excl (mapcar 'car absorbed)) net))
        (cond ((not sp) (setq more nil))
              ((cdr sp) (setq msg "\nAmbiguous collinear continuation. No change."))
              ((> (wt:dot (wt:v- (wt:wwe-far-end (car sp) cur) pf) o) (+ sx *wt:tol*))
               (setq msg "\nThe wall already continues past the connection point. No change."))
              (t (setq absorbed (cons (car sp) absorbed) cur (wt:wwe-far-end (car sp) cur)))))
      (cond
        (msg)
        ((wt:peq cur x) (if (= top "NONE") (setq msg "\nWall already reaches target.")))
        (t
         (setq m2 (wt:wwe-set (wt:wwe-set m "PEXT" cur) "CONN"
                              (wt:wwe-conn-at cur (cons (car w) (mapcar 'car absorbed)) net))
               topo (wt:wwe-topo-akd m2))
         (cond
           ((member (car topo) '("COMPLEX" "UNSUPPORTED")) (setq msg (wt:wwe-topo-reject (car topo))))
           ((= (type (setq cor (wt:wwe-corridor m2 tg x topo))) 'STR) (setq msg cor)))))
      (if (not msg) (setq rems (append rems absorbed)))))
  ;; 7. a moved target end must not cross another wall on its way
  (if (and (not msg) (= top "EXTEND"))
    (foreach mm (car net)
      (if (and (not msg) (not (member (car mm) excl))
               (wt:seg-touch (wt:v+ ne (wt:v* (wt:unit (wt:v- x ne)) *wt:tx-tol-col*)) x (cadr mm) (caddr mm))
               (not (wt:on-seg x (cadr mm) (caddr mm))))
        (setq msg "\nAnother wall lies between the target wall end and the connection point. No change."))))
  (if (and (not msg) (= sop "NONE") (= top "NONE") (not rems))
    (setq msg "\nWall already reaches target."))
  (setq *wt:tw-net* nil)
  (wt:dbg (list "WWE PLAN corner" x "type" typ "source" sop "target" top "removed" (length rems) "refusal" msg))
  (if msg
    msg
    (list (cons "X" x) (cons "TYPE" typ) (cons "SRC-OP" sop) (cons "TGT-OP" top)
          (cons "SRC-NEW" (if (member sop '("EXTEND" "TRIM")) (wt:wwe-moved w pe x)))
          (cons "TGT-NEW" tnew) (cons "REMOVE" rems) (cons "MOVE" (- sx l0)))))

(defun wt:wwe-new-master (seg th / en)
  (setq en (wt:pend-make (wt:mk-line (car seg) (cadr seg) (wt:cfg "AXIS_LAYER"))))
  (list en (car seg) (cadr seg) th "CENTER"))

;; MODIFY: one transaction. Old spans (moved source / target, overshoot and absorbed
;; pieces) leave through the EW path, the new centred masters enter through the WW path.
(defun wt:wwe-akd-connect (m tg / p w tw rems gone new sn tn rec)
  (setq p (wt:wwe-plan m tg))
  (if (= (type p) 'STR)
    p
    (progn
      (setq w (wt:wwe-g m "W") tw (cadr tg)
            sn (wt:wwe-g p "SRC-NEW") tn (wt:wwe-g p "TGT-NEW"))
      (foreach r (append (if sn (list w)) (if tn (list tw)) (wt:wwe-g p "REMOVE"))
        (if (not (member (car r) gone)) (setq rems (cons r rems) gone (cons (car r) gone))))
      (wt:pend-begin)
      (if rems (wt:rebuild nil (reverse rems)))
      (if sn (setq new (cons (wt:wwe-new-master sn (wt:w-thk w)) new)))
      (if tn (setq new (cons (wt:wwe-new-master tn (wt:w-thk tw)) new)))
      (if new (foreach nn (wt:rebuild (reverse new) nil) (wt:reg-add (car nn) (wt:w-thk nn) "CENTER")))
      ;; command-owned cleanup: old nodes the removed spans left, the new spans' nodes
      (wt:axis-heal-local (append (wt:heal-pts rems) (wt:heal-pts new)))
      (setq rec (wt:pend-end))
      (list rec "\nWall connected." (wt:wwe-g p "MOVE")))))

;; -> (transaction-record message distance) or a message (no change)
(defun wt:wwe-execute (m tg / c x d e dl rec n o p f topo cor)
  (wt:dbg (cons "WWE MOVING WALL source" (cons (wt:wwe-g m "SRC") (wt:wwe-g m "LABEL"))))
  (wt:dbg (list "WWE TARGET source" (car tg) (nth 8 tg) "target supporting line" (caddr tg) (cadddr tg)))
  (if (and (= (wt:wwe-g m "SRC") "AKD") (= (car tg) "AKD"))
    (setq c (wt:wwe-akd-connect m tg))
    (setq c (wt:wwe-generic-execute m tg)))
  c)

(defun wt:wwe-generic-execute (m tg / c x d e dl rec n o p f topo cor)
  (setq c (wt:wwe-target m tg))
  (cond
    ((= (type c) 'STR) c)
    ((progn
       (setq x (car c) d (cadr c))
       (wt:dbg (list "WWE EXTENSION axis intersection" x "extension distance" (wt:fmt d)
                     "classification" (if (<= d *wt:tol*) "ALREADY REACHES" "EXTEND")))
       nil))
    ((/= (wt:wwe-g m "SRC") (car tg))
     (wt:wwe-reject "MIXED AKD / GENERIC JUNCTION"
                    "\nMixed AKD / ordinary wall junctions are not supported yet. No change."))
    ;; connected start end: classify the old node, then the corridor (both read-only)
    ((and (> d *wt:tol*) (= (type (setq topo (wt:wwe-old-topology m))) 'STR)) topo)
    ((and (> d *wt:tol*) (= (type (setq cor (wt:wwe-corridor m tg x topo))) 'STR)) cor)
    ((progn
       (if (> d *wt:tol*)
         (wt:dbg (list "WWE TOPOLOGY TRANSITIONS OLD NODE"
                       (cond ((= (car topo) "T") "T -> CROSS (moving wall passes through)")
                             ((= (car topo) "L") "L -> T (old partner terminates on the moving wall)")
                             (t (strcat (car topo) " -> (end leaves)")))
                       "INTERMEDIATE CROSSES" (cadr cor) "TARGET NODE FREE -> junction")))
       nil))
    (t
     (setq f (wt:wwe-field m x) o (wt:wwe-g m "O"))
     (wt:pend-begin)
     (setq *wt:tx-nested* t *wt:tw-net* (wt:net-scan))
     (if (> d *wt:tol*)
       (progn
         (foreach e (wt:wwe-g m "ENDCAPS") (if (entget e) (wt:pend-erase e)))
         (foreach side (list (wt:wwe-g m "SIDEA") (wt:wwe-g m "SIDEB"))
           (setq e (wt:wwd-extreme side o) dl (entget (caddr e))
                 p (wt:v+ (car e) (wt:v* o (- (wt:dot x o) (wt:dot (car e) o)))))
           (wt:pend-modify (subst (cons (cadr e) (wt:tx-z3 p (cdr (assoc (cadr e) dl)))) (assoc (cadr e) dl) dl)))))
     (setq n (wt:wwd-clean f))
     (wt:dbg (list "WWE CLEANUP field" (car f) (caddr f) "junctions repaired" n
                   "old cap removed" (if (and (> d *wt:tol*) (wt:wwe-g m "ENDCAPS")) "YES" "NO")))
     (setq *wt:tx-nested* nil *wt:tw-net* nil rec *wt:pending* *wt:pending* nil)
     (cond ((> d *wt:tol*) (list rec "\nWall extended." d))
           ((> n 0) (list rec "\nWall already reaches target; junction repaired." d))
           (t (list rec "\nWall already reaches target." d))))))

;; non-interactive driver (tests): -> (record message distance) or a message
(defun wt:wwe-run (e1 q1 e2 q2 / m tg)
  (setq m (wt:wwe-resolve e1 q1))
  (cond ((= (type m) 'STR) m)
        ((= (type (setq tg (wt:wwd-resolve e2 q2))) 'STR) tg)
        ((wt:wwe-same-p m tg) "\nTarget must be a different wall.")
        (t (wt:wwe-execute m tg))))

(defun wt:wwe-pick (msg fn / e res out done)
  (while (not done)
    (setvar "ERRNO" 0)
    (setq e (entsel msg))
    (cond
      ((not e) (if (/= (getvar "ERRNO") 7) (setq done t)))
      ((= (type (setq res (apply fn (list (car e) (wt:pt2 (trans (cadr e) 1 0)))))) 'STR)
       (wt:dbg (list "WWE REJECT" res))
       (princ res))
      (t (setq out res done t))))
  out)

(defun c:WWE (/ *error* m tg res)
  (setq *error* wt:error)
  (wt:begin)
  (if (setq m (wt:wwe-pick "\nSelect wall end to connect: " 'wt:wwe-resolve))
    (progn
      (while (and (setq tg (wt:wwe-pick "\nSelect target wall: " 'wt:wwd-resolve))
                  (wt:wwe-same-p m tg))
        (princ "\nTarget must be a different wall."))
      (if tg
        (princ (if (= (type (setq res (wt:wwe-execute m tg))) 'STR) res (cadr res))))))
  (wt:end))

(defun c:WWO () (c:WWF))   ; old name, undocumented alias

(princ "\nAKD WallTool loaded: AX, ZXW, WW, XW, EW, WWF, WWD, WWE, TW, TX, WWR.")
(princ)
