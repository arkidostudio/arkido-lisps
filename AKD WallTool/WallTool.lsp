;;; WallTool.lsp -- AKD WallTool v0.1.0 (Stage 1: 2D Wall Core)
;;; Commands: AX (axis), ZXW (grid axis), WW (wall), XW (axis to wall), EW (erase wall), WWF (wall from wall), TW (connection repair), WR (wall/axis repair)
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
    (cons "DEFAULT_OFFSET" 1500.0) (cons "TW_CONNECT_DISTANCE" 150.0)))

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
  (setq *wt:pending* nil)
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

;; visible runs of an edge: a piece is visible when exactly one side is inside the union
(defun wt:edge-visible (a b polys cutters / u nn ts s0 m run out)
  (setq u (wt:unit (wt:v- b a)) nn (wt:v* (wt:perp u) *wt:tol-side*)
        ts (wt:edge-cuts a b cutters) s0 (car ts))
  (foreach s1 (cdr ts)
    (setq m (wt:v+ a (wt:v* u (/ (+ s0 s1) 2.0))))
    (if (not (eq (wt:inside-any (wt:v+ m nn) polys) (wt:inside-any (wt:v- m nn) polys)))
      (setq run (list (if run (car run) s0) s1))
      (if run (setq out (cons run out) run nil)))
    (setq s0 s1))
  (if run (setq out (cons run out)))
  (mapcar '(lambda (r) (list (wt:v+ a (wt:v* u (car r))) (wt:v+ a (wt:v* u (cadr r))))) out))

;; walls: all participating walls; regen: indices whose linework is emitted.
;; Returns list of segments (a b).
(defun wt:topo-linework (walls regen / res strips hubs polys cutters cand i runs out)
  (setq res (wt:topo-solve walls) strips (car res) hubs (cadr res)
        polys (append (mapcar 'wt:strip-poly strips) (mapcar 'car hubs))
        cutters (apply 'append (mapcar 'wt:poly-edges polys))
        i 0)
  (foreach s strips
    (if (member i regen) (setq cand (append cand (wt:poly-edges (wt:strip-poly s)))))
    (setq i (1+ i)))
  (foreach h hubs
    (if (wt:any-member (cadr h) regen) (setq cand (append cand (wt:poly-edges (car h))))))
  (foreach e cand
    (setq runs (append runs (wt:edge-visible (car e) (cadr e) polys cutters))))
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
;; +/- thickness/2. nil otherwise (plain axis, off-centre legacy master -> see WR).
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
(defun wt:pend-begin () (setq *wt:pending* (list nil nil nil)))
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
(defun wt:rebuild (new removed / lyr net faces changed rest regen ctx w walls idx keep grow)
  (if new (setq new (wt:normalize-masters new)))
  (setq lyr (wt:cfg "WALL_LAYER") net (wt:net-scan) faces (cadr net) changed (append new removed))
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
  (foreach f faces
    (if (or (wt:any-owned f new) (wt:any-owned f regen) (wt:any-owned f removed))
      (wt:pend-erase (car f))
      (setq keep (cons (list (cadr f) (caddr f)) keep))))
  (foreach w removed (if (entget (car w)) (wt:pend-erase (car w))))
  (foreach s (wt:linework-merged walls idx)
    (if (not (wt:seg-covered s keep))
      (wt:pend-make (wt:mk-line (car s) (cadr s) lyr))))
  new)

;; --- Creation alignment -> centerline. Masters are always wall centerlines. ---

;; Pure. Centerline of a wall of thickness th placed pos ("LEFT" body on the left of
;; travel, "RIGHT" on the right, "CENTER" on the line) relative to drawn p1->p2.
(defun wt:placement-to-centerline (p1 p2 th pos / off n)
  (setq off (cond ((= pos "LEFT") (/ th 2.0)) ((= pos "RIGHT") (/ th -2.0)) (t 0.0))
        n (wt:v* (wt:perp (wt:unit (wt:v- p2 p1))) off))
  (list (wt:v+ p1 n) (wt:v+ p2 n)))

;; WW chain context (only while c:WW binds *wt:chain-on*): (drawn-point ename end-point)
(defun wt:chain-entry (dpt en / r)
  (foreach c *wt:chain*
    (if (and (not r) (eq (cadr c) en) (wt:peq (car c) dpt) (entget en)) (setq r c)))
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
(defun wt:walls-add (segs / net new en rec bad res cls i moved)
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
      (setq rec *wt:pending* *wt:pending* nil)
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

;; Position submenu, left-hand keys: Q = LEFT, W = CENTER, E = RIGHT
;; (L/C/R also accepted). Keywords may contain hyphens; the capital is the key.
(defun wt:ww-position (/ k)
  (initget "Q-left W-center E-right Left Center Right")
  (if (setq k (getkword (strcat "\nPosition [Q-left/W-center/E-right] <"
                                (cdr (assoc *wt:pos* '(("LEFT" . "Q-left") ("CENTER" . "W-center") ("RIGHT" . "E-right"))))
                                ">: ")))
    (setq *wt:pos* (cdr (assoc (substr k 1 1) '(("Q" . "LEFT") ("W" . "CENTER") ("E" . "RIGHT")
                                                 ("L" . "LEFT") ("C" . "CENTER") ("R" . "RIGHT"))))))
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
(defun c:WW (/ *error* p0 p q pts hist r done *wt:chain* *wt:chain-on*)
  (setq *error* wt:error *wt:chain-on* t)
  (wt:begin)
  (wt:layer "AXIS")
  (wt:layer "WALL")
  (wt:status)
  (while (not done)
    ;; "posiTion": the capital T is the keyword letter
    (setq q (if p
              (wt:getpt p "\nSpecify next point or [Width/posiTion/Undo/Close]: "
                        "Width posiTion Undo Close")
              (wt:getpt nil (if hist "\nSpecify start point or [Width/posiTion/Rectangle/Undo/Settings]: "
                                     "\nSpecify start point or [Width/posiTion/Rectangle/Settings]: ")
                        "Width posiTion Rectangle Undo Settings")))
    (cond
      ((not q) (setq done t))
      ((= q "Width") (wt:ww-thickness))
      ((= q "posiTion") (wt:ww-position))
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
  (setq *wt:pending* nil)
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
(defun wt:ew-erase (items margin / net walls amb none other r)
  (setq net (wt:net-scan) amb 0 none 0 other 0)
  (foreach it items
    (if (/= (caddr it) "LINE")
      (setq other (1+ other))
      (progn
        (setq r (wt:ew-resolve (car it) (cadr it) net margin))
        (cond ((= (car r) "OK")
               (if (not (assoc (car (cadr r)) walls)) (setq walls (cons (cadr r) walls))))
              ((= (car r) "AMBIG") (setq amb (1+ amb)))
              (t (setq none (1+ none)))))))
  (if walls
    (progn
      (wt:pend-begin)
      (wt:rebuild nil walls)
      (setq *wt:pending* nil)
      (princ (strcat "\n" (itoa (length walls)) " wall(s) erased."))))
  (if (> amb 0)
    (princ (strcat "\n" (itoa amb) " ambiguous wall line(s) skipped. Select closer to the wall segment to erase.")))
  (if (> none 0)
    (princ (strcat "\n" (itoa none) " line(s) not identified as AKD WallTool walls, skipped.")))
  (if (> other 0)
    (princ (strcat "\n" (itoa other) " unsupported object(s) ignored.")))
  walls)

(defun c:EW (/ *error* ss)
  (setq *error* wt:error)
  (if (setq ss (ssget "_I")) (sssetfirst nil nil))   ; before any command clears PickFirst
  (wt:begin)
  (if (not ss) (progn (princ "\nSelect wall: ") (setq ss (ssget))))
  (if ss (wt:ew-erase (wt:ss-items ss) (wt:pick-margin)))
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
;;; 18. TW -- wall repair in a field (repairs the MASTER network, then wt:rebuild)
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
(defun wt:tw-repair (field tol / net walls w r moves stubs amb nodes keepws stubws n rec)
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
      (setq net (wt:net-scan))
      (foreach f (cadr net)
        (if (and (not (wt:any-owned f walls))
                 (not (wt:face-owners f net))
                 (wt:tw-near-master-p f walls))
          (wt:pend-erase (car f))))
      (wt:rebuild (reverse keepws) stubws)
      (setq rec *wt:pending* *wt:pending* nil)
      (setq n (+ (length nodes) (length stubs)))
      (princ (strcat "\nTW: "
                     (if (> n 0) (strcat (itoa n) " wall junction(s) repaired.") "Wall geometry rebuilt.")
                     (if (> amb 0) (strcat " " (itoa amb) " ambiguous connection(s) skipped.") "")))
      rec)))

(defun c:TW (/ *error* c1 c2)
  (setq *error* wt:error)
  (wt:begin)
  (wt:layer "AXIS")
  (wt:layer "WALL")
  (if (and (setq c1 (getpoint "\nSpecify first corner of wall repair area: "))
           (setq c2 (getcorner c1 "\nSpecify opposite corner: ")))
    (progn
      (princ "\nRepairing wall network...")
      (wt:tw-repair (wt:tw-field c1 c2) (wt:cfg "TW_CONNECT_DISTANCE"))))
  (wt:end))

;;; ===================================================================
;;; 19. WR -- wall repair: audit / repair centerline masters in a field
;;; ===================================================================
;;; WR repairs what the walls ARE (centerline, thickness, missing masters).
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

;; Audit and repair wall masters in field. Returns the transaction record (or nil).
(defun wt:wr-repair (field / net recs band reg adj amb ambs created walls i r w m p xs x nw k d
                            pass more c seg en rec checked msg)
  (setq net (wt:net-scan) adj 0 created 0)
  ;; 1. audit existing masters: faces decide the centerline and thickness
  (foreach m (car net)
    (if (wt:tw-seg-in-field (cadr m) (caddr m) field)
      (progn
        (setq band (wt:wr-band (wt:wr-offsets (cadr m) (caddr m) (cadr net)))
              reg (assoc (car m) *wt:reg*))
        (cond
          ((= (type band) 'LIST)
           (setq d (wt:v* (wt:perp (wt:unit (wt:v- (caddr m) (cadr m)))) (car band)))
           (setq recs (cons (list m (list (car m) (wt:v+ (cadr m) d) (wt:v+ (caddr m) d) (cadr band) "CENTER")) recs)))
          (reg                                  ; recognised wall with missing faces: master wins
           (setq recs (cons (list m (list (car m) (cadr m) (caddr m) (cadr reg) "CENTER")) recs)))
          (band (setq ambs (cons (car m) ambs)))))))
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
    (setq more nil pass (1+ pass) net (wt:net-scan))
    (foreach f (cadr net)
      (if (and (wt:tw-seg-in-field (cadr f) (caddr f) field)
               (> (wt:dist (cadr f) (caddr f)) *wt:tol*)
               (not (wt:any-owned f walls))
               (not (wt:face-owners f net)))
        (progn
          (setq c (wt:wr-candidate f (cadr net) walls))
          (cond
            ((not c))
            ((= (car c) "AMBIG") (if (not (member (car f) ambs)) (setq ambs (cons (car f) ambs))))
            (t
             (setq seg (cadr c))
             (if (and (wt:pip (wt:v* (wt:v+ (car seg) (cadr seg)) 0.5) field)
                      (not (wt:wr-overlaps-p seg (caddr c) walls))
                      (setq en (wt:pend-make (wt:mk-line (car seg) (cadr seg) (wt:cfg "AXIS_LAYER")))))
               (progn
                 (wt:reg-add en (caddr c) "CENTER")
                 (setq walls (cons (list en (car seg) (cadr seg) (caddr c) "CENTER") walls)
                       created (1+ created) more t)))))))))
  ;; 5. stale unclaimed A-WALL in the field beside the walls, then normalize + rebuild
  (setq net (wt:net-scan))
  (foreach f (cadr net)
    (if (and (wt:tw-seg-in-field (cadr f) (caddr f) field)
             (not (wt:any-owned f walls))
             (not (wt:face-owners f net))
             (not (member (car f) ambs))
             (wt:tw-near-master-p f walls))
      (wt:pend-erase (car f))))
  (if walls (wt:rebuild (reverse walls) nil))
  (setq rec *wt:pending* *wt:pending* nil
        checked (length walls) amb (length ambs))
  (setq msg (strcat "\nWR: " (itoa checked) " wall(s) checked."))
  (if (and (= adj 0) (= created 0))
    (setq msg (strcat msg " No axis repairs required."))
    (setq msg (strcat msg (if (> adj 0) (strcat " " (itoa adj) " axis/axes adjusted.") "")
                          (if (> created 0) (strcat " " (itoa created) " missing axis/axes rebuilt.") ""))))
  (if (> amb 0) (setq msg (strcat msg " " (itoa amb) " ambiguous wall(s) skipped.")))
  (princ msg)
  rec)

(defun c:WR (/ *error* c1 c2)
  (setq *error* wt:error)
  (wt:begin)
  (wt:layer "AXIS")
  (wt:layer "WALL")
  (if (and (setq c1 (getpoint "\nSpecify first corner of wall repair area: "))
           (setq c2 (getcorner c1 "\nSpecify opposite corner: ")))
    (wt:wr-repair (wt:tw-field c1 c2)))
  (wt:end))

(defun c:WWO () (c:WWF))   ; old name, undocumented alias

(princ "\nAKD WallTool loaded: AX, ZXW, WW, XW, EW, WWF, TW, WR.")
(princ)
