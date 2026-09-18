;; AKDColumn.lsp  -  Arkido Column
;; Command AC    Place a column and let WallTool rebuild affected wall ends.
;; Command CB    Set the base anchor (TL/TC/TR/ML/C/MR/BL/BC/BR).
;; Command CCW   Refuses resizing until it can update wall masters atomically.
;;
;; Load WallTool before this file. All column entities are grouped as AKCOLn.
;; Emits two AWALL POINT tags per
;; column so AKDProjections' WE/SECT picks up the column in either direction.

(if (null *col-w*)        (setq *col-w* 300.0))
(if (null *col-d*)        (setq *col-d* 300.0))
(if (null *col-dia*)      (setq *col-dia* 300.0))
(if (null *col-shape*)    (setq *col-shape* "R"))   ; "R" rect, "C" circle
(if (null *col-base*)     (setq *col-base* "C"))
(if (null *col-axis-off*) (setq *col-axis-off* 100.0))

(setq *col-layer*        "S-COLUMN")
(setq *col-layer-color*  3)     ; layer color -> outer poly is BYLAYER
(setq *col-hatch-color*  8)     ; grey solid hatch
(setq *col-axis-color*   1)     ; red axis cross

;; ---- helpers ------------------------------------------------------

(defun akc:ensure-lyr (name col / cmde)
  (if (null (tblsearch "LAYER" name))
    (progn (setq cmde (getvar "CMDECHO")) (setvar "CMDECHO" 0)
           (command-s "_.-layer" "_M" name "_C" (itoa col) "" "")
           (setvar "CMDECHO" cmde))))

;; Outer column polyline. On S-COLUMN, color BYLAYER (no 62 group).
(defun akc:mkpline (pts / e)
  (akc:ensure-lyr *col-layer* *col-layer-color*)
  (setq e (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                (cons 8 *col-layer*)
                '(100 . "AcDbPolyline") (cons 90 (length pts)) '(70 . 1)))
  (foreach v pts
    (setq e (append e (list (cons 10 (list (car v) (cadr v)))))))
  (entmakex e))

;; LINE on S-COLUMN; optional integer color override (nil = BYLAYER).
(defun akc:mkline (p1 p2 col-override)
  (akc:ensure-lyr *col-layer* *col-layer-color*)
  (entmakex (append (list '(0 . "LINE") (cons 8 *col-layer*)
                          (cons 10 p1) (cons 11 p2))
                    (if col-override (list (cons 62 col-override)) nil))))

(defun akc:bl-from (ins w d anchor / dx dy)
  (setq dx (cond ((member anchor '("TL" "ML" "BL")) 0.0)
                 ((member anchor '("TC" "C"  "BC")) (- (/ w 2.0)))
                 (t (- w)))
        dy (cond ((member anchor '("BL" "BC" "BR")) 0.0)
                 ((member anchor '("ML" "C"  "MR")) (- (/ d 2.0)))
                 (t (- d))))
  (list (+ (car ins) dx) (+ (cadr ins) dy) 0.0))

(defun akc:uniqname (prefix / gd i n)
  (setq gd (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_GROUP")))
        i  1
        n  (strcat prefix "1"))
  (while (and gd (dictsearch gd n))
    (setq i (1+ i) n (strcat prefix (itoa i))))
  n)

(defun akc:mkgroup (name ents / ss gd)
  (setq ss (ssadd))
  (foreach e ents (if e (ssadd e ss)))
  (command-s "_.-group" "_create" name "" ss "")
  (setq gd (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_GROUP"))))
  (if (and gd (dictsearch gd name)) name (exit)))

;; Rectangle geometry used by placement and restoration.
(defun akc:pt-in-rect (p bl tr / eps)
  (setq eps 1e-4)
  (and (> (car p) (+ (car bl) eps)) (< (car p) (- (car tr) eps))
       (> (cadr p) (+ (cadr bl) eps)) (< (cadr p) (- (cadr tr) eps))))

(defun akc:line-touches-box (p1 p2 bl tr)
  (and (>= (max (car p1) (car p2)) (car bl))
       (<= (min (car p1) (car p2)) (car tr))
       (>= (max (cadr p1) (cadr p2)) (cadr bl))
       (<= (min (cadr p1) (cadr p2)) (cadr tr))))

;; ---- axis ghost & pick --------------------------------------------

(defun akc:ghost-cross (bl tr cx cy)
  (grdraw (list (car bl) cy 0.0) (list (car tr) cy 0.0) 1 -1)
  (grdraw (list cx (cadr bl) 0.0) (list cx (cadr tr) 0.0) 1 -1))

(defun akc:pick-axis (bl tr / ax lx rx by ty mx my snaps cx cy g m best bd pt d
                                done abort)
  (setq ax *col-axis-off*
        lx (+ (car bl) ax) rx (- (car tr) ax)
        by (+ (cadr bl) ax) ty (- (cadr tr) ax)
        mx (* 0.5 (+ (car bl) (car tr)))
        my (* 0.5 (+ (cadr bl) (cadr tr)))
        snaps (list (list lx by) (list rx by) (list rx ty) (list lx ty)
                    (list mx my))
        cx mx cy my done nil abort nil)
  (princ "\nMove to snap axis cross (corner or center), click to set: ")
  (akc:ghost-cross bl tr cx cy)
  (while (not (or done abort))
    (setq g (vl-catch-all-apply 'grread (list t 13 0)))
    (cond
      ((vl-catch-all-error-p g) (setq abort t))
      ((or (= (car g) 5) (= (car g) 3))
        (setq m (cadr g) best nil bd 1e99)
        (foreach pt snaps
          (setq d (distance m pt))
          (if (< d bd) (setq bd d best pt)))
        (setq cx (car best) cy (cadr best))
        (redraw)
        (akc:ghost-cross bl tr cx cy)
        (if (= (car g) 3) (setq done t)))
      ((and (= (car g) 2) (= (cadr g) 27)) (setq abort t))))
  (redraw)
  (if (not abort) (list cx cy 0.0)))

;; ---- AWALL tag ----------------------------------------------------
(defun akc:tag-awall (p1 p2 thk h base / mid cmde)
  (if (null (tblsearch "LAYER" "A-WALL-DATA"))
    (progn (setq cmde (getvar "CMDECHO")) (setvar "CMDECHO" 0)
           (command-s "_.-layer" "_M" "A-WALL-DATA" "_C" "8" ""
                                "_OFF" "A-WALL-DATA" "")
           (setvar "CMDECHO" cmde)))
  (regapp "AWALL")
  (setq mid (list (* 0.5 (+ (car p1) (car p2)))
                  (* 0.5 (+ (cadr p1) (cadr p2))) 0.0))
  (entmakex (list (cons 0 "POINT") (cons 8 "A-WALL-DATA") (cons 10 mid)
                  (list -3 (list "AWALL"
                                 (cons 1000 "AWALL")
                                 (cons 1040 thk)
                                 (cons 1040 h)
                                 (cons 1040 base)
                                 (list 1011 (car p1) (cadr p1) 0.0)
                                 (list 1011 (car p2) (cadr p2) 0.0))))))

;; Read cut history from columns made by older AKDColumn versions.


(defun akc:read-cuts (ent / d xd out cur)
  (setq d (entget ent (list "AKCOL"))
        xd (cdr (assoc "AKCOL" (cdr (assoc -3 d))))
        out nil cur nil)
  (foreach it xd
    (cond ((and (= (car it) 1002) (= (cdr it) "{")) (setq cur nil))
          ((and (= (car it) 1002) (= (cdr it) "}"))
             (if (= (length cur) 3)
               (setq out (cons (reverse cur) out))))
          (t (setq cur (cons (cdr it) cur)))))
  (reverse out))

;; ---- column number pick (dynamic input keyword list) --------------
;; Scan all TEXT: pair each "C<n>" with the size text right below it.
;; Label writer places size 1.4*height below C-text at same x.
(defun akc:existing-cols ( / ss i e d s p n texts sz best bd q dx dy out)
  (setq ss (ssget "_X" '((0 . "TEXT"))) texts nil out nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) i (1+ i)
              d (entget e)
              s (cdr (assoc 1 d))
              p (cdr (assoc 10 d)))
        (if s (setq texts (cons (list s p) texts))))
      (foreach t1 texts
        (setq s (car t1) p (cadr t1))
        (if (and (> (strlen s) 1) (= (substr s 1 1) "C")
                 (> (setq n (atoi (substr s 2))) 0)
                 (not (assoc n out)))
          (progn
            (setq best nil bd 60.0)   ; within 60 units below, ~2*label height
            (foreach t2 texts
              (setq q (cadr t2)
                    dx (abs (- (car p) (car q)))
                    dy (- (cadr p) (cadr q)))
              (if (and (< dx 5.0) (> dy 20.0) (< dy bd)
                       (not (equal q p 1e-6)))
                (setq best (car t2) bd dy)))
            (if best (setq out (cons (list n best) out))))))))
  (vl-sort out '(lambda (a b) (< (car a) (car b)))))

(defun akc:existing-nums ( / )
  (mapcar 'car (akc:existing-cols)))

(defun akc:next-num (used / n)
  (setq n 1) (while (member n used) (setq n (1+ n))) n)

(defun akc:pick-num ( / cols nums dflt kws disp kw)
  (setq cols (akc:existing-cols)
        nums (mapcar 'car cols)
        dflt (akc:next-num nums)
        kws  (apply 'strcat
               (mapcar '(lambda (x) (strcat "C" (itoa x) " ")) nums))
        disp (if cols
               (apply 'strcat
                 (cons "["
                   (append
                     (mapcar '(lambda (c)
                                (strcat "C" (itoa (car c))
                                        "(" (cadr c) ")/"))
                             cols)
                     (list "new] "))))
               ""))
  (if (> (strlen kws) 0) (initget 128 kws) (initget 128))
  (setq kw (getkword (strcat "\nColumn tag " disp "<C" (itoa dflt) ">: ")))
  (cond
    ((or (null kw) (= kw "")) dflt)
    ((= (substr kw 1 1) "C") (max 1 (atoi (substr kw 2))))
    (t (max 1 (atoi kw)))))

;; ---- corner label (C#, WxD) --------------------------------------
(defun akc:mktext (p h str / )
  (akc:ensure-lyr "Defpoints" 7)
  (entmakex (list '(0 . "TEXT") (cons 8 "Defpoints") (cons 62 1)
                  (cons 10 p) (cons 11 p) (cons 40 h) (cons 1 str)
                  (cons 72 0) (cons 73 0))))

(defun akc:label (tl sizestr num / n th p1 p2)
  (setq n  (itoa num)
        th 30.0
        p1 (list (+ (car tl) (* 0.2 th)) (- (cadr tl) (* 2.6 th)) 0.0)
        p2 (list (+ (car tl) (* 0.2 th)) (- (cadr tl) (* 1.2 th)) 0.0))
  (list (akc:mktext p2 th (strcat "C" n))
        (akc:mktext p1 th sizestr)))

;; ---- base-anchor picker (interactive) -----------------------------

(defun akc:draw-base-picker (bl tr anchors hover / a pt bsz sz col k rot)
  (setq bsz (/ (- (car tr) (car bl)) 40.0))
  (grdraw bl (list (car tr) (cadr bl) 0.0) 3 -1)
  (grdraw (list (car tr) (cadr bl) 0.0) tr 3 -1)
  (grdraw tr (list (car bl) (cadr tr) 0.0) 3 -1)
  (grdraw (list (car bl) (cadr tr) 0.0) bl 3 -1)
  (foreach a anchors
    (setq pt (cadr a) k (car a)
          col (cond ((equal k hover)      2)     ; yellow
                    ((equal k *col-base*) 4)     ; cyan
                    (t                    1))    ; red
          sz  (if (equal k hover) (* 2.5 bsz) bsz))
    ;; box marker
    (grdraw (list (- (car pt) sz) (- (cadr pt) sz) 0.0)
            (list (+ (car pt) sz) (- (cadr pt) sz) 0.0) col -1)
    (grdraw (list (+ (car pt) sz) (- (cadr pt) sz) 0.0)
            (list (+ (car pt) sz) (+ (cadr pt) sz) 0.0) col -1)
    (grdraw (list (+ (car pt) sz) (+ (cadr pt) sz) 0.0)
            (list (- (car pt) sz) (+ (cadr pt) sz) 0.0) col -1)
    (grdraw (list (- (car pt) sz) (+ (cadr pt) sz) 0.0)
            (list (- (car pt) sz) (- (cadr pt) sz) 0.0) col -1)
    (if (equal k hover)
      (progn
        ;; hollow center cross-hair
        (grdraw (list (- (car pt) (* 0.5 sz)) (cadr pt) 0.0)
                (list (+ (car pt) (* 0.5 sz)) (cadr pt) 0.0) col -1)
        (grdraw (list (car pt) (- (cadr pt) (* 0.5 sz)) 0.0)
                (list (car pt) (+ (cadr pt) (* 0.5 sz)) 0.0) col -1)
        ;; label next to marker
        (grtext -2 (strcat " " k))))))

(defun akc:pick-base ( / vc vs w h cx cy bl tr anchors g pk bd best pt a ok abort)
  (setq vc (getvar "VIEWCTR")
        vs (getvar "VIEWSIZE")
        w  (/ vs 5.0)
        h  (/ vs 7.0)
        cx (car vc) cy (cadr vc)
        bl (list (- cx (/ w 2.0)) (- cy (/ h 2.0)) 0.0)
        tr (list (+ cx (/ w 2.0)) (+ cy (/ h 2.0)) 0.0)
        anchors (list
          (list "TL" (list (car bl) (cadr tr) 0.0))
          (list "TC" (list cx       (cadr tr) 0.0))
          (list "TR" (list (car tr) (cadr tr) 0.0))
          (list "ML" (list (car bl) cy        0.0))
          (list "C"  (list cx       cy        0.0))
          (list "MR" (list (car tr) cy        0.0))
          (list "BL" (list (car bl) (cadr bl) 0.0))
          (list "BC" (list cx       (cadr bl) 0.0))
          (list "BR" (list (car tr) (cadr bl) 0.0)))
        ok nil abort nil best nil)
  (akc:draw-base-picker bl tr anchors best)
  (princ (strcat "\nClick a base anchor (current=" *col-base*
                 ", Esc to keep): "))
  (while (not (or ok abort))
    (setq g (vl-catch-all-apply 'grread (list t 13 0)))
    (cond
      ((vl-catch-all-error-p g) (setq abort t))
      ((= (car g) 5)
        (setq pk (cadr g) bd 1e99 a nil pt nil)
        (foreach a anchors
          (setq pt (cadr a))
          (if (< (distance pk pt) bd)
            (setq bd (distance pk pt) best (car a))))
        (redraw)
        (akc:draw-base-picker bl tr anchors best))
      ((= (car g) 3)
        (setq pk (cadr g) bd 1e99)
        (foreach a anchors
          (setq pt (cadr a))
          (if (< (distance pk pt) bd)
            (setq bd (distance pk pt) best (car a))))
        (setq ok t))
      ((and (= (car g) 2) (= (cadr g) 27)) (setq abort t))))
  (redraw)
  (if (and ok best)
    (progn (setq *col-base* best)
           (princ (strcat "\nBase anchor = " *col-base* "."))))
  best)

;; ---- commands -----------------------------------------------------

(defun c:CB ( / )
  (akc:pick-base)
  (princ))

;; Return (bl tr) bbox of an LWPOLYLINE, or nil.
(defun akc:pl-bbox (ent / d mnx mny mxx mxy x y)
  (setq d (entget ent) mnx 1e99 mny 1e99 mxx -1e99 mxy -1e99)
  (foreach it d
    (if (= 10 (car it))
      (progn (setq x (cadr it) y (caddr it))
             (if (< x mnx) (setq mnx x)) (if (> x mxx) (setq mxx x))
             (if (< y mny) (setq mny y)) (if (> y mxy) (setq mxy y)))))
  (if (< mnx mxx) (list (list mnx mny 0.0) (list mxx mxy 0.0))))



;; Find AKCOL* group containing ent; returns (gname . ent-list) or nil.
(defun akc:group-of (ent / gd rec nm ents found targetH)
  (setq gd (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_GROUP")))
        targetH (cdr (assoc 5 (entget ent))))
  (if gd
    (progn
      (setq rec (dictnext gd T))
      (while (and rec (not found))
        (setq nm (cdr (assoc 3 rec)) ents nil)
        (foreach it rec
          (if (= (car it) 340) (setq ents (cons (cdr it) ents))))
        (if (and nm (wcmatch nm "AKCOL*")
                 (vl-some '(lambda (o)
                             (and (entget o)
                                  (= (cdr (assoc 5 (entget o))) targetH)))
                          ents))
          (setq found (cons nm ents)))
        (setq rec (dictnext gd)))))
  found)

;; Delete one resolved AKCOL group and restore the wall lines it cut.
;; Returns the number of restored walls, or nil if the group is invalid.
(defun akc:erase-group (grp / poly cuts nm ents)
  (if grp
    (progn
      (setq nm (car grp) ents (cdr grp))
      (foreach x ents
        (if (and x (entget x)
                 (member (cdr (assoc 0 (entget x))) '("LWPOLYLINE" "CIRCLE"))
                 (= (strcase (cdr (assoc 8 (entget x)))) (strcase *col-layer*)))
          (setq poly x)))
      (setq cuts (if poly (akc:read-cuts poly)))
      (foreach r cuts
        (akc:ensure-lyr (car r) 7)
        (entmakex (list '(0 . "LINE") (cons 8 (car r))
                        (cons 10 (cadr r)) (cons 11 (caddr r)))))
      (foreach x ents (if (and x (entget x)) (entdel x)))
      (length cuts))))

;; Circular column outline. Wall intersections are refused below.
(defun akc:mkcircle (ctr r / )
  (akc:ensure-lyr *col-layer* *col-layer-color*)
  (entmakex (list '(0 . "CIRCLE") (cons 8 *col-layer*)
                  (cons 10 ctr) (cons 40 r))))

;; WallTool integration. Load WallTool first, then this file last: both define EW.
;; Wall masters are planned and changed through WallTool's transaction path.
(defun akc:wt-ready ()
  (and (member 'WT:REBUILD (atoms-family 0))
       (member 'WT:EW-ERASE (atoms-family 0))
       (progn (wt:init) t)))

(defun akc:clip-rect (a b bl tr ex ey / lo hi v low high d t0 t1 swap ok k)
  ;; Parametric segment interval inside the rectangle enlarged by the
  ;; centered wall's perpendicular half-thickness in each coordinate.
  (setq lo 0.0 hi 1.0 ok t)
  (foreach k '(0 1)
    (setq v (nth k a) d (- (nth k b) v)
          low (- (nth k bl) (if (= k 0) ex ey))
          high (+ (nth k tr) (if (= k 0) ex ey)))
    (if (< (abs d) *wt:tol*)
      (if (or (<= v low) (>= v high)) (setq ok nil))
      (progn
        (setq t0 (/ (- low v) d) t1 (/ (- high v) d))
        (if (> t0 t1) (setq swap t0 t0 t1 t1 swap))
        (setq lo (max lo t0) hi (min hi t1)))))
  (if (and ok (< lo hi)) (list lo hi)))

(defun akc:inside-rect (p bl tr)
  (and p (<= (- (car bl) *wt:tol*) (car p) (+ (car tr) *wt:tol*))
         (<= (- (cadr bl) *wt:tol*) (cadr p) (+ (cadr tr) *wt:tol*))))

(defun akc:column-overlap (bl tr / ss i e d bb c r hit)
  (if (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE,CIRCLE")
                                 (cons 8 *col-layer*) (cons 410 (getvar "CTAB")))))
    (repeat (setq i (sslength ss))
      (setq e (ssname ss (setq i (1- i))) d (entget e)
            c (cdr (assoc 10 d)) r (cdr (assoc 40 d))
            bb (if (= (cdr (assoc 0 d)) "CIRCLE")
                 (list (list (- (car c) r) (- (cadr c) r))
                       (list (+ (car c) r) (+ (cadr c) r)))
                 (akc:pl-bbox e)))
      (if (and bb (< (car bl) (car (cadr bb)))
                   (> (car tr) (car (car bb)))
                   (< (cadr bl) (cadr (cadr bb)))
                   (> (cadr tr) (cadr (car bb))))
        (setq hit t))))
  hit)

(defun akc:point-in-wall-field (pt w bl tr / u h ex ey)
  (setq u (wt:w-u w) h (/ (wt:w-thk w) 2.0)
        ex (* h (abs (cadr u))) ey (* h (abs (car u))))
  (akc:inside-rect pt
    (list (- (car bl) ex) (- (cadr bl) ey))
    (list (+ (car tr) ex) (+ (cadr tr) ey))))

(defun akc:wall-cut-plan (bl tr / net ms faces m w a b u half iv spans
                              plan bad p q cross other ow f owner x)
  (setq net (wt:net-scan) ms (car net) faces (cadr net))
  (foreach m ms
    (setq a (cadr m) b (caddr m))
    (if (<= (wt:dist a b) *wt:tol*) (setq bad "Zero-length master"))
    (if (not bad)
      (progn
        (setq w (wt:wall-from-master m faces)
              u (wt:unit (wt:v- b a))
              half (if w (/ (wt:w-thk w) 2.0) 0.0)
              iv (akc:clip-rect a b bl tr (* half (abs (cadr u)))
                                         (* half (abs (car u)))))
        (if iv
          (if (not w)
            (setq bad "Wall ownership cannot be reconstructed")
            (progn
              (setq spans nil)
              (if (> (car iv) 0.0)
                (if (> (* (car iv) (wt:dist a b)) *wt:tol*)
                  (setq spans (cons (list a (wt:v+ a (wt:v* (wt:v- b a) (car iv)))) spans))
                  (setq bad "Zero-length wall remnant")))
              (if (< (cadr iv) 1.0)
                (if (> (* (- 1.0 (cadr iv)) (wt:dist a b)) *wt:tol*)
                  (setq spans (cons (list (wt:v+ a (wt:v* (wt:v- b a) (cadr iv))) b) spans))
                  (setq bad "Zero-length wall remnant")))
              (setq plan (cons (list w spans) plan))))))))
  ;; A merged face can belong to adjacent pieces of one straight wall.
  ;; Other multiple-owner faces are not safe evidence for a cut.
  (foreach x plan
    (setq w (car x))
    (foreach f faces
      (if (wt:owned-line-p f w)
        (foreach owner (wt:face-owners f net)
          (if (and (not (eq (car owner) (car w)))
                   (not (and (wt:par (wt:w-u w) (wt:w-u owner))
                             (< (abs (wt:cross (wt:w-u w)
                                  (wt:v- (wt:w-p1 owner) (wt:w-p1 w)))) *wt:tol*)
                             (wt:seg-touch (wt:w-p1 w) (wt:w-p2 w)
                                           (wt:w-p1 owner) (wt:w-p2 owner)))))
            (setq bad "Ambiguous wall face ownership"))))))
  ;; Every master touching the field must be in the same cut plan. Junctions
  ;; are valid only when all their arms are accounted for in that plan.
  (foreach x plan
    (setq w (car x))
    (foreach other ms
      (if (not (eq (car w) (car other)))
        (progn
          (setq p (cadr other) q (caddr other)
                cross (wt:xline (wt:w-p1 w) (wt:w-u w) p
                                (wt:unit (wt:v- q p))))
          (if (and cross (wt:on-seg cross (wt:w-p1 w) (wt:w-p2 w))
                         (wt:on-seg cross p q) (akc:point-in-wall-field cross w bl tr)
                         (not (assoc (car other) (mapcar 'car plan))))
            (setq bad "Junction arm is outside the cut plan"))
          (if (and (wt:par (wt:w-u w) (wt:unit (wt:v- q p)))
                   (wt:seg-touch (wt:w-p1 w) (wt:w-p2 w) p q))
            (cond
              ((or (wt:in-seg p (wt:w-p1 w) (wt:w-p2 w))
                   (wt:in-seg q (wt:w-p1 w) (wt:w-p2 w))
                   (wt:in-seg (wt:w-p1 w) p q)
                   (wt:in-seg (wt:w-p2 w) p q))
               (setq bad "Overlapping wall masters"))
              ((and (setq ow (wt:wall-from-master other faces))
                    (> (abs (- (wt:w-thk w) (wt:w-thk ow))) *wt:tol*)
                    (or (akc:inside-rect p bl tr) (akc:inside-rect q bl tr)))
               (setq bad "Split masters have incompatible thickness"))))))))
  (if bad (list nil bad) (list (reverse plan) nil)))

(defun akc:cut-masters (plan / new en w s item result)
  (wt:pend-begin)
  (foreach item plan (wt:pend-erase (car (car item))))
  (foreach item plan
    (setq w (car item))
    (foreach s (cadr item)
      (setq en (wt:pend-make (wt:mk-line (car s) (cadr s) (wt:cfg "AXIS_LAYER"))))
      (wt:reg-add en (wt:w-thk w) "CENTER")
      (setq new (cons (list en (car s) (cadr s) (wt:w-thk w) "CENTER") new))))
  (setq result (wt:rebuild new (mapcar 'car plan)))
  (setq *wt:pending* nil)
  result)

(defun akc:mark-wall-gap (poly count / d)
  ;; A count only. No cut coordinates, master handles, or AKCOL cut history.
  (regapp "AKCOLW")
  (setq d (entget poly))
  (entmod (append d (list (list -3 (list "AKCOLW" (cons 1070 count)))))))

(defun akc:wall-gap-count (poly / d xd)
  (setq d (entget poly (list "AKCOLW"))
        xd (cdr (assoc "AKCOLW" (cdr (assoc -3 d)))))
  (if xd (cdr (assoc 1070 xd))))

;; Drawing-owned XRecord keyed by the column outline handle. Handles survive
;; reloads; enames do not. Each M is an original master, each S a new stub.
(defun akc:record-dict (create / d e)
  (setq d (cdr (assoc -1 (dictsearch (namedobjdict) "AKCOL_MASTERS"))))
  (if (and (not d) create)
    (progn
      (setq e (entmakex '((0 . "DICTIONARY") (100 . "AcDbDictionary"))))
      (if e (setq d (dictadd (namedobjdict) "AKCOL_MASTERS" e)))))
  d)

(defun akc:record-entry (kind w / d)
  (setq d (entget (car w)))
  (append (list (cons 1 kind))
          (if (= kind "S") (list (cons 2 (cdr (assoc 5 d)))))
          (list (cons 10 (wt:w-p1 w)) (cons 11 (wt:w-p2 w))
                (cons 40 (wt:w-thk w)))))

(defun akc:original-records (plan / out item)
  (foreach item plan (setq out (cons (akc:record-entry "M" (car item)) out)))
  (reverse out))

(defun akc:save-record (poly originals stubs / data item rec dict en)
  (setq data (list '(1 . "AKCOL2") (cons 70 (length originals))
                   (cons 71 (length stubs))))
  (foreach item originals (setq data (append data item)))
  (foreach rec stubs
    (setq data (append data (akc:record-entry "S" rec))))
  (setq dict (akc:record-dict t)
        en (if dict (entmakex (append '((0 . "XRECORD") (100 . "AcDbXrecord")) data))))
  (if en (dictadd dict (cdr (assoc 5 (entget poly))) en)))

(defun akc:read-record (poly / dict d out row item val)
  (if (and (setq dict (akc:record-dict nil))
           (setq d (dictsearch dict (cdr (assoc 5 (entget poly))))))
    (progn
      (foreach item d
        (cond
          ((= (car item) 1)
           (if row (setq out (cons (reverse row) out)))
           (setq row (if (= (cdr item) "AKCOL2") nil (list item))))
          ((and row (member (car item) '(2 10 11 40)))
           (setq row (cons item row)))))
      (if row (setq out (cons (reverse row) out)))
      (reverse out))))

(defun akc:has-record (poly / dict)
  (and (setq dict (akc:record-dict nil))
       (dictsearch dict (cdr (assoc 5 (entget poly))))))

(defun akc:delete-record (poly / dict key en)
  (if (and (setq dict (akc:record-dict nil))
           (setq key (cdr (assoc 5 (entget poly))))
           (setq en (cdr (assoc -1 (dictsearch dict key)))))
    (progn (dictremove dict key) (entdel en))))

(defun akc:group-outline (grp / e d found)
  (foreach e (cdr grp)
    (if (and (setq d (entget e))
             (member (cdr (assoc 0 d)) '("LWPOLYLINE" "CIRCLE"))
             (= (strcase (cdr (assoc 8 d))) (strcase *col-layer*)))
      (setq found e)))
  found)



(defun akc:stub-end (w bl tr / a b u h ex ey p far lo hi candidate result)
  (setq a (wt:w-p1 w) b (wt:w-p2 w) u (wt:w-u w)
        h (/ (wt:w-thk w) 2.0) ex (* h (abs (cadr u)))
        ey (* h (abs (car u))) lo (list (- (car bl) ex) (- (cadr bl) ey))
        hi (list (+ (car tr) ex) (+ (cadr tr) ey)))
  (foreach candidate (list (list a b) (list b a))
    (setq p (car candidate) far (cadr candidate))
    (if (and (akc:inside-rect p lo hi)
             (or (< (abs (- (car p) (car lo))) *wt:tol*)
                 (< (abs (- (car p) (car hi))) *wt:tol*)
                 (< (abs (- (cadr p) (cadr lo))) *wt:tol*)
                 (< (abs (- (cadr p) (cadr hi))) *wt:tol*))
             (not (akc:inside-rect far lo hi)))
      (setq result (list w p far))))
  result)

(defun akc:remove-one (x items / out item)
  (foreach item items (if (not (equal x item)) (setq out (cons item out))))
  (reverse out))

(defun akc:column-in-gap (outline a b / ss i e d c r bb found)
  (if (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE,CIRCLE")
                                 (cons 8 *col-layer*) (cons 410 (getvar "CTAB")))))
    (repeat (setq i (sslength ss))
      (setq e (ssname ss (setq i (1- i))))
      (if (not (eq e outline))
        (progn
          (setq d (entget e) c (cdr (assoc 10 d)) r (cdr (assoc 40 d))
                bb (if (= (cdr (assoc 0 d)) "CIRCLE")
                     (list (list (- (car c) r) (- (cadr c) r))
                           (list (+ (car c) r) (+ (cadr c) r)))
                     (akc:pl-bbox e)))
          (if (and bb (akc:line-touches-box a b (car bb) (cadr bb)))
            (setq found t))))))
  found)

(defun akc:legacy-safe (poly cuts / rec layer ss i d hit)
  ;; Old cut history is usable only for independent non-WallTool LINEs whose
  ;; original spans have not been redrawn or occupied by another column.
  (foreach rec cuts
    (setq layer (car rec))
    (if (or (= (strcase layer) (strcase (wt:cfg "AXIS_LAYER")))
            (= (strcase layer) (strcase (wt:cfg "WALL_LAYER")))
            (akc:column-in-gap poly (cadr rec) (caddr rec)))
      (setq hit t))
    (if (setq ss (ssget "_X" (list '(0 . "LINE") (cons 8 layer)
                                   (cons 410 (getvar "CTAB")))))
      (repeat (setq i (sslength ss))
        (setq d (entget (ssname ss (setq i (1- i)))))
        (if (or (and (wt:peq (cdr (assoc 10 d)) (cadr rec))
                     (wt:peq (cdr (assoc 11 d)) (caddr rec)))
                (and (wt:peq (cdr (assoc 11 d)) (cadr rec))
                     (wt:peq (cdr (assoc 10 d)) (caddr rec))))
          (setq hit t)))))
  (not hit))

(defun akc:record-stub (row / en d net w)
  (setq en (if (cdr (assoc 2 row)) (handent (cdr (assoc 2 row))))
        d (if en (entget en)))
  (if (and d (= (cdr (assoc 0 d)) "LINE")
           (= (strcase (cdr (assoc 8 d))) (strcase (wt:cfg "AXIS_LAYER")))
           (or (and (wt:peq (cdr (assoc 10 d)) (cdr (assoc 10 row)))
                    (wt:peq (cdr (assoc 11 d)) (cdr (assoc 11 row))))
               (and (wt:peq (cdr (assoc 11 d)) (cdr (assoc 10 row)))
                    (wt:peq (cdr (assoc 10 d)) (cdr (assoc 11 row))))))
    (progn
      (setq net (wt:net-scan) w (wt:wall-from-master
        (assoc en (car net)) (cadr net)))
      (if (and w (<= (abs (- (wt:w-thk w) (cdr (assoc 40 row)))) *wt:tol*)) w))))

(defun akc:record-restore-plan (poly rows / originals stubs row w bad net m a b
                                    other u half iv expected rec d stub-count)
  (setq d (dictsearch (akc:record-dict nil) (cdr (assoc 5 (entget poly)))))
  (foreach row rows
    (cond
      ((= (cdr (assoc 1 row)) "M") (setq originals (cons row originals)))
      ((= (cdr (assoc 1 row)) "S")
       (setq stub-count (1+ (if stub-count stub-count 0)))
       (if (setq w (akc:record-stub row))
         (if (assoc (car w) stubs)
           (setq bad "Duplicate wall stub in column record")
           (setq stubs (cons w stubs)))
         (setq bad "Original wall stub was moved, deleted, or replaced")))))
  (setq net (wt:net-scan) expected (mapcar 'car stubs))
  (if (or (not (numberp (cdr (assoc 70 d))))
          (not (numberp (cdr (assoc 71 d))))
          (/= (length originals) (cdr (assoc 70 d)))
          (/= (if stub-count stub-count 0) (cdr (assoc 71 d))))
    (setq bad "Column wall record is incomplete"))
  (foreach row originals
    (setq a (cdr (assoc 10 row)) b (cdr (assoc 11 row)))
    (if (or (not a) (not b) (not (cdr (assoc 40 row)))
            (<= (wt:dist a b) *wt:tol*)
            (akc:column-in-gap poly a b))
      (setq bad "Original wall path is blocked"))
    (foreach m (car net)
      (if (not (member (car m) expected))
        (progn
          (setq other (wt:wall-from-master m (cadr net)))
          (if (or (and (not other) (wt:seg-touch a b (cadr m) (caddr m)))
                  (and (wt:seg-touch a b (cadr m) (caddr m))
                       (or (wt:in-seg (cadr m) a b)
                           (wt:in-seg (caddr m) a b)
                           (wt:in-seg a (cadr m) (caddr m))
                           (wt:in-seg b (cadr m) (caddr m)))))
            (setq bad "Conflicting wall junction or master on original path"))))))
  ;; A new wall can cross the gap without meeting an original centerline.
  (if (setq rec (akc:pl-bbox poly))
    (foreach m (car net)
      (if (not (member (car m) expected))
        (if (setq other (wt:wall-from-master m (cadr net)))
          (progn
            (setq u (wt:w-u other) half (/ (wt:w-thk other) 2.0)
                  iv (akc:clip-rect (wt:w-p1 other) (wt:w-p2 other)
                       (car rec) (cadr rec) (* half (abs (cadr u)))
                       (* half (abs (car u)))))
            (if iv (setq bad "New wall crosses the column gap")))
          (if (akc:line-touches-box (cadr m) (caddr m)
                 (car rec) (cadr rec))
            (setq bad "Unrecognized wall master near column"))))))
  (if bad (list nil bad) (list (list 'RECORD (reverse originals) stubs) nil)))

(defun akc:restore-record-masters (checked / originals stubs row en new old)
  (setq originals (cadr checked) stubs (caddr checked))
  (if originals
    (progn
      (wt:pend-begin)
      (foreach w stubs (wt:pend-erase (car w)))
      (foreach row originals
        (setq en (wt:pend-make (wt:mk-line (cdr (assoc 10 row))
                  (cdr (assoc 11 row)) (wt:cfg "AXIS_LAYER"))))
        (wt:reg-add en (cdr (assoc 40 row)) "CENTER")
        (setq new (cons (list en (cdr (assoc 10 row)) (cdr (assoc 11 row))
                              (cdr (assoc 40 row)) "CENTER") new)))
      (wt:rebuild new stubs)
      (setq *wt:pending* nil)))
  (length originals))

(defun akc:restore-plan (grp / poly bb bl tr net faces ms stubs w ep
                              x y matches pairs bad other bridge full)
  (setq poly (akc:group-outline grp))
  (cond
    ((not poly) (list nil "Column outline missing"))
    ((akc:read-cuts poly)
     (if (akc:legacy-safe poly (akc:read-cuts poly))
       (list 'LEGACY nil)
       (list nil "Legacy cuts require manual wall review")))
    ((akc:has-record poly) (akc:record-restore-plan poly (akc:read-record poly)))
    ((not (akc:wall-gap-count poly))
     (list nil "Column wall provenance is unknown"))
    ((= (akc:wall-gap-count poly) 0) (list nil nil))
    ((= (cdr (assoc 0 (entget poly))) "CIRCLE") (list nil nil))
    (t
      (setq bb (akc:pl-bbox poly) bl (car bb) tr (cadr bb)
            net (wt:net-scan) faces (cadr net) ms (car net))
      (foreach m ms
        (if (setq w (wt:wall-from-master m faces))
          (if (setq ep (akc:stub-end w bl tr)) (setq stubs (cons ep stubs)))))
      (if (and (akc:wall-gap-count poly)
               (/= (length stubs) (akc:wall-gap-count poly)))
        (setq bad "Original wall stubs are missing or changed"))
      (while (and stubs (not bad))
        (setq x (car stubs) stubs (cdr stubs) matches nil)
        (foreach y stubs
          (if (and (<= (abs (- (wt:w-thk (car x)) (wt:w-thk (car y)))) *wt:tol*)
                   (wt:par (wt:w-u (car x)) (wt:w-u (car y)))
                   (< (abs (wt:cross (wt:w-u (car x))
                                      (wt:v- (cadr y) (cadr x)))) *wt:tol*)
                   (< (wt:dot (wt:unit (wt:v- (cadr x) (caddr x)))
                              (wt:unit (wt:v- (cadr y) (caddr y))))
                      (- *wt:tol*)))
            (setq matches (cons y matches))))
        (if (/= (length matches) 1)
          (setq bad "Wall gap has no unique compatible pair of stubs")
          (progn
            (setq y (car matches) stubs (akc:remove-one y stubs)
                  bridge (list (cadr x) (cadr y)))
            (if (or (<= (wt:dist (car bridge) (cadr bridge)) *wt:tol*)
                    (not (akc:pt-in-rect
                           (wt:v* (wt:v+ (car bridge) (cadr bridge)) 0.5) bl tr))
                    (akc:column-in-gap poly (car bridge) (cadr bridge)))
              (setq bad "Wall gap is blocked or not across this column"))
            (setq full (list (caddr x) (caddr y)))
            (foreach other ms
              (if (and (not (eq (car other) (car (car x))))
                       (not (eq (car other) (car (car y)))))
                (cond
                  ((wt:seg-touch (car bridge) (cadr bridge)
                                 (cadr other) (caddr other))
                   (setq bad "Conflicting wall junction across column gap"))
                  ((and (wt:par (wt:unit (wt:v- (cadr full) (car full)))
                                (wt:unit (wt:v- (caddr other) (cadr other))))
                        (wt:seg-touch (car full) (cadr full)
                                      (cadr other) (caddr other))
                        (or (wt:in-seg (cadr other) (car full) (cadr full))
                            (wt:in-seg (caddr other) (car full) (cadr full))
                            (wt:in-seg (car full) (cadr other) (caddr other))
                            (wt:in-seg (cadr full) (cadr other) (caddr other))))
                   (setq bad "Restored wall would overlap another master")))))
            (setq pairs (cons (list x y) pairs)))))
      (if bad (list nil bad) (list pairs nil)))))

(defun akc:restore-masters (pairs / a b en new old pair)
  (if pairs
    (progn
      (wt:pend-begin)
      (foreach pair pairs
        (setq a (car pair) b (cadr pair))
        (wt:pend-erase (car (car a))) (wt:pend-erase (car (car b)))
        (setq en (wt:pend-make (wt:mk-line
                   (caddr a)
                   (caddr b)
                   (wt:cfg "AXIS_LAYER"))))
        (wt:reg-add en (wt:w-thk (car a)) "CENTER")
        (setq new (cons (list en (caddr a) (caddr b) (wt:w-thk (car a)) "CENTER") new)
              old (append old (list (car a) (car b)))))
      (wt:rebuild new old)
      (setq *wt:pending* nil)))
  (length pairs))

(defun akc:erase-checked (grp pairs / e)
  (if (eq pairs 'LEGACY)
    (akc:erase-group grp)
    (progn
      (if (and pairs (eq (car pairs) 'RECORD))
        (akc:restore-record-masters pairs)
        (akc:restore-masters pairs))
      (if (akc:group-outline grp) (akc:delete-record (akc:group-outline grp)))
      (foreach e (cdr grp) (if (entget e) (entdel e)))
      (length pairs))))

(defun akc:without-code (data code / out item)
  (foreach item data (if (/= (car item) code) (setq out (cons item out))))
  (reverse out))

(defun akc:place-rect (p hh bb / w d bl tr br tl plan err axis poly hatch
                                clay color axh axv tag1 tag2 labels n group
                                originals stubs)
  (if (not (akc:wt-ready))
    (princ "\nLoad WallTool before AKDColumn.")
    (progn
      (setq w *col-w* d *col-d* bl (akc:bl-from p w d *col-base*)
            tr (list (+ (car bl) w) (+ (cadr bl) d) 0.0))
      (cond
        ((or (<= w 0.0) (<= d 0.0)) (princ "\nColumn dimensions must be positive."))
        ((akc:column-overlap bl tr) (princ "\nColumn overlaps an existing column."))
        (t
         (setq plan (akc:wall-cut-plan bl tr) err (cadr plan))
         (if err (princ (strcat "\nColumn refused: " err "."))
           (progn
             (setq axis (akc:pick-axis bl tr))
             (if axis
               (progn
                 (wt:begin)
                 (setq *akc:mutating* t *akc:placing* t
                       br (list (car tr) (cadr bl) 0.0)
                       tl (list (car bl) (cadr tr) 0.0))
                 (setq originals (akc:original-records (car plan))
                       stubs (akc:cut-masters (car plan)))
                 (setq poly (akc:mkpline (list bl br tr tl))
                       clay (getvar "CLAYER") color (getvar "CECOLOR"))
                 (if (not poly) (exit))
                 (setq *akc:old-layer* clay *akc:old-color* color)
                 (setvar "CLAYER" *col-layer*)
                 (setvar "CECOLOR" (itoa *col-hatch-color*))
                 (command-s "_.-HATCH" "_P" "_SOLID" "_S" poly "" "")
                 (setq hatch (entlast))
                 (if (not (and hatch (= (cdr (assoc 0 (entget hatch))) "HATCH")))
                   (exit))
                 (setvar "CECOLOR" color) (setvar "CLAYER" clay)
                 (setq *akc:old-color* nil *akc:old-layer* nil)
                 (if hatch
                   (entmod (append (akc:without-code (entget hatch) 62)
                                   (list (cons 62 *col-hatch-color*)))))
                 (if (not (akc:save-record poly originals stubs))
                   (exit))
                 (setq axh (akc:mkline (list (car bl) (cadr axis) 0.0)
                                        (list (car tr) (cadr axis) 0.0)
                                        *col-axis-color*)
                       axv (akc:mkline (list (car axis) (cadr bl) 0.0)
                                        (list (car axis) (cadr tr) 0.0)
                                        *col-axis-color*)
                       tag1 (akc:tag-awall (list (car bl) (cadr axis) 0.0)
                                            (list (car tr) (cadr axis) 0.0) d hh bb)
                       tag2 (akc:tag-awall (list (car axis) (cadr bl) 0.0)
                                            (list (car axis) (cadr tr) 0.0) w hh bb)
                       n (akc:pick-num)
                       labels (akc:label tl (strcat (rtos w 2 0) "x" (rtos d 2 0)) n)
                       group (akc:uniqname "AKCOL"))
                 (if (not (and axh axv tag1 tag2)) (exit))
                 (akc:mkgroup group (append (list poly hatch axh axv tag1 tag2) labels))
                 (wt:end)
                 (setq *akc:mutating* nil *akc:placing* nil)
                 (princ (strcat "\nColumn placed (" group ")."))))))))))
  (princ))

(defun akc:circle-hits-wall (ctr r / net m w q)
  (setq net (wt:net-scan))
  (foreach m (car net)
    (setq w (wt:wall-from-master m (cadr net)))
    (if (< (wt:seg-dist ctr (cadr m) (caddr m))
           (+ r (if w (/ (wt:w-thk w) 2.0) 0.0) *wt:tol*))
      (setq q t)))
  q)

(defun akc:place-circ (ctr hh bb / r e hatch clay color axh axv tag1 tag2 n tl labels group)
  (setq r (/ *col-dia* 2.0))
  (cond
    ((not (akc:wt-ready)) (princ "\nLoad WallTool before AKDColumn."))
    ((<= r 0.0) (princ "\nDiameter must be positive."))
    ((akc:circle-hits-wall ctr r)
     (princ "\nRound column refused: WallTool walls need a curved-end strategy."))
    ((akc:column-overlap (list (- (car ctr) r) (- (cadr ctr) r))
                         (list (+ (car ctr) r) (+ (cadr ctr) r)))
     (princ "\nColumn overlaps an existing column."))
    (t
     (wt:begin) (setq *akc:mutating* t *akc:placing* t)
     (setq e (akc:mkcircle ctr r) clay (getvar "CLAYER") color (getvar "CECOLOR"))
     (if (not e) (exit))
     (if (not (akc:mark-wall-gap e 0)) (exit))
     (setq *akc:old-layer* clay *akc:old-color* color)
     (setvar "CLAYER" *col-layer*) (setvar "CECOLOR" (itoa *col-hatch-color*))
     (command-s "_.-HATCH" "_P" "_SOLID" "_S" e "" "")
     (setq hatch (entlast))
     (if (not (and hatch (= (cdr (assoc 0 (entget hatch))) "HATCH")))
       (exit))
     (setvar "CECOLOR" color) (setvar "CLAYER" clay)
     (setq *akc:old-color* nil *akc:old-layer* nil)
     (if hatch
       (entmod (append (akc:without-code (entget hatch) 62)
                       (list (cons 62 *col-hatch-color*)))))
     (setq axh (akc:mkline (list (- (car ctr) r) (cadr ctr) 0.0)
                            (list (+ (car ctr) r) (cadr ctr) 0.0) *col-axis-color*)
           axv (akc:mkline (list (car ctr) (- (cadr ctr) r) 0.0)
                            (list (car ctr) (+ (cadr ctr) r) 0.0) *col-axis-color*)
           tag1 (akc:tag-awall (list (- (car ctr) r) (cadr ctr) 0.0)
                                (list (+ (car ctr) r) (cadr ctr) 0.0) *col-dia* hh bb)
           tag2 (akc:tag-awall (list (car ctr) (- (cadr ctr) r) 0.0)
                                (list (car ctr) (+ (cadr ctr) r) 0.0) *col-dia* hh bb)
           n (akc:pick-num) tl (list (- (car ctr) r) (+ (cadr ctr) r) 0.0)
           labels (akc:label tl (strcat "%%C" (rtos *col-dia* 2 0)) n)
           group (akc:uniqname "AKCOL"))
     (if (not (and axh axv tag1 tag2)) (exit))
     (akc:mkgroup group (append (list e hatch axh axv tag1 tag2) labels))
     (wt:end) (setq *akc:mutating* nil *akc:placing* nil)
     (princ (strcat "\nRound column placed (" group ")."))))
  (princ))

(defun akc:command-error (msg)
  (setq *wt:pending* nil)
  (if *wt:open* (wt:end))
  (if *akc:mutating* (command-s "_.U"))
  (setq *akc:mutating* nil *akc:placing* nil)
  (if *akc:old-color* (setvar "CECOLOR" *akc:old-color*))
  (if *akc:old-layer* (setvar "CLAYER" *akc:old-layer*))
  (setq *akc:old-color* nil *akc:old-layer* nil)
  (if *akc:cmdecho* (setvar "CMDECHO" *akc:cmdecho*))
  (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*,*BREAK*"))
    (princ (strcat "\nError: " msg)))
  (princ))

(defun c:EC (/ *error* e grp check)
  (setq *error* akc:command-error *akc:mutating* nil
        *akc:cmdecho* (getvar "CMDECHO") e (entsel "\nSelect column to erase: "))
  (if e
    (if (setq grp (akc:group-of (car e)))
      (if (not (akc:wt-ready))
        (princ "\nLoad WallTool before AKDColumn.")
        (progn
          (setq check (akc:restore-plan grp))
          (if (cadr check)
            (princ (strcat "\nColumn kept: " (cadr check) "."))
            (progn
              (wt:begin)
              (setq *akc:mutating* t)
              (akc:erase-checked grp (car check))
              (wt:end)
              (setq *akc:mutating* nil)
              (princ (strcat "\nErased " (car grp) "."))))))
      (princ "\nNot an AKCOL group member.")))
  (setvar "CMDECHO" *akc:cmdecho*)
  (princ))

(defun c:EW (/ *error* pre ss items groups names wins win-groups
                info win-group win-names wall-items e d grp checks bad check
                selected r net margin item pair)
  (setq *error* akc:command-error *akc:mutating* nil
        *akc:cmdecho* (getvar "CMDECHO"))
  (if (not (akc:wt-ready))
    (princ "\nLoad WallTool before AKDColumn.")
    (progn
      (setq pre (ssgetfirst) ss (cadr pre))
      (if ss (sssetfirst nil nil)
        (progn (princ "\nSelect columns, walls, doors, or windows to erase: ")
               (setq ss (ssget))))
      (if ss
        (progn
          (setq items (wt:ss-items ss) net (wt:net-scan)
                margin (wt:pick-margin))
          (if (member 'EW:AKD-GROUPS (atoms-family 0))
            (setq win-groups (ew:akd-groups)))
          (foreach item items
            (setq e (car item) d (entget e) grp (if d (akc:group-of e)))
            (cond
              (grp
               (if (not (member (car grp) names))
                 (setq names (cons (car grp) names)
                       groups (cons grp groups))))
              ((and win-groups
                    (setq win-group (ew:group-of-ent e win-groups))
                    (setq info (ew:info-from-group win-group)))
               (if (not (member (car win-group) win-names))
                 (setq win-names (cons (car win-group) win-names)
                       wins (cons info wins))))
              ((and (member 'CW:READ-XD (atoms-family 0))
                    (setq info (cw:read-xd e)))
               (if (not (member (_extract-gname (cadr info)) win-names))
                 (setq win-names (cons (_extract-gname (cadr info)) win-names)
                       wins (cons info wins))))
              ((= (caddr item) "LINE")
               ;; Pass the untouched item, including its click point, to WallTool.
               (setq wall-items (cons item wall-items)))))
          (foreach grp groups
            (setq check (akc:restore-plan grp))
            (if (cadr check)
              (setq bad (cadr check))
              (setq checks (cons (cons grp (car check)) checks))))
          ;; A selected wall must not also be a stub that a column restores.
          (foreach item wall-items
            (setq r (wt:ew-resolve (car item) (cadr item) net margin))
            (if (= (car r) "OK")
              (foreach check checks
                (if (and (cdr check) (eq (car (cdr check)) 'RECORD))
                  (foreach w (caddr (cdr check))
                    (if (eq (car (cadr r)) (car w))
                      (setq bad "Selected wall is a column gap stub")))
                (if (and (listp (cdr check))
                         (not (eq (car (cdr check)) 'RECORD)))
                  (foreach pair (cdr check)
                    (if (and (listp pair)
                           (or (eq (car (cadr r)) (car (car (car pair))))
                               (eq (car (cadr r)) (car (car (cadr pair))))))
                      (setq bad "Selected wall is a column gap stub"))))))))
          (if bad
            (princ (strcat "\nSelection kept: " bad "."))
            (if (or groups wins wall-items)
              (progn
                (wt:begin)
                (setq *akc:mutating* t)
                (foreach check checks (akc:erase-checked (car check) (cdr check)))
                (foreach info wins (ew:do-one info))
                (if wall-items (wt:ew-erase (reverse wall-items) margin))
                (wt:end)
                (setq *akc:mutating* nil))
              (princ "\nNo recognized objects selected."))))
        (princ "\nNothing selected."))))
  (setvar "CMDECHO" *akc:cmdecho*)
  (princ))

(defun c:AC (/ *error* done p hh bb v)
  (setq *error* akc:command-error *akc:mutating* nil *akc:placing* nil
        *akc:cmdecho* (getvar "CMDECHO") done nil hh (if *WW_Height* *WW_Height* 2700.0)
        bb (if *WW_BaseElev* *WW_BaseElev* 0.0))
  (setvar "CMDECHO" 0)
  (while (not done)
    (if (= *col-shape* "R")
      (progn
        (princ (strcat "\n[Rect Column  W=" (rtos *col-w* 2 0)
                       "  D=" (rtos *col-d* 2 0) "  Base=" *col-base* "]"))
        (initget "Rect Circle Width Depth Base")
        (setq p (getpoint "\nInsertion point or [Rect/Circle/Width/Depth/Base]: ")))
      (progn
        (princ (strcat "\n[Round Column  Dia=" (rtos *col-dia* 2 0) "]"))
        (initget "Rect Circle Diameter")
        (setq p (getpoint "\nCenter point or [Rect/Circle/Diameter]: "))))
    (cond
      ((null p) (setq done t))
      ((= p "Rect") (setq *col-shape* "R"))
      ((= p "Circle") (setq *col-shape* "C"))
      ((= p "Width")
       (setq v (getdist (strcat "\nWidth <" (rtos *col-w* 2 0) ">: ")))
       (if (and v (> v 0.0)) (setq *col-w* v)))
      ((= p "Depth")
       (setq v (getdist (strcat "\nDepth <" (rtos *col-d* 2 0) ">: ")))
       (if (and v (> v 0.0)) (setq *col-d* v)))
      ((= p "Base") (c:CB))
      ((= p "Diameter")
       (setq v (getdist (strcat "\nDiameter <" (rtos *col-dia* 2 0) ">: ")))
       (if (and v (> v 0.0)) (setq *col-dia* v)))
      ((listp p)
       (if (= *col-shape* "R")
         (akc:place-rect p hh bb)
         (akc:place-circ p hh bb)))))
  (setvar "CMDECHO" *akc:cmdecho*)
  (princ))

;; The former STRETCH selection can move a gap without moving its masters, and
;; can delete a neighboring column's projection tags. Keep CCW read-only until
;; group-scoped resizing and atomic master movement are implemented.
(defun c:CCW ()
  (princ "\nCCW is disabled: resizing must update WallTool masters and this column's tags together.")
  (princ))

(princ "\nAKDColumn WallTool integration loaded (load WallTool first).")
(princ)
