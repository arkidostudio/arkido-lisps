;; AKDColumn.lsp  -  Arkido Column
;; Command AC    Place a column, cut crossing walls, cap with end-wall lines,
;;               then live-place an axis cross inside the column.
;; Command CB    Set the base anchor (TL/TC/TR/ML/C/MR/BL/BC/BR).
;; Command CCW   Change column width via base + reference + new distance.
;;
;; All column entities are grouped as AKCOLn. Emits two AWALL POINT tags per
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

(defun akc:mkgroup (name ents / ss)
  (setq ss (ssadd))
  (foreach e ents (if e (ssadd e ss)))
  (command-s "_.-group" "_create" name "" ss "")
  name)

;; ---- wall cutting -------------------------------------------------

;; Segment-segment intersection (2D). Both endpoints inclusive with slop.
(defun akc:seg-int (a b c d / rx ry sx sy den qx qy tt uu)
  (setq rx (- (car b) (car a)) ry (- (cadr b) (cadr a))
        sx (- (car d) (car c)) sy (- (cadr d) (cadr c))
        den (- (* rx sy) (* ry sx)))
  (if (equal den 0.0 1e-9) nil
    (progn
      (setq qx (- (car c) (car a)) qy (- (cadr c) (cadr a))
            tt (/ (- (* qx sy) (* qy sx)) den)
            uu (/ (- (* qx ry) (* qy rx)) den))
      (if (and (>= tt -1e-6) (<= tt (+ 1.0 1e-6))
               (>= uu -1e-6) (<= uu (+ 1.0 1e-6)))
        (list (+ (car a) (* tt rx)) (+ (cadr a) (* tt ry)) 0.0)))))

(defun akc:pt-in-rect (p bl tr / eps)
  (setq eps 1e-4)
  (and (> (car  p) (+ (car  bl) eps)) (< (car  p) (- (car  tr) eps))
       (> (cadr p) (+ (cadr bl) eps)) (< (cadr p) (- (cadr tr) eps))))

;; True if LINE segment ent bbox overlaps column bbox.
(defun akc:line-touches-box (p1 p2 bl tr)
  (and (>= (max (car p1) (car p2)) (car bl))
       (<= (min (car p1) (car p2)) (car tr))
       (>= (max (cadr p1) (cadr p2)) (cadr bl))
       (<= (min (cadr p1) (cadr p2)) (cadr tr))))

;; De-dup a hit against an existing list within tol.
(defun akc:add-uniq (pt lst / tol dup)
  (setq tol 1e-3 dup nil)
  (foreach q lst (if (< (distance pt q) tol) (setq dup t)))
  (if dup lst (cons pt lst)))

;; Split LINE ent, dropping the piece between pa and pb.
(defun akc:split-line-at (ent pa pb / ln s e da db near far lay)
  (setq ln (entget ent)
        s (cdr (assoc 10 ln)) e (cdr (assoc 11 ln))
        lay (cdr (assoc 8 ln))
        da (distance s pa) db (distance s pb)
        near (if (< da db) pa pb)
        far  (if (< da db) pb pa))
  (entmod (subst (cons 11 near) (assoc 11 ln) ln))
  (entmakex (list '(0 . "LINE") (cons 8 lay) (cons 10 far) (cons 11 e))))

;; Trim LINE so it keeps the endpoint near keepEnd; the other becomes newEnd.
(defun akc:trim-line-to (ent keepEnd newEnd / ln s e)
  (setq ln (entget ent)
        s (cdr (assoc 10 ln)) e (cdr (assoc 11 ln)))
  (if (< (distance s keepEnd) (distance e keepEnd))
    (entmod (subst (cons 11 newEnd) (assoc 11 ln) ln))
    (entmod (subst (cons 10 newEnd) (assoc 10 ln) ln))))

(defun akc:cap-add (caps idx pt / out j)
  (setq out nil j 0)
  (foreach lst caps
    (setq out (cons (if (= j idx) (akc:add-uniq pt lst) lst) out) j (1+ j)))
  (reverse out))

;; Layers that must NOT be cut (doors, windows, tags, columns themselves).
(defun akc:cuttable-lyr (lyr)
  (not (wcmatch (strcase lyr)
                "A-DOOR*,A-WIN*,A-COLUMN*,S-COLUMN*,A-WALL-DATA,X-TAGS*,X-TAGS & SYMBOLS")))

;; Scan LINEs on wall-ish layers, cut those crossing the column rect.
;; Returns per-edge hit lists (bottom right top left).
;; Returns (caps records). records = list of (layer origP1 origP2) per cut wall.
(defun akc:cut-walls (bl tr / ss i e d p1 p2 hits pt inA inB pts caps edges j ed
                                dup lay records did)
  (setq ss (ssget "_X" '((0 . "LINE")))
        caps (list nil nil nil nil)
        records nil
        edges (list
          (list bl (list (car tr) (cadr bl) 0.0))
          (list (list (car tr) (cadr bl) 0.0) tr)
          (list tr (list (car bl) (cadr tr) 0.0))
          (list (list (car bl) (cadr tr) 0.0) bl)))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) i (1+ i)
              d (entget e)
              p1 (cdr (assoc 10 d))
              p2 (cdr (assoc 11 d))
              lay (cdr (assoc 8 d)))
        (if (and (akc:cuttable-lyr lay)
                 (akc:line-touches-box p1 p2 bl tr))
          (progn
            (setq hits nil j 0)
            (foreach ed edges
              (setq pt (akc:seg-int p1 p2 (car ed) (cadr ed)))
              (if pt
                (progn
                  (setq dup nil)
                  (foreach q hits (if (< (distance pt q) 1e-3) (setq dup t)))
                  (if (not dup)
                    (progn
                      (setq hits (cons pt hits))
                      (setq caps (akc:cap-add caps j pt))))))
              (setq j (1+ j)))
            (setq inA (akc:pt-in-rect p1 bl tr)
                  inB (akc:pt-in-rect p2 bl tr)
                  pts (reverse hits))
            (setq did nil)
            (cond
              ((and inA inB) (entdel e) (setq did t))
              ((and (not inA) (not inB) (>= (length pts) 2))
                (akc:split-line-at e (car pts) (cadr pts)) (setq did t))
              ((and inA pts) (akc:trim-line-to e p2 (car pts)) (setq did t))
              ((and inB pts) (akc:trim-line-to e p1 (car pts)) (setq did t)))
            (if did (setq records (cons (list lay p1 p2) records))))))))
  (list caps records))

;; For each edge, sort hits along the edge and cap adjacent pairs.
(defun akc:draw-caps (caps / j lst pts capEnts e)
  (setq j 0 capEnts nil)
  (foreach lst caps
    (if (>= (length lst) 2)
      (progn
        (setq pts (if (or (= j 0) (= j 2))
                    (vl-sort lst '(lambda (a b) (< (car a) (car b))))
                    (vl-sort lst '(lambda (a b) (< (cadr a) (cadr b))))))
        (while (>= (length pts) 2)
          (setq e (akc:mkline (car pts) (cadr pts) nil))
          (if e (setq capEnts (cons e capEnts)))
          (setq pts (cddr pts)))))
    (setq j (1+ j)))
  capEnts)

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
  (list cx cy 0.0))

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

;; ---- cut-wall xdata (for c:EC rejoin) -----------------------------
(defun akc:attach-cuts (ent records / d xlist)
  (if (and ent records)
    (progn
      (regapp "AKCOL")
      (setq xlist (list "AKCOL"))
      (foreach r records
        (setq xlist (append xlist
                     (list (cons 1002 "{")
                           (cons 1000 (car r))
                           (list 1010 (car (cadr r))  (cadr (cadr r))  0.0)
                           (list 1010 (car (caddr r)) (cadr (caddr r)) 0.0)
                           (cons 1002 "}")))))
      (setq d (entget ent (list "AKCOL")))
      (entmod (append d (list (cons -3 (list xlist))))))))

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

(defun akc:pt-in-bb (p bb / bl tr)
  (setq bl (car bb) tr (cadr bb))
  (and (>= (car  p) (- (car  bl) 1e-3)) (<= (car  p) (+ (car  tr) 1e-3))
       (>= (cadr p) (- (cadr bl) 1e-3)) (<= (cadr p) (+ (cadr tr) 1e-3))))

;; Find A-COLUMN LWPOLYLINE whose bbox contains pt.
(defun akc:find-col-at (pt / ss i e bb best)
  (setq ss (ssget "_X" (list '(0 . "LWPOLYLINE") (cons 8 *col-layer*))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) i (1+ i) bb (akc:pl-bbox e))
        (if (and bb (akc:pt-in-bb pt bb)) (setq best e)))))
  best)

;; Return AWALL POINTs whose xdata midpoint sits inside bbox.
(defun akc:awalls-in (bb / ss i e xd mid out)
  (regapp "AWALL")
  (setq ss (ssget "_X" (list '(0 . "POINT") (list -3 (list "AWALL"))))
        out nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) i (1+ i)
              xd (cdr (assoc "AWALL" (cdr (assoc -3 (entget e (list "AWALL"))))))
              mid (cdr (assoc 10 (entget e))))
        (if (and xd mid (akc:pt-in-bb mid bb))
          (setq out (cons e out))))))
  out)

;; Extract numeric xdata (thk h base) from an AWALL POINT.
(defun akc:awall-nums (ent / xd nums)
  (setq xd (cdr (assoc "AWALL" (cdr (assoc -3 (entget ent (list "AWALL"))))))
        nums nil)
  (foreach it xd (if (= (car it) 1040) (setq nums (cons (cdr it) nums))))
  (reverse nums))

;; Stretch the ref edge of a column to a new distance from the base edge.
;; Runs native STRETCH on a crossing window at the ref edge, then rebuilds
;; the column's AWALL projection tags from the new polyline bbox.
(defun c:CCW ( / bp rp dist ux uy new dx dy tol cross-h c1 c2 cmde
                 colEnt colH oldBB newBB nums hh bb tags nBL nTR w2 d2 cy cx)
  (setq bp (getpoint "\nBase point (fixed side): "))
  (if bp (setq rp (getpoint bp "\nReference point (edge to stretch): ")))
  (cond
    ((or (null bp) (null rp)) (princ "\nCancelled."))
    (t
      (setq dist (distance bp rp))
      (cond
        ((< dist 1e-6) (princ "\nZero reference."))
        (t
          (setq new (getdist bp
                      (strcat "\nNew width <" (rtos dist 2 0) ">: ")))
          (cond
            ((or (null new) (<= new 0.0)) (princ "\nCancelled."))
            (t
              (setq ux (/ (- (car rp) (car bp)) dist)
                    uy (/ (- (cadr rp) (cadr bp)) dist)
                    dx (* ux (- new dist))
                    dy (* uy (- new dist))
                    tol      5.0        ; slop along stretch axis
                    cross-h  5000.0     ; extent perpendicular
                    c1 (list (- (car rp)  (if (> (abs ux) 0.5) tol cross-h))
                             (- (cadr rp) (if (> (abs uy) 0.5) tol cross-h)) 0.0)
                    c2 (list (+ (car rp)  (if (> (abs ux) 0.5) tol cross-h))
                             (+ (cadr rp) (if (> (abs uy) 0.5) tol cross-h)) 0.0)
                    cmde (getvar "CMDECHO"))
              (setq colEnt (akc:find-col-at bp))
              (if colEnt
                (setq colH (cdr (assoc 5 (entget colEnt)))
                      oldBB (akc:pl-bbox colEnt)))
              (setvar "CMDECHO" 0)
              (command-s "_.STRETCH" "_C" c1 c2 ""
                         bp (list (+ (car bp) dx) (+ (cadr bp) dy) 0.0))
              (setvar "CMDECHO" cmde)
              ;; refresh AWALL tags to reflect the new bbox
              (if (and colH oldBB)
                (progn
                  (setq colEnt (handent colH)
                        newBB  (if colEnt (akc:pl-bbox colEnt))
                        tags   (akc:awalls-in oldBB))
                  (if (and newBB tags)
                    (progn
                      (setq nums (akc:awall-nums (car tags))
                            hh   (cadr nums)
                            bb   (caddr nums))
                      (foreach tg tags (entdel tg))
                      (setq nBL (car newBB) nTR (cadr newBB)
                            w2 (- (car nTR) (car nBL))
                            d2 (- (cadr nTR) (cadr nBL))
                            cy (* 0.5 (+ (cadr nBL) (cadr nTR)))
                            cx (* 0.5 (+ (car  nBL) (car  nTR))))
                      (akc:tag-awall (list (car nBL) cy 0.0)
                                     (list (car nTR) cy 0.0) d2 hh bb)
                      (akc:tag-awall (list cx (cadr nBL) 0.0)
                                     (list cx (cadr nTR) 0.0) w2 hh bb)))))
              (princ (strcat "\nStretched " (rtos dist 2 0)
                             " -> " (rtos new 2 0) "."))))))))
  (princ))

(defun akc:place-rect (p hh bb / w d bl tr br tl corners polyEnt hatchEnt
                                  caps capEnts axCtr axH axV clay oldCE ents
                                  gname tag1 tag2 ax lblEnts cutRes cutRecs
                                  colNum)
        (command-s "_.UNDO" "_BE")
        (setq w  *col-w* d *col-d*
              bl (akc:bl-from p w d *col-base*)
              tr (list (+ (car bl) w) (+ (cadr bl) d) 0.0)
              br (list (car tr) (cadr bl) 0.0)
              tl (list (car bl) (cadr tr) 0.0)
              corners (list bl br tr tl)
              polyEnt (akc:mkpline corners))
        ;; cut walls & draw caps FIRST so hatch tops out at final geometry
        (setq cutRes  (akc:cut-walls bl tr)
              caps    (car cutRes)
              cutRecs (cadr cutRes))
        (setq capEnts (akc:draw-caps caps))
        ;; solid grey hatch on column poly (color 8 explicit, layer S-COLUMN)
        (setq clay (getvar "CLAYER") oldCE (getvar "CECOLOR"))
        (akc:ensure-lyr *col-layer* *col-layer-color*)
        (setvar "CLAYER" *col-layer*)
        (setvar "CECOLOR" (itoa *col-hatch-color*))
        (command-s "_.-HATCH" "_P" "_SOLID" "_S" polyEnt "" "")
        (setq hatchEnt (entlast))
        (setvar "CECOLOR" oldCE)
        (setvar "CLAYER" clay)
        ;; force hatch color (Mac -HATCH sometimes ignores CECOLOR)
        (if hatchEnt
          (entmod (append (vl-remove-if '(lambda (x) (= (car x) 62))
                                        (entget hatchEnt))
                          (list (cons 62 *col-hatch-color*)))))
        ;; live-place axis cross (red on S-COLUMN)
        (setq axCtr (akc:pick-axis bl tr)
              ax *col-axis-off*
              axH (akc:mkline (list (car bl) (cadr axCtr) 0.0)
                              (list (car tr) (cadr axCtr) 0.0)
                              *col-axis-color*)
              axV (akc:mkline (list (car axCtr) (cadr bl) 0.0)
                              (list (car axCtr) (cadr tr) 0.0)
                              *col-axis-color*))
        ;; AKDProjections tags
        (setq tag1 (akc:tag-awall (list (car bl) (cadr axCtr) 0.0)
                                  (list (car tr) (cadr axCtr) 0.0) d hh bb)
              tag2 (akc:tag-awall (list (car axCtr) (cadr bl) 0.0)
                                  (list (car axCtr) (cadr tr) 0.0) w hh bb))
        ;; prompt for column tag (dynamic-input keyword pick), then label
        (setq colNum  (akc:pick-num)
              lblEnts (akc:label tl (strcat (rtos w 2 0) "x" (rtos d 2 0))
                                            colNum))
        ;; stash cut-wall records on the column poly so c:EC can rejoin
        (akc:attach-cuts polyEnt cutRecs)
        ;; group everything
        (setq gname (akc:uniqname "AKCOL")
              ents (append (list polyEnt hatchEnt axH axV tag1 tag2)
                           capEnts lblEnts))
        (akc:mkgroup gname ents)
        (command-s "_.UNDO" "_E")
        (princ (strcat "\nColumn placed (" gname
                       "). Wall cuts: "
                       (itoa (length (apply 'append caps))) " intersections.")))

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

;; Erase Column: pick any column entity, restore walls, delete group.
(defun c:EC ( / e ent grp poly cuts nm ents)
  (setq e (entsel "\nSelect column to erase: "))
  (if e
    (progn
      (setq ent (car e)
            grp (akc:group-of ent))
      (cond
        ((null grp) (princ "\nNot an AKCOL group member."))
        (t
          (setq nm (car grp) ents (cdr grp))
          ;; find the polyline (source of xdata)
          (foreach x ents
            (if (and x (entget x)
                     (member (cdr (assoc 0 (entget x)))
                             '("LWPOLYLINE" "CIRCLE"))
                     (= (cdr (assoc 8 (entget x))) *col-layer*))
              (setq poly x)))
          (setq cuts (if poly (akc:read-cuts poly)))
          ;; restore each cut wall as a fresh LINE on its original layer
          ;; ponytail: leaves any surviving fragments in place — overlap is
          ;; harmless visually. Fragment cleanup deferred until it matters.
          (foreach r cuts
            (akc:ensure-lyr (car r) 7)
            (entmakex (list '(0 . "LINE") (cons 8 (car r))
                            (cons 10 (cadr r)) (cons 11 (caddr r)))))
          ;; delete group members
          (foreach x ents (if (and x (entget x)) (entdel x)))
          (princ (strcat "\nErased " nm ", restored "
                         (itoa (length cuts)) " wall(s)."))))))
  (princ))

;; ---- circular column ---------------------------------------------

(defun akc:mkcircle (ctr r / )
  (akc:ensure-lyr *col-layer* *col-layer-color*)
  (entmakex (list '(0 . "CIRCLE") (cons 8 *col-layer*)
                  (cons 10 ctr) (cons 40 r))))

;; Circle-line intersections. Returns list of on-segment hit points.
(defun akc:circ-int (p1 p2 ctr r / dx dy fx fy a b c disc s t1 t2 hits)
  (setq dx (- (car p2) (car p1)) dy (- (cadr p2) (cadr p1))
        fx (- (car p1) (car ctr)) fy (- (cadr p1) (cadr ctr))
        a  (+ (* dx dx) (* dy dy))
        b  (* 2.0 (+ (* fx dx) (* fy dy)))
        c  (- (+ (* fx fx) (* fy fy)) (* r r))
        disc (- (* b b) (* 4.0 a c))
        hits nil)
  (if (and (> a 1e-12) (>= disc 0.0))
    (progn
      (setq s (sqrt disc)
            t1 (/ (- (- b) s) (* 2.0 a))
            t2 (/ (+ (- b) s) (* 2.0 a)))
      (if (and (>= t1 -1e-6) (<= t1 (+ 1.0 1e-6)))
        (setq hits (cons (list (+ (car p1) (* t1 dx))
                               (+ (cadr p1) (* t1 dy)) 0.0) hits)))
      (if (and (>= t2 -1e-6) (<= t2 (+ 1.0 1e-6))
               (> (abs (- t2 t1)) 1e-6))
        (setq hits (cons (list (+ (car p1) (* t2 dx))
                               (+ (cadr p1) (* t2 dy)) 0.0) hits)))))
  hits)

(defun akc:pt-in-circ (p ctr r)
  (< (distance p ctr) (- r 1e-4)))

;; Cut walls crossing the circle. Returns list of (layer origP1 origP2).
(defun akc:cut-walls-circ (ctr r / ss i e d p1 p2 lay hits inA inB records)
  (setq ss (ssget "_X" '((0 . "LINE"))) records nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) i (1+ i)
              d (entget e)
              p1 (cdr (assoc 10 d))
              p2 (cdr (assoc 11 d))
              lay (cdr (assoc 8 d)))
        (if (akc:cuttable-lyr lay)
          (progn
            (setq hits (akc:circ-int p1 p2 ctr r)
                  inA  (akc:pt-in-circ p1 ctr r)
                  inB  (akc:pt-in-circ p2 ctr r))
            (cond
              ((and inA inB)
                (entdel e)
                (setq records (cons (list lay p1 p2) records)))
              ((and (not inA) (not inB) (>= (length hits) 2))
                (akc:split-line-at e (car hits) (cadr hits))
                (setq records (cons (list lay p1 p2) records)))
              ((and inA hits)
                (akc:trim-line-to e p2 (car hits))
                (setq records (cons (list lay p1 p2) records)))
              ((and inB hits)
                (akc:trim-line-to e p1 (car hits))
                (setq records (cons (list lay p1 p2) records)))))))))
  records)

(defun akc:place-circ (ctr hh bb / r circEnt hatchEnt clay oldCE cutRecs
                                    axH axV tag1 tag2 gname ents colNum
                                    lblEnts tl)
      (command-s "_.UNDO" "_BE")
      (setq r (/ *col-dia* 2.0)
            circEnt (akc:mkcircle ctr r)
            cutRecs (akc:cut-walls-circ ctr r))
      ;; hatch
      (setq clay (getvar "CLAYER") oldCE (getvar "CECOLOR"))
      (setvar "CLAYER" *col-layer*)
      (setvar "CECOLOR" (itoa *col-hatch-color*))
      (command-s "_.-HATCH" "_P" "_SOLID" "_S" circEnt "" "")
      (setq hatchEnt (entlast))
      (setvar "CECOLOR" oldCE)
      (setvar "CLAYER" clay)
      (if hatchEnt
        (entmod (append (vl-remove-if '(lambda (x) (= (car x) 62))
                                      (entget hatchEnt))
                        (list (cons 62 *col-hatch-color*)))))
      ;; axis cross = full-diameter H+V through center
      (setq axH (akc:mkline (list (- (car ctr) r) (cadr ctr) 0.0)
                            (list (+ (car ctr) r) (cadr ctr) 0.0)
                            *col-axis-color*)
            axV (akc:mkline (list (car ctr) (- (cadr ctr) r) 0.0)
                            (list (car ctr) (+ (cadr ctr) r) 0.0)
                            *col-axis-color*))
      ;; AWALL tags: two diameters
      (setq tag1 (akc:tag-awall (list (- (car ctr) r) (cadr ctr) 0.0)
                                (list (+ (car ctr) r) (cadr ctr) 0.0)
                                *col-dia* hh bb)
            tag2 (akc:tag-awall (list (car ctr) (- (cadr ctr) r) 0.0)
                                (list (car ctr) (+ (cadr ctr) r) 0.0)
                                *col-dia* hh bb))
      (setq colNum  (akc:pick-num)
            tl      (list (- (car ctr) r) (+ (cadr ctr) r) 0.0)
            lblEnts (akc:label tl (strcat "%%C" (rtos *col-dia* 2 0)) colNum))
      (akc:attach-cuts circEnt cutRecs)
      (setq gname (akc:uniqname "AKCOL")
            ents  (append (list circEnt hatchEnt axH axV tag1 tag2) lblEnts))
      (akc:mkgroup gname ents)
      (command-s "_.UNDO" "_E")
      (princ (strcat "\nRound column placed (" gname
                     "). Wall cuts: " (itoa (length cutRecs)))))

(defun c:AC ( / done p hh bb v k)
  (setvar "CMDECHO" 0)
  (setq done nil
        hh (if *WW_Height*   *WW_Height*   2700.0)
        bb (if *WW_BaseElev* *WW_BaseElev* 0.0))
  (while (not done)
    (if (= *col-shape* "R")
      (progn
        (princ (strcat "\n[Rect Column  W=" (rtos *col-w* 2 0)
                       "  D=" (rtos *col-d* 2 0)
                       "  Base=" *col-base* "]"))
        (initget "Rect Circle Width Depth Base")
        (setq p (getpoint
          "\nInsertion point or [Rect/Circle/Width/Depth/Base]: ")))
      (progn
        (princ (strcat "\n[Round Column  Dia=" (rtos *col-dia* 2 0) "]"))
        (initget "Rect Circle Diameter")
        (setq p (getpoint
          "\nCenter point or [Rect/Circle/Diameter]: "))))
    (cond
      ((null p) (setq done t))
      ((= p "Rect")   (setq *col-shape* "R"))
      ((= p "Circle") (setq *col-shape* "C"))
      ((= p "Width")
        (setq v (getdist (strcat "\nWidth <" (rtos *col-w* 2 0) ">: ")))
        (if v (setq *col-w* v)))
      ((= p "Depth")
        (setq v (getdist (strcat "\nDepth <" (rtos *col-d* 2 0) ">: ")))
        (if v (setq *col-d* v)))
      ((= p "Base") (c:CB))
      ((= p "Diameter")
        (setq v (getdist (strcat "\nDiameter <" (rtos *col-dia* 2 0) ">: ")))
        (if v (setq *col-dia* v)))
      ((listp p)
        (if (= *col-shape* "R")
          (akc:place-rect p hh bb)
          (akc:place-circ p hh bb)))))
  (princ))

(princ "\nAKDColumn loaded. Commands: AC (R/C), CB, CCW, EC.")
(princ)
