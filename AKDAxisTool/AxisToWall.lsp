;; XWW - Axis to Wall. Preselect axis lines, run XWW.
;; Each axis becomes two parallel wall lines offset by thickness/2.
;; Junctions between axes are cleaned up:
;;   L-corner (endpoint meets endpoint)  -> fillet interior + exterior pairs
;;   T-junction (endpoint meets interior)-> break crossbar's near wall, trim stem walls
;; Default thickness 150. T at prompt to change.

(if (not *ax-wall-thk*) (setq *ax-wall-thk* 150.0))

;; ---------- small vector helpers (2D) ----------
(defun ax-pteq (a b) (< (distance a b) 1e-6))
(defun ax-sub (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun ax-dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun ax-len (v)   (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v)))))
(defun ax-unit (v / L) (setq L (ax-len v))
  (if (> L 1e-9) (list (/ (car v) L) (/ (cadr v) L)) '(0.0 0.0)))
(defun ax-perp (v) (list (- (cadr v)) (car v)))   ; rotate 90 CCW

;; ---------- main ----------
(defun c:XWW ( / ss i ent kw newthk half oldlayer oldcecolor oldcltype
                 oldcltscale oldcmdecho oldfilletrad oldtrim axes a b rest)
  (setq ss (ssget "_I" '((0 . "LINE"))))
  (if (not ss)
    (progn (princ "\nSelect axis lines: ")
           (setq ss (ssget '((0 . "LINE"))))))
  (if ss
    (progn
      (initget "Thickness")
      (setq kw (getkword (strcat "\nWall thickness <" (rtos *ax-wall-thk* 2 2)
                                 ">, or [Thickness] to change: ")))
      (if (= kw "Thickness")
        (progn (setq newthk (getdist (strcat "\nNew wall thickness <"
                                             (rtos *ax-wall-thk* 2 2) ">: ")))
               (if newthk (setq *ax-wall-thk* newthk))))

      (setq oldlayer    (getvar "CLAYER")
            oldcecolor  (getvar "CECOLOR")
            oldcltype   (getvar "CELTYPE")
            oldcltscale (getvar "CELTSCALE")
            oldcmdecho  (getvar "CMDECHO")
            oldfilletrad(getvar "FILLETRAD")
            oldtrim     (getvar "TRIMMODE"))
      (setvar "CMDECHO" 0)
      (setvar "FILLETRAD" 0.0)
      (setvar "TRIMMODE" 1)
      (setq *ax-nL* 0 *ax-nT* 0 *ax-nX* 0)

      (command "_.undo" "_begin")

      (command "_.-layer" "_Make" "WALL" "_Color" "7" "WALL"
               "_Ltype" "Continuous" "WALL" "")

      (setq half (/ *ax-wall-thk* 2.0) axes '() i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              axes (cons (ax-build-axis ent half) axes)
              i (1+ i)))

      ;; L/T pass: ordered pairs (T fires only from the stem side)
      (foreach a axes
        (foreach b axes
          (if (not (eq a b)) (ax-process-junction a b))))

      ;; X pass: unordered pairs
      (setq rest axes)
      (while rest
        (setq a (car rest) rest (cdr rest))
        (foreach b rest (ax-try-cross a b)))

      ;; cap any axis endpoint that doesn't touch another axis
      (ax-cap-ends axes)

      (command "_.undo" "_end")

      (setvar "CLAYER"    oldlayer)
      (setvar "CECOLOR"   oldcecolor)
      (setvar "CELTYPE"   oldcltype)
      (setvar "CELTSCALE" oldcltscale)
      (setvar "FILLETRAD" oldfilletrad)
      (setvar "TRIMMODE"  oldtrim)
      (setvar "CMDECHO"   oldcmdecho)
      (princ (strcat "\nJunctions detected: L=" (itoa *ax-nL*)
                     "  T=" (itoa *ax-nT*)
                     "  X=" (itoa *ax-nX*))))
    (princ "\nNo lines selected."))
  (princ))

;; ---------- axis record: (axis-ent p1 p2 wall+ wall-) ----------
;; wall+ is offset in +perp(dir); wall- in the opposite direction.
(defun ax-build-axis (ent half / d p1 p2 wp wm)
  (setq d (entget ent)
        p1 (cdr (assoc 10 d))
        p2 (cdr (assoc 11 d))
        wp (ax-offset-line p1 p2 half)
        wm (ax-offset-line p1 p2 (- half)))
  (list ent p1 p2 wp wm))

(defun ax-offset-line (p1 p2 dist / dir per np1 np2)
  (setq dir (ax-unit (ax-sub p2 p1))
        per (ax-perp dir)
        np1 (list (+ (car p1) (* (car per) dist))
                  (+ (cadr p1) (* (cadr per) dist)) 0.0)
        np2 (list (+ (car p2) (* (car per) dist))
                  (+ (cadr p2) (* (cadr per) dist)) 0.0))
  (entmakex (list '(0 . "LINE") '(8 . "WALL")
                  '(62 . 256) '(6 . "BYLAYER")
                  (cons 10 np1) (cons 11 np2))))

;; ---------- junction classification ----------
(defun ax-process-junction (a b / ep-a idx)
  (foreach idx '(1 2)
    (setq ep-a (nth idx a))
    (cond
      ((ax-pteq ep-a (nth 1 b)) (setq *ax-nL* (1+ *ax-nL*)) (ax-do-L a idx b 1))
      ((ax-pteq ep-a (nth 2 b)) (setq *ax-nL* (1+ *ax-nL*)) (ax-do-L a idx b 2))
      ((ax-pt-strict-inside ep-a (nth 1 b) (nth 2 b))
       (setq *ax-nT* (1+ *ax-nT*)) (ax-do-T a idx b)))))

(defun ax-pt-strict-inside (pt p1 p2 / v w cross tt)
  (setq v (ax-sub p2 p1) w (ax-sub pt p1)
        cross (- (* (car v) (cadr w)) (* (cadr v) (car w))))
  (if (< (abs cross) (* 1e-6 (max 1.0 (ax-len v))))
    (progn
      (setq tt (if (> (abs (car v)) (abs (cadr v)))
                 (/ (car w) (car v)) (/ (cadr w) (cadr v))))
      (and (> tt 1e-6) (< tt (- 1.0 1e-6))))
    nil))

;; ---------- L-corner ----------
;; A's endpoint (idx=1 or 2) meets B's endpoint (bidx).
;; Interior walls of A and B meet at the inner corner; exterior at the outer.
(defun ax-do-L (a a-idx b b-idx / ep-a ep-b to-a to-b per-a per-b half
                                  wa-int-off wa-ext-off wb-int-off wb-ext-off
                                  wa-int wa-ext wb-int wb-ext)
  (setq ep-a (nth a-idx a) ep-b (nth b-idx b)
        to-a (ax-unit (ax-sub (nth (if (= a-idx 1) 2 1) a) ep-a))
        to-b (ax-unit (ax-sub (nth (if (= b-idx 1) 2 1) b) ep-b))
        per-a (ax-perp (ax-unit (ax-sub (nth 2 a) (nth 1 a))))
        per-b (ax-perp (ax-unit (ax-sub (nth 2 b) (nth 1 b))))
        half (/ *ax-wall-thk* 2.0))
  (if (> (ax-dot per-a to-b) 0)
    (setq wa-int-off half wa-ext-off (- half))
    (setq wa-int-off (- half) wa-ext-off half))
  (if (> (ax-dot per-b to-a) 0)
    (setq wb-int-off half wb-ext-off (- half))
    (setq wb-int-off (- half) wb-ext-off half))
  (setq wa-int (ax-wall-at-point a wa-int-off (ax-endpoint-wall-pt a a-idx wa-int-off))
        wa-ext (ax-wall-at-point a wa-ext-off (ax-endpoint-wall-pt a a-idx wa-ext-off))
        wb-int (ax-wall-at-point b wb-int-off (ax-endpoint-wall-pt b b-idx wb-int-off))
        wb-ext (ax-wall-at-point b wb-ext-off (ax-endpoint-wall-pt b b-idx wb-ext-off)))
  (ax-fillet-pair wa-int wb-int)
  (ax-fillet-pair wa-ext wb-ext))

;; ---------- T-junction ----------
;; A is stem (endpoint sits on B interior). B is crossbar.
;; Break B's near-side wall between A's two wall intersections (removes middle piece).
;; Trim A's two walls back to that intersection.
(defun ax-do-T (a a-idx b / ep-a oa dir-a per-b half
                            near-off wa-plus wa-minus near-b
                            ip-plus ip-minus)
  (setq ep-a  (nth a-idx a)
        oa    (nth (if (= a-idx 1) 2 1) a)
        dir-a (ax-unit (ax-sub oa ep-a))
        per-b (ax-perp (ax-unit (ax-sub (nth 2 b) (nth 1 b))))
        half  (/ *ax-wall-thk* 2.0)
        near-off (if (> (ax-dot per-b dir-a) 0) half (- half))
        ip-plus  (ax-compute-ip a half        b near-off)
        ip-minus (ax-compute-ip a (- half)    b near-off))
  (if (and ip-plus ip-minus)
    (progn
      (setq near-b  (ax-wall-at-point b near-off ip-plus)
            wa-plus  (ax-wall-at-point a half     (ax-endpoint-wall-pt a a-idx half))
            wa-minus (ax-wall-at-point a (- half) (ax-endpoint-wall-pt a a-idx (- half))))
      (if (and near-b (entget near-b))
        (ax-split-line near-b ip-plus ip-minus))
      (if wa-plus  (ax-trim-endpoint wa-plus  ep-a ip-plus))
      (if wa-minus (ax-trim-endpoint wa-minus ep-a ip-minus)))))

;; Delete the segment of `ent` between c1 and c2 by shrinking the original
;; to end at the nearer cut, and adding a new line from the farther cut to the far endpoint.
(defun ax-split-line (ent c1 c2 / d p1 p2 near-cut far-cut)
  (setq d (entget ent) p1 (cdr (assoc 10 d)) p2 (cdr (assoc 11 d)))
  (if (< (distance p1 c1) (distance p1 c2))
    (setq near-cut c1 far-cut c2)
    (setq near-cut c2 far-cut c1))
  (entmod (subst (cons 11 near-cut) (assoc 11 d) d))
  (entmakex (list '(0 . "LINE") '(8 . "WALL") '(62 . 256) '(6 . "BYLAYER")
                  (cons 10 far-cut) (cons 11 p2))))

;; Replace the endpoint of `wall` nearest to ref-pt with new-pt.
(defun ax-trim-endpoint (wall ref-pt new-pt / d p1 p2)
  (if (and wall (entget wall))
    (progn
      (setq d (entget wall) p1 (cdr (assoc 10 d)) p2 (cdr (assoc 11 d)))
      (if (< (distance p1 ref-pt) (distance p2 ref-pt))
        (entmod (subst (cons 10 new-pt) (assoc 10 d) d))
        (entmod (subst (cons 11 new-pt) (assoc 11 d) d))))))

(defun ax-line-inters (e1 e2 / d1 d2)
  (if (and e1 e2 (entget e1) (entget e2))
    (progn (setq d1 (entget e1) d2 (entget e2))
      (inters (cdr (assoc 10 d1)) (cdr (assoc 11 d1))
              (cdr (assoc 10 d2)) (cdr (assoc 11 d2)) nil))))

;; Fillet-equivalent: move the nearer endpoint of each line to the intersection.
;; Works for both trim (ip inside segment) and extend (ip past endpoint).
(defun ax-fillet-pair (e1 e2 / d1 d2 a1 a2 b1 b2 ip)
  (if (and e1 e2 (entget e1) (entget e2))
    (progn
      (setq d1 (entget e1) d2 (entget e2)
            a1 (cdr (assoc 10 d1)) a2 (cdr (assoc 11 d1))
            b1 (cdr (assoc 10 d2)) b2 (cdr (assoc 11 d2))
            ip (inters a1 a2 b1 b2 nil))
      (if ip
        (progn (ax-move-nearest-endpoint e1 ip)
               (ax-move-nearest-endpoint e2 ip))))))

(defun ax-move-nearest-endpoint (ent target / d p1 p2 tgt3d)
  (setq d (entget ent) p1 (cdr (assoc 10 d)) p2 (cdr (assoc 11 d))
        tgt3d (list (car target) (cadr target) 0.0))
  (if (< (distance p1 target) (distance p2 target))
    (entmod (subst (cons 10 tgt3d) (assoc 10 d) d))
    (entmod (subst (cons 11 tgt3d) (assoc 11 d) d))))

;; ---------- X (cross) ----------
;; Axes cross strictly interior to both -> break each of the 4 walls between
;; its two intersection points with the perpendicular pair, leaving an open +.
(defun ax-try-cross (a b / p1a p2a p1b p2b ip half wap wam wbp wbm i1 i2 i3 i4)
  (setq p1a (nth 1 a) p2a (nth 2 a)
        p1b (nth 1 b) p2b (nth 2 b)
        ip  (inters p1a p2a p1b p2b nil))
  (if (and ip
           (ax-pt-strict-inside ip p1a p2a)
           (ax-pt-strict-inside ip p1b p2b))
    (progn
      (setq *ax-nX* (1+ *ax-nX*)
            half (/ *ax-wall-thk* 2.0)
            i1 (ax-compute-ip a half     b half)
            i2 (ax-compute-ip a half     b (- half))
            i3 (ax-compute-ip a (- half) b half)
            i4 (ax-compute-ip a (- half) b (- half)))
      (if (and i1 i2)
        (progn (setq wap (ax-wall-at-point a half i1))
               (if wap (ax-split-line wap i1 i2))))
      (if (and i3 i4)
        (progn (setq wam (ax-wall-at-point a (- half) i3))
               (if wam (ax-split-line wam i3 i4))))
      (if (and i1 i3)
        (progn (setq wbp (ax-wall-at-point b half i1))
               (if wbp (ax-split-line wbp i1 i3))))
      (if (and i2 i4)
        (progn (setq wbm (ax-wall-at-point b (- half) i2))
               (if wbm (ax-split-line wbm i2 i4)))))))

;; ---------- end caps ----------
(defun ax-cap-ends (axes / a ep)
  (foreach a axes
    (foreach ep (list (nth 1 a) (nth 2 a))
      (if (not (ax-endpoint-connected ep a axes))
        (ax-draw-cap a ep)))))

(defun ax-endpoint-connected (ep self axes / found b)
  (setq found nil)
  (foreach b axes
    (if (and (not (eq b self)) (not found)
             (or (ax-pteq ep (nth 1 b))
                 (ax-pteq ep (nth 2 b))
                 (ax-pt-strict-inside ep (nth 1 b) (nth 2 b))))
      (setq found T)))
  found)

(defun ax-draw-cap (a ep / p1 p2 per half hp1 hp2)
  (setq p1 (nth 1 a) p2 (nth 2 a)
        per (ax-perp (ax-unit (ax-sub p2 p1)))
        half (/ *ax-wall-thk* 2.0)
        hp1 (list (+ (car ep) (* (car per) half))
                  (+ (cadr ep) (* (cadr per) half)) 0.0)
        hp2 (list (- (car ep) (* (car per) half))
                  (- (cadr ep) (* (cadr per) half)) 0.0))
  (entmakex (list '(0 . "LINE") '(8 . "WALL") '(62 . 256) '(6 . "BYLAYER")
                  (cons 10 hp1) (cons 11 hp2))))

;; ---------- geometry-based wall lookup ----------
;; Find a WALL-layer line parallel to `axis` at signed perp offset `offset`
;; that contains `pt` within (or on the boundary of) its segment.
(defun ax-wall-at-point (axis offset pt / axp1 axp2 axdir axper
                                          ss i n ent d wp1 wp2 wdir mid perpd found)
  (setq axp1 (nth 1 axis) axp2 (nth 2 axis)
        axdir (ax-unit (ax-sub axp2 axp1))
        axper (ax-perp axdir)
        found nil
        ss (ssget "_X" '((0 . "LINE") (8 . "WALL"))))
  (if ss
    (progn
      (setq i 0 n (sslength ss))
      (while (and (not found) (< i n))
        (setq ent (ssname ss i) d (entget ent)
              wp1 (cdr (assoc 10 d)) wp2 (cdr (assoc 11 d))
              wdir (ax-unit (ax-sub wp2 wp1)))
        (if (> (abs (ax-dot wdir axdir)) 0.999999)
          (progn
            (setq mid (list (/ (+ (car wp1) (car wp2)) 2.0)
                            (/ (+ (cadr wp1) (cadr wp2)) 2.0))
                  perpd (ax-dot (ax-sub mid axp1) axper))
            (if (and (< (abs (- perpd offset)) 1e-3)
                     (ax-pt-on-seg-inclusive pt wp1 wp2))
              (setq found ent))))
        (setq i (1+ i)))))
  found)

(defun ax-endpoint-wall-pt (axis a-idx offset / ep per)
  (setq ep (nth a-idx axis)
        per (ax-perp (ax-unit (ax-sub (nth 2 axis) (nth 1 axis)))))
  (list (+ (car ep) (* (car per) offset))
        (+ (cadr ep) (* (cadr per) offset))))

;; Intersection of the mathematical line of axis A offset by a-off, with axis B offset by b-off.
(defun ax-compute-ip (a a-off b b-off / axp1 axp2 axper aop1 aop2
                                        bxp1 bxp2 bxper bop1 bop2)
  (setq axp1 (nth 1 a) axp2 (nth 2 a)
        axper (ax-perp (ax-unit (ax-sub axp2 axp1)))
        aop1  (list (+ (car axp1) (* (car axper) a-off))
                    (+ (cadr axp1) (* (cadr axper) a-off)))
        aop2  (list (+ (car axp2) (* (car axper) a-off))
                    (+ (cadr axp2) (* (cadr axper) a-off)))
        bxp1 (nth 1 b) bxp2 (nth 2 b)
        bxper (ax-perp (ax-unit (ax-sub bxp2 bxp1)))
        bop1  (list (+ (car bxp1) (* (car bxper) b-off))
                    (+ (cadr bxp1) (* (cadr bxper) b-off)))
        bop2  (list (+ (car bxp2) (* (car bxper) b-off))
                    (+ (cadr bxp2) (* (cadr bxper) b-off))))
  (inters aop1 aop2 bop1 bop2 nil))

(defun ax-pt-on-seg-inclusive (pt p1 p2 / v w cross tt tol)
  (setq v (ax-sub p2 p1) w (ax-sub pt p1)
        cross (- (* (car v) (cadr w)) (* (cadr v) (car w)))
        tol (* 1e-4 (max 1.0 (ax-len v))))
  (if (< (abs cross) tol)
    (progn (setq tt (if (> (abs (car v)) (abs (cadr v)))
                      (/ (car w) (car v)) (/ (cadr w) (cadr v))))
           (and (>= tt -1e-3) (<= tt (+ 1.0 1e-3))))
    nil))

(princ "\nAxis-to-Wall loaded. Preselect axis lines, then type XWW.")
(princ)
