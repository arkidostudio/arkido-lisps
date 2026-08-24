;; HOLE.LSP - Cut a door/window opening through a two-parallel-line wall.
;; Works on LINE and LWPOLYLINE (straight segments) walls.
;;
;; Command: HH  (loops until Esc / Enter)
;;   [C]enter - hole centered on the clicked wall segment
;;   [F]romWall - hole starts G inward from the nearer end of the clicked segment
;;   [W]idth    - opening width
;;   [G]ap      - inset used in FromWall mode

(defun hole:getw () (if (> (getvar "USERR1") 0) (getvar "USERR1") 900.0))
(defun hole:getg () (if (> (getvar "USERR2") 0) (getvar "USERR2") 100.0))
(defun hole:getm () (if (= (getvar "USERI1") 1) "F" "C"))
(defun hole:setm (m) (setvar "USERI1" (if (= m "F") 1 0)))

(defun hole:prompt ()
  (strcat "\n[" (if (= (hole:getm) "F") "FromWall" "Center")
          " W=" (rtos (hole:getw) 2 2)
          (if (= (hole:getm) "F") (strcat " G=" (rtos (hole:getg) 2 2)) "")
          "] Click wall or [Center/FromWall/Width/Gap]: "))

(defun c:HH ( / pk v done)
  (setvar "CMDECHO" 0)
  (if (not *hole-hinted*)
    (progn
      (princ "\nHH: click a LINE or polyline segment; the parallel partner is auto-detected.")
      (princ "\n    Center = hole at segment midpoint. FromWall = hole G inward from nearer end.")
      (princ "\n    Loops until Esc or Enter.")
      (setq *hole-hinted* t)))
  (setq done nil)
  (while (not done)
    (initget "Center FromWall Width Gap")
    (setq pk (entsel (hole:prompt)))
    (cond
      ((= pk "Center")   (hole:setm "C"))
      ((= pk "FromWall") (hole:setm "F"))
      ((= pk "Width")
        (setq v (getdist (strcat "\nWidth <" (rtos (hole:getw) 2 2) ">: ")))
        (if v (setvar "USERR1" v)))
      ((= pk "Gap")
        (setq v (getdist (strcat "\nGap from clicked end <"
                                 (rtos (hole:getg) 2 2) ">: ")))
        (if v (progn (setvar "USERR2" v) (hole:setm "F"))))
      ((and pk (listp pk))
        (command-s "_.UNDO" "_BE")
        (hole:do (car pk) (cadr pk))
        (command-s "_.UNDO" "_E"))
      (t (setq done t))))
  (princ))

(defun hole:do (ent pk / seg1 seg2 p1a p1b p2a p2b dir len w d half
                          ctr1 c1 c2 b1a b1b b2a b2b osm nearEnd mode)
  (setq mode (hole:getm))
  (setq seg1 (if (and (= mode "F") (= "LWPOLYLINE" (cdr (assoc 0 (entget ent)))))
               (hole:pickseg-by-vertex ent pk)
               (hole:picksegment ent pk)))
  (cond
    ((not seg1) (princ "\nUnsupported entity or click too far from segment."))
    (t
      (setq p1a (nth 2 seg1) p1b (nth 3 seg1)
            dir (mapcar '- p1b p1a)
            len (distance p1a p1b)
            dir (list (/ (car dir) len) (/ (cadr dir) len) 0.0)
            seg2 (hole:findpartner seg1 pk dir))
      (cond
        ((not seg2) (princ "\nNo parallel partner segment found."))
        (t
          (setq p2a (nth 2 seg2) p2b (nth 3 seg2)
                w (hole:getw) d (hole:getg)
                half (/ w 2.0))

          (if (= mode "F")
            (progn
              (setq nearEnd (if (< (distance pk p1a) (distance pk p1b)) p1a p1b))
              (setq ctr1 (mapcar '+ nearEnd
                          (hole:scl (if (equal nearEnd p1a) dir
                                      (hole:scl dir -1.0))
                                    (+ d half)))))
            (setq ctr1 (mapcar '+ p1a (hole:scl dir (/ len 2.0)))))

          (setq c1 ctr1
                c2 (hole:proj ctr1 p2a dir)
                b1a (mapcar '+ c1 (hole:scl dir (- half)))
                b1b (mapcar '+ c1 (hole:scl dir half))
                b2a (mapcar '+ c2 (hole:scl dir (- half)))
                b2b (mapcar '+ c2 (hole:scl dir half)))

          (cond
            ((not (and (hole:onseg b1a p1a p1b) (hole:onseg b1b p1a p1b)
                       (hole:onseg b2a p2a p2b) (hole:onseg b2b p2a p2b)))
              (princ "\nHole doesn't fit within both wall segments — aborted."))
            ((equal (car seg1) (car seg2))
              (hole:split-pl-two (car seg1) (cadr seg1) b1a b1b (cadr seg2) b2a b2b)
              (setq osm (getvar "OSMODE"))
              (setvar "OSMODE" 0)
              (command-s "_.LINE" b1a b2a "")
              (command-s "_.LINE" b1b b2b "")
              (setvar "OSMODE" osm)
              (princ "\nHole cut."))
            (t
              (hole:splitseg seg1 b1a b1b)
              (hole:splitseg seg2 b2a b2b)
              (setq osm (getvar "OSMODE"))
              (setvar "OSMODE" 0)
              (command-s "_.LINE" b1a b2a "")
              (command-s "_.LINE" b1b b2b "")
              (setvar "OSMODE" osm)
              (princ "\nHole cut."))))))))

;; ---------- segment abstraction ----------
;; segment = (ent segIdx startPt endPt)   segIdx = -1 for LINE

(defun hole:segs (ent / d type verts n i out closed)
  (setq d (entget ent) type (cdr (assoc 0 d)))
  (cond
    ((= type "LINE")
      (list (list ent -1 (cdr (assoc 10 d)) (cdr (assoc 11 d)))))
    ((= type "LWPOLYLINE")
      (setq verts (hole:pl-verts d)
            n (length verts)
            closed (and (assoc 70 d) (= 1 (logand 1 (cdr (assoc 70 d)))))
            i 0 out '())
      (while (< i (1- n))
        (setq out (cons (list ent i (nth i verts) (nth (1+ i) verts)) out)
              i (1+ i)))
      (if closed
        (setq out (cons (list ent (1- n) (nth (1- n) verts) (nth 0 verts)) out)))
      (reverse out))
    (t nil)))

(defun hole:pl-verts (d / v out)
  (setq out '())
  (foreach x d
    (if (= 10 (car x))
      (setq out (cons (list (car (cdr x)) (cadr (cdr x)) 0.0) out))))
  (reverse out))

(defun hole:picksegment (ent pk / segs best bd s e dir len k perp)
  (setq segs (hole:segs ent) best nil bd 1e99)
  (foreach seg segs
    (setq s (nth 2 seg) e (nth 3 seg)
          dir (mapcar '- e s) len (distance s e))
    (if (> len 1e-9)
      (progn
        (setq dir (list (/ (car dir) len) (/ (cadr dir) len) 0.0)
              k (+ (* (- (car pk) (car s)) (car dir))
                   (* (- (cadr pk) (cadr s)) (cadr dir)))
              perp (distance pk (hole:proj pk s dir)))
        (if (and (>= k -1e-6) (<= k (+ len 1e-6)) (< perp bd))
          (setq bd perp best seg)))))
  best)

;; Distance-mode polyline picker: find the vertex nearest to click, then use
;; the adjacent segment whose extent contains the click's projection (or the
;; closer one if both do).
(defun hole:pickseg-by-vertex (ent pk / segs vseg best bd s e dir len k perp)
  (setq segs (hole:segs ent) best nil bd 1e99)
  ;; nearest vertex = nearest endpoint across all segments
  (foreach seg segs
    (foreach v (list (nth 2 seg) (nth 3 seg))
      (if (< (distance pk v) bd)
        (setq bd (distance pk v) vseg v))))
  ;; among segments touching that vertex, pick the one where click projects in
  (setq bd 1e99)
  (foreach seg segs
    (if (or (equal vseg (nth 2 seg) 1e-6) (equal vseg (nth 3 seg) 1e-6))
      (progn
        (setq s (nth 2 seg) e (nth 3 seg)
              dir (mapcar '- e s) len (distance s e))
        (if (> len 1e-9)
          (progn
            (setq dir (list (/ (car dir) len) (/ (cadr dir) len) 0.0)
                  k (+ (* (- (car pk) (car s)) (car dir))
                       (* (- (cadr pk) (cadr s)) (cadr dir)))
                  perp (distance pk (hole:proj pk s dir)))
            (if (and (>= k -1e-6) (<= k (+ len 1e-6)) (< perp bd))
              (setq bd perp best seg)))))))
  ;; fallback to regular picker if nothing matched
  (if best best (hole:picksegment ent pk)))

(defun hole:findpartner (wseg pk a / ss n i e segs best bd s e2 dir2 len2 k perp)
  (setq ss (ssget "_X" '((0 . "LINE,LWPOLYLINE")))
        n (if ss (sslength ss) 0) i 0 segs '())
  (while (< i n)
    (setq segs (append (hole:segs (ssname ss i)) segs) i (1+ i)))
  (setq best nil bd 1e99)
  (foreach seg segs
    (if (not (and (equal (car seg) (car wseg)) (= (cadr seg) (cadr wseg))))
      (progn
        (setq s (nth 2 seg) e2 (nth 3 seg)
              dir2 (mapcar '- e2 s) len2 (distance s e2))
        (if (> len2 1e-9)
          (progn
            (setq dir2 (list (/ (car dir2) len2) (/ (cadr dir2) len2) 0.0))
            (if (< (abs (- (* (car a) (cadr dir2))
                           (* (cadr a) (car dir2)))) 1e-4)
              (progn
                (setq k (+ (* (- (car pk) (car s)) (car dir2))
                           (* (- (cadr pk) (cadr s)) (cadr dir2)))
                      perp (distance pk (hole:proj pk s dir2)))
                (if (and (>= k -1e-6) (<= k (+ len2 1e-6)) (< perp bd))
                  (setq bd perp best seg)))))))))
  best)

;; ---------- splitting ----------

(defun hole:splitseg (seg pa pb)
  (if (= -1 (cadr seg))
    (hole:split-line (car seg) pa pb)
    (hole:split-pl   (car seg) (cadr seg) pa pb)))

(defun hole:split-line (ent pa pb / ln s e da db near far new)
  (setq ln (entget ent)
        s (cdr (assoc 10 ln)) e (cdr (assoc 11 ln))
        da (distance s pa) db (distance s pb)
        near (if (< da db) pa pb)
        far  (if (< da db) pb pa))
  (entmod (subst (cons 11 near) (assoc 11 ln) ln))
  (setq new (list '(0 . "LINE")
                  (cons 8 (cdr (assoc 8 ln)))
                  (cons 10 far) (cons 11 e)))
  (if (assoc 62 ln) (setq new (append new (list (assoc 62 ln)))))
  (if (assoc 6  ln) (setq new (append new (list (assoc 6  ln)))))
  (entmake new))

(defun hole:split-pl (ent segIdx pa pb / d verts closed layer sv near far
                                          part1 part2 i n loopList)
  (setq d (entget ent)
        verts (hole:pl-verts d)
        n (length verts)
        closed (and (assoc 70 d) (= 1 (logand 1 (cdr (assoc 70 d)))))
        layer (cdr (assoc 8 d))
        sv (nth segIdx verts))
  (if (< (distance sv pa) (distance sv pb))
    (setq near pa far pb)
    (setq near pb far pa))
  (entdel ent)
  (cond
    (closed
      ;; one open polyline: far, walk forward wrapping until back to segIdx, then near
      (setq loopList (list far) i (rem (1+ segIdx) n))
      (setq loopList (cons (nth i verts) loopList))
      (while (/= i segIdx)
        (setq i (rem (1+ i) n))
        (setq loopList (cons (nth i verts) loopList)))
      (setq loopList (cons near loopList))
      (hole:make-pl (reverse loopList) nil layer))
    (t
      (setq part1 (append (hole:take verts (1+ segIdx)) (list near))
            part2 (cons far (hole:drop verts (1+ segIdx))))
      (if (>= (length part1) 2) (hole:make-pl part1 nil layer))
      (if (>= (length part2) 2) (hole:make-pl part2 nil layer)))))

;; Split a single (usually closed) polyline with two cuts on two different segments.
;; Produces two open polylines.
(defun hole:split-pl-two (ent segI aI bI segJ aJ bJ /
                            d verts layer n vI vJ nearI farI nearJ farJ
                            polyA polyB i)
  (setq d (entget ent)
        verts (hole:pl-verts d)
        n (length verts)
        layer (cdr (assoc 8 d)))
  ;; ensure segI < segJ so we can walk forward
  (if (> segI segJ)
    (progn
      (setq i segI segI segJ segJ i)
      (setq vI aI aI aJ aJ vI)
      (setq vI bI bI bJ bJ vI)))
  (setq vI (nth segI verts) vJ (nth segJ verts))
  (if (< (distance vI aI) (distance vI bI))
    (setq nearI aI farI bI) (setq nearI bI farI aI))
  (if (< (distance vJ aJ) (distance vJ bJ))
    (setq nearJ aJ farJ bJ) (setq nearJ bJ farJ aJ))
  ;; polyA: farI, v_{segI+1}, ..., v_segJ, nearJ
  (setq polyA (list farI) i (1+ segI))
  (while (<= i segJ)
    (setq polyA (cons (nth i verts) polyA) i (1+ i)))
  (setq polyA (reverse (cons nearJ polyA)))
  ;; polyB: farJ, v_{segJ+1}, ..., v_{n-1}, v_0, ..., v_segI, nearI
  (setq polyB (list farJ) i (rem (1+ segJ) n))
  (while (/= i (rem (1+ segI) n))
    (setq polyB (cons (nth i verts) polyB) i (rem (1+ i) n)))
  (setq polyB (reverse (cons nearI polyB)))
  (entdel ent)
  (if (>= (length polyA) 2) (hole:make-pl polyA nil layer))
  (if (>= (length polyB) 2) (hole:make-pl polyB nil layer)))

(defun hole:take (l n)
  (if (or (<= n 0) (null l)) '() (cons (car l) (hole:take (cdr l) (1- n)))))
(defun hole:drop (l n)
  (if (or (<= n 0) (null l)) l (hole:drop (cdr l) (1- n))))

(defun hole:make-pl (verts closed layer / e)
  (setq e (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer)
                '(100 . "AcDbPolyline") (cons 90 (length verts))
                (cons 70 (if closed 1 0))))
  (foreach v verts
    (setq e (append e (list (cons 10 (list (car v) (cadr v)))))))
  (entmake e))

;; ---------- vector helpers ----------
(defun hole:scl (v s) (list (* (car v) s) (* (cadr v) s) (* (caddr v) s)))
(defun hole:proj (p a d / ap k)
  (setq ap (mapcar '- p a)
        k (+ (* (car ap) (car d)) (* (cadr ap) (cadr d))))
  (mapcar '+ a (hole:scl d k)))
(defun hole:onseg (p a b / la lb lab)
  (setq la (distance a p) lb (distance b p) lab (distance a b))
  (<= (+ la lb) (+ lab 1e-4)))

(defun c:HOLE () (c:HH))

(princ (strcat "\nHOLE.LSP loaded. Type HH to run. ["
               (hole:getm) " W=" (rtos (hole:getw) 2 2)
               " G=" (rtos (hole:getg) 2 2) "]"))
(princ)
