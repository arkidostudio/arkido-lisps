; ============================================================
; AKDWallTool.lsp
; Combined wall-cleanup toolkit
;   TW       = Junction Scissor (X / T cleanup + inner stub removal)
;   FW       = Fix Walls (cap open wall ends)
;   FIXWALLS = alias of FW
; Source: WallScissor.lsp + FixWalls.lsp (from AKD Scissor Backup)
; ============================================================

; ============================================================
; TW.LSP
; Scissor Junctions + T Junction Support
; Command: TW
; Cleans:
;   X crossings
;   T junctions
;   Plus removes inner stubs
; Mac compatible
; ============================================================

(setq *tw:stub-tol* 1.0)
(setq *tw:corner-tol* 0.5)   ; distance tolerance for matching original endpoints

(defun tw:~= (a b) (< (abs (- a b)) 1e-3))
(defun tw:3d (p) (list (car p) (cadr p) 0.0))

(defun tw:pts (e / d)
  (if (and e (setq d (entget e)))
    (list (tw:3d (cdr (assoc 10 d)))
          (tw:3d (cdr (assoc 11 d))))
  )
)

(defun tw:mid (a b)
  (list (* 0.5 (+ (car a)(car b)))
        (* 0.5 (+ (cadr a)(cadr b)))
        0.0)
)

(defun tw:inside (p mnx mny mxx mxy / eps)
  (setq eps 0.1)
  (and
    (>= (car p) (- mnx eps))
    (<= (car p) (+ mxx eps))
    (>= (cadr p) (- mny eps))
    (<= (cadr p) (+ mxy eps))
  )
)

(defun tw:props (e / d)
  (setq d (entget e))
  (list
    (cdr (assoc 8 d))
    (cdr (assoc 62 d))
    (cdr (assoc 6 d))
    (cdr (assoc 370 d))
  )
)

(defun tw:make-line (a b pr)
  (entmake
    (append
      (list '(0 . "LINE")
            (cons 8 (nth 0 pr))
            (cons 10 a)
            (cons 11 b))
      (if (nth 1 pr) (list (cons 62 (nth 1 pr))) nil)
      (if (nth 2 pr) (list (cons 6 (nth 2 pr))) nil)
      (if (nth 3 pr) (list (cons 370 (nth 3 pr))) nil)
    )
  )
)

(defun tw:isect (p1 p2 p3 p4 / dx1 dy1 dx2 dy2 den tt)
  (setq dx1 (- (car p2)(car p1))
        dy1 (- (cadr p2)(cadr p1))
        dx2 (- (car p4)(car p3))
        dy2 (- (cadr p4)(cadr p3))
        den (- (* dx1 dy2)(* dy1 dx2)))

  (if (tw:~= den 0.0)
    nil
    (progn
      (setq tt
        (/ (+ (* (- (car p3)(car p1)) dy2)
              (* (- (cadr p1)(cadr p3)) dx2))
           den))
      (list
        (+ (car p1)(* tt dx1))
        (+ (cadr p1)(* tt dy1))
        0.0
      )
    )
  )
)

(defun tw:tparam (p a b / dx dy l2)
  (setq dx (- (car b)(car a))
        dy (- (cadr b)(cadr a))
        l2 (+ (* dx dx)(* dy dy)))
  (if (tw:~= l2 0.0)
    nil
    (/ (+ (* (- (car p)(car a)) dx)
          (* (- (cadr p)(cadr a)) dy))
       l2))
)

(defun tw:sort (lst / s)
  (setq s (acad_strlsort (mapcar '(lambda (x) (rtos x 2 8)) lst)))
  (mapcar 'atof s)
)

(defun tw:dedup (lst / out prev)
  (setq out nil prev nil)
  (foreach v (tw:sort lst)
    (if (or (null prev) (> (abs (- v prev)) 1e-6))
      (setq out (append out (list v))
            prev v)))
  out
)

(defun tw:remove-key (k lst / r)
  (setq r nil)
  (foreach x lst
    (if (/= (car x) k)
      (setq r (cons x r))))
  r
)

;; val is (t . cause) where cause is 'T (perp ended at ip) or 'X (perp passed through)
(defun tw:addtbl (tbl e val)
  (cons
    (cons e (cons val (cdr (assoc e tbl))))
    (tw:remove-key e tbl))
)

;; tvs is a list of (t . cause) pairs.
;; Returns list of (ename cause-at-start cause-at-end).
;; Cause is 'orig at t=0 and t=1 (original line endpoints).
(defun tw:split (e tvs / pts p1 p2 pr sorted a b out ta tb ca cb)
  (setq pts (tw:pts e)
        p1 (car pts)
        p2 (cadr pts)
        pr (tw:props e)
        sorted (tw:sort-tvs
                 (append (list (cons 0.0 'orig) (cons 1.0 'orig)) tvs))
        out nil)

  (entdel e)

  (while (>= (length sorted) 2)
    (setq ta (car (nth 0 sorted))
          ca (cdr (nth 0 sorted))
          tb (car (nth 1 sorted))
          cb (cdr (nth 1 sorted)))

    (setq a (list (+ (car p1)(* ta (- (car p2)(car p1))))
                  (+ (cadr p1)(* ta (- (cadr p2)(cadr p1))))
                  0.0)
          b (list (+ (car p1)(* tb (- (car p2)(car p1))))
                  (+ (cadr p1)(* tb (- (cadr p2)(cadr p1))))
                  0.0))

    (if (> (distance a b) *tw:stub-tol*)
      (progn
        (tw:make-line a b pr)
        (setq out (cons (list (entlast) ca cb) out))
      )
    )
    (setq sorted (cdr sorted))
  )
  out
)

;; Sort a list of (t . cause) pairs by t; dedupe near-equal t (keeping first).
(defun tw:sort-tvs (lst / order out prev)
  (setq order
    (vl-sort lst '(lambda (a b) (< (car a) (car b)))))
  (setq out nil prev nil)
  (foreach p order
    (if (or (null prev) (> (abs (- (car p) prev)) 1e-6))
      (setq out (append out (list p))
            prev (car p))))
  out
)

(defun tw:trim-to-point (e ip / pts p1 p2 pr cp)
  (setq pts (tw:pts e)
        p1 (car pts)
        p2 (cadr pts)
        pr (tw:props e)
        cp (list (car ip) (cadr ip) 0.0))

  (entdel e)

  (if (< (distance p1 cp) (distance p2 cp))
    (tw:make-line p1 cp pr)
    (tw:make-line cp p2 pr)
  )
)

;; Callable cleanup: mnx/mny/mxx/mxy in WCS. Same logic as c:TW.
;; Used by WW.lsp for auto-cleanup after drawing walls.
(defun tw:cleanup-box (mnx mny mxx mxy / ss enames tbl i j ea eb
                       p1a p2a p1b p2b ip ta tb new kept x ne pts
                       p1 p2 seg ca cb parents frags par allKept fe origPts)

  (defun tw:is-corner (p / n)
    (setq n 0)
    (foreach q origPts
      (if (< (distance p q) *tw:corner-tol*) (setq n (1+ n))))
    (> n 1))

  (setq ss
    (ssget "_C"
      (trans (list mnx mny 0) 0 1)
      (trans (list mxx mxy 0) 0 1)
      '((0 . "LINE"))))
  (if ss
    (progn
      (setq enames nil i 0)
      (repeat (sslength ss)
        (setq enames (cons (ssname ss i) enames) i (1+ i)))

      (setq tbl nil origPts nil)
      (foreach e enames
        (setq tbl (cons (cons e nil) tbl))
        (setq pts (tw:pts e))
        (setq origPts (cons (car pts) origPts))
        (setq origPts (cons (cadr pts) origPts)))

      (setq i 0)
      (foreach ea enames
        (setq p1a (car (tw:pts ea))
              p2a (cadr (tw:pts ea))
              j 0)
        (foreach eb enames
          (if (> j i)
            (progn
              (setq p1b (car (tw:pts eb))
                    p2b (cadr (tw:pts eb))
                    ip  (tw:isect p1a p2a p1b p2b))
              (if ip
                (progn
                  (setq ta (tw:tparam ip p1a p2a)
                        tb (tw:tparam ip p1b p2b))
                  (if (and ta tb)
                    (progn
                      (if (and (> ta 1e-6) (< ta (- 1.0 1e-6)))
                        (setq tbl (tw:addtbl tbl ea
                          (cons ta
                            (if (or (< (abs tb) 1e-6)
                                    (< (abs (- tb 1.0)) 1e-6))
                              'T 'X)))))
                      (if (and (> tb 1e-6) (< tb (- 1.0 1e-6)))
                        (setq tbl (tw:addtbl tbl eb
                          (cons tb
                            (if (or (< (abs ta) 1e-6)
                                    (< (abs (- ta 1.0)) 1e-6))
                              'T 'X))))))))))
          (setq j (1+ j)))
        (setq i (1+ i)))

      (setq new nil kept nil parents nil)
      (foreach x tbl
        (if (cdr x)
          (progn
            (setq pts (tw:pts (car x)))
            (setq parents
              (cons (list (car pts) (cadr pts)
                          (tw:props (car x)) nil)
                    parents))
            (setq frags (tw:split (car x) (cdr x)))
            (setq parents
              (cons (list (car (car parents))
                          (cadr (car parents))
                          (caddr (car parents))
                          (mapcar 'car frags))
                    (cdr parents)))
            (setq new (append new frags)))
          (setq kept (cons (car x) kept))))

      (foreach seg new
        (setq ne (nth 0 seg) ca (nth 1 seg) cb (nth 2 seg))
        (if (and (entget ne) (setq pts (tw:pts ne)))
          (progn
            (setq p1 (car pts) p2 (cadr pts))
            (if (and (tw:inside p1 mnx mny mxx mxy)
                     (tw:inside p2 mnx mny mxx mxy)
                     (not (and (eq ca 'T) (eq cb 'T)))
                     (not (tw:is-corner p1))
                     (not (tw:is-corner p2)))
              (entdel ne)
              (setq kept (cons ne kept))))))

      (foreach par parents
        (setq allKept T)
        (foreach fe (nth 3 par)
          (if (not (entget fe)) (setq allKept nil)))
        (if (and allKept (nth 3 par))
          (progn
            (foreach fe (nth 3 par) (entdel fe))
            (tw:make-line (nth 0 par) (nth 1 par) (nth 2 par))))))))
  (princ))

(defun c:TW (/ pt1 pt2 ss mnx mny mxx mxy enames tbl
               i j ea eb p1a p2a p1b p2b ip ta tb
               new kept x ne pts p1 p2 seg ca cb
               parents frags par allKept fe origPts)

  ;; True if p is close to ≥2 original endpoints — i.e. sits at a real
  ;; wall corner where multiple lines terminate.
  (defun tw:is-corner (p / n)
    (setq n 0)
    (foreach q origPts
      (if (< (distance p q) *tw:corner-tol*) (setq n (1+ n))))
    (> n 1))

  (princ "\nTW - X + T + L Junction Cleaner")

  (setq pt1 (getpoint "\nFirst corner: "))
  (if pt1
    (setq pt2 (getcorner pt1 "\nOpposite corner: "))
  )

  (if (and pt1 pt2)
    (progn
      (setq pt1 (trans pt1 1 0)
            pt2 (trans pt2 1 0)
            mnx (min (car pt1) (car pt2))
            mny (min (cadr pt1) (cadr pt2))
            mxx (max (car pt1) (car pt2))
            mxy (max (cadr pt1) (cadr pt2)))

      (setq ss
        (ssget "C"
          (trans (list mnx mny 0) 0 1)
          (trans (list mxx mxy 0) 0 1)
          '((0 . "LINE"))
        )
      )

      (if ss
        (progn
          ;; collect entities
          (setq enames nil
                i 0)

          (repeat (sslength ss)
            (setq enames (cons (ssname ss i) enames)
                  i (1+ i))
          )

          ;; init split table + capture all original endpoints for corner detection
          (setq tbl nil origPts nil)
          (foreach e enames
            (setq tbl (cons (cons e nil) tbl))
            (setq pts (tw:pts e))
            (setq origPts (cons (car pts) origPts))
            (setq origPts (cons (cadr pts) origPts)))

          ;; compare every pair
          (setq i 0)
          (foreach ea enames
            (setq p1a (car (tw:pts ea))
                  p2a (cadr (tw:pts ea))
                  j 0)

            (foreach eb enames
              (if (> j i)
                (progn
                  (setq p1b (car (tw:pts eb))
                        p2b (cadr (tw:pts eb))
                        ip  (tw:isect p1a p2a p1b p2b))

                  (if ip
                    (progn
                      (setq ta (tw:tparam ip p1a p2a)
                            tb (tw:tparam ip p1b p2b))

                      ;; Classify the intersection and tag each split with
                      ;; its cause: 'T (the OTHER line ended at ip — a real
                      ;; T-stem meeting this line) or 'X (the other line
                      ;; passed through — a crossing / overshoot).
                      (if (and ta tb)
                        (progn
                          ;; Split A if ip is interior of A.
                          ;; Cause for A's split = whether B was ending (T)
                          ;; or passing through (X) at ip.
                          (if (and (> ta 1e-6) (< ta (- 1.0 1e-6)))
                            (setq tbl
                              (tw:addtbl tbl ea
                                (cons ta
                                  (if (or (< (abs tb) 1e-6)
                                          (< (abs (- tb 1.0)) 1e-6))
                                    'T 'X))))
                          )

                          ;; Split B if ip is interior of B.
                          (if (and (> tb 1e-6) (< tb (- 1.0 1e-6)))
                            (setq tbl
                              (tw:addtbl tbl eb
                                (cons tb
                                  (if (or (< (abs ta) 1e-6)
                                          (< (abs (- ta 1.0)) 1e-6))
                                    'T 'X))))
                          )
                        )
                      )
                    )
                  )
                )
              )
              (setq j (1+ j))
            )

            (setq i (1+ i))
          )

          ;; split lines — each returned segment carries the cause
          ;; ('T / 'X / 'orig) at each of its two endpoints.
          ;; Also remember each parent's original endpoints + props so we
          ;; can rebuild it as a single line if no fragments end up deleted.
          (setq new nil
                kept nil
                parents nil)

          (foreach x tbl
            (if (cdr x)
              (progn
                (setq pts (tw:pts (car x)))
                (setq parents
                  (cons (list (car pts)             ; orig p1
                              (cadr pts)            ; orig p2
                              (tw:props (car x))    ; orig properties
                              nil)                  ; fragment enames (filled next)
                        parents))
                (setq frags (tw:split (car x) (cdr x)))
                (setq parents
                  (cons (list (car (car parents))
                              (cadr (car parents))
                              (caddr (car parents))
                              (mapcar 'car frags))
                        (cdr parents)))
                (setq new (append new frags)))
              (setq kept (cons (car x) kept))
            )
          )

          ;; Remove inside scraps. Delete any new segment entirely inside
          ;; the picked box UNLESS both its endpoints are T-junction splits
          ;; — that "T-T" case is the back-line of a wall continuing past
          ;; the junction and must survive.
          ;;
          ;; This catches:
          ;;   X-X  = stub sitting inside a perpendicular wall's material
          ;;   X-orig / orig-X = perpendicular's own overshoot past a crossing
          ;;   T-orig / orig-T = short tail past a real T stem inside the box
          (foreach seg new
            (setq ne (nth 0 seg)
                  ca (nth 1 seg)
                  cb (nth 2 seg))
            (if (and (entget ne)
                     (setq pts (tw:pts ne)))
              (progn
                (setq p1 (car pts)
                      p2 (cadr pts))

                (if (and (tw:inside p1 mnx mny mxx mxy)
                         (tw:inside p2 mnx mny mxx mxy)
                         (not (and (eq ca 'T) (eq cb 'T)))
                         (not (tw:is-corner p1))
                         (not (tw:is-corner p2)))
                  (entdel ne)
                  (setq kept (cons ne kept))
                )
              )
            )
          )

          ;; Merge back — for each parent line where every fragment survived
          ;; the scrap pass, remove the fragments and recreate the parent as
          ;; a single continuous LINE.
          (foreach par parents
            (setq allKept T)
            (foreach fe (nth 3 par)
              (if (not (entget fe)) (setq allKept nil)))
            (if (and allKept (nth 3 par))
              (progn
                (foreach fe (nth 3 par) (entdel fe))
                (tw:make-line (nth 0 par) (nth 1 par) (nth 2 par))
              )
            )
          )

          (princ "\nJunctions cleaned.")
        )
        (princ "\nNo lines found.")
      )
    )
  )

  (princ)
)
;; -- Old c:TW body kept below (dead) for reference; skip when reading. --

(princ "\nTW loaded. Type TW")
(princ)
; ============================================================
; FIXWALLS.LSP
; Caps open wall ends only
; Command: FIXWALLS
; Based on your working TW Mac v6 cap logic
; ============================================================

(setq *fw:stub-tol* 1.0)
(setq *fw:cap-tol* 500.0)
(setq *fw:axial-tol* 10.0)

(defun fw:~= (a b) (< (abs (- a b)) 1e-4))
(defun fw:3d (p) (list (car p) (cadr p) 0.0))

(defun fw:pts (e / d)
  (if (and e (setq d (entget e)))
    (list (fw:3d (cdr (assoc 10 d)))
          (fw:3d (cdr (assoc 11 d))))
  )
)

(defun fw:uvec (e / pts p1 p2 dx dy len)
  (if (setq pts (fw:pts e))
    (progn
      (setq p1 (car pts)
            p2 (cadr pts)
            dx (- (car p2)(car p1))
            dy (- (cadr p2)(cadr p1))
            len (sqrt (+ (* dx dx)(* dy dy))))
      (if (> len 1e-8)
        (list (/ dx len) (/ dy len) 0.0)
      )
    )
  )
)

(defun fw:inside (p mnx mny mxx mxy / eps)
  (setq eps 0.1)
  (and
    (>= (car p) (- mnx eps))
    (<= (car p) (+ mxx eps))
    (>= (cadr p) (- mny eps))
    (<= (cadr p) (+ mxy eps))
  )
)

(defun fw:props (e / d)
  (setq d (entget e))
  (list
    (cdr (assoc 8 d))
    (cdr (assoc 62 d))
    (cdr (assoc 6 d))
    (cdr (assoc 370 d))
  )
)

(defun fw:make-line (a b pr)
  (entmake
    (append
      (list '(0 . "LINE")
            (cons 8 (nth 0 pr))
            (cons 10 a)
            (cons 11 b))
      (if (nth 1 pr) (list (cons 62 (nth 1 pr))) nil)
      (if (nth 2 pr) (list (cons 6 (nth 2 pr))) nil)
      (if (nth 3 pr) (list (cons 370 (nth 3 pr))) nil)
    )
  )
)

(defun fw:key (a b)
  (list
    (fix (* (car a) 10))
    (fix (* (cadr a) 10))
    (fix (* (car b) 10))
    (fix (* (cadr b) 10))
  )
)

(defun c:FIXWALLS (/ pt1 pt2 ss i enames ea eb u1 u2 ptsA ptsB
                     p1a p2a p1b p2b eae ebe perp axa axb
                     mnx mny mxx mxy done j os *error* old-error nL nM)

  (setq os (getvar "OSMODE"))
  (setvar "OSMODE" 0)

  ;; Local *error* handler — always restores OSMODE, even on Esc/error
  (setq old-error *error*)
  (defun *error* (msg)
    (setvar "OSMODE" os)
    (setq *error* old-error)
    (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
      (princ (strcat "\nError: " msg)))
    (princ))

  (princ "\nFIXWALLS - Cap Open Walls")

  (setq pt1 (getpoint "\nFirst corner: "))
  (if pt1 (setq pt2 (getcorner pt1 "\nOpposite corner: ")))

  (if (and pt1 pt2)
    (progn
      (setq pt1 (trans pt1 1 0)
            pt2 (trans pt2 1 0)
            mnx (min (car pt1)(car pt2))
            mny (min (cadr pt1)(cadr pt2))
            mxx (max (car pt1)(car pt2))
            mxy (max (cadr pt1)(cadr pt2)))

      (setq ss
        (ssget "C"
          (trans (list mnx mny 0) 0 1)
          (trans (list mxx mxy 0) 0 1)
          '((0 . "LINE"))))

      (if ss
        (progn
          (setq enames nil i 0)
          (repeat (sslength ss)
            (setq enames (cons (ssname ss i) enames)
                  i (1+ i)))

          (setq done nil i 0)

          (foreach ea enames
            (if (and (entget ea) (setq u1 (fw:uvec ea)))
              (progn
                (setq ptsA (fw:pts ea)
                      p1a (car ptsA)
                      p2a (cadr ptsA)
                      j 0)

                (foreach eb enames
                  (if (and (> j i) (entget eb) (setq u2 (fw:uvec eb)))
                    (if (> (abs (+ (* (car u1)(car u2))
                                   (* (cadr u1)(cadr u2)))) 0.9998)
                      (progn
                        (setq ptsB (fw:pts eb)
                              p1b (car ptsB)
                              p2b (cadr ptsB)
                              perp
                              (abs (- (* (- (car p1b)(car p1a))(cadr u1))
                                      (* (- (cadr p1b)(cadr p1a))(car u1)))))

                        (if (and (> perp *fw:stub-tol*)
                                 (< perp *fw:cap-tol*))
                          (foreach eae (list p1a p2a)
                            (foreach ebe (list p1b p2b)
                              (setq axa (+ (* (- (car eae)(car p1a))(car u1))
                                           (* (- (cadr eae)(cadr p1a))(cadr u1)))
                                    axb (+ (* (- (car ebe)(car p1a))(car u1))
                                           (* (- (cadr ebe)(cadr p1a))(cadr u1))))

                              (if (and
                                    (< (abs (- axa axb)) *fw:axial-tol*)
                                    (> (distance eae ebe) *fw:stub-tol*)
                                    (or (fw:inside eae mnx mny mxx mxy)
                                        (fw:inside ebe mnx mny mxx mxy)))
                                (if (not (member (fw:key eae ebe) done))
                                  (progn
                                    (setq done (cons (fw:key eae ebe) done))
                                    (fw:make-line eae ebe (fw:props ea))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                  (setq j (1+ j))
                )
              )
            )
            (setq i (1+ i))
          )
          (princ "\nWalls capped.")
          (setq nL (fw:merge-lines mnx mny mxx mxy)
                nM (fw:merge-markers mnx mny mxx mxy))
          (if (or (> nL 0) (> nM 0))
            (princ (strcat "\nMerged " (itoa nL) " colinear line runs, "
                                       (itoa nM) " AWALL runs.")))
        )
        (princ "\nNo lines found.")
      )
    )
  )

  (setvar "OSMODE" os)
  (setq *error* old-error)
  (princ)
)

; ------------------------------------------------------------
; MERGE COLINEAR — collapses adjacent/overlapping colinear A-WALL
; LINEs (and their AWALL POINT markers) into single segments.
; Called as a second pass by c:FIXWALLS.
; ------------------------------------------------------------
(if (null *fw:merge-gap*) (setq *fw:merge-gap* 5.0))

(defun fw:canon-u (u)
  (cond ((> (car u)  1e-6) u)
        ((< (car u) -1e-6) (list (- (car u)) (- (cadr u)) 0.0))
        ((> (cadr u) 0.0)  u)
        (T                 (list (- (car u)) (- (cadr u)) 0.0))))

(defun fw:linekey (a u / uc off ang)
  (setq uc  (fw:canon-u u)
        off (- (* (cadr uc) (car a)) (* (car uc) (cadr a)))
        ang (atan (cadr uc) (car uc)))
  (list (fix (/ off 0.5)) (fix (* 1000.0 ang))))

(defun fw:axpar (p a u)
  (+ (* (- (car p)(car a))(car u))
     (* (- (cadr p)(cadr a))(cadr u))))

(defun fw:merge-ivs (ivs tol / out cur)
  (setq ivs (vl-sort ivs '(lambda (x y) (< (car x)(car y)))) cur nil)
  (foreach iv ivs
    (if (null cur)
      (setq cur (list (car iv)(cadr iv)(list (caddr iv))))
      (if (<= (- (car iv)(cadr cur)) tol)
        (setq cur (list (car cur)
                        (max (cadr cur)(cadr iv))
                        (cons (caddr iv)(caddr cur))))
        (progn (setq out (cons cur out))
               (setq cur (list (car iv)(cadr iv)(list (caddr iv))))))))
  (if cur (setq out (cons cur out)))
  out)

(defun fw:cl-uvec (p1 p2 / dx dy len)
  (setq dx (- (car p2)(car p1))
        dy (- (cadr p2)(cadr p1))
        len (sqrt (+ (* dx dx)(* dy dy))))
  (if (> len 1e-8) (list (/ dx len)(/ dy len) 0.0)))

;; Read AWALL xdata from a POINT → (thk h base p1 p2) or nil
(defun fw:awall-data (e / d xd items)
  (setq d (entget e '("AWALL"))
        xd (cdr (assoc -3 d)))
  (if xd
    (progn
      (setq items (cdr (assoc "AWALL" xd)))
      (if (>= (length items) 6)
        (list (cdr (nth 1 items))
              (cdr (nth 2 items))
              (cdr (nth 3 items))
              (cdr (nth 4 items))
              (cdr (nth 5 items)))))))

(defun fw:mk-marker (thk h base p1 p2 / mid)
  (setq mid (list (* 0.5 (+ (car p1)(car p2)))
                  (* 0.5 (+ (cadr p1)(cadr p2))) 0.0))
  (regapp "AWALL")
  (entmake (list '(0 . "POINT") '(8 . "A-WALL-DATA") (cons 10 mid)
                 (list -3 (list "AWALL"
                                (cons 1000 "AWALL")
                                (cons 1040 thk)
                                (cons 1040 h)
                                (cons 1040 base)
                                (list 1011 (car p1)(cadr p1) 0.0)
                                (list 1011 (car p2)(cadr p2) 0.0))))))

(defun fw:merge-lines (mnx mny mxx mxy / ss lst tbl bkt b k u a pts pr merged
                                          ivs t1 t2 tmp g)
  (setq merged 0
        ss (ssget "C" (trans (list mnx mny 0) 0 1)
                       (trans (list mxx mxy 0) 0 1)
                       '((0 . "LINE") (8 . "A-WALL"))))
  (if ss
    (progn
      (setq lst nil)
      (repeat (sslength ss)
        (setq lst (cons (ssname ss 0) lst))
        (ssdel (ssname ss 0) ss))
      (setq tbl nil)
      (foreach e lst
        (if (and (setq u (fw:uvec e)) (setq pts (fw:pts e)))
          (progn
            (setq a (car pts) k (fw:linekey a u) b (assoc k tbl))
            (if b
              (setq tbl (subst (cons k (cons e (cdr b))) b tbl))
              (setq tbl (cons (cons k (list e)) tbl))))))
      (foreach bkt tbl
        (if (> (length (cdr bkt)) 1)
          (progn
            (setq u (fw:canon-u (fw:uvec (car (cdr bkt))))
                  a (car (fw:pts (car (cdr bkt))))
                  ivs nil)
            (foreach e (cdr bkt)
              (setq pts (fw:pts e)
                    t1 (fw:axpar (car pts) a u)
                    t2 (fw:axpar (cadr pts) a u))
              (if (> t1 t2) (progn (setq tmp t1 t1 t2 t2 tmp)))
              (setq ivs (cons (list t1 t2 e) ivs)))
            (foreach g (fw:merge-ivs ivs *fw:merge-gap*)
              (if (> (length (caddr g)) 1)
                (progn
                  (setq pr (fw:props (car (caddr g))))
                  (foreach e (caddr g) (entdel e))
                  (fw:make-line
                    (list (+ (car a) (* (car u) (car g)))
                          (+ (cadr a) (* (cadr u) (car g))) 0.0)
                    (list (+ (car a) (* (car u) (cadr g)))
                          (+ (cadr a) (* (cadr u) (cadr g))) 0.0)
                    pr)
                  (setq merged (1+ merged))))))))))
  merged)

(defun fw:merge-markers (mnx mny mxx mxy / ss lst tbl bkt b k u a data d
                                            ivs t1 t2 tmp g merged
                                            thk h base seg)
  (setq merged 0
        ss (ssget "C" (trans (list mnx mny 0) 0 1)
                       (trans (list mxx mxy 0) 0 1)
                       '((0 . "POINT") (8 . "A-WALL-DATA") (-3 ("AWALL")))))
  (if ss
    (progn
      (setq lst nil)
      (repeat (sslength ss)
        (setq lst (cons (ssname ss 0) lst))
        (ssdel (ssname ss 0) ss))
      (setq tbl nil)
      (foreach e lst
        (if (and (setq data (fw:awall-data e))
                 (setq u (fw:cl-uvec (nth 3 data) (nth 4 data))))
          (progn
            (setq a (nth 3 data)
                  k (append (list (nth 0 data) (nth 1 data) (nth 2 data))
                            (fw:linekey a u))
                  b (assoc k tbl))
            (if b
              (setq tbl (subst (cons k (cons (list e data) (cdr b))) b tbl))
              (setq tbl (cons (cons k (list (list e data))) tbl))))))
      (foreach bkt tbl
        (if (> (length (cdr bkt)) 1)
          (progn
            (setq thk  (nth 0 (car bkt))
                  h    (nth 1 (car bkt))
                  base (nth 2 (car bkt))
                  data (cadr (car (cdr bkt)))
                  u    (fw:canon-u (fw:cl-uvec (nth 3 data) (nth 4 data)))
                  a    (nth 3 data)
                  ivs  nil)
            (foreach seg (cdr bkt)
              (setq d  (cadr seg)
                    t1 (fw:axpar (nth 3 d) a u)
                    t2 (fw:axpar (nth 4 d) a u))
              (if (> t1 t2) (progn (setq tmp t1 t1 t2 t2 tmp)))
              (setq ivs (cons (list t1 t2 (car seg)) ivs)))
            (foreach g (fw:merge-ivs ivs *fw:merge-gap*)
              (if (> (length (caddr g)) 1)
                (progn
                  (foreach e (caddr g) (entdel e))
                  (fw:mk-marker thk h base
                    (list (+ (car a) (* (car u) (car g)))
                          (+ (cadr a) (* (cadr u) (car g))) 0.0)
                    (list (+ (car a) (* (car u) (cadr g)))
                          (+ (cadr a) (* (cadr u) (cadr g))) 0.0))
                  (setq merged (1+ merged))))))))))
  merged)

(princ "\nFIXWALLS loaded. Type FIXWALLS")
(princ)
; ------------------------------------------------------------
; Aliases
; ------------------------------------------------------------
(defun c:FW () (c:FIXWALLS))

(princ "
AKDWallTool loaded. Commands: TW  FW  FIXWALLS")
(princ)

; ============================================================
; WW.lsp
; Command: WW
; Draws continuous double-line walls with corner filleting.
; Osnaps active throughout. Standard AutoCAD rubberband preview.
; ============================================================

;; ---- Settings (edit these) ----
(setq *WW_Thickness* 150.0)   ; wall thickness in drawing units
(setq *WW_Align*     1)       ; 1 = inside (CCW click), 2 = outside, 3 = center
(if (null *WW_Height*)   (setq *WW_Height*   2700.0)) ; wall height (for ELEV/SECT)
(if (null *WW_BaseElev*) (setq *WW_BaseElev*    0.0)) ; wall base Z

; ============================================================
; Helpers
; ============================================================

;; Perpendicular offsets from the click line. Returns (offL offR).
;; Left = angle + 90°, Right = angle - 90°.
(defun ww:offsets (align thick)
  (cond
    ((= align 1) (list 0.0 thick))
    ((= align 2) (list thick 0.0))
    (T           (list (/ thick 2.0) (/ thick 2.0)))
  )
)

;; Given segment (p1 p2) + offsets, return endpoints of the two parallel
;; lines: ((a1 a2) (b1 b2))
(defun ww:segment-lines (p1 p2 offL offR / ang)
  (setq ang (angle p1 p2))
  (list
    (list (polar p1 (+ ang (/ pi 2)) offL)
          (polar p2 (+ ang (/ pi 2)) offL))
    (list (polar p1 (- ang (/ pi 2)) offR)
          (polar p2 (- ang (/ pi 2)) offR))
  )
)

;; Move a line's start point (group 10) to newpt.
(defun ww:set-start (e newpt / d)
  (setq d (entget e))
  (entmod (subst (cons 10 newpt) (assoc 10 d) d))
)

;; Move a line's end point (group 11) to newpt.
(defun ww:set-end (e newpt / d)
  (setq d (entget e))
  (entmod (subst (cons 11 newpt) (assoc 11 d) d))
)

;; True if p lies on segment a-b within `tol` perpendicular distance
;; and strictly interior along-parameter (not near either endpoint).
(defun ww:pt-on-seg (p a b tol / dxv dyv l2 tt qx qy)
  (setq dxv (- (car b) (car a))
        dyv (- (cadr b) (cadr a))
        l2  (+ (* dxv dxv) (* dyv dyv)))
  (if (< l2 1e-6)
    nil
    (progn
      (setq tt (/ (+ (* (- (car p) (car a)) dxv)
                     (* (- (cadr p) (cadr a)) dyv)) l2))
      (and (> tt 0.02) (< tt 0.98)
           (progn
             (setq qx (+ (car a) (* tt dxv))
                   qy (+ (cadr a) (* tt dyv)))
             (< (distance p (list qx qy 0.0)) tol))))))

;; Ensure the AWALL-DATA layer exists (invisible marker layer).
(defun ww:ensure-lyr ( / cmde)
  (setq cmde (getvar "CMDECHO")) (setvar "CMDECHO" 0)
  (if (null (tblsearch "LAYER" "A-WALL-DATA"))
    (command "_.-layer" "_M" "A-WALL-DATA" "_C" "8" "" "_OFF" "A-WALL-DATA" ""))
  (if (null (tblsearch "LAYER" "A-WALL"))
    (command "_.-layer" "_M" "A-WALL" "_C" "7" "" ""))
  (setvar "CLAYER" "A-WALL")
  (setvar "CMDECHO" cmde))

;; Drop an AWALL marker POINT at the segment midpoint carrying centerline
;; p1/p2 + thk + height + baseElev as xdata. Points are ignored by the
;; junction cleanup so the tag survives.
(defun ww:tag-awall (p1 p2 thk h base / mid)
  (ww:ensure-lyr)
  (regapp "AWALL")
  (setq mid (list (* 0.5 (+ (car p1) (car p2)))
                  (* 0.5 (+ (cadr p1) (cadr p2))) 0.0))
  (entmake (list (cons 0 "POINT")
                 (cons 8 "A-WALL-DATA")
                 (cons 10 mid)
                 (list -3
                   (list "AWALL"
                         (cons 1000 "AWALL")
                         (cons 1040 thk)
                         (cons 1040 h)
                         (cons 1040 base)
                         (list 1011 (car p1) (cadr p1) 0.0)
                         (list 1011 (car p2) (cadr p2) 0.0))))))

; ============================================================
; c:WW
; ============================================================

; ============================================================
; AWALL Junction Engine — surgical T + X cleanup using AWALL data
; Detection: AWALL POINT xdata (true centerline + thickness).
; T = new-wall endpoint lands interior of existing wall's centerline.
;     Cut gap in existing wall's NEAR face only. New wall's faces stop
;     at that face, no end cap.
; X = new-wall segment crosses existing wall mid-both. Symmetric cut:
;     both walls' both faces get gaps at the crossing rectangle.
; ============================================================

;; Every AWALL wall in drawing. Each: (ent thk h base cp1 cp2).
(defun aw:all-walls (/ ss lst i e d)
  (setq lst nil
        ss  (ssget "_X" '((0 . "POINT")(8 . "A-WALL-DATA")(-3 ("AWALL")))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) d (fw:awall-data e))
        (if d
          (setq lst (cons (list e (nth 0 d)(nth 1 d)(nth 2 d)
                                  (nth 3 d)(nth 4 d)) lst)))
        (setq i (1+ i)))))
  lst)

;; Project p onto segment a→b. Returns (t perp-abs-dist) or nil.
(defun aw:proj (p a b / dx dy l2 tt qx qy)
  (setq dx (- (car b)(car a))
        dy (- (cadr b)(cadr a))
        l2 (+ (* dx dx)(* dy dy)))
  (if (< l2 1e-9) nil
    (progn
      (setq tt (/ (+ (* (- (car p)(car a)) dx)
                     (* (- (cadr p)(cadr a)) dy)) l2)
            qx (+ (car a)(* tt dx))
            qy (+ (cadr a)(* tt dy)))
      (list tt (distance p (list qx qy 0.0))))))

;; +1 if p left of a→b (standard 2D cross), -1 if right, 0 collinear.
(defun aw:side (p a b / cx)
  (setq cx (- (* (- (car b)(car a)) (- (cadr p)(cadr a)))
              (* (- (cadr b)(cadr a)) (- (car p)(car a)))))
  (cond ((> cx 1e-6) 1) ((< cx -1e-6) -1) (T 0)))

;; Endpoints of wall's near face (relative to ref-pt's side).
(defun aw:near-face (cp1 cp2 thk ref / s ang off)
  (setq s   (aw:side ref cp1 cp2)
        ang (angle cp1 cp2)
        off (* s (/ thk 2.0)))
  (list (polar cp1 (+ ang (/ pi 2)) off)
        (polar cp2 (+ ang (/ pi 2)) off)))

;; First existing wall whose centerline INTERIOR contains p.
;; Tol=0.55*thk so a click anywhere in wall material registers.
;; Returns (ent thk cp1 cp2) or nil.
(defun aw:t-hit (p excludes / walls hit pj)
  (setq walls (aw:all-walls) hit nil)
  (foreach w walls
    (if (and (not hit) (not (member (nth 0 w) excludes)))
      (progn
        (setq pj (aw:proj p (nth 4 w)(nth 5 w)))
        (if (and pj (> (car pj) 0.05) (< (car pj) 0.95)
                 (< (cadr pj) (max 1.0 (* (nth 1 w) 0.55))))
          (setq hit (list (nth 0 w)(nth 1 w)(nth 4 w)(nth 5 w)))))))
  hit)

;; Walls whose centerline crosses segment (a1 a2) mid-both.
;; Returns list of (ent thk cp1 cp2 ip).
(defun aw:x-hits (a1 a2 excludes / walls out ip pja pjb)
  (setq walls (aw:all-walls) out nil)
  (foreach w walls
    (if (not (member (nth 0 w) excludes))
      (progn
        (setq ip (inters a1 a2 (nth 4 w)(nth 5 w) T))
        (if ip
          (progn
            (setq pja (aw:proj ip a1 a2)
                  pjb (aw:proj ip (nth 4 w)(nth 5 w)))
            (if (and pja pjb (> (car pja) 0.05)(< (car pja) 0.95)
                              (> (car pjb) 0.05)(< (car pjb) 0.95))
              (setq out (cons (list (nth 0 w)(nth 1 w)(nth 4 w)
                                    (nth 5 w) ip) out))))))))
  out)

;; Trim N's endpoint to E's near face.
;; p-other: N's opposite endpoint (picks E's near side).
;; p-clicked: N's endpoint being trimmed.
;; seg-ang: N's segment direction (p1→p2, always) — required so faL/faR
;;   land on the correct side for both start-T and end-T.
;; thk-n: N's thickness. e-hit: (ent thk-E cp1 cp2).
;; Returns (new-cx new-faL new-faR) all on E's near face; or nil.
(defun aw:trim-endpoint (p-other p-clicked seg-ang thk-n e-hit / thk-e cp1 cp2 nf
                                                                 faLp faRp
                                                                 hit-cl hit-a hit-b)
  (setq thk-e  (nth 1 e-hit) cp1 (nth 2 e-hit) cp2 (nth 3 e-hit)
        nf     (aw:near-face cp1 cp2 thk-e p-other)
        faLp   (polar p-other (+ seg-ang (/ pi 2)) (/ thk-n 2.0))
        faRp   (polar p-other (- seg-ang (/ pi 2)) (/ thk-n 2.0))
        hit-cl (inters p-other p-clicked (car nf)(cadr nf) nil)
        hit-a  (inters faLp (polar faLp seg-ang 1.0) (car nf)(cadr nf) nil)
        hit-b  (inters faRp (polar faRp seg-ang 1.0) (car nf)(cadr nf) nil))
  (if (and hit-cl hit-a hit-b)
    (list hit-cl hit-a hit-b)))

;; LINE entities on A-WALL parallel to (cp1-cp2) at signed perp offset
;; = side * thk/2 (from centerline). Returns list of entnames.
(defun aw:find-face-lines (cp1 cp2 thk side / mid m ss lst i e d p1 p2
                                              ang la dp cmid pj)
  (setq lst nil
        mid (list (* 0.5 (+ (car cp1)(car cp2)))
                  (* 0.5 (+ (cadr cp1)(cadr cp2))) 0.0)
        m   (+ (* 0.5 (distance cp1 cp2)) thk 100.0)
        ang (angle cp1 cp2)
        ss  (ssget "_C"
              (trans (list (- (car mid) m)(- (cadr mid) m) 0) 0 1)
              (trans (list (+ (car mid) m)(+ (cadr mid) m) 0) 0 1)
              '((0 . "LINE")(8 . "A-WALL"))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e  (ssname ss i) d (entget e)
              p1 (cdr (assoc 10 d))
              p2 (cdr (assoc 11 d))
              la (angle p1 p2)
              dp (abs (+ (* (cos ang)(cos la))(* (sin ang)(sin la)))))
        (if (> dp 0.9995)
          (progn
            (setq cmid (list (* 0.5 (+ (car p1)(car p2)))
                             (* 0.5 (+ (cadr p1)(cadr p2))) 0.0)
                  pj   (aw:proj cmid cp1 cp2))
            (if (and pj (< (abs (- (cadr pj) (/ thk 2.0))) 1.5)
                     (= (aw:side cmid cp1 cp2) side))
              (setq lst (cons e lst)))))
        (setq i (1+ i)))))
  lst)

;; Cut a LINE at two points that land on it (interior); keep outer parts,
;; delete original. Returns T if cut succeeded.
(defun aw:cut-line-gap (e h1 h2 / d p1 p2 lyr pj1 pj2 t1 t2 first-h second-h tmp)
  (setq d   (entget e)
        p1  (cdr (assoc 10 d))
        p2  (cdr (assoc 11 d))
        lyr (cdr (assoc 8 d))
        pj1 (aw:proj h1 p1 p2)
        pj2 (aw:proj h2 p1 p2))
  (if (and pj1 pj2 (< (cadr pj1) 1.5) (< (cadr pj2) 1.5))
    (progn
      (setq t1 (car pj1) t2 (car pj2))
      (if (< t1 t2)
        (setq first-h h1 second-h h2)
        (setq first-h h2 second-h h1 tmp t1 t1 t2 t2 tmp))
      (if (and (> t1 0.002) (< t2 0.998))
        (progn
          (entdel e)
          (if (> (distance p1 first-h) 0.5)
            (entmake (list '(0 . "LINE")(cons 8 lyr)
                           (cons 10 p1)(cons 11 first-h))))
          (if (> (distance second-h p2) 0.5)
            (entmake (list '(0 . "LINE")(cons 8 lyr)
                           (cons 10 second-h)(cons 11 p2))))
          T)))))

;; Cut T-gap in E's near face between hit-a and hit-b (N's face endpts).
(defun aw:cut-t-gap (e-hit hit-a hit-b ref-pt / thk cp1 cp2 s lines)
  (setq thk (nth 1 e-hit) cp1 (nth 2 e-hit) cp2 (nth 3 e-hit)
        s   (aw:side ref-pt cp1 cp2)
        lines (aw:find-face-lines cp1 cp2 thk s))
  (foreach ln lines (aw:cut-line-gap ln hit-a hit-b)))

;; L-hits at p — ALL existing walls with an endpoint near p (0.55*thk).
;; Each entry: (ent thk cp1 cp2 corner-pt other-end).
(defun aw:l-hits (p excludes / walls out d1 d2)
  (setq walls (aw:all-walls) out nil)
  (foreach w walls
    (if (not (member (nth 0 w) excludes))
      (progn
        (setq d1 (distance p (nth 4 w))
              d2 (distance p (nth 5 w)))
        (cond
          ((< d1 (max 1.0 (* (nth 1 w) 0.85)))
           (setq out (cons (list (nth 0 w)(nth 1 w)(nth 4 w)(nth 5 w)
                                 (nth 4 w)(nth 5 w)) out)))
          ((< d2 (max 1.0 (* (nth 1 w) 0.85)))
           (setq out (cons (list (nth 0 w)(nth 1 w)(nth 4 w)(nth 5 w)
                                 (nth 5 w)(nth 4 w)) out)))))))
  out)

(defun aw:l-hit (p excludes) (car (aw:l-hits p excludes)))

;; Signed perpendicular distance of p from infinite line through 'from'
;; with direction 'dir'. Positive = LEFT of dir.
(defun aw:perp-signed (p from dir / dx dy)
  (setq dx (- (car p)(car from))
        dy (- (cadr p)(cadr from)))
  (+ (* (- (sin dir)) dx) (* (cos dir) dy)))

;; Given N's face at corner and outward direction, find the best existing
;; face ray from l-hits to pair with. Returns:
;;   (KIND w-hit w-face w-out w-fside apex) — see below — or nil.
;; KIND = 'MERGE (colinear) or 'MITER (needs intersection).
;; w-fside = which side of W's stored centerline W's face ray sits on
;; (+1 = W's LEFT, -1 = W's RIGHT), for aw:extend-face-end.
;; apex = for MITER, intersection apex. For MERGE, N's far face endpoint
;; (so W's face LINE extends all the way out).
(defun aw:pair-face (corner seg-ang n-out-ang thk-n face-corner far-endpt n-side l-hits
                     / cands w w-out wL wR wL-nside wR-nside
                       wL-fside wR-fside c best best-kind best-score w-perp
                       w-face w-fside ip raw nrm anti-para para)
  (setq cands nil)
  (foreach w l-hits
    (setq w-out    (angle (nth 4 w)(nth 5 w))
          wL      (polar (nth 4 w) (+ w-out (/ pi 2)) (/ (nth 1 w) 2.0))
          wR      (polar (nth 4 w) (- w-out (/ pi 2)) (/ (nth 1 w) 2.0))
          wL-nside (aw:perp-signed wL corner seg-ang)
          wR-nside (aw:perp-signed wR corner seg-ang)
          wL-fside (aw:side wL (nth 2 w)(nth 3 w))
          wR-fside (aw:side wR (nth 2 w)(nth 3 w)))
    (if (or (and (> n-side 0)(> wL-nside 0.5))
            (and (< n-side 0)(< wL-nside -0.5)))
      (setq cands (cons (list w wL w-out wL-fside) cands)))
    (if (or (and (> n-side 0)(> wR-nside 0.5))
            (and (< n-side 0)(< wR-nside -0.5)))
      (setq cands (cons (list w wR w-out wR-fside) cands))))
  ;; Two classes of pair:
  ;;   MERGE  = anti-parallel to N.face AND perp offset ~0 (colinear).
  ;;   MITER  = neither parallel nor anti-parallel (real angle).
  ;; Parallel-but-not-colinear and colinear-same-direction are skipped
  ;; (would produce degenerate or nonsensical geometry).
  (setq best nil best-kind nil best-score 1e99)
  (foreach c cands
    (setq w-face (nth 1 c)
          w-out  (nth 2 c)
          w-perp (abs (aw:perp-signed w-face face-corner seg-ang))
          raw    (- w-out n-out-ang)
          nrm    (abs (atan (sin raw)(cos raw)))
          anti-para (< (abs (- pi nrm)) 0.05)
          para      (< nrm 0.05))
    (cond
      ((and anti-para (< w-perp 1.5))
       (if (< w-perp best-score)
         (setq best c best-kind 'MERGE best-score w-perp)))
      ((or anti-para para) nil)
      (T
       (if (< w-perp best-score)
         (setq best c best-kind 'MITER best-score w-perp)))))
  (if best
    (progn
      (setq w      (nth 0 best)
            w-face (nth 1 best)
            w-out  (nth 2 best)
            w-fside (nth 3 best))
      (if (eq best-kind 'MERGE)
        (list 'MERGE w w-face w-out w-fside far-endpt)
        (progn
          (setq ip (inters face-corner (polar face-corner n-out-ang 1.0)
                           w-face      (polar w-face w-out 1.0) nil))
          (if (and ip (> (distance ip corner) 0.1))
            (list 'MITER w w-face w-out w-fside ip)))))))

;; Corner-pair: N joins into a shared corner (multiple existing walls).
;; Returns (faL-new faR-new emit-faL? emit-faR?).
;; Emit-flag = nil means MERGE happened, N's face LINE is not emitted;
;; the existing wall's face LINE has been extended to cover.
(defun aw:corner-pair (p-other p-clicked seg-ang thk-n l-hits
                       / corner n-out-ang faL-corner faR-corner
                         far-faL far-faR faL-r faR-r
                         faL-new faR-new emit-faL emit-faR
                         apex kind w-hit w-face w-out w-fside)
  (setq corner    p-clicked
        n-out-ang (angle p-clicked p-other)
        faL-corner (polar corner (+ seg-ang (/ pi 2)) (/ thk-n 2.0))
        faR-corner (polar corner (- seg-ang (/ pi 2)) (/ thk-n 2.0))
        far-faL   (polar p-other (+ seg-ang (/ pi 2)) (/ thk-n 2.0))
        far-faR   (polar p-other (- seg-ang (/ pi 2)) (/ thk-n 2.0))
        faL-r     (aw:pair-face corner seg-ang n-out-ang thk-n
                                faL-corner far-faL +1 l-hits)
        faR-r     (aw:pair-face corner seg-ang n-out-ang thk-n
                                faR-corner far-faR -1 l-hits)
        faL-new   faL-corner
        faR-new   faR-corner
        emit-faL  T
        emit-faR  T)
  (if faL-r
    (progn
      (setq kind (nth 0 faL-r) w-hit (nth 1 faL-r) w-face (nth 2 faL-r)
            w-out (nth 3 faL-r) w-fside (nth 4 faL-r) apex (nth 5 faL-r))
      (aw:extend-face-end (nth 2 w-hit)(nth 3 w-hit)(nth 1 w-hit)
                          w-fside (nth 4 w-hit) apex)
      (if (eq kind 'MERGE)
        (setq emit-faL nil)
        (setq faL-new apex))))
  (if faR-r
    (progn
      (setq kind (nth 0 faR-r) w-hit (nth 1 faR-r) w-face (nth 2 faR-r)
            w-out (nth 3 faR-r) w-fside (nth 4 faR-r) apex (nth 5 faR-r))
      (aw:extend-face-end (nth 2 w-hit)(nth 3 w-hit)(nth 1 w-hit)
                          w-fside (nth 4 w-hit) apex)
      (if (eq kind 'MERGE)
        (setq emit-faR nil)
        (setq faR-new apex))))
  (list faL-new faR-new emit-faL emit-faR))

;; Move the endpoint (at corner-pt) of E's face LINE on given side to
;; new-endpt. Finds the LINE via aw:find-face-lines and picks the one
;; whose end is closest to corner-pt.
(defun aw:extend-face-end (cp1 cp2 thk side corner-pt new-endpt / lines ln
                                                                 chosen best-d
                                                                 d pts p1 p2)
  (setq lines (aw:find-face-lines cp1 cp2 thk side)
        best-d 1e99 chosen nil)
  (foreach ln lines
    (setq p1 (cdr (assoc 10 (entget ln)))
          p2 (cdr (assoc 11 (entget ln)))
          d  (min (distance corner-pt p1)(distance corner-pt p2)))
    (if (< d best-d) (setq best-d d chosen ln)))
  (if (and chosen (< best-d thk))
    (progn
      (setq p1 (cdr (assoc 10 (entget chosen)))
            p2 (cdr (assoc 11 (entget chosen))))
      (if (< (distance corner-pt p1)(distance corner-pt p2))
        (ww:set-start chosen new-endpt)
        (ww:set-end   chosen new-endpt)))))

;; Miter N's face endpoints against E's face endpoints at corner (L join).
;; N.faL pairs with E.LEFT face (of E's direction toward corner) → inner
;; apex. N.faR pairs with E.RIGHT → outer apex. Extends E's face LINEs
;; too.
;; Returns (ipL ipR) or nil. Caller updates faL, faR (NOT cx — true
;; centerline of N still meets at corner along its own direction).
(defun aw:miter-l (p-other p-clicked seg-ang thk-n l-hit / thk-e cp1 cp2
                                                          c-pt o-pt eDir
                                                          eL1 eL2 eR1 eR2
                                                          faLp faRp ipL ipR)
  (setq thk-e (nth 1 l-hit)
        cp1   (nth 2 l-hit)
        cp2   (nth 3 l-hit)
        c-pt  (nth 4 l-hit)
        o-pt  (nth 5 l-hit)
        eDir  (angle o-pt c-pt)
        eL1   (polar o-pt (+ eDir (/ pi 2)) (/ thk-e 2.0))
        eL2   (polar c-pt (+ eDir (/ pi 2)) (/ thk-e 2.0))
        eR1   (polar o-pt (- eDir (/ pi 2)) (/ thk-e 2.0))
        eR2   (polar c-pt (- eDir (/ pi 2)) (/ thk-e 2.0))
        faLp  (polar p-other (+ seg-ang (/ pi 2)) (/ thk-n 2.0))
        faRp  (polar p-other (- seg-ang (/ pi 2)) (/ thk-n 2.0))
        ipL   (inters faLp (polar faLp seg-ang 1.0) eL1 eL2 nil)
        ipR   (inters faRp (polar faRp seg-ang 1.0) eR1 eR2 nil))
  (if (and ipL ipR)
    (progn
      (aw:extend-face-end cp1 cp2 thk-e (aw:side eL2 cp1 cp2) c-pt ipL)
      (aw:extend-face-end cp1 cp2 thk-e (aw:side eR2 cp1 cp2) c-pt ipR)
      (list ipL ipR))))

;; X-cut: both walls' both faces get gaps at the crossing rectangle.
;; la, lb = N's face LINE entnames (may become nil after cut).
;; Returns (la lb).
(defun aw:cut-x (x-hit la lb cx1 cx2 thk-n / thk-e cp1 cp2 ang angE
                                             faLN1 faLN2 faRN1 faRN2
                                             faLE1 faLE2 faRE1 faRE2
                                             xLL xLR xRL xRR)
  (setq thk-e (nth 1 x-hit) cp1 (nth 2 x-hit) cp2 (nth 3 x-hit)
        ang   (angle cx1 cx2) angE (angle cp1 cp2)
        faLN1 (polar cx1 (+ ang (/ pi 2)) (/ thk-n 2.0))
        faLN2 (polar cx2 (+ ang (/ pi 2)) (/ thk-n 2.0))
        faRN1 (polar cx1 (- ang (/ pi 2)) (/ thk-n 2.0))
        faRN2 (polar cx2 (- ang (/ pi 2)) (/ thk-n 2.0))
        faLE1 (polar cp1 (+ angE (/ pi 2)) (/ thk-e 2.0))
        faLE2 (polar cp2 (+ angE (/ pi 2)) (/ thk-e 2.0))
        faRE1 (polar cp1 (- angE (/ pi 2)) (/ thk-e 2.0))
        faRE2 (polar cp2 (- angE (/ pi 2)) (/ thk-e 2.0))
        xLL   (inters faLN1 faLN2 faLE1 faLE2 nil)
        xLR   (inters faLN1 faLN2 faRE1 faRE2 nil)
        xRL   (inters faRN1 faRN2 faLE1 faLE2 nil)
        xRR   (inters faRN1 faRN2 faRE1 faRE2 nil))
  (if (and la xLL xLR (aw:cut-line-gap la xLL xLR)) (setq la nil))
  (if (and lb xRL xRR (aw:cut-line-gap lb xRL xRR)) (setq lb nil))
  (if (and xLL xRL)
    (foreach ln (aw:find-face-lines cp1 cp2 thk-e 1)
      (aw:cut-line-gap ln xLL xRL)))
  (if (and xLR xRR)
    (foreach ln (aw:find-face-lines cp1 cp2 thk-e -1)
      (aw:cut-line-gap ln xLR xRR)))
  (list la lb))

(defun c:WW (/ p1 p2 offs offL offR ang off
                cx1 cx2 faL1 faL2 faR1 faR2 la lb
                prev-la prev-lb prev-faL1 prev-faL2 prev-faR1 prev-faR2
                first-la first-lb firstL1 firstR1 lastL2 lastR2
                startPt closing start-t end-t start-l end-l
                x-hits tr ipL ipR
                d1 d2 fla-p1 fla-p2 flb-p1 flb-p2
                start-t-close t-close
                emit-faL emit-faR
                emit-faL-s emit-faR-s emit-faL-e emit-faR-e)

  (or *WW_Thickness* (setq *WW_Thickness* 150.0))
  (or *WW_Align*     (setq *WW_Align*     2))

  (setq offs (ww:offsets *WW_Align* *WW_Thickness*)
        offL (car offs)
        offR (cadr offs))

  (princ (strcat "\nWW  |  Thickness=" (rtos *WW_Thickness* 2 1)
                 "  Align=" (itoa *WW_Align*)
                 " (" (nth (1- *WW_Align*) '("inside" "outside" "center")) ")"))

  (ww:ensure-lyr)
  (initget "A1 A2 A3 R")
  (setq p1 (getpoint "\nStart point [A1=inside A2=outside A3=center R=rectangle]: "))
  (while (member p1 '("A1" "A2" "A3"))
    (setq *WW_Align* (cond ((= p1 "A1") 1) ((= p1 "A2") 2) (T 3))
          offs (ww:offsets *WW_Align* *WW_Thickness*)
          offL (car offs)
          offR (cadr offs))
    (princ (strcat "\nAlign=" (itoa *WW_Align*)
                   " (" (nth (1- *WW_Align*) '("inside" "outside" "center")) ")"))
    (initget "A1 A2 A3 R")
    (setq p1 (getpoint "\nStart point [A1=inside A2=outside A3=center R=rectangle]: ")))
  (if (= p1 "R")
    (progn
      (setq p1 (getpoint "\nFirst corner: "))
      (if p1 (setq p2 (getcorner p1 "\nOpposite corner: ")))
      (if (and p1 p2) (ww:rect p1 p2 offL offR))
      (setq p1 nil)))
  (setq startPt p1
        prev-la nil prev-lb nil
        first-la nil first-lb nil
        firstL1 nil firstR1 nil lastL2 nil lastR2 nil
        closing nil start-t-close nil t-close nil)

  (while
    (and p1
         (progn
           (if first-la (initget "Close"))
           (setq p2 (getpoint p1 (if first-la
                                   "\nNext point [Close]: "
                                   "\nNext point: ")))
           (cond
             ((= p2 "Close")
              (setq p2 startPt closing T))
             ((and p2 first-la
                   (< (distance p2 startPt) (* *WW_Thickness* 0.5)))
              (setq p2 startPt closing T)))
           p2))

    ;; Convert click line + align → TRUE centerline (cx1 cx2), then
    ;; face lines as ±thk/2 perp from centerline.
    (setq ang  (angle p1 p2)
          off  (/ (- offL offR) 2.0)
          cx1  (polar p1 (+ ang (/ pi 2)) off)
          cx2  (polar p2 (+ ang (/ pi 2)) off)
          faL1 (polar cx1 (+ ang (/ pi 2)) (/ *WW_Thickness* 2.0))
          faL2 (polar cx2 (+ ang (/ pi 2)) (/ *WW_Thickness* 2.0))
          faR1 (polar cx1 (- ang (/ pi 2)) (/ *WW_Thickness* 2.0))
          faR2 (polar cx2 (- ang (/ pi 2)) (/ *WW_Thickness* 2.0)))

    ;; Junction detection at endpoints. Click point (p1/p2) is used
    ;; because it's within wall material regardless of new wall's align.
    ;; L = click at existing wall's ENDPOINT (l-hits list — every wall
    ;; that shares that endpoint). T = click INTERIOR of an existing
    ;; wall's centerline. Mutually exclusive.
    (setq start-l (if (not first-la) (aw:l-hits p1 nil))
          start-t (if (and (not first-la)(not start-l)) (aw:t-hit p1 nil))
          end-l   (if (not closing)  (aw:l-hits p2 nil))
          end-t   (if (and (not closing)(not end-l)) (aw:t-hit p2 nil))
          emit-faL-s T emit-faR-s T emit-faL-e T emit-faR-e T)

    ;; T-trim: cx & face endpoints move to E's near face.
    (if start-t
      (progn
        (setq tr (aw:trim-endpoint cx2 cx1 ang *WW_Thickness* start-t))
        (if tr (setq cx1 (nth 0 tr) faL1 (nth 1 tr) faR1 (nth 2 tr)))))
    (if end-t
      (progn
        (setq tr (aw:trim-endpoint cx1 cx2 ang *WW_Thickness* end-t))
        (if tr (setq cx2 (nth 0 tr) faL2 (nth 1 tr) faR2 (nth 2 tr)))))

    ;; L corner-pair: for each of N's faces, MERGE (colinear) or MITER
    ;; (angled) against best existing face ray from the shared corner.
    ;; Existing wall face LINEs get extended in place.
    (if start-l
      (progn
        (setq tr (aw:corner-pair cx2 cx1 ang *WW_Thickness* start-l))
        (if tr (setq faL1 (nth 0 tr) faR1 (nth 1 tr)
                     emit-faL-s (nth 2 tr) emit-faR-s (nth 3 tr)))))
    (if end-l
      (progn
        (setq tr (aw:corner-pair cx1 cx2 ang *WW_Thickness* end-l))
        (if tr (setq faL2 (nth 0 tr) faR2 (nth 1 tr)
                     emit-faL-e (nth 2 tr) emit-faR-e (nth 3 tr)))))

    (setq emit-faL (and emit-faL-s emit-faL-e)
          emit-faR (and emit-faR-s emit-faR-e))

    ;; In-run corner miter vs previous segment (only if this segment's
    ;; start is still free — no start-T/L anchor).
    (if (and prev-la (not start-t) (not start-l))
      (progn
        (setq ipL (inters prev-faL1 prev-faL2 faL1 faL2 nil)
              ipR (inters prev-faR1 prev-faR2 faR1 faR2 nil))
        (if ipL (progn (ww:set-end prev-la ipL) (setq faL1 ipL)))
        (if ipR (progn (ww:set-end prev-lb ipR) (setq faR1 ipR)))))

    ;; Mid-segment X-crossings vs existing walls.
    (setq x-hits (aw:x-hits cx1 cx2 nil))

    ;; Emit N's face LINEs (skip a side that MERGED with existing wall
    ;; — existing wall's face LINE was already extended to cover).
    (setq la (if emit-faL
               (entmakex (list '(0 . "LINE")(cons 8 "A-WALL")
                               (cons 10 faL1)(cons 11 faL2))))
          lb (if emit-faR
               (entmakex (list '(0 . "LINE")(cons 8 "A-WALL")
                               (cons 10 faR1)(cons 11 faR2)))))

    ;; Apply X cuts (may nullify la/lb; also cuts existing walls' faces).
    (foreach xh x-hits
      (setq tr (aw:cut-x xh la lb cx1 cx2 *WW_Thickness*)
            la (nth 0 tr) lb (nth 1 tr)))

    ;; Cut gap in E's near face for each T-junction.
    (if end-t   (aw:cut-t-gap end-t   faL2 faR2 cx1))
    (if start-t (aw:cut-t-gap start-t faL1 faR1 cx2))

    ;; Drop AWALL POINT with TRUE centerline (post-trim).
    (ww:tag-awall cx1 cx2 *WW_Thickness* *WW_Height* *WW_BaseElev*)

    ;; Track first + last face-line endpoints for caps.
    (if (not first-la)
      (setq firstL1 faL1 firstR1 faR1 first-la la first-lb lb
            start-t-close (if (or start-t start-l) T nil)))
    (setq lastL2 faL2 lastR2 faR2 t-close (if (or end-t end-l) T nil))

    ;; Advance prev (only if faces survived — X cuts nullify la/lb).
    (if (or x-hits (null la) (null lb))
      (setq prev-la nil prev-lb nil)
      (setq prev-la la prev-lb lb
            prev-faL1 faL1 prev-faL2 faL2
            prev-faR1 faR1 prev-faR2 faR2))

    (setq p1 (if (or closing end-t end-l) nil p2)))

  ;; Close-loop: miter last vs first segment's face lines.
  (if (and closing first-la first-lb la lb (/= first-la la))
    (progn
      (setq d1 (entget first-la)
            fla-p1 (cdr (assoc 10 d1))
            fla-p2 (cdr (assoc 11 d1))
            d2 (entget first-lb)
            flb-p1 (cdr (assoc 10 d2))
            flb-p2 (cdr (assoc 11 d2))
            ipL (inters faL1 faL2 fla-p1 fla-p2 nil)
            ipR (inters faR1 faR2 flb-p1 flb-p2 nil))
      (if ipL (progn (ww:set-end la ipL) (ww:set-start first-la ipL)))
      (if ipR (progn (ww:set-end lb ipR) (ww:set-start first-lb ipR))))

    ;; Open run — caps only at free ends.
    (progn
      (if (and (not start-t-close) firstL1 firstR1)
        (entmake (list '(0 . "LINE")(cons 8 "A-WALL")
                       (cons 10 firstL1)(cons 11 firstR1))))
      (if (and (not t-close) lastL2 lastR2)
        (entmake (list '(0 . "LINE")(cons 8 "A-WALL")
                       (cons 10 lastL2)(cons 11 lastR2))))))

  (princ)
)

; ============================================================
; ww:rect — Rectangle mode. CW walk (BL→TL→TR→BR): left of
; travel = outside rect, right of travel = inside rect. This
; matches the linear WW mapping (align=1 inside puts thickness
; on the right side of a CW-drawn perimeter).
; ============================================================
(defun ww:rect (p1 p2 offL offR / xmin xmax ymin ymax
                                    oBL oTL oTR oBR iBL iTL iTR iBR
                                    cBL cTL cTR cBR ang off cx1 cx2)
  (setq xmin (min (car p1)(car p2))
        xmax (max (car p1)(car p2))
        ymin (min (cadr p1)(cadr p2))
        ymax (max (cadr p1)(cadr p2))
        oBL (list (- xmin offL) (- ymin offL) 0.0)
        oTL (list (- xmin offL) (+ ymax offL) 0.0)
        oTR (list (+ xmax offL) (+ ymax offL) 0.0)
        oBR (list (+ xmax offL) (- ymin offL) 0.0)
        iBL (list (+ xmin offR) (+ ymin offR) 0.0)
        iTL (list (+ xmin offR) (- ymax offR) 0.0)
        iTR (list (- xmax offR) (- ymax offR) 0.0)
        iBR (list (- xmax offR) (+ ymin offR) 0.0)
        cBL (list xmin ymin 0.0) cTL (list xmin ymax 0.0)
        cTR (list xmax ymax 0.0) cBR (list xmax ymin 0.0))
  (ww:ensure-lyr)
  (foreach seg (list (list oBL oTL) (list oTL oTR) (list oTR oBR) (list oBR oBL)
                     (list iBL iTL) (list iTL iTR) (list iTR iBR) (list iBR iBL))
    (entmake (list '(0 . "LINE") '(8 . "A-WALL")
                   (cons 10 (car seg)) (cons 11 (cadr seg)))))
  ;; Convert click-rectangle edges to TRUE centerlines and tag.
  (foreach seg (list (list cBL cTL)(list cTL cTR)(list cTR cBR)(list cBR cBL))
    (setq ang (angle (car seg)(cadr seg))
          off (/ (- offL offR) 2.0)
          cx1 (polar (car seg) (+ ang (/ pi 2)) off)
          cx2 (polar (cadr seg)(+ ang (/ pi 2)) off))
    (ww:tag-awall cx1 cx2 *WW_Thickness* *WW_Height* *WW_BaseElev*))
  (princ "\nRectangle wall drawn."))

; ============================================================
; c:QW — convert existing LINE(s) to WW walls (with AWALL data)
; Pre-select lines, or picks interactively. Original lines deleted.
; Alignment uses global *WW_Align* (set via WW's A1/A2/A3).
; ============================================================
;; Explode selection into (src p1 p2) segments. Handles LINE and LWPOLYLINE.
(defun qw:collect-segs (ss / segs i n e d type coords cn ci closed)
  (setq segs nil n (sslength ss) i 0)
  (repeat n
    (setq e (ssname ss i) d (entget e) type (cdr (assoc 0 d)))
    (cond
      ((= type "LINE")
       (setq segs (append segs
         (list (list e
                     (list (car (cdr (assoc 10 d))) (cadr (cdr (assoc 10 d))) 0.0)
                     (list (car (cdr (assoc 11 d))) (cadr (cdr (assoc 11 d))) 0.0))))))
      ((= type "LWPOLYLINE")
       (setq coords nil)
       (foreach x d
         (if (= (car x) 10)
           (setq coords (cons (list (car (cdr x)) (cadr (cdr x)) 0.0) coords))))
       (setq coords (reverse coords)
             cn (length coords)
             closed (= 1 (logand (cdr (assoc 70 d)) 1))
             ci 0)
       (while (< ci (1- cn))
         (setq segs (append segs (list (list e (nth ci coords) (nth (1+ ci) coords)))))
         (setq ci (1+ ci)))
       (if (and closed (> cn 2))
         (setq segs (append segs (list (list e (nth (1- cn) coords) (nth 0 coords))))))))
    (setq i (1+ i)))
  segs)

;; Record: (idx src p1 p2 a1 a2 b1 b2 cap1 cap2)
(defun qw:mk-rec (idx e p1 p2 offL offR / ang)
  (setq ang (angle p1 p2))
  (list idx e p1 p2
        (polar p1 (+ ang (/ pi 2.0)) offL)
        (polar p2 (+ ang (/ pi 2.0)) offL)
        (polar p1 (- ang (/ pi 2.0)) offR)
        (polar p2 (- ang (/ pi 2.0)) offR)
        T T))

;; Count how many OTHER segments have an endpoint within tol of pt.
(defun qw:count-nbrs (state idx pt tol / cnt)
  (setq cnt 0)
  (foreach r state
    (if (/= (car r) idx)
      (if (or (< (distance pt (nth 2 r)) tol)
              (< (distance pt (nth 3 r)) tol))
        (setq cnt (1+ cnt)))))
  cnt)

(defun qw:process (ss / segs state offs offL offR tol n i j
                        ri rj pti1 pti2 ptj1 ptj2 wi wj
                        ai1 ai2 bi1 bi2 aj1 aj2 bj1 bj2 ipA ipB
                        ri-new rj-new p1 p2 a1 a2 b1 b2 cap1 cap2
                        sources src rec r
                        mnx mny mxx mxy margin p)
  (setq offs (ww:offsets *WW_Align* *WW_Thickness*)
        offL (car offs) offR (cadr offs)
        tol (max 1.0 (* *WW_Thickness* 0.5))
        segs (qw:collect-segs ss)
        state nil i 0)
  (foreach s segs
    (if (> (distance (cadr s) (caddr s)) 1e-6)
      (progn
        (setq state (append state (list (qw:mk-rec i (car s) (cadr s) (caddr s) offL offR))))
        (setq i (1+ i)))))
  (setq n (length state) i 0)
  (repeat n
    (setq j (1+ i))
    (repeat (- n (1+ i))
      (setq ri (nth i state) rj (nth j state)
            pti1 (nth 2 ri) pti2 (nth 3 ri)
            ptj1 (nth 2 rj) ptj2 (nth 3 rj)
            wi nil wj nil)
      (cond ((< (distance pti1 ptj1) tol) (setq wi 'p1 wj 'p1))
            ((< (distance pti1 ptj2) tol) (setq wi 'p1 wj 'p2))
            ((< (distance pti2 ptj1) tol) (setq wi 'p2 wj 'p1))
            ((< (distance pti2 ptj2) tol) (setq wi 'p2 wj 'p2)))
      ;; Only intersect 2-way corners (each endpoint has exactly 1 neighbor).
      (if (and wi
               (= 1 (qw:count-nbrs state (car ri) (if (= wi 'p1) pti1 pti2) tol))
               (= 1 (qw:count-nbrs state (car rj) (if (= wj 'p1) ptj1 ptj2) tol)))
        (progn
          (setq ai1 (nth 4 ri) ai2 (nth 5 ri) bi1 (nth 6 ri) bi2 (nth 7 ri)
                aj1 (nth 4 rj) aj2 (nth 5 rj) bj1 (nth 6 rj) bj2 (nth 7 rj)
                ipA (inters ai1 ai2 aj1 aj2 nil)
                ipB (inters bi1 bi2 bj1 bj2 nil))
          (if (and ipA ipB)
            (progn
              (setq ri-new (list (nth 0 ri) (nth 1 ri) pti1 pti2
                                 (if (= wi 'p1) ipA ai1)
                                 (if (= wi 'p2) ipA ai2)
                                 (if (= wi 'p1) ipB bi1)
                                 (if (= wi 'p2) ipB bi2)
                                 (if (= wi 'p1) nil (nth 8 ri))
                                 (if (= wi 'p2) nil (nth 9 ri)))
                    rj-new (list (nth 0 rj) (nth 1 rj) ptj1 ptj2
                                 (if (= wj 'p1) ipA aj1)
                                 (if (= wj 'p2) ipA aj2)
                                 (if (= wj 'p1) ipB bj1)
                                 (if (= wj 'p2) ipB bj2)
                                 (if (= wj 'p1) nil (nth 8 rj))
                                 (if (= wj 'p2) nil (nth 9 rj))))
              (setq state (mapcar '(lambda (r)
                                     (cond ((= (car r) (car ri-new)) ri-new)
                                           ((= (car r) (car rj-new)) rj-new)
                                           (T r))) state)))))
        ;; T-junction (3+ walls meet): skip caps at shared ends, leave faces alone
        (if wi
          (progn
            (setq ri-new (list (nth 0 ri) (nth 1 ri) pti1 pti2
                               (nth 4 ri)(nth 5 ri)(nth 6 ri)(nth 7 ri)
                               (if (= wi 'p1) nil (nth 8 ri))
                               (if (= wi 'p2) nil (nth 9 ri)))
                  rj-new (list (nth 0 rj) (nth 1 rj) ptj1 ptj2
                               (nth 4 rj)(nth 5 rj)(nth 6 rj)(nth 7 rj)
                               (if (= wj 'p1) nil (nth 8 rj))
                               (if (= wj 'p2) nil (nth 9 rj))))
            (setq state (mapcar '(lambda (r)
                                   (cond ((= (car r) (car ri-new)) ri-new)
                                         ((= (car r) (car rj-new)) rj-new)
                                         (T r))) state)))))
      (setq j (1+ j)))
    (setq i (1+ i)))
  ;; Emit face lines, caps, and AWALL markers
  (foreach r state
    (setq p1 (nth 2 r) p2 (nth 3 r)
          a1 (nth 4 r) a2 (nth 5 r) b1 (nth 6 r) b2 (nth 7 r)
          cap1 (nth 8 r) cap2 (nth 9 r))
    (entmake (list '(0 . "LINE") '(8 . "A-WALL") (cons 10 a1) (cons 11 a2)))
    (entmake (list '(0 . "LINE") '(8 . "A-WALL") (cons 10 b1) (cons 11 b2)))
    (if cap1 (entmake (list '(0 . "LINE") '(8 . "A-WALL") (cons 10 a1) (cons 11 b1))))
    (if cap2 (entmake (list '(0 . "LINE") '(8 . "A-WALL") (cons 10 a2) (cons 11 b2))))
    ;; Store TRUE centerline (click line offset by (offL-offR)/2 perp).
    (setq mnx (angle p1 p2)
          mxx (/ (- offL offR) 2.0))
    (ww:tag-awall (polar p1 (+ mnx (/ pi 2)) mxx)
                  (polar p2 (+ mnx (/ pi 2)) mxx)
                  *WW_Thickness* *WW_Height* *WW_BaseElev*))
  ;; Delete unique source entities
  (setq sources nil)
  (foreach r state
    (if (not (member (nth 1 r) sources))
      (setq sources (cons (nth 1 r) sources))))
  (foreach src sources (if (entget src) (entdel src)))
  (princ (strcat "\n" (itoa n) " segment(s) converted.")))

(defun qw:choose-align (/ k)
  (initget "Inside Outside Center")
  (setq k (getkword
    (strcat "\nAlignment [Inside/Outside/Center] <"
            (nth (1- *WW_Align*) '("Inside" "Outside" "Center")) ">: ")))
  (cond ((= k "Inside")  (setq *WW_Align* 1))
        ((= k "Outside") (setq *WW_Align* 2))
        ((= k "Center")  (setq *WW_Align* 3)))
  (princ (strcat "\nAlign=" (itoa *WW_Align*) " ("
                 (nth (1- *WW_Align*) '("inside" "outside" "center")) ")")))

(defun qw:set-thk (/ v)
  (setq v (getreal (strcat "\nThickness <" (rtos *WW_Thickness* 2 1) ">: ")))
  (if v (setq *WW_Thickness* v))
  (princ (strcat "\nThickness=" (rtos *WW_Thickness* 2 1))))

(defun c:QW (/ ss done k filt)
  (or *WW_Thickness* (setq *WW_Thickness* 150.0))
  (or *WW_Align*     (setq *WW_Align*     1))
  (or *WW_Height*    (setq *WW_Height*    2700.0))
  (or *WW_BaseElev*  (setq *WW_BaseElev*  0.0))
  (ww:ensure-lyr)
  (setq filt '((-4 . "<OR") (0 . "LINE") (0 . "LWPOLYLINE") (-4 . "OR>")))
  (princ (strcat "\nQW  |  Thickness=" (rtos *WW_Thickness* 2 1)
                 "  Align=" (itoa *WW_Align*)
                 " (" (nth (1- *WW_Align*) '("inside" "outside" "center")) ")"))
  ;; Try implied selection first (pickfirst).
  (setq ss (ssget "_I" filt))
  (if ss
    (qw:process ss)
    (progn
      (setq done nil)
      (while (not done)
        (initget "Align Thickness")
        (setq k (getkword
                  "\nOptions [Align/Thickness] or Enter to select: "))
        (cond ((= k "Align")     (qw:choose-align))
              ((= k "Thickness") (qw:set-thk))
              (T (setq done T))))
      (princ "\nSelect lines/polylines: ")
      (if (setq ss (ssget filt)) (qw:process ss))))
  (princ))

(princ "\nAKDWall loaded. Commands: WW  QW  TW  FW  FIXWALLS")
(princ)
