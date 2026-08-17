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
                     mnx mny mxx mxy done j os *error* old-error)

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
        )
        (princ "\nNo lines found.")
      )
    )
  )

  (setvar "OSMODE" os)
  (setq *error* old-error)
  (princ)
)

(princ "\nFIXWALLS loaded. Type FIXWALLS")
(princ)
; ------------------------------------------------------------
; Aliases
; ------------------------------------------------------------
(defun c:FW () (c:FIXWALLS))

(princ "
AKDWallTool loaded. Commands: TW  FW  FIXWALLS")
(princ)

