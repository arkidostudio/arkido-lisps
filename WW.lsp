; ============================================================
; WW.lsp
; Command: WW
; Draws continuous double-line walls with corner filleting.
; Osnaps active throughout. Standard AutoCAD rubberband preview.
; ============================================================

;; ---- Settings (edit these) ----
(setq *WW_Thickness* 150.0)   ; wall thickness in drawing units
(setq *WW_Align*     1)       ; 1 = inside (CCW click), 2 = outside, 3 = center

; ============================================================
; Helpers
; ============================================================

;; Perpendicular offsets from the click line. Returns (offL offR).
;; Left = angle + 90°, Right = angle - 90°.
;; align: 1 = inside (thickness on right of click, CCW room), 2 = outside (left), 3 = center
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

; ============================================================
; c:WW
; ============================================================

(defun c:WW (/ p1 p2 offs offL offR lines
                a1 a2 b1 b2 la lb
                prev-la prev-lb prevA1 prevA2 prevB1 prevB2
                first-la first-lb firstA firstB lastA lastB
                ipA ipB startPt closing d1 d2 fla-p1 fla-p2 flb-p1 flb-p2
                allPts pt margin mnx mny mxx mxy)

  (or *WW_Thickness* (setq *WW_Thickness* 150.0))
  (or *WW_Align*     (setq *WW_Align*     2))

  (setq offs (ww:offsets *WW_Align* *WW_Thickness*)
        offL (car offs)
        offR (cadr offs))

  (princ (strcat "\nWW  |  Thickness=" (rtos *WW_Thickness* 2 1)
                 "  Align=" (itoa *WW_Align*)
                 " (" (nth (1- *WW_Align*) '("inside" "outside" "center")) ")"))

  (initget "A1 A2 A3")
  (setq p1 (getpoint "\nStart point [A1=inside A2=outside A3=center]: "))
  (while (member p1 '("A1" "A2" "A3"))
    (setq *WW_Align* (cond ((= p1 "A1") 1) ((= p1 "A2") 2) (T 3))
          offs (ww:offsets *WW_Align* *WW_Thickness*)
          offL (car offs)
          offR (cadr offs))
    (princ (strcat "\nAlign=" (itoa *WW_Align*)
                   " (" (nth (1- *WW_Align*) '("inside" "outside" "center")) ")"))
    (initget "A1 A2 A3")
    (setq p1 (getpoint "\nStart point [A1=inside A2=outside A3=center]: ")))
  (setq startPt p1
        prev-la nil prev-lb nil
        first-la nil first-lb nil
        firstA nil firstB nil lastA nil lastB nil
        allPts nil closing nil)

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
                   (or (< (distance p2 startPt) (* *WW_Thickness* 0.5))
                       (< (distance p2 firstA)  (* *WW_Thickness* 0.5))
                       (< (distance p2 firstB)  (* *WW_Thickness* 0.5))))
              (setq p2 startPt closing T)))
           p2))

    (setq lines (ww:segment-lines p1 p2 offL offR)
          a1 (car  (car lines))
          a2 (cadr (car lines))
          b1 (car  (cadr lines))
          b2 (cadr (cadr lines)))

    ;; Corner cleanup vs previous segment
    (if prev-la
      (progn
        (setq ipA (inters prevA1 prevA2 a1 a2 nil)
              ipB (inters prevB1 prevB2 b1 b2 nil))

        (if ipA
          (progn
            (ww:set-end prev-la ipA)   ; trim/extend prev A-line end
            (setq a1 ipA)))            ; new A-line starts at corner

        (if ipB
          (progn
            (ww:set-end prev-lb ipB)
            (setq b1 ipB)))
      )
    )

    ;; Commit the two parallel lines for this segment
    (setq la (entmakex (list '(0 . "LINE") (cons 10 a1) (cons 11 a2)))
          lb (entmakex (list '(0 . "LINE") (cons 10 b1) (cons 11 b2))))

    (if (not firstA)
      (setq firstA a1 firstB b1 first-la la first-lb lb))

    (setq lastA a2 lastB b2
          prev-la la  prev-lb lb
          prevA1 a1 prevA2 a2
          prevB1 b1 prevB2 b2
          allPts (cons a1 (cons a2 (cons b1 (cons b2 allPts))))
          p1 (if closing nil p2)))

  ;; If the user closed the loop, fillet the last segment against the
  ;; first segment (both parallel-line pairs) and skip the caps.
  (if (and closing first-la first-lb la lb (/= first-la la))
    (progn
      (setq d1 (entget first-la)
            fla-p1 (cdr (assoc 10 d1))
            fla-p2 (cdr (assoc 11 d1))
            d2 (entget first-lb)
            flb-p1 (cdr (assoc 10 d2))
            flb-p2 (cdr (assoc 11 d2)))
      (setq ipA (inters a1 a2 fla-p1 fla-p2 nil)
            ipB (inters b1 b2 flb-p1 flb-p2 nil))
      (if ipA
        (progn (ww:set-end la ipA) (ww:set-start first-la ipA)))
      (if ipB
        (progn (ww:set-end lb ipB) (ww:set-start first-lb ipB))))

    ;; Open polyline — draw caps at both ends
    (progn
      (if (and firstA firstB)
        (entmake (list '(0 . "LINE") (cons 10 firstA) (cons 11 firstB))))
      (if (and lastA lastB)
        (entmake (list '(0 . "LINE") (cons 10 lastA) (cons 11 lastB)))))
  )

  ;; Auto junction cleanup on the drawn walls' bounding box.
  ;; Uses tw:cleanup-box from AKDWallTool.lsp if it's loaded.
  (if (and allPts (= (type tw:cleanup-box) 'USUBR))
    (progn
      (setq mnx 1e99 mny 1e99 mxx -1e99 mxy -1e99)
      (foreach pt allPts
        (if (< (car pt)  mnx) (setq mnx (car pt)))
        (if (< (cadr pt) mny) (setq mny (cadr pt)))
        (if (> (car pt)  mxx) (setq mxx (car pt)))
        (if (> (cadr pt) mxy) (setq mxy (cadr pt))))
      (setq margin (* *WW_Thickness* 0.6))
      (tw:cleanup-box (- mnx margin) (- mny margin)
                      (+ mxx margin) (+ mxy margin))
      (princ "\nJunctions cleaned."))
    (if allPts
      (princ "\n(Load AKDWallTool.lsp for automatic junction cleanup.)"))
  )

  (princ)
)

(princ "\nWW loaded. Type WW.")
(princ)
