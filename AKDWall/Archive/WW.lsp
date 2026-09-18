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

(defun c:WW (/ p1 p2 offs offL offR lines
                a1 a2 b1 b2 la lb
                prev-la prev-lb prevA1 prevA2 prevB1 prevB2
                first-la first-lb firstA firstB lastA lastB
                ipA ipB startPt closing d1 d2 fla-p1 fla-p2 flb-p1 flb-p2
                allPts pt margin mnx mny mxx mxy
                segs t-close hit-seg sla slb sp1 sp2
                sla-p1 sla-p2 slb-p1 slb-p2)

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
        firstA nil firstB nil lastA nil lastB nil
        allPts nil closing nil
        segs nil t-close nil hit-seg nil)

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
    (setq la (entmakex (list '(0 . "LINE") '(8 . "A-WALL") (cons 10 a1) (cons 11 a2)))
          lb (entmakex (list '(0 . "LINE") '(8 . "A-WALL") (cons 10 b1) (cons 11 b2))))

    ;; Drop an AWALL marker for ELEV/SECT.
    (ww:tag-awall p1 p2 *WW_Thickness* *WW_Height* *WW_BaseElev*)

    (if (not firstA)
      (setq firstA a1 firstB b1 first-la la first-lb lb))

    ;; Record this segment for mid-sequence T-detection.
    (setq segs (cons (list la lb p1 p2) segs))

    ;; Mid-sequence T-hit: if p2 lands on the centerline of any EARLIER
    ;; segment (not the immediate prev, which corner-cleanup already
    ;; handled), fillet current's END lines against that segment's
    ;; offset lines and stop the sequence — same effect as Close but
    ;; against any prior wall in this run.
    (if (and (not closing) (cddr segs))
      (foreach seg (cddr segs)
        (setq sp1 (nth 2 seg) sp2 (nth 3 seg))
        (if (and (not t-close)
                 (ww:pt-on-seg p2 sp1 sp2 (* *WW_Thickness* 0.5)))
          (progn
            (setq hit-seg seg t-close T
                  sla (nth 0 seg) slb (nth 1 seg)
                  d1  (entget sla)
                  sla-p1 (cdr (assoc 10 d1))
                  sla-p2 (cdr (assoc 11 d1))
                  d2  (entget slb)
                  slb-p1 (cdr (assoc 10 d2))
                  slb-p2 (cdr (assoc 11 d2))
                  ipA (inters a1 a2 sla-p1 sla-p2 nil)
                  ipB (inters b1 b2 slb-p1 slb-p2 nil))
            (if ipA (ww:set-end la ipA))
            (if ipB (ww:set-end lb ipB))))))

    (setq lastA a2 lastB b2
          prev-la la  prev-lb lb
          prevA1 a1 prevA2 a2
          prevB1 b1 prevB2 b2
          allPts (cons a1 (cons a2 (cons b1 (cons b2 allPts))))
          p1 (if (or closing t-close) nil p2)))

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

    ;; Open polyline — draw caps at both ends, but skip the end cap if
    ;; the last segment T-junctioned into an earlier wall (its end is
    ;; already trimmed to that wall's face).
    (progn
      (if (and firstA firstB)
        (entmake (list '(0 . "LINE") '(8 . "A-WALL") (cons 10 firstA) (cons 11 firstB))))
      (if (and (not t-close) lastA lastB)
        (entmake (list '(0 . "LINE") '(8 . "A-WALL") (cons 10 lastA) (cons 11 lastB)))))
  )

  ;; Auto junction cleanup on the drawn walls' bounding box.
  ;; Uses tw:cleanup-box from AKDWallTool.lsp if it's loaded.
  (if (and allPts (member "TW:CLEANUP-BOX" (atoms-family 1)))
    (progn
      (setq mnx 1e99 mny 1e99 mxx -1e99 mxy -1e99)
      (foreach pt allPts
        (if (< (car pt)  mnx) (setq mnx (car pt)))
        (if (< (cadr pt) mny) (setq mny (cadr pt)))
        (if (> (car pt)  mxx) (setq mxx (car pt)))
        (if (> (cadr pt) mxy) (setq mxy (cadr pt))))
      (setq margin 1000.0)   ; buffer around this session's bbox, catches adjacent walls
      (tw:cleanup-box (- mnx margin) (- mny margin)
                      (+ mxx margin) (+ mxy margin))
      (princ "\nJunctions cleaned."))
    (if allPts
      (princ "\n(Load AKDWallTool.lsp for automatic junction cleanup.)"))
  )

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
                                    cBL cTL cTR cBR margin)
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
  (foreach seg (list (list cBL cTL) (list cTL cTR) (list cTR cBR) (list cBR cBL))
    (ww:tag-awall (car seg) (cadr seg)
                  *WW_Thickness* *WW_Height* *WW_BaseElev*))
  (if (member "TW:CLEANUP-BOX" (atoms-family 1))
    (progn
      (setq margin 1000.0)
      (tw:cleanup-box (- xmin *WW_Thickness* margin)
                      (- ymin *WW_Thickness* margin)
                      (+ xmax *WW_Thickness* margin)
                      (+ ymax *WW_Thickness* margin))))
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
    (ww:tag-awall p1 p2 *WW_Thickness* *WW_Height* *WW_BaseElev*))
  ;; Delete unique source entities
  (setq sources nil)
  (foreach r state
    (if (not (member (nth 1 r) sources))
      (setq sources (cons (nth 1 r) sources))))
  (foreach src sources (if (entget src) (entdel src)))
  ;; T-junction cleanup for anything not caught
  (if (member "TW:CLEANUP-BOX" (atoms-family 1))
    (progn
      (setq mnx 1e99 mny 1e99 mxx -1e99 mxy -1e99)
      (foreach r state
        (foreach p (list (nth 2 r) (nth 3 r))
          (if (< (car p)  mnx) (setq mnx (car p)))
          (if (< (cadr p) mny) (setq mny (cadr p)))
          (if (> (car p)  mxx) (setq mxx (car p)))
          (if (> (cadr p) mxy) (setq mxy (cadr p)))))
      (setq margin (* *WW_Thickness* 2.0))
      (tw:cleanup-box (- mnx margin) (- mny margin)
                      (+ mxx margin) (+ mxy margin))))
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

(princ "\nWW loaded. Type WW / QW.")
(princ)
