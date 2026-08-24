;;; SmartBoundary.lsp
;;; Command: SB
;;; Like BOUNDARY (BO), but temporarily bridges small gaps
;;; (e.g. door openings) so a closed polyline can be traced.
;;;
;;; Usage:  SB   -> pick internal point(s), Enter to finish
;;;         Use "Gap" option to change the max bridging distance.
;;;
;;; Method:
;;;   1. Collect endpoints of all LINE/ARC/POLYLINE curves in the
;;;      visible area (or a window around the pick point).
;;;   2. Find pairs of endpoints on DIFFERENT curves whose distance
;;;      is <= gap tolerance and that are otherwise "free"
;;;      (no other endpoint already touching them within fuzz).
;;;   3. Draw those bridges as temp LINEs on layer SB_TEMP.
;;;   4. Run -BOUNDARY at the pick point.
;;;   5. Delete the temp lines and layer, keep the polyline.

;; Pure AutoLISP - no vl-load-com / vlax-* (Mac-safe)

(setq *sb-gap*   3000.0)  ; default max gap width (drawing units, e.g. mm)
(setq *sb-fuzz*  0.5)     ; endpoints closer than this are "already connected"
(setq *sb-align* 0.85)    ; min |cos(angle)| between bridge dir and wall tangent
(setq *sb-cap-max* 300.0) ; max length of a "cap" line (wall thickness)
(setq *sb-gap-min* 500.0) ; min opening width - shorter candidate bridges rejected
(setq *sb-out-layer* "X-AREA BOUNDARY") ; output layer for boundaries; nil = current layer
(setq *sb-wall-layers* nil) ; list of wall layers to analyze; nil = all layers
(setq *sb-debug* nil)       ; T = keep bridges, label them, skip freeze, dump coords

;; -- helpers ---------------------------------------------------------------

(defun sb:polar3 (p ang dist)
  (list (+ (car p) (* dist (cos ang)))
        (+ (cadr p) (* dist (sin ang)))
        (if (caddr p) (caddr p) 0.0)))

(defun sb:arc-endpoints (e / c r a1 a2)
  (setq c  (cdr (assoc 10 e))
        r  (cdr (assoc 40 e))
        a1 (cdr (assoc 50 e))
        a2 (cdr (assoc 51 e)))
  (list (sb:polar3 c a1 r) (sb:polar3 c a2 r)))

(defun sb:lwpoly-endpoints (e / closed pts flag)
  (setq closed (= 1 (logand 1 (cdr (assoc 70 e)))))
  (if closed
    nil
    (progn
      (setq pts (mapcar 'cdr (vl-remove-if-not
                               '(lambda (x) (= (car x) 10)) e)))
      (if (>= (length pts) 2)
        (list (append (car pts) '(0.0))
              (append (last pts) '(0.0)))
        nil))))

(defun sb:poly-endpoints (ename / v vlist typ next e70)
  ;; heavy POLYLINE: walk VERTEX seqend
  (setq e70 (cdr (assoc 70 (entget ename))))
  (if (= 1 (logand 1 e70))
    nil
    (progn
      (setq v (entnext ename) vlist nil)
      (while (and v (/= "SEQEND" (cdr (assoc 0 (entget v)))))
        (setq vlist (cons (cdr (assoc 10 (entget v))) vlist))
        (setq v (entnext v)))
      (setq vlist (reverse vlist))
      (if (>= (length vlist) 2)
        (list (car vlist) (last vlist))
        nil))))

(defun sb:unit (v / d)
  (setq d (distance '(0 0 0) v))
  (if (< d 1e-9) '(0.0 0.0 0.0)
    (list (/ (car v) d) (/ (cadr v) d) 0.0)))

(defun sb:sub (a b)
  (list (- (car a) (car b)) (- (cadr a) (cadr b)) 0.0))

;; Return list of (point . outward-tangent) pairs for the entity.
(defun sb:end-tangents (ename / e typ p1 p2 pts n)
  (setq e (entget ename) typ (cdr (assoc 0 e)))
  (cond
    ((= typ "LINE")
     (setq p1 (cdr (assoc 10 e)) p2 (cdr (assoc 11 e)))
     (list (cons p1 (sb:unit (sb:sub p1 p2)))
           (cons p2 (sb:unit (sb:sub p2 p1)))))
    ((= typ "ARC")
     ;; tangent perpendicular to radius; sign chosen outward-ish
     (setq pts (sb:arc-endpoints e))
     (list (cons (car  pts) (sb:unit (sb:sub (car  pts) (cadr pts))))
           (cons (cadr pts) (sb:unit (sb:sub (cadr pts) (car  pts))))))
    ((= typ "LWPOLYLINE")
     (if (= 1 (logand 1 (cdr (assoc 70 e)))) nil
       (progn
         (setq pts (mapcar '(lambda (x) (append (cdr x) '(0.0)))
                           (vl-remove-if-not
                             '(lambda (x) (= (car x) 10)) e))
               n   (length pts))
         (if (>= n 2)
           (list (cons (car pts)
                       (sb:unit (sb:sub (car pts) (cadr pts))))
                 (cons (nth (1- n) pts)
                       (sb:unit (sb:sub (nth (1- n) pts)
                                        (nth (- n 2) pts)))))))))
    ((= typ "POLYLINE")
     (if (= 1 (logand 1 (cdr (assoc 70 e)))) nil
       (progn
         (setq pts (sb:poly-vertices ename) n (length pts))
         (if (>= n 2)
           (list (cons (car pts)
                       (sb:unit (sb:sub (car pts) (cadr pts))))
                 (cons (nth (1- n) pts)
                       (sb:unit (sb:sub (nth (1- n) pts)
                                        (nth (- n 2) pts))))))))) ))

(defun sb:poly-vertices (ename / v out)
  (setq v (entnext ename) out nil)
  (while (and v (/= "SEQEND" (cdr (assoc 0 (entget v)))))
    (setq out (cons (cdr (assoc 10 (entget v))) out))
    (setq v (entnext v)))
  (reverse out))

;; Each entry: (pt tan . ename)
(defun sb:collect (ss / i n ename et out)
  (setq i 0 n (sslength ss) out nil)
  (while (< i n)
    (setq ename (ssname ss i))
    (foreach pt (sb:end-tangents ename)
      (if pt (setq out (cons (list (car pt) (cdr pt) ename) out))))
    (setq i (1+ i)))
  out)

(defun sb:free? (pt ename eps / hit)
  (setq hit nil)
  (foreach ep eps
    (if (and (not hit)
             (not (equal ename (caddr ep)))
             (< (distance pt (car ep)) *sb-fuzz*))
      (setq hit T)))
  (not hit))

(defun sb:dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))

;; Find "door notches" in closed LWPOLYLINEs: vertex pairs whose entering
;; segments are perpendicular to the direct line between them, and whose
;; direct distance is within gap.
(defun sb:closed-notches (ename gap / e verts n i j vi vj prev next
                          d dseg1 dseg2 out nm1 nm2)
  (setq e (entget ename) out nil)
  (if (and (= (cdr (assoc 0 e)) "LWPOLYLINE")
           (= 1 (logand 1 (cdr (assoc 70 e)))))
    (progn
      (setq verts (mapcar '(lambda (x) (append (cdr x) '(0.0)))
                          (vl-remove-if-not
                            '(lambda (x) (= (car x) 10)) e))
            n (length verts))
      (setq i 0)
      (while (< i n)
        (setq j (+ i 2))
        (while (< j n)
          ;; skip wrap-around adjacency
          (if (and (/= (- n (- j i)) 1)
                   (< (distance (nth i verts) (nth j verts)) gap)
                   (> (distance (nth i verts) (nth j verts)) 0.1))
            (progn
              (setq vi (nth i verts) vj (nth j verts)
                    prev (nth (rem (+ i n -1) n) verts)
                    next (nth (rem (1+ j) n) verts)
                    d     (sb:unit (sb:sub vj vi))
                    dseg1 (sb:unit (sb:sub vi prev))
                    dseg2 (sb:unit (sb:sub next vj)))
              ;; both entering segments should be perpendicular to bridge
              (if (and (< (abs (sb:dot d dseg1)) 0.3)
                       (< (abs (sb:dot d dseg2)) 0.3))
                (setq out (cons (list vi vj) out)))))
          (setq j (1+ j)))
        (setq i (1+ i)))))
  out)

;; Cap-based detection: short LINEs are wall-thickness caps.
;; A pair of parallel caps facing each other across a gap = an opening.
(defun sb:cap-bridges (ss cap-max gap / i n e p1 p2 mid tan caps c1 c2
                       out used mid-dir)
  (setq caps nil i 0 n (sslength ss))
  (while (< i n)
    (setq e (entget (ssname ss i)))
    (if (= (cdr (assoc 0 e)) "LINE")
      (progn
        (setq p1 (cdr (assoc 10 e)) p2 (cdr (assoc 11 e)))
        (if (and (< (distance p1 p2) cap-max)
                 (> (distance p1 p2) 1e-6))
          (setq caps
            (cons (list p1 p2
                        (list (/ (+ (car p1) (car p2)) 2.0)
                              (/ (+ (cadr p1) (cadr p2)) 2.0) 0.0)
                        (sb:unit (sb:sub p2 p1)))
                  caps)))))
    (setq i (1+ i)))
  (setq out nil used nil)
  (foreach c1 caps
    (if (not (member c1 used))
      (foreach c2 caps
        (if (and (not (equal c1 c2))
                 (not (member c1 used))
                 (not (member c2 used))
                 (> (abs (sb:dot (cadddr c1) (cadddr c2))) 0.9)
                 (<= (distance (caddr c1) (caddr c2)) gap)
                 (>= (distance (caddr c1) (caddr c2)) *sb-gap-min*))
          (progn
            (setq mid-dir (sb:unit (sb:sub (caddr c2) (caddr c1))))
            (if (< (abs (sb:dot mid-dir (cadddr c1))) 0.2)
              (progn
                (if (< (distance (car c1) (car c2))
                       (distance (car c1) (cadr c2)))
                  (setq out (cons (list (car  c1) (car  c2)) out)
                        out (cons (list (cadr c1) (cadr c2)) out))
                  (setq out (cons (list (car  c1) (cadr c2)) out)
                        out (cons (list (cadr c1) (car  c2)) out)))
                (setq used (cons c1 used) used (cons c2 used)))))))))
  out)

(defun sb:all-notches (ss gap / i n out)
  (setq i 0 n (sslength ss) out nil)
  (while (< i n)
    (setq out (append (sb:closed-notches (ssname ss i) gap) out))
    (setq i (1+ i)))
  out)

;; ep entry = (pt tangent ename)
(defun sb:bridges (eps gap / bridges used a b da ta ea db tb eb best bestd dir)
  (setq bridges nil used nil)
  (foreach a eps
    (setq da (car a) ta (cadr a) ea (caddr a))
    (if (and (sb:free? da ea eps)
             (not (member da used)))
      (progn
        (setq best nil bestd gap)
        (foreach b eps
          (setq db (car b) tb (cadr b) eb (caddr b))
          (if (and (not (equal ea eb))
                   (sb:free? db eb eps)
                   (not (member db used))
                   (<= (distance da db) bestd)
                   (> (distance da db) 1e-6))
            (progn
              (setq dir (sb:unit (sb:sub db da)))
              ;; bridge must extend a's wall AND arrive opposite to b's
              (if (and (>= (sb:dot ta dir) *sb-align*)
                       (<= (sb:dot tb dir) (- *sb-align*)))
                (setq best b bestd (distance da db))))))
        (if best
          (progn
            (setq bridges (cons (list da (car best)) bridges))
            (setq used (cons da used))
            (setq used (cons (car best) used)))))))
  bridges)

(defun sb:draw-bridges (bridges / out i a b mid len h)
  (setq out nil i 1)
  (foreach br bridges
    (setq a (car br) b (cadr br)
          mid (list (/ (+ (car a) (car b)) 2.0)
                    (/ (+ (cadr a) (cadr b)) 2.0) 0.0)
          len (distance a b))
    (setq out
      (cons
        (entmakex
          (list '(0 . "LINE") '(8 . "SB_TEMP")
                (cons 10 a) (cons 11 b)))
        out))
    (if *sb-debug*
      (progn
        (setq h (max 50.0 (* 0.05 len)))
        (setq out
          (cons
            (entmakex
              (list '(0 . "TEXT") '(8 . "SB_TEMP")
                    (cons 10 mid) (cons 40 h)
                    (cons 1 (itoa i))))
            out))
        (princ (strcat "\n  #" (itoa i)
                       "  len=" (rtos len 2 1)
                       "  a=(" (rtos (car a) 2 1) "," (rtos (cadr a) 2 1) ")"
                       "  b=(" (rtos (car b) 2 1) "," (rtos (cadr b) 2 1) ")"))
        (setq i (1+ i)))))
  out)

(defun sb:cleanup (temp-enames / )
  (if (and (not *sb-keep*) (not *sb-debug*))
    (foreach en temp-enames
      (if (and en (entget en)) (entdel en)))))

;; -- manual bridging -------------------------------------------------------

(defun sb:ensure-temp-layer ()
  (if (not (tblsearch "LAYER" "SB_TEMP"))
    (command "_.-LAYER" "_N" "SB_TEMP" "_C" "1" "SB_TEMP" "")))

(defun sb:ensure-out-layer (lay)
  (if (and lay (not (tblsearch "LAYER" lay)))
    (command "_.-LAYER" "_N" lay "_C" "11" lay "")))

(defun sb:move-to-layer (enames lay / en e new)
  (if (and lay enames)
    (progn
      (if (equal lay *sb-out-layer*)
        (sb:ensure-out-layer lay)
        (if (not (tblsearch "LAYER" lay))
          (command "_.-LAYER" "_N" lay "")))
      (foreach en enames
        (if (and en (setq e (entget en)))
          (progn
            (setq new (subst (cons 8 lay) (assoc 8 e) e))
            (entmod new)))))))

(defun sb:run-boundary-and-report (pt temp / before after new i en frozen)
  (setq before (ssget "_X" '((0 . "LWPOLYLINE"))))
  (setq frozen (sb:freeze-non-wall))
  (command "_.-BOUNDARY" pt "")
  (if frozen (sb:thaw frozen))
  (setq after (ssget "_X" '((0 . "LWPOLYLINE"))))
  (setq new nil)
  (if after
    (progn
      (setq i 0)
      (while (< i (sslength after))
        (setq en (ssname after i))
        (if (or (null before) (not (ssmemb en before)))
          (setq new (cons en new)))
        (setq i (1+ i)))))
  (sb:cleanup temp)
  (if *sb-out-layer* (sb:move-to-layer new *sb-out-layer*))
  (princ (strcat "\nCreated " (itoa (length new)) " boundary polyline(s)"
                 (if *sb-out-layer*
                   (strcat " on layer " *sb-out-layer*) "") "."))
  new)

;; -- freeze non-wall layers around a BOUNDARY call -------------------------

(defun sb:all-layer-names ( / rec out)
  (setq out nil rec (tblnext "LAYER" T))
  (while rec
    (setq out (cons (cdr (assoc 2 rec)) out))
    (setq rec (tblnext "LAYER")))
  out)

(setq *sb-saved-clayer* nil)

(defun sb:freeze-non-wall ( / cur safe keep frozen name lrec)
  (setq frozen nil)
  (if (not *sb-wall-layers*) nil
    (progn
      ;; switch current layer to something safe so we can freeze the original
      (setq *sb-saved-clayer* (getvar "CLAYER"))
      (setq safe (cond (*sb-out-layer*) ((car *sb-wall-layers*)) ("0")))
      (if (equal safe *sb-out-layer*)
        (sb:ensure-out-layer safe)
        (if (not (tblsearch "LAYER" safe))
          (command "_.-LAYER" "_N" safe "")))
      (setvar "CLAYER" safe)
      (setq cur   safe
            keep  (append (list cur "SB_TEMP") *sb-wall-layers*
                          (if *sb-out-layer* (list *sb-out-layer*) nil)))
      (foreach name (sb:all-layer-names)
        (if (and (not (member name keep))
                 (setq lrec (tblsearch "LAYER" name))
                 (= 0 (logand 1 (cdr (assoc 70 lrec)))))
          (progn
            (command "_.-LAYER" "_F" name "")
            (setq frozen (cons name frozen)))))
      (princ (strcat "\n  Kept visible: "
                     (apply 'strcat
                       (cdr (apply 'append
                         (mapcar '(lambda (x) (list "," x)) keep))))))
      (princ (strcat "\n  Froze " (itoa (length frozen)) " layer(s)."))))
  frozen)

(defun sb:thaw (names)
  (foreach n names
    (command "_.-LAYER" "_T" n ""))
  (if *sb-saved-clayer*
    (progn (setvar "CLAYER" *sb-saved-clayer*)
           (setq *sb-saved-clayer* nil))))

;; -- wall layer selection --------------------------------------------------

(defun sb:layer-filter ()
  (if *sb-wall-layers*
    (list (cons 0 "LINE,ARC,LWPOLYLINE,POLYLINE,CIRCLE")
          (cons 8
                (apply 'strcat
                  (cdr (apply 'append
                    (mapcar '(lambda (l) (list "," l)) *sb-wall-layers*))))))
    '((0 . "LINE,ARC,LWPOLYLINE,POLYLINE,CIRCLE"))))

(defun sb:set-walls ( / ans ss i l lays)
  (initget "Pick Type All")
  (setq ans (getkword
              (strcat "\nWall layers - [Pick objects/Type name/All] <"
                      (if *sb-wall-layers*
                        (apply 'strcat
                          (cdr (apply 'append
                            (mapcar '(lambda (l) (list "," l))
                                    *sb-wall-layers*))))
                        "all")
                      ">: ")))
  (cond
    ((null ans) nil)
    ((= ans "All")
     (setq *sb-wall-layers* nil)
     (princ "\nAll layers considered."))
    ((= ans "Type")
     (setq l (getstring T "\nWall layer name: "))
     (if (and l (/= l ""))
       (progn
         (setq *sb-wall-layers* (list l))
         (princ (strcat "\nWall layer set: " l ".")))))
    ((= ans "Pick")
     (princ "\nSelect one or more wall objects: ")
     (setq ss (ssget))
     (if ss
       (progn
         (setq i 0 lays nil)
         (while (< i (sslength ss))
           (setq l (cdr (assoc 8 (entget (ssname ss i)))))
           (if (not (member l lays)) (setq lays (cons l lays)))
           (setq i (1+ i)))
         (setq *sb-wall-layers* lays)
         (princ (strcat "\nWall layers set: "
                        (apply 'strcat
                          (cdr (apply 'append
                            (mapcar '(lambda (l) (list ", " l)) lays))))
                        ".")))))))

;; -- layer selection -------------------------------------------------------

(defun sb:set-layer ( / ans e l)
  (initget "Pick Current Type")
  (setq ans (getkword
              (strcat "\nBoundary layer - [Pick object/Type name/Current] <"
                      (if *sb-out-layer* *sb-out-layer* "current") ">: ")))
  (cond
    ((null ans) nil)
    ((= ans "Current")
     (setq *sb-out-layer* nil)
     (princ "\nBoundaries will use current layer."))
    ((= ans "Pick")
     (setq e (entsel "\nPick object on target layer: "))
     (if e
       (progn
         (setq l (cdr (assoc 8 (entget (car e)))))
         (setq *sb-out-layer* l)
         (princ (strcat "\nBoundaries will go to layer " l ".")))))
    ((= ans "Type")
     (setq l (getstring T "\nLayer name: "))
     (if (and l (/= l ""))
       (progn
         (setq *sb-out-layer* l)
         (princ (strcat "\nBoundaries will go to layer " l ".")))))))

;; -- main ------------------------------------------------------------------

(defun c:SB ( / pt ss eps bridges temp before after new opt stale frozen g l r i en)
  (princ (strcat "\nSmart Boundary - gap tol = "
                 (rtos *sb-gap* 2 2)))
  ;; wipe any leftover bridges from a crashed prior run
  (if (setq stale (ssget "_X" '((8 . "SB_TEMP"))))
    (command "_.ERASE" stale ""))
  ;; first run this session: prompt for wall layer
  (if (not *sb-wall-layers*)
    (progn
      (princ "\nWall layer not set - restricting analysis avoids catching doors/arcs.")
      (sb:set-walls)))
  (setq opt T)
  (while opt
    (initget "Gap Layer Walls")
    (setq pt (getpoint (strcat "\nPick internal point [Gap/Layer/Walls] <exit>: ")))
    (cond
      ((null pt) (setq opt nil))
      ((= pt "Gap")
       (setq g (getdist (strcat "\nMax gap width <"
                                (rtos *sb-gap* 2 2) ">: ")))
       (if g (setq *sb-gap* g)))
      ((= pt "Layer") (sb:set-layer))
      ((= pt "Walls") (sb:set-walls))
      (T
       (command "_.UNDO" "_BE")
       ;; select curves in a generous window around pt
       (setq r (* *sb-gap* 20.0)  ; search radius (~room-sized)
             ss (ssget "_C"
                       (list (- (car pt) r) (- (cadr pt) r))
                       (list (+ (car pt) r) (+ (cadr pt) r))
                       (sb:layer-filter)))
       (if (null ss)
         (princ "\nNo curves found near pick point.")
         (progn
           (setq eps     (sb:collect ss)
                 bridges (sb:bridges eps *sb-gap*)
                 bridges (append bridges (sb:all-notches ss *sb-gap*))
                 bridges (append bridges
                          (sb:cap-bridges ss *sb-cap-max* *sb-gap*)))
           (princ (strcat "\nCurves: " (itoa (sslength ss))
                          "  Endpoints: " (itoa (length eps))
                          "  Bridges: " (itoa (length bridges))))
           ;; ensure temp layer exists (do NOT set current)
           (if (not (tblsearch "LAYER" "SB_TEMP"))
             (command "_.-LAYER" "_N" "SB_TEMP" "_C" "1" "SB_TEMP" ""))
           (setq temp (sb:draw-bridges bridges))
           ;; snapshot polylines before
           (setq before (ssget "_X" '((0 . "LWPOLYLINE"))))
           (if *sb-debug*
             (princ "\n  [debug] skipping freeze + BOUNDARY. Inspect SB_TEMP.")
             (progn
               (setq frozen (sb:freeze-non-wall))
               (command "_.-BOUNDARY" pt "")
               (if frozen (sb:thaw frozen))))
           (setq after (ssget "_X" '((0 . "LWPOLYLINE"))))
           ;; find new pline(s)
           (setq new nil)
           (if after
             (progn
               (setq i 0)
               (while (< i (sslength after))
                 (setq en (ssname after i))
                 (if (or (null before)
                         (not (ssmemb en before)))
                   (setq new (cons en new)))
                 (setq i (1+ i)))))
           (sb:cleanup temp)
           (if *sb-out-layer* (sb:move-to-layer new *sb-out-layer*))
           (princ (strcat "\nCreated " (itoa (length new))
                          " boundary polyline(s)"
                          (if *sb-out-layer*
                            (strcat " on layer " *sb-out-layer*) "")
                          "."))))
       (command "_.UNDO" "_E"))))
  (princ))

(princ "\nSmartBoundary loaded. Type SB to run.")
(princ)
