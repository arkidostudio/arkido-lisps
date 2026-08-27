;; AKDDW.lsp  -  Arkido Doors & Windows
;; Plan-view door and window tool for AutoCAD (Mac & Windows).
;;
;; Commands:
;;   AW   Add window - click 2 points, D=set divisions.
;;   WC   Window count (tally by width).
;;   WR   Window renumber (select subset or Enter=all).
;;   AD   Add door   - click 2 points. Keywords:
;;          S = Single, D = Double, G = sliding (then panel count),
;;          P = set sliding panel count.
;;        Live ghost preview - move mouse to flip, click to place.
;;   DC   Door count.
;;   DR   Door renumber.
;;
;; ===================================================================
;;                       C O N F I G  ( edit here )
;; ===================================================================
;; layer name . AutoCAD color index (1=red 2=yellow 3=green 4=cyan
;; 5=blue 6=magenta 7=white/black 8=dark grey ... 256=BYLAYER, 0=BYBLOCK)

;; Window
(setq *cfg-win-frame*     '("A-WINDOW" . 2))  ; jambs / mullion frames (yellow)
(setq *cfg-win-glass*     '("A-WINDOW" . 1))  ; glass line (red)
(setq *cfg-win-wall*      '("A-WINDOW" . 1))  ; two offset wall lines (red)

;; Door
(setq *cfg-door-frame*    '("A-DOOR"   . 2))  ; jamb frames (yellow)
(setq *cfg-door-panel*    '("A-DOOR"   . 2))  ; leaf / panel (yellow)
(setq *cfg-door-arc*      '("A-DOOR"   . 1))  ; swing arc (red)
(setq *cfg-door-wall*     '("A-DOOR"   . 1))  ; wall lines on sliding (red)

;; Labels (all types)
(setq *cfg-lbl-shape*     '("X-TAGS & SYMBOLS" . 1))  ; hexagon / circle (red)
(setq *cfg-lbl-text*      '("X-TAGS & SYMBOLS" . 2))  ; label text (yellow)
(setq *cfg-dim-text*      '("Defpoints"        . 1))  ; middle dimension text (red on Defpoints)

;; Dimensions (in drawing units)
(setq *cfg-win-fw*        50.0)   ; window frame width along wall
(setq *cfg-win-fd*       100.0)   ; window frame depth across wall
(setq *cfg-win-wall-off*  75.0)   ; window wall-line offset each side

(setq *cfg-door-fw*       50.0)   ; door frame width along wall
(setq *cfg-door-fd*      100.0)   ; single/double door depth across wall
(setq *cfg-door-panel-t*  35.0)   ; door panel thickness
(setq *cfg-slide-fw*      50.0)   ; sliding-door frame width along wall
(setq *cfg-slide-p*       35.0)   ; sliding panel thickness (per track)
(setq *cfg-slide-ext*     25.0)   ; sliding-panel extension past meeting
(setq *cfg-slide-wall-off* 75.0)  ; sliding wall-line offset each side

;; Pocket door (dimensions in mm)
(setq *cfg-pd-opening*   700.0)   ; opening width (leaf swing area)
(setq *cfg-pd-jamb*       50.0)   ; jamb length along wall (both ends)
(setq *cfg-pd-jamb-h*     65.0)   ; leftmost jamb depth across wall
(setq *cfg-pd-grey-t*     75.0)   ; pocket wall (front) thickness across
(setq *cfg-pd-leaf-t*     40.0)   ; leaf thickness across
(setq *cfg-pd-stud-t*     25.0)   ; support stud thickness across
(setq *cfg-pd-gyp-t*      10.0)   ; gypsum board thickness across
(setq *cfg-pd-stud-w*     50.0)   ; support stud length along wall
(setq *cfg-pd-lipext*     50.0)   ; leaf lip past pocket wall
(setq *cfg-pd-groove*     18.0)   ; right jamb groove depth
(setq *cfg-pd-grey*      '("A-DOOR" . 8))  ; grey pocket wall (grey)

(setq *cfg-lbl-hex-r*    250.0)   ; window label hexagon radius (500 dia)
(setq *cfg-lbl-cir-r*    225.0)   ; door label circle radius (450 dia)
(setq *cfg-lbl-h*        150.0)   ; label text height
(setq *cfg-lbl-off-win*  500.0)   ; label offset from window midpoint
(setq *cfg-lbl-off-door* 450.0)   ; label offset from door   midpoint

;; ===================================================================
;;                       (nothing below here needs editing)
;; ===================================================================

;; session defaults
(if (null *win-div*)   (setq *win-div* 1))
(if (null *ac-mode*)   (setq *ac-mode* "M"))
(if (null *ac-spacing*) (setq *ac-spacing* 1200.0))
(if (null *ac-div*)    (setq *ac-div* 4))
(if (null *cfg-corner-post*) (setq *cfg-corner-post* 100.0))
(if (null *axw-ref*)   (setq *axw-ref* "C"))
(if (null *win-tag-type*) (setq *win-tag-type* "W"))
(if (null *win-slide*) (setq *win-slide* nil))
(if (null *cfg-slide-win-tick*) (setq *cfg-slide-win-tick* 100.0))
(if (null *door-type*) (setq *door-type* "S"))
(if (null *slide-div*) (setq *slide-div* 2))
(if (null *label-on*)    (setq *label-on* t))    ; master label on/off
(if (null *label-solo*)  (setq *label-solo* nil)); nil = Continuous, t = New batch
(if (null *label-batch*) (setq *label-batch* 0)) ; 0 = main sequence; >0 = new sequence #

(defun _lbl-status ()
  (strcat "Labels: " (if *label-on* "ON" "OFF") ", "
          (if *label-solo* "Start New" "Continuous")))

(defun c:LT ( / k)
  (initget "On oFf")
  (setq k (getkword
    (strcat "\n" (_lbl-status)
            "  |  Labels [On/oFf] <" (if *label-on* "On" "oFf") ">: ")))
  (cond ((eq k "On")  (setq *label-on* t))
        ((eq k "oFf") (setq *label-on* nil)))
  (princ (strcat "\n" (_lbl-status) ".")) (princ))

(defun c:LC ( / k)
  (initget "Continuous New")
  (setq k (getkword
    (strcat "\n" (_lbl-status)
            "  |  Numbering [Continuous/New] <"
            (if *label-solo* "New" "Continuous") ">: ")))
  (cond ((eq k "Continuous") (setq *label-solo* nil *label-batch* 0))
        ((eq k "New")
          (setq *label-solo* t
                *label-batch* (1+ *label-batch*))))
  (princ (strcat "\n" (_lbl-status) ".")) (princ))

;; --- math -----------------------------------------------------------
(defun _scale (u k) (mapcar '(lambda (x) (* x k)) u))
(defun _add   (a b) (mapcar '+ a b))
(defun _sub   (a b) (mapcar '- a b))

;; --- layer helper ---------------------------------------------------
(defun _ensure-layer (name / cmde)
  (if (not (tblsearch "LAYER" name))
    (progn
      (setq cmde (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command "_.LAYER" "_M" name "")
      (setvar "CMDECHO" cmde))))

;; --- primitive makers (layer . color pair) --------------------------
(defun _mkpline (pts cfg / lyr)
  (setq lyr (car cfg)) (_ensure-layer lyr)
  (entmakex
    (append
      (list '(0 . "LWPOLYLINE")
            '(100 . "AcDbEntity")
            (cons 8 lyr)
            '(100 . "AcDbPolyline")
            (cons 90 (length pts))
            '(70 . 1)
            (cons 62 (cdr cfg)))
      (mapcar '(lambda (p) (list 10 (car p) (cadr p))) pts))))

(defun _mkline (p1 p2 cfg / lyr)
  (setq lyr (car cfg)) (_ensure-layer lyr)
  (entmakex
    (list '(0 . "LINE")
          '(100 . "AcDbEntity")
          (cons 8 lyr)
          '(100 . "AcDbLine")
          (cons 62 (cdr cfg))
          (list 10 (car p1) (cadr p1) 0.0)
          (list 11 (car p2) (cadr p2) 0.0))))

(defun _mkarc (c r sa ea cfg / lyr)
  (setq lyr (car cfg)) (_ensure-layer lyr)
  (entmakex
    (list '(0 . "ARC")
          '(100 . "AcDbEntity")
          (cons 8 lyr)
          (cons 62 (cdr cfg))
          '(100 . "AcDbCircle")
          (list 10 (car c) (cadr c) 0.0)
          (cons 40 r)
          '(100 . "AcDbArc")
          (cons 50 sa)
          (cons 51 ea))))

(defun _mkcircle (c r cfg / lyr)
  (setq lyr (car cfg)) (_ensure-layer lyr)
  (entmakex
    (list '(0 . "CIRCLE")
          '(100 . "AcDbEntity")
          (cons 8 lyr)
          (cons 62 (cdr cfg))
          '(100 . "AcDbCircle")
          (list 10 (car c) (cadr c) 0.0)
          (cons 40 r))))

(defun _mkhex (c r cfg / i pts th)
  (setq i 0 pts nil)
  (while (< i 6)
    (setq th  (* (/ pi 3.0) i)
          pts (cons (list (+ (car c) (* r (cos th)))
                          (+ (cadr c) (* r (sin th))))
                    pts)
          i (1+ i)))
  (_mkpline pts cfg))

(defun _txt-ang (a)
  (if (and (> a (/ pi 2)) (< a (/ (* 3 pi) 2))) (- a pi) a))

(defun _mktext (pt h ang str cfg / lyr)
  (setq lyr (car cfg)) (_ensure-layer lyr)
  (entmakex
    (list '(0 . "TEXT")
          '(100 . "AcDbEntity")
          (cons 8 lyr)
          (cons 62 (cdr cfg))
          '(100 . "AcDbText")
          (list 10 (car pt) (cadr pt) 0.0)
          (cons 40 h)
          (cons 1 str)
          (cons 50 ang)
          (cons 72 1)
          (list 11 (car pt) (cadr pt) 0.0)
          '(100 . "AcDbText")
          (cons 73 2))))

(defun _rect (pa pb v-perp fd cfg / d)
  (setq d (_scale v-perp (/ fd 2.0)))
  (_mkpline (list (_add pa d) (_add pb d)
                  (_sub pb d) (_sub pa d))
            cfg))

;; --- groups ---------------------------------------------------------
(defun _uniqname (prefix / gd i n)
  (setq gd (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_GROUP")))
        i  1
        n  (strcat prefix "1"))
  (while (and gd (dictsearch gd n))
    (setq i (1+ i)
          n (strcat prefix (itoa i))))
  n)

(defun _mkgroup (name ents / ss)
  (setq ss (ssadd))
  (foreach e ents (if e (ssadd e ss)))
  (command "_.-group" "_create" name "" ss "")
  name)

;; --- xdata helpers --------------------------------------------------
(defun _extract-gname (xd)
  (car (vl-remove nil
    (mapcar '(lambda (it)
               (if (and (= (car it) 1000)
                        (>= (strlen (cdr it)) 2)
                        (= (substr (cdr it) 1 2) "G:"))
                 (substr (cdr it) 3)))
      xd))))

;; Read the stored label number (e.g. "D3") from the tagged entity's xdata.
(defun _extract-lblnum (xd)
  (car (vl-remove nil
    (mapcar '(lambda (it)
               (if (and (= (car it) 1000)
                        (>= (strlen (cdr it)) 2)
                        (= (substr (cdr it) 1 2) "L:"))
                 (substr (cdr it) 3)))
      xd))))

;; Write / update the stored label number on the tagged entity's xdata.
(defun _tag-lblnum (ent app lbl-str / ed old-xd items new-xd new-ed)
  (regapp app)
  (setq ed     (entget ent (list app))
        old-xd (assoc -3 ed)
        items  (cdr (assoc app (cdr old-xd))))
  (setq items
    (vl-remove-if
      '(lambda (it)
         (and (= (car it) 1000)
              (>= (strlen (cdr it)) 2)
              (= (substr (cdr it) 1 2) "L:")))
      items))
  (setq items  (append items (list (cons 1000 (strcat "L:" lbl-str))))
        new-xd (list -3 (cons app items)))
  (setq new-ed
    (if old-xd
      (subst new-xd old-xd ed)
      (append ed (list new-xd))))
  (entmod new-ed))

(defun _labels-for-gname (gname app / ss i e xd result gn)
  (setq ss (ssget "_X" (list (list -3 (list app)))) result nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e  (ssname ss i)
              xd (cdr (assoc app
                       (cdr (assoc -3 (entget e (list app))))))
              gn (_extract-gname xd))
        (if (and gn (equal gn gname))
          (setq result (cons e result)))
        (setq i (1+ i)))))
  result)

;; --- label-side picker (ghost hex/circle + mouse flip) --------------
(defun _grcircle (c r n / i th1 th2 p q d)
  (setq d (/ (* 2.0 pi) n) i 0)
  (while (< i n)
    (setq th1 (* d i)  th2 (* d (1+ i))
          p (list (+ (car c) (* r (cos th1))) (+ (cadr c) (* r (sin th1))) 0.0)
          q (list (+ (car c) (* r (cos th2))) (+ (cadr c) (* r (sin th2))) 0.0))
    (_grseg p q)
    (setq i (1+ i))))

(defun _grhex (c r / i th1 th2 p q d)
  (setq d (/ pi 3.0) i 0)
  (while (< i 6)
    (setq th1 (* d i)  th2 (* d (1+ i))
          p (list (+ (car c) (* r (cos th1))) (+ (cadr c) (* r (sin th1))) 0.0)
          q (list (+ (car c) (* r (cos th2))) (+ (cadr c) (* r (sin th2))) 0.0))
    (_grseg p q)
    (setq i (1+ i))))

(defun _lbl-ghost (mid perp off side r hex? / pt)
  (setq pt (list (+ (car mid)  (* (car perp)  off side))
                 (+ (cadr mid) (* (cadr perp) off side))
                 0.0))
  (if hex? (_grhex pt r) (_grcircle pt r 24)))

;; Ask the user which side of the wall the label sits on. Returns 1 or -1.
(defun _pick-lbl-side (mid perp off r hex? / side g m mv my)
  (setq side 1.0)
  (_lbl-ghost mid perp off side r hex?)
  (princ "\nLabel side: move mouse to flip, click to place: ")
  (while
    (progn
      (setq g (vl-catch-all-apply 'grread (list t 13 0)))
      (cond
        ((vl-catch-all-error-p g) nil)
        ((= (car g) 5)
         (setq m  (cadr g)
               mv (mapcar '- m mid)
               my (+ (* (car mv) (car perp)) (* (cadr mv) (cadr perp)))
               side (if (< my 0) -1.0 1.0))
         (redraw)
         (_lbl-ghost mid perp off side r hex?) t)
        ((or (= (car g) 3)
             (and (= (car g) 2) (member (cadr g) '(13 32)))) nil)
        ((and (= (car g) 2) (= (cadr g) 27)) nil)
        (t t))))
  (redraw)
  side)

;; --- ghost primitives ----------------------------------------------
(defun _grseg (a b) (grdraw a b 1 -1))
(defun _grrect (pa pb wperp)
  (_grseg (_add pa wperp) (_add pb wperp))
  (_grseg (_add pb wperp) (_sub pb wperp))
  (_grseg (_sub pb wperp) (_sub pa wperp))
  (_grseg (_sub pa wperp) (_add pa wperp)))
(defun _grarc (c r sa ea n / i th1 th2 p q d)
  (if (< ea sa) (setq ea (+ ea (* 2 pi))))
  (setq d (/ (- ea sa) (float n)) i 0)
  (while (< i n)
    (setq th1 (+ sa (* d i))
          th2 (+ sa (* d (1+ i)))
          p (list (+ (car c) (* r (cos th1))) (+ (cadr c) (* r (sin th1))) 0.0)
          q (list (+ (car c) (* r (cos th2))) (+ (cadr c) (* r (sin th2))) 0.0))
    (_grseg p q)
    (setq i (1+ i))))

;; ===================================================================
;;                        W I N D O W S
;; ===================================================================
(defun _tagwin (ent w div side mid vdir gname)
  (regapp "AWIN")
  (entmod
    (append (entget ent)
      (list (list -3
              (list "AWIN"
                    (cons 1000 "AWIN")
                    (cons 1040 w)
                    (cons 1070 div)
                    (cons 1071 (fix side))
                    (cons 1041 (float *label-batch*))
                    (list 1011 (car mid) (cadr mid) 0.0)
                    (list 1013 (car vdir) (cadr vdir) 0.0)
                    (cons 1000 (strcat "G:" (if gname gname "")))
                    (cons 1000 (strcat "T:" *win-tag-type*))))))))

(defun _extract-type (xd)
  (cond
    ((car (vl-remove nil
       (mapcar '(lambda (it)
                  (if (and (= (car it) 1000)
                           (>= (strlen (cdr it)) 2)
                           (= (substr (cdr it) 1 2) "T:"))
                    (substr (cdr it) 3)))
         xd))))
    ("W")))

(defun _tag-winlbl (ent gname)
  (regapp "AWINLBL")
  (entmod
    (append (entget ent)
      (list (list -3 (list "AWINLBL"
                     (cons 1000 "AWINLBL")
                     (cons 1000 (strcat "G:" (if gname gname "")))))))))

(defun c:AWW ( / fw fd os p1 p2 ang v perp p1i p2i
                 nd n step i pc edges e1 e2 cmde
                 ents width mid gname )
  (setq fw *cfg-win-fw* fd *cfg-win-fd* os *cfg-win-wall-off*)
  (while
    (progn
      (initget "D S")
      (setq p1 (getpoint
                 (strcat "\nFirst window point or [D=divisions/S=sliding] ("
                         (if *win-slide* "sliding" "fixed") ", "
                         (itoa *win-div*) "div): ")))
      (cond
        ((eq p1 "D")
          (initget 6)
          (setq nd (getint
                     (strcat "\nNumber of divisions <" (itoa *win-div*) ">: ")))
          (if nd (setq *win-div* nd)) t)
        ((eq p1 "S")
          (setq *win-slide* (not *win-slide*))
          (princ (strcat "\nWindow: " (if *win-slide* "Sliding" "Fixed") ".")) t))))
  (if p1 (setq p2 (getpoint p1 "\nSecond window point: ")))
  (akd:place-window p1 p2)
  (princ))

(defun akd:place-window (p1 p2 / fw fd os ang v perp p1i p2i
                                  nd n step i pc edges e1 e2 cmde
                                  ents width mid gname)
  (setq fw *cfg-win-fw* fd *cfg-win-fd* os *cfg-win-wall-off*)
  (cond
    ((or (null p1) (null p2)) (princ "\nCancelled."))
    ((<= (distance p1 p2) (* 2.0 fw))
     (princ "\nPicked points are too close for the frame width."))
    (t
      (setq ang   (angle p1 p2)
            v     (list (cos ang) (sin ang) 0.0)
            perp  (list (- (sin ang)) (cos ang) 0.0)
            p1i   (_add p1 (_scale v fw))
            p2i   (_sub p2 (_scale v fw))
            width (distance p1 p2)
            mid   (_scale (_add p1 p2) 0.5)
            cmde  (getvar "CMDECHO")
            ents  nil)
      (setvar "CMDECHO" 0)
      (command "_.UNDO" "_BE")
      (setq ents (cons (_rect p1 p1i perp fd *cfg-win-frame*) ents))
      (setq ents (cons (_rect p2 p2i perp fd *cfg-win-frame*) ents))
      (cond
        (*win-slide*
          (setq ents (cons (_mkline p1i p2i *cfg-win-glass*) ents))
          (if (> *win-div* 1)
            (progn
              (setq n    (1- *win-div*)
                    step (/ (distance p1i p2i) (float *win-div*))
                    i    1)
              (while (<= i n)
                (setq pc (_add p1i (_scale v (* step i))))
                (setq ents (cons
                             (_mkline (_sub pc (_scale perp (/ *cfg-slide-win-tick* 2.0)))
                                      (_add pc (_scale perp (/ *cfg-slide-win-tick* 2.0)))
                                      *cfg-win-frame*)
                             ents))
                (setq i (1+ i))))))
        (t
          (setq edges (list p1i))
          (if (> *win-div* 1)
            (progn
              (setq n        (1- *win-div*)
                    glassLen (/ (- (distance p1i p2i) (* n fw))
                                (float *win-div*))
                    i        1)
              (while (<= i n)
                (setq pc (_add p1i (_scale v
                            (+ (* i glassLen) (* (- i 0.5) fw)))))
                (setq ents (cons
                             (_rect (_sub pc (_scale v (/ fw 2.0)))
                                    (_add pc (_scale v (/ fw 2.0)))
                                    perp fd *cfg-win-frame*)
                             ents))
                (setq edges (append edges
                              (list (_sub pc (_scale v (/ fw 2.0)))
                                    (_add pc (_scale v (/ fw 2.0))))))
                (setq i (1+ i)))))
          (setq edges (append edges (list p2i)))
          (while (cdr edges)
            (setq e1 (car edges) e2 (cadr edges))
            (setq ents (cons (_mkline e1 e2 *cfg-win-glass*) ents))
            (setq edges (cddr edges)))))
      (setq ents (cons (_mkline (_add p1 (_scale perp os))
                                (_add p2 (_scale perp os)) *cfg-win-wall*) ents))
      (setq ents (cons (_mkline (_sub p1 (_scale perp os))
                                (_sub p2 (_scale perp os)) *cfg-win-wall*) ents))
      (setq ents (cons (_mktext mid 100.0 (_txt-ang ang)
                                (rtos width 2 0) *cfg-dim-text*) ents))
      (setq gname (_uniqname "AWIN"))
      (_mkgroup gname (reverse ents))
      (_tagwin (car ents) width *win-div*
               (if *label-on*
                 (_pick-lbl-side mid perp *cfg-lbl-off-win* *cfg-lbl-hex-r* t)
                 1.0)
               mid v gname)
      (if *label-on* (_win-renum nil *label-batch*))
      (command "_.UNDO" "_E")
      (setvar "CMDECHO" cmde)
      (princ (strcat "\nWindow created (width " (rtos width 2 2)
                     ", divisions " (itoa *win-div*) ")."))))
  (princ))

(defun _win-renum (ss batch / i e xd items lh off n mid vd sd bt ang pt w dv gn
                                tp keys key kmap prefix prefixes hex txt lg)
  (regapp "AWIN")
  (regapp "AWINLBL")
  (if (null ss) (setq ss (ssget "_X" '((-3 ("AWIN"))))))
  (cond
   ((null ss) (princ "\nNo windows found."))
   (t
  (setq i 0 items nil)
  (repeat (sslength ss)
    (setq e   (ssname ss i)
          xd  (cdr (assoc "AWIN"
                    (cdr (assoc -3 (entget e '("AWIN"))))))
          w   (cdr (assoc 1040 xd))
          dv  (cond ((cdr (assoc 1070 xd))) (1))
          sd  (cond ((cdr (assoc 1071 xd))) (1))
          bt  (cond ((cdr (assoc 1041 xd))) (0.0))
          mid (cdr (assoc 1011 xd))
          vd  (cdr (assoc 1013 xd))
          gn  (_extract-gname xd)
          tp  (_extract-type xd))
    (if (and w mid vd
             (or (null batch) (= (fix bt) batch)))
      (setq items (cons (list w dv mid vd gn sd e tp) items)))
    (setq i (1+ i)))
  (foreach it items
    (setq gn (nth 4 it))
    (if gn (foreach lbl (_labels-for-gname gn "AWINLBL") (entdel lbl))))
  ;; Separate sequences per type (W, CW, ...). Key = (type width div).
  (setq keys nil)
  (foreach it items
    (setq key (list (nth 7 it) (fix (+ 0.5 (car it))) (cadr it)))
    (if (not (member key keys)) (setq keys (cons key keys))))
  (setq keys (vl-sort keys
    '(lambda (a b)
       (cond ((/= (car a) (car b)) (< (car a) (car b)))
             ((> (cadr a) (cadr b)) t)
             ((< (cadr a) (cadr b)) nil)
             (t (> (caddr a) (caddr b)))))))
  (setq kmap nil prefixes nil)
  (foreach k keys
    (if (not (member (car k) prefixes))
      (setq prefixes (cons (car k) prefixes))))
  (foreach prefix (reverse prefixes)
    (setq n 1)
    (foreach k keys
      (if (= (car k) prefix)
        (setq kmap (cons (cons k n) kmap) n (1+ n)))))
  (setq lh *cfg-lbl-h* off *cfg-lbl-off-win*)
  (foreach it items
    (setq w   (car   it) dv  (cadr it) mid (caddr it) vd (cadddr it)
          gn  (nth 4 it) sd (cond ((nth 5 it)) (1))
          tp  (nth 7 it)
          key (cdr (assoc (list tp (fix (+ 0.5 w)) dv) kmap))
          ang (atan (cadr vd) (car vd))
          pt  (list (+ (car mid) (* (- (sin ang)) off sd))
                    (+ (cadr mid) (* (cos ang) off sd)) 0.0))
    (setq hex (_mkhex pt *cfg-lbl-hex-r* *cfg-lbl-shape*)
          txt (_mktext pt lh 0.0 (strcat tp (itoa key)) *cfg-lbl-text*))
    (_tag-winlbl hex gn) (_tag-winlbl txt gn)
    (_tag-lblnum (nth 6 it) "AWIN" (strcat tp (itoa key)))
    (setq lg (_uniqname "AWINLBL"))
    (_mkgroup lg (list hex txt)))
  (princ (strcat "\nRenumbered " (itoa (length items))
                 " window(s) into " (itoa (length keys)) " type(s)."))))
  (princ))

(defun c:WR ( / cmde)
  (setq cmde (getvar "CMDECHO")) (setvar "CMDECHO" 0)
  (princ "\nSelect windows to renumber [Enter=all]: ")
  (_win-renum (ssget '((-3 ("AWIN")))) nil)
  (setvar "CMDECHO" cmde) (princ))

(defun c:WC ( / ss i e xd w tally item total)
  (regapp "AWIN")
  (setq ss (ssget "_X" '((-3 ("AWIN")))))
  (cond
    ((null ss) (princ "\nNo tagged windows found."))
    (t
      (setq i 0 tally nil total 0)
      (repeat (sslength ss)
        (setq e  (ssname ss i)
              xd (cdr (assoc "AWIN"
                       (cdr (assoc -3 (entget e '("AWIN"))))))
              w  (cdr (assoc 1040 xd)))
        (if w (setq w (fix (+ 0.5 w))))
        (if w
          (progn
            (setq item (assoc w tally))
            (if item
              (setq tally (subst (cons w (1+ (cdr item))) item tally))
              (setq tally (cons (cons w 1) tally)))))
        (setq i (1+ i)))
      (princ "\n--- Window count ---")
      (foreach it (vl-sort tally '(lambda (a b) (> (car a) (car b))))
        (princ (strcat "\n  width " (rtos (car it) 2 2)
                       " : " (itoa (cdr it))))
        (setq total (+ total (cdr it))))
      (princ (strcat "\n  total: " (itoa total)))))
  (princ))

;; ===================================================================
;;                          D O O R S
;; ===================================================================
(defun _tagdoor (ent w typ dv side mid vdir gname)
  (regapp "ADOOR")
  (entmod
    (append (entget ent)
      (list (list -3
              (list "ADOOR"
                    (cons 1000 "ADOOR")
                    (cons 1040 w)
                    (cons 1070 (cond ((eq typ "D") 2) ((eq typ "G") 3)
                                     ((eq typ "P") 4) (1)))
                    (cons 1071 dv)
                    (cons 1042 side)
                    (cons 1041 (float *label-batch*))
                    (list 1011 (car mid) (cadr mid) 0.0)
                    (list 1013 (car vdir) (cadr vdir) 0.0)
                    (cons 1000 (strcat "G:" (if gname gname "")))))))))

(defun _tag-doorlbl (ent gname)
  (regapp "ADOORLBL")
  (entmod
    (append (entget ent)
      (list (list -3 (list "ADOORLBL"
                     (cons 1000 "ADOORLBL")
                     (cons 1000 (strcat "G:" (if gname gname "")))))))))

;; --- door corner math ----------------------------------------------
(defun _door-corners (p1 p2 v perp fw fd pt-thk fx fy
                      / ea eb ev eperp eai ebi gap hinge open-end
                        strike-c back-h back-s sa-arc ea-arc)
  (if (< fx 0) (setq ea p2 eb p1 ev (_scale v -1.0))
               (setq ea p1 eb p2 ev v))
  (setq eperp    (_scale perp fy)
        eai      (_add ea (_scale ev fw))
        ebi      (_sub eb (_scale ev fw))
        gap      (distance eai ebi)
        hinge    (_add eai (_scale eperp (/ fd 2.0)))
        open-end (_add hinge (_scale eperp gap))
        strike-c (_add hinge (_scale ev gap))
        back-h   (_add hinge (_scale ev pt-thk))
        back-s   (_add open-end (_scale ev pt-thk))
        sa-arc   (if (> (* fx fy) 0) (angle hinge strike-c) (angle hinge open-end))
        ea-arc   (if (> (* fx fy) 0) (angle hinge open-end) (angle hinge strike-c)))
  (list hinge open-end back-h back-s gap sa-arc ea-arc))

(defun _dbl-corners (p1 p2 v perp fw fd pt-thk fx fy
                     / ev eperp p1i p2i gap half
                       hA openA strikeA backA1 backA2
                       hB openB strikeB backB1 backB2
                       saA eaA saB eaB)
  (setq ev v  eperp (_scale perp fy)
        p1i (_add p1 (_scale ev fw))
        p2i (_sub p2 (_scale ev fw))
        gap (distance p1i p2i)  half (/ gap 2.0)
        hA (_add p1i (_scale eperp (/ fd 2.0)))
        openA (_add hA (_scale eperp half))
        strikeA (_add hA (_scale ev half))
        backA1 (_add hA (_scale ev pt-thk))
        backA2 (_add openA (_scale ev pt-thk))
        hB (_add p2i (_scale eperp (/ fd 2.0)))
        openB (_add hB (_scale eperp half))
        strikeB (_sub hB (_scale ev half))
        backB1 (_sub hB (_scale ev pt-thk))
        backB2 (_sub openB (_scale ev pt-thk)))
  (if (> fy 0)
    (setq saA (angle hA strikeA) eaA (angle hA openA)
          saB (angle hB openB)   eaB (angle hB strikeB))
    (setq saA (angle hA openA)   eaA (angle hA strikeA)
          saB (angle hB strikeB) eaB (angle hB openB)))
  (list hA openA backA1 backA2 saA eaA
        hB openB backB1 backB2 saB eaB
        half))

;; --- sliding door math ---------------------------------------------
(defun _slide-fd (n)
  (* *cfg-slide-p* (if (= n 4) 2 n)))

(defun _slide-panel (p1i v eperp s e li pt half / a b y0 y1)
  (setq a  (_add p1i (_scale v s))
        b  (_add p1i (_scale v e))
        y1 (- half (* pt li))
        y0 (- y1 pt))
  (list (_add a (_scale eperp y0))
        (_add b (_scale eperp y0))
        (_add b (_scale eperp y1))
        (_add a (_scale eperp y1))))

(defun _slide-corners (p1 p2 v perp fw fd fy n
                       / eperp p1i p2i gap ext pt half seg
                         panels i s e q)
  (setq eperp (_scale perp fy)
        p1i (_add p1 (_scale v fw))
        p2i (_sub p2 (_scale v fw))
        gap (distance p1i p2i)
        ext *cfg-slide-ext*
        pt  *cfg-slide-p*
        half (/ (* pt (if (= n 4) 2 n)) 2.0))
  (cond
    ((= n 4)
      (setq q (/ gap 4.0))
      (list
        (_slide-panel p1i v eperp 0.0             (+ q ext)          1 pt half)
        (_slide-panel p1i v eperp (- q ext)       (/ gap 2.0)        0 pt half)
        (_slide-panel p1i v eperp (/ gap 2.0)     (+ (* 3 q) ext)    0 pt half)
        (_slide-panel p1i v eperp (- (* 3 q) ext) gap                1 pt half)))
    (t
      (setq seg (/ gap (float n)) i 0 panels nil)
      (while (< i n)
        (setq s (if (zerop i)    0.0 (- (* seg i)      ext))
              e (if (= i (1- n)) gap (+ (* seg (1+ i)) ext)))
        (setq panels (cons (_slide-panel p1i v eperp s e i pt half) panels))
        (setq i (1+ i)))
      (reverse panels))))

;; --- pocket door helpers -------------------------------------------
(defun _pd-corner (origin v perp fy x y)
  (_add origin (_add (_scale v x) (_scale perp (* y fy)))))

(defun _pd-rect (x0 x1 y0 y1 origin v perp fy cfg)
  (_mkpline
    (list (_pd-corner origin v perp fy x0 y0)
          (_pd-corner origin v perp fy x1 y0)
          (_pd-corner origin v perp fy x1 y1)
          (_pd-corner origin v perp fy x0 y1))
    cfg))

(defun _mkpline-open (pts cfg / lyr)
  (setq lyr (car cfg)) (_ensure-layer lyr)
  (entmakex
    (append
      (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lyr)
            '(100 . "AcDbPolyline") (cons 90 (length pts))
            '(70 . 0) (cons 62 (cdr cfg)))
      (mapcar '(lambda (p) (list 10 (car p) (cadr p))) pts))))

(defun _pd-solid (x0 x1 y0 y1 origin v perp fy col / c1 c2 c3 c4)
  (setq c1 (_pd-corner origin v perp fy x0 y0)
        c2 (_pd-corner origin v perp fy x1 y0)
        c3 (_pd-corner origin v perp fy x1 y1)
        c4 (_pd-corner origin v perp fy x0 y1))
  (_ensure-layer "A-DOOR")
  (entmakex (list
    '(0 . "SOLID") '(100 . "AcDbEntity") '(8 . "A-DOOR")
    (cons 62 col) '(100 . "AcDbTrace")
    (list 10 (car c1) (cadr c1) 0.0)
    (list 11 (car c2) (cadr c2) 0.0)
    (list 12 (car c4) (cadr c4) 0.0)
    (list 13 (car c3) (cadr c3) 0.0))))

(defun _pd-x (x0 x1 y0 y1 origin v perp fy cfg / c1 c2 c3 c4)
  (setq c1 (_pd-corner origin v perp fy x0 y0)
        c2 (_pd-corner origin v perp fy x1 y0)
        c3 (_pd-corner origin v perp fy x1 y1)
        c4 (_pd-corner origin v perp fy x0 y1))
  (list (_mkpline-open (list c1 c3) cfg) (_mkpline-open (list c2 c4) cfg)))

;; Draw all pocket-door components; returns updated ents list.
;; Layout (across wall, +y = grey-wall side / "front"):
;;   yGF=+fdH -------- grey wall front face
;;   yGB=yGF-gT ------ grey back / leaf front
;;   yLB=yGB-lT ------ leaf back / stud front
;;   ySB=yLB-sT ------ stud back / gyp front
;;   yGyB=ySB-gyT = -fdH  gyp back face
(defun akd:pd-parts (p1 p2 v perp fx fy ents / origin dir totalW opening jamb
                                                fd fdH gT lT sT gyT jH sW lip gr
                                                yGF yGB yLB ySB yGyB greyLen
                                                midX rSupX)
  (setq totalW  (distance p1 p2)
        fd      *cfg-door-fd*
        fdH     (/ fd 2.0)
        opening *cfg-pd-opening*
        jamb    *cfg-pd-jamb*
        jH      *cfg-pd-jamb-h*
        gT      *cfg-pd-grey-t*
        lT      *cfg-pd-leaf-t*
        sT      *cfg-pd-stud-t*
        gyT     *cfg-pd-gyp-t*
        sW      *cfg-pd-stud-w*
        lip     *cfg-pd-lipext*
        gr      *cfg-pd-groove*
        greyLen (- totalW opening lip jamb)
        yGF     (- fdH (/ (- fd (+ gT lT sT gyT)) 2.0))
        yGB     (- yGF gT)
        yLB     (- yGB lT)
        ySB     (- yLB sT)
        yGyB    (- ySB gyT)
        origin  (if (< fx 0) p2 p1)
        dir     (if (< fx 0) (_scale v -1.0) v))
  (cond
    ((< greyLen (+ opening 150.0))
      (princ (strcat "\nPocket door: wall too short. Need "
                     (rtos (+ (* 2.0 opening) 250.0) 2 0)
                     " for " (rtos opening 2 0) " clear opening, got "
                     (rtos totalW 2 0) "."))
      ents)
    (t
      ;; Wall lines: only in the opening (between greyWall right and strikeFrame left)
      (setq ents (cons (_mkpline-open (list
        (_pd-corner origin dir perp fy greyLen yGF)
        (_pd-corner origin dir perp fy (- totalW jamb) yGF))
        *cfg-door-wall*) ents))
      (setq ents (cons (_mkpline-open (list
        (_pd-corner origin dir perp fy greyLen yGyB)
        (_pd-corner origin dir perp fy (- totalW jamb) yGyB))
        *cfg-door-wall*) ents))
      ;; Grey pocket wall: solid color-9 fill + outline (outline drawn on top)
      (setq ents (cons (_pd-solid 0.0 greyLen yGB yGF
                                  origin dir perp fy 9) ents))
      (setq ents (cons (_pd-rect 0.0 greyLen yGB yGF
                                 origin dir perp fy *cfg-pd-grey*) ents))
      (command "_.DRAWORDER" (car ents) "" "_F")
      ;; Gypsum back board (starts at x=0)
      (setq ents (cons (_pd-rect 0.0 greyLen yGyB ySB
                                 origin dir perp fy *cfg-door-frame*) ents))
      ;; Leaf: starts AFTER jambPost, extends jamb-length past greyWall (lip)
      (setq ents (cons (_pd-rect jamb (+ jamb greyLen) yLB yGB
                                 origin dir perp fy *cfg-door-panel*) ents))
      ;; jambPost 50 x 65: at x=[0, jamb], spans stud + leaf zone
      (setq ents (cons (_pd-rect 0.0 jamb ySB yGB
                                 origin dir perp fy *cfg-door-frame*) ents))
      (foreach x (_pd-x 0.0 jamb ySB yGB
                        origin dir perp fy *cfg-door-frame*)
        (setq ents (cons x ents)))
      ;; endStud (aligned with greyWall right end)
      (setq rSupX (- greyLen sW))
      (setq ents (cons (_pd-rect rSupX (+ rSupX sW) ySB yLB
                                 origin dir perp fy *cfg-door-frame*) ents))
      (foreach x (_pd-x rSupX (+ rSupX sW) ySB yLB
                        origin dir perp fy *cfg-door-frame*)
        (setq ents (cons x ents)))
      ;; midStud, midway between jambPost right and endStud left
      (setq midX (- (/ (+ jamb rSupX) 2.0) (/ sW 2.0)))
      (setq ents (cons (_pd-rect midX (+ midX sW) ySB yLB
                                 origin dir perp fy *cfg-door-frame*) ents))
      (foreach x (_pd-x midX (+ midX sW) ySB yLB
                        origin dir perp fy *cfg-door-frame*)
        (setq ents (cons x ents)))
      ;; strikeFrame: full wall thickness (10 taller than before), C-shape with groove
      (setq ents (cons (_mkpline
        (list (_pd-corner origin dir perp fy (- totalW jamb) yGF)
              (_pd-corner origin dir perp fy totalW yGF)
              (_pd-corner origin dir perp fy totalW yGyB)
              (_pd-corner origin dir perp fy (- totalW jamb) yGyB)
              (_pd-corner origin dir perp fy (- totalW jamb) yLB)
              (_pd-corner origin dir perp fy (+ (- totalW jamb) gr) yLB)
              (_pd-corner origin dir perp fy (+ (- totalW jamb) gr) yGB)
              (_pd-corner origin dir perp fy (- totalW jamb) yGB))
        *cfg-door-frame*) ents))
      ents)))

(defun _pd-ghost-box (x0 x1 y0 y1 origin v perp fy / a b c d)
  (setq a (_pd-corner origin v perp fy x0 y0)
        b (_pd-corner origin v perp fy x1 y0)
        c (_pd-corner origin v perp fy x1 y1)
        d (_pd-corner origin v perp fy x0 y1))
  (_grseg a b) (_grseg b c) (_grseg c d) (_grseg d a))

(defun _ghost-pocket (p1 p2 v perp fw fd pt-thk fx fy /
                        origin dir totalW opening jamb gT lT sT gyT lip gr
                        yGF yGB yLB ySB yGyB greyLen fdH totPd off tj)
  (setq totalW  (distance p1 p2)
        fdH     (/ fd 2.0)
        opening *cfg-pd-opening*
        jamb    *cfg-pd-jamb*
        gT      *cfg-pd-grey-t*
        lT      *cfg-pd-leaf-t*
        sT      *cfg-pd-stud-t*
        gyT     *cfg-pd-gyp-t*
        lip     *cfg-pd-lipext*
        gr      *cfg-pd-groove*
        greyLen (- totalW opening lip jamb)
        totPd   (+ gT lT sT gyT)
        off     (/ (- fd totPd) 2.0)
        yGF     (- fdH off)
        yGB     (- yGF gT)
        yLB     (- yGB lT)
        ySB     (- yLB sT)
        yGyB    (- ySB gyT)
        origin  (if (< fx 0) p2 p1)
        dir     (if (< fx 0) (_scale v -1.0) v)
        tj      (- totalW jamb))
  (cond
    ((< greyLen (+ opening 150.0)) nil)
    (t
      (_pd-ghost-box 0.0 greyLen yGB yGF origin dir perp fy)
      (_pd-ghost-box jamb (+ jamb greyLen) yLB yGB origin dir perp fy)
      (_pd-ghost-box 0.0 greyLen yGyB ySB origin dir perp fy)
      (_grseg (_pd-corner origin dir perp fy greyLen yGF)
              (_pd-corner origin dir perp fy tj yGF))
      (_grseg (_pd-corner origin dir perp fy greyLen yGyB)
              (_pd-corner origin dir perp fy tj yGyB))
      (_grseg (_pd-corner origin dir perp fy tj yGF)
              (_pd-corner origin dir perp fy totalW yGF))
      (_grseg (_pd-corner origin dir perp fy totalW yGF)
              (_pd-corner origin dir perp fy totalW yGyB))
      (_grseg (_pd-corner origin dir perp fy totalW yGyB)
              (_pd-corner origin dir perp fy tj yGyB))
      (_grseg (_pd-corner origin dir perp fy tj yGyB)
              (_pd-corner origin dir perp fy tj yLB))
      (_grseg (_pd-corner origin dir perp fy tj yLB)
              (_pd-corner origin dir perp fy (+ tj gr) yLB))
      (_grseg (_pd-corner origin dir perp fy (+ tj gr) yLB)
              (_pd-corner origin dir perp fy (+ tj gr) yGB))
      (_grseg (_pd-corner origin dir perp fy (+ tj gr) yGB)
              (_pd-corner origin dir perp fy tj yGB))
      (_grseg (_pd-corner origin dir perp fy tj yGB)
              (_pd-corner origin dir perp fy tj yGF)))))

;; --- ghosts --------------------------------------------------------
(defun _ghost (p1 p2 v perp fw fd pt-thk fx fy / c wperp)
  (setq wperp (_scale perp (/ fd 2.0)))
  (_grrect p1 (_add p1 (_scale v fw)) wperp)
  (_grrect p2 (_sub p2 (_scale v fw)) wperp)
  (setq c (_door-corners p1 p2 v perp fw fd pt-thk fx fy))
  (_grseg (nth 0 c) (nth 1 c)) (_grseg (nth 1 c) (nth 3 c))
  (_grseg (nth 3 c) (nth 2 c)) (_grseg (nth 2 c) (nth 0 c))
  (_grarc  (nth 0 c) (nth 4 c) (nth 5 c) (nth 6 c) 16))

(defun _ghost-dbl (p1 p2 v perp fw fd pt-thk fx fy / c wperp)
  (setq wperp (_scale perp (/ fd 2.0)))
  (_grrect p1 (_add p1 (_scale v fw)) wperp)
  (_grrect p2 (_sub p2 (_scale v fw)) wperp)
  (setq c (_dbl-corners p1 p2 v perp fw fd pt-thk fx fy))
  (_grseg (nth 0 c) (nth 1 c)) (_grseg (nth 1 c) (nth 3 c))
  (_grseg (nth 3 c) (nth 2 c)) (_grseg (nth 2 c) (nth 0 c))
  (_grseg (nth 6 c) (nth 7 c)) (_grseg (nth 7 c) (nth 9 c))
  (_grseg (nth 9 c) (nth 8 c)) (_grseg (nth 8 c) (nth 6 c))
  (_grarc  (nth 0 c) (nth 12 c) (nth 4 c)  (nth 5 c)  12)
  (_grarc  (nth 6 c) (nth 12 c) (nth 10 c) (nth 11 c) 12))

(defun _ghost-slide (p1 p2 v perp fw fd pt-thk fx fy / panels wperp)
  (setq wperp (_scale perp (/ fd 2.0)))
  (_grrect p1 (_add p1 (_scale v fw)) wperp)
  (_grrect p2 (_sub p2 (_scale v fw)) wperp)
  (setq panels (_slide-corners p1 p2 v perp fw fd fy *slide-div*))
  (foreach pn panels
    (_grseg (nth 0 pn) (nth 1 pn))
    (_grseg (nth 1 pn) (nth 2 pn))
    (_grseg (nth 2 pn) (nth 3 pn))
    (_grseg (nth 3 pn) (nth 0 pn)))
  (_grseg (_add p1 (_scale perp *cfg-slide-wall-off*))
          (_add p2 (_scale perp *cfg-slide-wall-off*)))
  (_grseg (_sub p1 (_scale perp *cfg-slide-wall-off*))
          (_sub p2 (_scale perp *cfg-slide-wall-off*))))

;; --- AD ------------------------------------------------------------
(defun c:ADD ( / fw fd pt-thk p1 p2 ang v perp mid width
                  fx fy g m mv mx my c ghost-fn
                  cmde ents gname old-err tk )
  (setq old-err *error*
        *error* (lambda (msg) (redraw) (setq *error* old-err) (princ)))
  (setq fw *cfg-door-fw* fd *cfg-door-fd* pt-thk *cfg-door-panel-t*)
  (while
    (progn
      (initget "A D S Pocket Number")
      (setq p1 (getpoint
                 (strcat "\nFirst door point or [A=Single/D=Double/S=sliding/Pocket/Number] ("
                         *door-type*
                         (if (eq *door-type* "G")
                           (strcat ", " (itoa *slide-div*) "p") "")
                         "): ")))
      (cond
        ((eq p1 "A") (setq *door-type* "S") t)
        ((eq p1 "D") (setq *door-type* "D") t)
        ((eq p1 "S")
         (setq *door-type* "G")
         (initget 6)
         (setq tk (getint
                    (strcat "\nSliding panels <" (itoa *slide-div*) ">: ")))
         (if tk (setq *slide-div* tk)) t)
        ((eq p1 "Pocket") (setq *door-type* "P") t)
        ((eq p1 "Number")
         (initget 6)
         (setq tk (getint
                    (strcat "\nSliding panels <" (itoa *slide-div*) ">: ")))
         (if tk (setq *slide-div* tk)) t))))
  (if p1 (setq p2 (getpoint p1 "\nSecond door point: ")))
  (akd:place-door p1 p2)
  (setq *error* old-err) (princ))

(defun akd:place-door (p1 p2 / fw fd pt-thk ang v perp mid width
                                fx fy g m mv mx my c ghost-fn
                                cmde ents gname old-err)
  (setq old-err *error*
        *error* (lambda (msg) (redraw) (setq *error* old-err) (princ)))
  (setq fw *cfg-door-fw* fd *cfg-door-fd* pt-thk *cfg-door-panel-t*)
  (cond
    ((or (null p1) (null p2)) (princ "\nCancelled."))
    ((<= (distance p1 p2) (* 2.0 fw))
     (princ "\nPicked points are too close for the frame width."))
    (t
      (setq ang   (angle p1 p2)
            v     (list (cos ang) (sin ang) 0.0)
            perp  (list (- (sin ang)) (cos ang) 0.0)
            mid   (_scale (_add p1 p2) 0.5)
            width (distance p1 p2)
            fx 1.0 fy 1.0
            cmde  (getvar "CMDECHO")
            ents  nil)
      (if (eq *door-type* "G")
        (setq fw *cfg-slide-fw* fd (_slide-fd *slide-div*)))
      (setq ghost-fn (cond ((eq *door-type* "D") '_ghost-dbl)
                           ((eq *door-type* "G") '_ghost-slide)
                           ((eq *door-type* "P") '_ghost-pocket)
                           ('_ghost)))
      (princ "\nMove mouse to flip, click to place: ")
      (apply ghost-fn (list p1 p2 v perp fw fd pt-thk fx fy))
      (while
        (progn
          (setq g (vl-catch-all-apply 'grread (list t 13 0)))
          (cond
            ((vl-catch-all-error-p g) (setq fx nil) nil)
            ((= (car g) 5)
             (setq m  (cadr g)
                   mv (mapcar '- m mid)
                   mx (+ (* (car mv) (car v))    (* (cadr mv) (cadr v)))
                   my (+ (* (car mv) (car perp)) (* (cadr mv) (cadr perp)))
                   fx (if (eq *door-type* "D") 1.0
                        (if (< mx 0) -1.0 1.0))
                   fy (if (< my 0) -1.0 1.0))
             (redraw)
             (apply ghost-fn (list p1 p2 v perp fw fd pt-thk fx fy)) t)
            ((or (= (car g) 3)
                 (and (= (car g) 2) (member (cadr g) '(13 32)))) nil)
            ((and (= (car g) 2) (= (cadr g) 27)) (setq fx nil) nil)
            (t t))))
      (redraw)
      (cond
        ((null fx) (setq *akd-cancel* T) (princ "\nCancelled — hole reverted."))
        (t
          (setvar "CMDECHO" 0)
          (command "_.UNDO" "_BE")
          (cond
            ((eq *door-type* "P")
              (setq ents (akd:pd-parts p1 p2 v perp fx fy ents)))
            (t
              (setq ents (cons (_rect p1 (_add p1 (_scale v fw)) perp fd *cfg-door-frame*) ents))
              (setq ents (cons (_rect p2 (_sub p2 (_scale v fw)) perp fd *cfg-door-frame*) ents))))
          (cond
            ((eq *door-type* "P") nil)
            ((eq *door-type* "G")
              (foreach pn (_slide-corners p1 p2 v perp fw fd fy *slide-div*)
                (setq ents (cons (_mkpline pn *cfg-door-panel*) ents)))
              (setq ents (cons (_mkline
                (_add p1 (_scale perp *cfg-slide-wall-off*))
                (_add p2 (_scale perp *cfg-slide-wall-off*)) *cfg-door-wall*) ents))
              (setq ents (cons (_mkline
                (_sub p1 (_scale perp *cfg-slide-wall-off*))
                (_sub p2 (_scale perp *cfg-slide-wall-off*)) *cfg-door-wall*) ents)))
            ((eq *door-type* "D")
              (setq c (_dbl-corners p1 p2 v perp fw fd pt-thk fx fy))
              (setq ents (cons
                (_mkpline (list (nth 0 c) (nth 1 c) (nth 3 c) (nth 2 c)) *cfg-door-panel*) ents))
              (setq ents (cons
                (_mkpline (list (nth 6 c) (nth 7 c) (nth 9 c) (nth 8 c)) *cfg-door-panel*) ents))
              (setq ents (cons (_mkarc (nth 0 c) (nth 12 c) (nth 4 c)  (nth 5 c)  *cfg-door-arc*) ents))
              (setq ents (cons (_mkarc (nth 6 c) (nth 12 c) (nth 10 c) (nth 11 c) *cfg-door-arc*) ents)))
            (t
              (setq c (_door-corners p1 p2 v perp fw fd pt-thk fx fy))
              (setq ents (cons
                (_mkpline (list (nth 0 c) (nth 1 c) (nth 3 c) (nth 2 c)) *cfg-door-panel*) ents))
              (setq ents (cons (_mkarc (nth 0 c) (nth 4 c) (nth 5 c) (nth 6 c) *cfg-door-arc*) ents))))
          (setq ents (cons (_mktext mid 100.0 (_txt-ang ang)
                                    (rtos width 2 0) *cfg-dim-text*) ents))
          (setq gname (_uniqname "ADOOR"))
          (_mkgroup gname (reverse ents))
          (_tagdoor (car ents) width *door-type*
                    (if (eq *door-type* "G") *slide-div* 1)
                    (if *label-on*
                      (_pick-lbl-side mid perp *cfg-lbl-off-door* *cfg-lbl-cir-r* nil)
                      1.0)
                    mid v gname)
          (if *label-on* (_door-renum nil *label-batch*))
          (command "_.UNDO" "_E")
          (setvar "CMDECHO" cmde)
          (princ (strcat "\nDoor created (width " (rtos width 2 2) ")."))))))
  (setq *error* old-err) (princ))

(defun _door-renum (ss batch / i e xd items lh off n mid vd sd bt ang pt w ty dv gn
                                 keys key kmap cir txt lg)
  (regapp "ADOOR")
  (regapp "ADOORLBL")
  (if (null ss) (setq ss (ssget "_X" '((-3 ("ADOOR"))))))
  (cond
   ((null ss) (princ "\nNo doors found."))
   (t
  (setq i 0 items nil)
  (repeat (sslength ss)
    (setq e   (ssname ss i)
          xd  (cdr (assoc "ADOOR"
                    (cdr (assoc -3 (entget e '("ADOOR"))))))
          w   (cdr (assoc 1040 xd))
          ty  (cond ((cdr (assoc 1070 xd))) (1))
          dv  (cond ((cdr (assoc 1071 xd))) (1))
          sd  (cond ((cdr (assoc 1042 xd))) (1))
          bt  (cond ((cdr (assoc 1041 xd))) (0.0))
          mid (cdr (assoc 1011 xd))
          vd  (cdr (assoc 1013 xd))
          gn  (_extract-gname xd))
    (if (and w mid vd
             (or (null batch) (= (fix bt) batch)))
      (setq items (cons (list w ty dv mid vd gn sd e) items)))
    (setq i (1+ i)))
  (foreach it items
    (setq gn (nth 5 it))
    (if gn (foreach lbl (_labels-for-gname gn "ADOORLBL") (entdel lbl))))
  (setq keys nil)
  (foreach it items
    (setq key (list (fix (+ 0.5 (car it))) (cadr it) (caddr it)))
    (if (not (member key keys)) (setq keys (cons key keys))))
  (setq keys (vl-sort keys
    '(lambda (a b)
       (cond ((> (car a) (car b)) t)
             ((< (car a) (car b)) nil)
             ((< (cadr a) (cadr b)) t)
             ((> (cadr a) (cadr b)) nil)
             (t (< (caddr a) (caddr b)))))))
  (setq n 1 kmap nil)
  (foreach k keys (setq kmap (cons (cons k n) kmap) n (1+ n)))
  (setq lh *cfg-lbl-h* off *cfg-lbl-off-door*)
  (foreach it items
    (setq w (car it) ty (cadr it) dv (caddr it)
          mid (cadddr it) vd (nth 4 it) gn (nth 5 it)
          sd  (cond ((nth 6 it)) (1))
          key (cdr (assoc (list (fix (+ 0.5 w)) ty dv) kmap))
          ang (atan (cadr vd) (car vd))
          pt  (list (+ (car mid) (* (- (sin ang)) off sd))
                    (+ (cadr mid) (* (cos ang) off sd)) 0.0))
    (setq cir (_mkcircle pt *cfg-lbl-cir-r* *cfg-lbl-shape*)
          txt (_mktext pt lh 0.0 (strcat "D" (itoa key)) *cfg-lbl-text*))
    (_tag-doorlbl cir gn) (_tag-doorlbl txt gn)
    (_tag-lblnum (nth 7 it) "ADOOR" (strcat "D" (itoa key)))
    (setq lg (_uniqname "ADOORLBL"))
    (_mkgroup lg (list cir txt)))
  (princ (strcat "\nRenumbered " (itoa (length items))
                 " door(s) into " (itoa (length keys)) " type(s)."))))
  (princ))

(defun c:DR ( / cmde)
  (setq cmde (getvar "CMDECHO")) (setvar "CMDECHO" 0)
  (princ "\nSelect doors to renumber [Enter=all]: ")
  (_door-renum (ssget '((-3 ("ADOOR")))) nil)
  (setvar "CMDECHO" cmde) (princ))

(defun c:DC ( / ss i e xd w tally item total)
  (regapp "ADOOR")
  (setq ss (ssget "_X" '((-3 ("ADOOR")))))
  (cond
    ((null ss) (princ "\nNo tagged doors found."))
    (t
      (setq i 0 tally nil total 0)
      (repeat (sslength ss)
        (setq e  (ssname ss i)
              xd (cdr (assoc "ADOOR"
                       (cdr (assoc -3 (entget e '("ADOOR"))))))
              w  (cdr (assoc 1040 xd)))
        (if w (setq w (fix (+ 0.5 w))))
        (if w
          (progn
            (setq item (assoc w tally))
            (if item
              (setq tally (subst (cons w (1+ (cdr item))) item tally))
              (setq tally (cons (cons w 1) tally)))))
        (setq i (1+ i)))
      (princ "\n--- Door count ---")
      (foreach it (vl-sort tally '(lambda (a b) (> (car a) (car b))))
        (princ (strcat "\n  width " (rtos (car it) 2 2)
                       " : " (itoa (cdr it))))
        (setq total (+ total (cdr it))))
      (princ (strcat "\n  total: " (itoa total)))))
  (princ))

;; ===================================================================
;;                     S C H E D U L E   T A B L E
;; ===================================================================
;; Command:  DWT   Draw a doors & windows schedule at a picked point.

(setq *cfg-tbl-layer*    "X-TAGS & SYMBOLS")
(setq *cfg-tbl-frame*    3)     ; outer frame (green)
(setq *cfg-tbl-hcol*     1)     ; horizontal separators (red)
(setq *cfg-tbl-vcol*     2)     ; vertical separators (yellow)
(setq *cfg-tbl-txt*      2)     ; body text (yellow)
(setq *cfg-tbl-hdr*      7)     ; header/title/divider text (white)
(setq *cfg-tbl-title*    "DOORS & WINDOWS SCHEDULE")
(setq *cfg-tbl-titleH*   680.0)
(setq *cfg-tbl-hdrH*     750.0)
(setq *cfg-tbl-rowH*     550.0)
(setq *cfg-tbl-txtH*     180.0)
(setq *cfg-tbl-titleTxtH* 250.0)
(setq *cfg-tbl-cols*   '(2400.0 3600.0 2400.0 2000.0))  ; LABEL TYPE WIDTH COUNT

;; ---- primitives (fixed layer for table) ---------------------------
(defun _tblline (p1 p2 col)
  (_ensure-layer *cfg-tbl-layer*)
  (entmakex
    (list '(0 . "LINE")
          '(100 . "AcDbEntity")
          (cons 8 *cfg-tbl-layer*)
          '(100 . "AcDbLine")
          (cons 62 col)
          (list 10 (car p1) (cadr p1) 0.0)
          (list 11 (car p2) (cadr p2) 0.0))))

(defun _tbltext (px py str h col just / j72)
  (_ensure-layer *cfg-tbl-layer*)
  (if (not (eq (type str) 'STR)) (setq str (vl-princ-to-string str)))
  (setq j72 (cond ((eq just "CENTER") 1) (0)))
  (entmakex
    (list '(0 . "TEXT")
          '(100 . "AcDbEntity")
          (cons 8 *cfg-tbl-layer*)
          (cons 62 col)
          '(100 . "AcDbText")
          (list 10 px py 0.0)
          (cons 40 h)
          (cons 1 str)
          (cons 72 j72)
          (list 11 px py 0.0)
          '(100 . "AcDbText"))))

(defun _tblwidth () (apply '+ *cfg-tbl-cols*))

;; ---- row drawing --------------------------------------------------
;; kind = 'HEADER 'DATA 'DIVIDER
(defun _tblrow (x y vals kind / w h xpos i col-widths topcol vcol txtcol
                                 padL padTxt)
  (setq w (_tblwidth) h *cfg-tbl-rowH*)
  (cond
    ((eq kind 'HEADER) (setq h *cfg-tbl-hdrH*)))
  (setq col-widths *cfg-tbl-cols*
        topcol (cond ((eq kind 'DIVIDER) *cfg-tbl-hdr*)
                     (t *cfg-tbl-hcol*))
        vcol   (cond ((eq kind 'HEADER) *cfg-tbl-hdr*)
                     ((eq kind 'DIVIDER) *cfg-tbl-hdr*)
                     (t *cfg-tbl-vcol*))
        txtcol (cond ((eq kind 'DATA) *cfg-tbl-txt*) (*cfg-tbl-hdr*)))
  ;; top horizontal
  (_tblline (list x y 0) (list (+ x w) y 0) topcol)
  ;; verticals (leftmost + all inner + rightmost)
  (setq xpos x)
  (_tblline (list xpos y 0) (list xpos (- y h) 0) *cfg-tbl-frame*)
  (foreach cw col-widths
    (setq xpos (+ xpos cw))
    (_tblline (list xpos y 0)
              (list xpos (- y h) 0)
              (if (equal xpos (+ x w) 0.001) *cfg-tbl-frame*
                (if (eq kind 'DIVIDER) *cfg-tbl-hdr* vcol))))
  ;; text
  (setq xpos x  i 0  padL 120.0)
  (cond
    ((eq kind 'DIVIDER)
      (_tbltext (+ x padL) (- y (/ h 1.5))
                (car vals) *cfg-tbl-txtH* txtcol "LEFT"))
    (t
      (foreach cw col-widths
        (setq padTxt (- y (- h 200.0)))
        (if (eq kind 'HEADER)
          (_tbltext (+ xpos padL) padTxt (nth i vals)
                    *cfg-tbl-txtH* txtcol "LEFT")
          (_tbltext (+ xpos (/ cw 2.0)) padTxt (nth i vals)
                    *cfg-tbl-txtH* txtcol "CENTER"))
        (setq xpos (+ xpos cw))
        (setq i (1+ i))))))

;; ---- data collection ---------------------------------------------
;; Returns list of rows: (label type width count) sorted by label num.
;; Shared helpers: one set for doors (key = w ty dv), one for windows (key = w dv).
(setq *door-key* '(lambda (xd)
  (list (fix (+ 0.5 (cdr (assoc 1040 xd))))
        (cond ((cdr (assoc 1070 xd))) (1))
        (cond ((cdr (assoc 1071 xd))) (1)))))
(setq *door-sort* '(lambda (a b)
  (cond ((> (car a) (car b)) t)
        ((< (car a) (car b)) nil)
        ((< (cadr a) (cadr b)) t)
        ((> (cadr a) (cadr b)) nil)
        (t (< (caddr a) (caddr b))))))
(setq *door-type-str* '(lambda (k)
  (cond ((= (cadr k) 1) "Single")
        ((= (cadr k) 2) "Double")
        (t (strcat "Sliding (" (itoa (caddr k)) "p)")))))

(setq *win-key* '(lambda (xd)
  (list (fix (+ 0.5 (cdr (assoc 1040 xd))))
        (cond ((cdr (assoc 1070 xd))) (1)))))
(setq *win-sort* '(lambda (a b)
  (cond ((> (car a) (car b)) t)
        ((< (car a) (car b)) nil)
        (t (> (cadr a) (cadr b))))))
(setq *win-type-str* '(lambda (k)
  (if (= (cadr k) 1) "Window"
    (strcat "Window (" (itoa (cadr k)) " div)"))))

;; Iterate every tagged entity in the drawing, apply f to its xdata.
(defun _xd-map (app f / ss i e xd out)
  (regapp app)
  (setq ss (ssget "_X" (list (list -3 (list app)))) out nil)
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e  (ssname ss i)
              xd (cdr (assoc app (cdr (assoc -3 (entget e (list app)))))))
        (if (and xd (cdr (assoc 1040 xd)))
          (setq out (cons (apply f (list xd)) out)))
        (setq i (1+ i)))))
  out)

;; Global label map: {key -> N} using ALL tagged entities (matches DR/WR order).
(defun _kmap (app key-fn sort-fn / keys n out)
  (setq keys nil)
  (foreach k (_xd-map app key-fn)
    (if (not (member k keys)) (setq keys (cons k keys))))
  (setq keys (vl-sort keys sort-fn) n 1 out nil)
  (foreach k keys (setq out (cons (cons k n) out) n (1+ n)))
  out)

(defun _collect (app ss key-fn sort-fn type-fn prefix
                 / kmap i e xd items lbl tally k cnt row n out)
  (regapp app)
  (setq kmap (_kmap app key-fn sort-fn))
  (if (null ss) (setq ss (ssget "_X" (list (list -3 (list app))))))
  (if (null ss) '()
    (progn
      (setq i 0 items nil)
      (repeat (sslength ss)
        (setq e  (ssname ss i)
              xd (cdr (assoc app (cdr (assoc -3 (entget e (list app)))))))
        (if (and xd (cdr (assoc 1040 xd)))
          (progn
            (setq k   (apply key-fn (list xd))
                  lbl (cdr (assoc k kmap)))
            (setq items (cons
              (list k (if lbl (strcat prefix (itoa lbl)))) items))))
        (setq i (1+ i)))
      (setq tally nil)
      (foreach it items
        (setq k (car it) lbl (cadr it) cnt (assoc k tally))
        (if cnt
          (setq tally (subst
            (list k (1+ (cadr cnt))
                    (if (caddr cnt) (caddr cnt) lbl))
            cnt tally))
          (setq tally (cons (list k 1 lbl) tally))))
      (setq tally (vl-sort tally
        '(lambda (a b) (apply sort-fn (list (car a) (car b))))))
      (setq out nil n 1)
      (foreach row tally
        (setq k (car row))
        (setq out (cons
          (list (if (caddr row) (caddr row) (strcat prefix (itoa n)))
                (apply type-fn (list k))
                (itoa (car k))
                (itoa (cadr row)))
          out))
        (setq n (1+ n)))
      (reverse out))))

(defun _collect-doors (ss)
  (_collect "ADOOR" ss *door-key* *door-sort* *door-type-str* "D"))

(defun _collect-windows (ss)
  (_collect "AWIN" ss *win-key* *win-sort* *win-type-str* "W"))

;; ---- DWT command --------------------------------------------------
(defun c:DWT ( / ss doors wins pt x y w cmde)
  (regapp "ADOOR") (regapp "AWIN")
  (princ "\nSelect doors/windows for schedule [Enter=all in drawing]: ")
  (setq ss (ssget '((-4 . "<OR") (-3 ("ADOOR")) (-3 ("AWIN")) (-4 . "OR>"))))
  (setq doors (_collect-doors ss)
        wins  (_collect-windows ss))
  (cond
    ((and (null doors) (null wins))
      (princ "\nNo doors or windows found."))
    (t
      (setq pt (getpoint "\nSchedule insertion point (top-left): "))
      (if pt
        (progn
          (setq cmde (getvar "CMDECHO")) (setvar "CMDECHO" 0)
          (command "_.UNDO" "_BE")
          (setq x (car pt) y (cadr pt) w (_tblwidth))
          ;; title
          (_tblline (list x y 0)                    (list (+ x w) y 0)                    *cfg-tbl-frame*)
          (_tblline (list x y 0)                    (list x (- y *cfg-tbl-titleH*) 0)     *cfg-tbl-frame*)
          (_tblline (list (+ x w) y 0)              (list (+ x w) (- y *cfg-tbl-titleH*) 0) *cfg-tbl-frame*)
          (_tbltext (+ x (/ w 2.0)) (- y (- *cfg-tbl-titleH* 250.0))
                    *cfg-tbl-title* *cfg-tbl-titleTxtH* *cfg-tbl-hdr* "CENTER")
          (setq y (- y *cfg-tbl-titleH*))
          ;; header
          (_tblrow x y '("LABEL" "TYPE" "WIDTH (mm)" "COUNT") 'HEADER)
          (setq y (- y *cfg-tbl-hdrH*))
          ;; DOORS section
          (if doors
            (progn
              (_tblrow x y '("DOORS") 'DIVIDER)
              (setq y (- y *cfg-tbl-rowH*))
              (foreach r doors
                (_tblrow x y r 'DATA)
                (setq y (- y *cfg-tbl-rowH*)))))
          ;; WINDOWS section
          (if wins
            (progn
              (_tblrow x y '("WINDOWS") 'DIVIDER)
              (setq y (- y *cfg-tbl-rowH*))
              (foreach r wins
                (_tblrow x y r 'DATA)
                (setq y (- y *cfg-tbl-rowH*)))))
          ;; bottom border
          (_tblline (list x y 0) (list (+ x w) y 0) *cfg-tbl-frame*)
          (command "_.UNDO" "_E")
          (setvar "CMDECHO" cmde)
          (princ "\nSchedule drawn.")))))
  (princ))

(princ "\nAKDDW loaded.  AW WC WR  |  AD DC DR  |  DWT  |  LT LC")
(princ)
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

(defun c:HH () (hole:loop nil "hole" 'hole:getw 'hole:setw "" nil nil))
(defun c:AD () (hole:loop 'hole:post-door "door" 'hd:getw 'hd:setw
                          "A D S Pocket Number" 'hd:extra 'hd:status))
(defun c:AW () (hole:loop 'hole:post-window "window" 'hw:getw 'hw:setw
                          "Divisions S" 'hw:extra 'hw:status))
(defun c:ACW ()
  (hole:loop 'hole:post-curtain "curtain" 'ac:getw 'ac:setw
             "Spacing Divisions" 'ac:extra 'ac:status))

(defun ac:status ()
  (if (= *ac-mode* "M")
    (strcat "Spacing: " (rtos *ac-spacing* 2 0))
    (strcat "Divisions: " (itoa *ac-div*))))

(defun hd:status ()
  (strcat "Type: " (cond ((= *door-type* "D") "Double")
                         ((= *door-type* "G")
                           (strcat "Sliding/" (itoa *slide-div*) "p"))
                         ((= *door-type* "P")
                           (strcat "Pocket/open " (rtos *cfg-pd-opening* 2 0)))
                         (t "Single"))))

(defun hw:status ()
  (strcat "Divisions: " (itoa *win-div*)
          (if *win-slide* " | Sliding" "")))

(defun hole:post-door (b1a b1b b2a b2b)
  (akd:place-door (hole:mid b1a b2a) (hole:mid b1b b2b)))
(defun hole:post-window (b1a b1b b2a b2b)
  (akd:place-window (hole:mid b1a b2a) (hole:mid b1b b2b)))
(defun hole:post-curtain (b1a b1b b2a b2b)
  (akd:place-curtain (hole:mid b1a b2a) (hole:mid b1b b2b)))
(defun hole:mid (a b) (mapcar '(lambda (x y) (/ (+ x y) 2.0)) a b))

(defun hole:setw (v) (setvar "USERR1" v))
(defun hd:getw ()
  (cond ((= *door-type* "P") (+ (* 2.0 *cfg-pd-opening*) 250.0))
        (*hd-w* *hd-w*)
        (t 900.0)))
(defun hd:setw (v)
  (cond ((= *door-type* "P") (setq *cfg-pd-opening* v))
        (t (setq *hd-w* v))))
(defun hw:getw () (if *hw-w* *hw-w* 1200.0))
(defun hw:setw (v) (setq *hw-w* v))
(defun ac:getw () (if *ac-w* *ac-w* 4800.0))
(defun ac:setw (v) (setq *ac-w* v))

(defun hd:extra (kw / v)
  (cond
    ((= kw "A") (setq *door-type* "S") (princ "\nDoor: Single leaf.") t)
    ((= kw "D") (setq *door-type* "D") (princ "\nDoor: Double leaf.") t)
    ((= kw "S") (setq *door-type* "G") (princ "\nDoor: Sliding.") t)
    ((= kw "Pocket") (setq *door-type* "P") (princ "\nDoor: Pocket.") t)
    ((= kw "Number")
      (initget 6)
      (setq v (getint (strcat "\nSliding panels <" (itoa *slide-div*) ">: ")))
      (if v (setq *slide-div* v)) t)
    (t nil)))

(defun ac:extra (kw / sub v)
  (cond
    ((= kw "Spacing")
      (setq *ac-mode* "M")
      (initget "Spacing")
      (setq sub (getkword
        (strcat "\nMode: Spacing (" (rtos *ac-spacing* 2 0)
                "). [S=set value, Enter=keep]: ")))
      (cond ((= sub "Spacing")
             (initget 6)
             (setq v (getreal (strcat "\nMullion spacing <"
                                      (rtos *ac-spacing* 2 0) ">: ")))
             (if v (setq *ac-spacing* v)))) t)
    ((= kw "Divisions")
      (setq *ac-mode* "D")
      (initget "Divisions")
      (setq sub (getkword
        (strcat "\nMode: Divisions (" (itoa *ac-div*)
                "). [D=set value, Enter=keep]: ")))
      (cond ((= sub "Divisions")
             (initget 6)
             (setq v (getint (strcat "\nDivisions <" (itoa *ac-div*) ">: ")))
             (if v (setq *ac-div* v)))) t)
    (t nil)))

(defun akd:place-curtain (p1 p2 / width divs ratio use-auto oldDiv oldSlide oldType)
  (cond
    ((or (null p1) (null p2)) (princ "\nCancelled."))
    (t
      (setq width (distance p1 p2))
      (cond
        ((eq *ac-mode* "M")
          (setq ratio (/ width *ac-spacing*)
                divs (fix (+ 0.5 ratio)))
          (if (< divs 1) (setq divs 1))
          (if (not (equal ratio (float divs) 1e-3))
            (progn
              (initget "Yes No")
              (setq use-auto (getkword
                (strcat "\nLength " (rtos width 2 0) " / spacing "
                        (rtos *ac-spacing* 2 0) " = "
                        (rtos ratio 2 3)
                        ". Auto-fit to " (itoa divs)
                        " divs (spacing " (rtos (/ width divs) 2 0)
                        ")? [Yes/No] <Yes>: ")))
              (if (eq use-auto "No") (setq divs (fix ratio)))
              (if (< divs 1) (setq divs 1)))))
        (t (setq divs *ac-div*)))
      (setq oldDiv *win-div* oldSlide *win-slide* oldType *win-tag-type*
            *win-div* divs *win-slide* nil *win-tag-type* "CW")
      (akd:place-window p1 p2)
      (setq *win-div* oldDiv *win-slide* oldSlide *win-tag-type* oldType))))

(defun c:ACWW ( / p1 p2 nd sp)
  (princ (strcat "\nCurtain Wall. "
                 (if (= *ac-mode* "M")
                   (strcat "Spacing=" (rtos *ac-spacing* 2 0))
                   (strcat "Divs=" (itoa *ac-div*)))))
  (while (progn
    (initget "Spacing Divisions")
    (setq p1 (getpoint (strcat "\nFirst curtain point or [Spacing/Divisions] ("
                       (if (= *ac-mode* "M")
                         (strcat "sp=" (rtos *ac-spacing* 2 0))
                         (strcat (itoa *ac-div*) "div")) "): ")))
    (cond
      ((eq p1 "Spacing")
        (initget 6)
        (setq sp (getreal (strcat "\nMullion spacing <"
                                  (rtos *ac-spacing* 2 0) ">: ")))
        (if sp (progn (setq *ac-spacing* sp) (setq *ac-mode* "M"))) t)
      ((eq p1 "Divisions")
        (initget 6)
        (setq nd (getint (strcat "\nDivisions <" (itoa *ac-div*) ">: ")))
        (if nd (progn (setq *ac-div* nd) (setq *ac-mode* "D"))) t))))
  (if p1 (setq p2 (getpoint p1 "\nSecond curtain point: ")))
  (akd:place-curtain p1 p2)
  (princ))

;; -- Corner window (AXW) --------------------------------------------

;; Line-line intersection (2D). Returns nil if parallel.
(defun _isect (p1 v1 p2 v2 / det s)
  (setq det (- (* (car v1) (- 0 (cadr v2))) (* (- 0 (car v2)) (cadr v1))))
  (if (equal det 0.0 1e-9) nil
    (progn
      (setq s (/ (- (* (car v1) (- (cadr p2) (cadr p1)))
                    (* (cadr v1) (- (car p2) (car p1))))
                 det))
      (list (+ (car p2) (* s (car v2)))
            (+ (cadr p2) (* s (cadr v2))) 0.0))))

(defun _sgn (x) (cond ((> x 0) 1.0) ((< x 0) -1.0) (0.0)))

;; Draw one arm of a corner window: jamb at outer end, mullions, glass.
(defun akd:place-corner-arm (armC p2 fw fd v perp ents / p2i intLen
                                                        n glassLen i pc
                                                        edges e1 e2)
  (setq p2i (_sub p2 (_scale v fw)))
  (setq ents (cons (_rect p2 p2i perp fd *cfg-win-frame*) ents))
  (setq edges (list armC))
  (if (> *win-div* 1)
    (progn
      (setq n        (1- *win-div*)
            intLen   (distance armC p2i)
            glassLen (/ (- intLen (* n fw)) (float *win-div*))
            i        1)
      (while (<= i n)
        (setq pc (_add armC (_scale v (+ (* i glassLen) (* (- i 0.5) fw)))))
        (setq ents (cons (_rect (_sub pc (_scale v (/ fw 2.0)))
                                (_add pc (_scale v (/ fw 2.0)))
                                perp fd *cfg-win-frame*) ents))
        (setq edges (append edges
                     (list (_sub pc (_scale v (/ fw 2.0)))
                           (_add pc (_scale v (/ fw 2.0))))))
        (setq i (1+ i)))))
  (setq edges (append edges (list p2i)))
  (while (cdr edges)
    (setq e1 (car edges) e2 (cadr edges))
    (setq ents (cons (_mkline e1 e2 *cfg-win-glass*) ents))
    (setq edges (cddr edges)))
  ents)

(defun c:AXW ( / ref refPt p1 p2 fw fd os postSz halfP
                 ang1 ang2 v1 v2 perp1 perp2 sIn1 sIn2 pi1 pi2
                 cornerCtr armC1 armC2 innerCorn outerCorn
                 postP1 postP2 postP3 postP4 ents gname cmde nd choice totalW
                 oldForce oldWw oldMode wallEnt)
  (setq fw *cfg-win-fw* fd *cfg-win-fd* os *cfg-win-wall-off*
        postSz *cfg-corner-post* halfP (/ postSz 2.0))
  (setq *axw-ref* "C")
  (while (progn
    (initget "Divisions Hole")
    (setq p1 (getpoint (strcat "\n[Corner Window | Divisions: "
                          (itoa *win-div*)
                          "/arm] First wall end or [Divisions/Hole]: ")))
    (cond
      ((eq p1 "Divisions")
        (initget 6)
        (setq nd (getint (strcat "\nDivisions per arm <"
                                 (itoa *win-div*) ">: ")))
        (if nd (setq *win-div* nd)) t)
      ((eq p1 "Hole")
        (c:HHX)
        (princ "\nHole cut. Restart AXW to place the corner window.")
        (setq p1 nil) nil))))
  (if p1 (setq refPt (getpoint p1 "\nCorner point: ")))
  (if refPt (setq p2 (getpoint refPt "\nSecond wall end: ")))
  (cond
    ((or (null refPt) (null p1) (null p2)) (princ "\nCancelled."))
    (t
      (setq ang1 (angle refPt p1) ang2 (angle refPt p2)
            v1 (list (cos ang1) (sin ang1) 0.0)
            v2 (list (cos ang2) (sin ang2) 0.0)
            perp1 (list (- (sin ang1)) (cos ang1) 0.0)
            perp2 (list (- (sin ang2)) (cos ang2) 0.0)
            sIn1 (_sgn (+ (* (car perp1) (car v2))
                          (* (cadr perp1) (cadr v2))))
            sIn2 (_sgn (+ (* (car perp2) (car v1))
                          (* (cadr perp2) (cadr v1))))
            pi1 (_scale perp1 sIn1)
            pi2 (_scale perp2 sIn2))
      ;; Compute cornerCtr from reference choice
      (setq cornerCtr
        (cond
          ((= *axw-ref* "I")
            (_sub refPt (_add (_scale pi1 os) (_scale pi2 os))))
          ((= *axw-ref* "O")
            (_add refPt (_add (_scale pi1 os) (_scale pi2 os))))
          (t refPt)))
      ;; Recompute v1/v2 from cornerCtr for accuracy
      (setq ang1 (angle cornerCtr p1) ang2 (angle cornerCtr p2)
            v1 (list (cos ang1) (sin ang1) 0.0)
            v2 (list (cos ang2) (sin ang2) 0.0)
            perp1 (list (- (sin ang1)) (cos ang1) 0.0)
            perp2 (list (- (sin ang2)) (cos ang2) 0.0)
            sIn1 (_sgn (+ (* (car perp1) (car v2))
                          (* (cadr perp1) (cadr v2))))
            sIn2 (_sgn (+ (* (car perp2) (car v1))
                          (* (cadr perp2) (cadr v1))))
            pi1 (_scale perp1 sIn1)
            pi2 (_scale perp2 sIn2)
            armC1 (_add cornerCtr (_scale v1 halfP))
            armC2 (_add cornerCtr (_scale v2 halfP)))
      (cond
        ((or (<= (distance armC1 p1) (* 2.0 fw))
             (<= (distance armC2 p2) (* 2.0 fw)))
          (princ "\nArms too short for the frame width."))
        (t
          ;; Wall corner fillet intersections
          (setq innerCorn (_isect (_add cornerCtr (_scale pi1 os)) v1
                                  (_add cornerCtr (_scale pi2 os)) v2)
                outerCorn (_isect (_sub cornerCtr (_scale pi1 os)) v1
                                  (_sub cornerCtr (_scale pi2 os)) v2))
          ;; Post quad, centered on cornerCtr (100x100)
          (setq postP1 (_sub cornerCtr (_add (_scale v1 halfP) (_scale v2 halfP)))
                postP2 (_add (_sub cornerCtr (_scale v2 halfP)) (_scale v1 halfP))
                postP3 (_add cornerCtr (_add (_scale v1 halfP) (_scale v2 halfP)))
                postP4 (_add (_sub cornerCtr (_scale v1 halfP)) (_scale v2 halfP)))
          (setq cmde (getvar "CMDECHO"))
          (setvar "CMDECHO" 0)
          (command "_.UNDO" "_BE")
          (setq ents nil)
          (setq ents (cons (_mkpline (list postP1 postP2 postP3 postP4)
                                     *cfg-win-frame*) ents))
          (setq ents (akd:place-corner-arm armC1 p1 fw fd v1 perp1 ents))
          (setq ents (akd:place-corner-arm armC2 p2 fw fd v2 perp2 ents))
          ;; Filleted wall lines (interior + exterior) for each arm
          (if innerCorn
            (progn
              (setq ents (cons (_mkline innerCorn
                                (_add p1 (_scale pi1 os)) *cfg-win-wall*) ents))
              (setq ents (cons (_mkline innerCorn
                                (_add p2 (_scale pi2 os)) *cfg-win-wall*) ents))))
          (if outerCorn
            (progn
              (setq ents (cons (_mkline outerCorn
                                (_sub p1 (_scale pi1 os)) *cfg-win-wall*) ents))
              (setq ents (cons (_mkline outerCorn
                                (_sub p2 (_scale pi2 os)) *cfg-win-wall*) ents))))
          (setq gname (_uniqname "AWIN")
                totalW (+ (distance armC1 p1) (distance armC2 p2)))
          (_mkgroup gname (reverse ents))
          (_tagwin (car ents) totalW 1
            (if *label-on*
              (_pick-lbl-side cornerCtr perp1 *cfg-lbl-off-win*
                              *cfg-lbl-hex-r* t)
              1.0)
            cornerCtr v1 gname)
          (if *label-on* (_win-renum nil *label-batch*))
          (command "_.UNDO" "_E")
          (setvar "CMDECHO" cmde)
          (princ (strcat "\nCorner window created (total width "
                         (rtos totalW 2 0) ")."))))))
  (princ))

(defun hw:extra (kw / v)
  (cond
    ((= kw "Divisions")
      (initget 6)
      (setq v (getint (strcat "\nWindow divisions <" (itoa *win-div*) ">: ")))
      (if v (setq *win-div* v)) t)
    ((= kw "S")
      (setq *win-slide* (not *win-slide*))
      (princ (strcat "\nWindow: " (if *win-slide* "Sliding" "Fixed") ".")) t)
    (t nil)))

(defun hole:prompt2 (label wget extra status)
  (strcat "\n[" (strcase (substr label 1 1)) (substr label 2)
          (if (/= status "") (strcat " | " status) "")
          " | Width: " (rtos (apply wget nil) 2 2)
          " | Placement: " (if (= (hole:getm) "F") "FromWall" "Center")
          (if (= (hole:getm) "F") (strcat " | Gap: " (rtos (hole:getg) 2 2)) "")
          "] Click wall or [Center/FromWall/Width"
          (if (= (hole:getm) "F") "/Gap" "")
          (if (/= extra "") (strcat "/" (vl-string-translate " " "/" extra)) "")
          "]: "))

(defun hole:loop (post label wget wset extra extracb statusfn
                    / inp v done ss ent oldw kws)
  (setvar "CMDECHO" 0)
  (if (not *hole-hinted*)
    (progn
      (princ "\nClick on a LINE or polyline segment; parallel partner auto-detected.")
      (princ "\nType a number at the prompt to set width directly. Loops until Esc/Enter.")
      (setq *hole-hinted* t)))
  (setq done nil)
  (while (not done)
    (setq kws (strcat "Center FromWall Width"
                      (if (= (hole:getm) "F") " Gap" "")
                      (if (/= extra "") (strcat " " extra) "")))
    (initget 128 kws)
    (setq inp (getpoint (hole:prompt2 label wget extra
                          (if statusfn (apply statusfn nil) ""))))
    (cond
      ((null inp) (setq done t))
      ((and (= (type inp) 'STR)
            (or (setq v (distof inp)) (setq v (atof inp)))
            (> v 0))
        (apply wset (list v))
        (princ (strcat "\n" label " width set to " (rtos v 2 2) ".")))
      ((= inp "Center")   (hole:setm "C"))
      ((= inp "FromWall") (hole:setm "F"))
      ((= inp "Gap")
        (setq v (getdist (strcat "\nGap from clicked end <"
                                 (rtos (hole:getg) 2 2) ">: ")))
        (if v (progn (setvar "USERR2" v) (hole:setm "F"))))
      ((= inp "Width")
        (setq v (getdist (strcat "\n" label " width <"
                                 (rtos (apply wget nil) 2 2) ">: ")))
        (if v (apply wset (list v))))
      ((and (= (type inp) 'STR) extracb (apply extracb (list inp))))
      ((listp inp)
        (setq ss (ssget inp '((0 . "LINE,LWPOLYLINE"))))
        (cond
          ((null ss) (princ "\nNo LINE or polyline at that point. Use snap."))
          (t
            (setq ent (ssname ss 0)
                  oldw (getvar "USERR1"))
            (setvar "USERR1" (apply wget nil))
            (setq *akd-cancel* nil)
            (command-s "_.UNDO" "_BE")
            (hole:do ent inp post)
            (command-s "_.UNDO" "_E")
            (if *akd-cancel* (command-s "_.U"))
            (setvar "USERR1" oldw))))))
  (princ))

(defun hole:do (ent pk post / seg1 seg2 p1a p1b p2a p2b dir len w d half
                          ctr1 c1 c2 b1a b1b b2a b2b osm nearEnd mode capLay clay)
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

          (cond
            (*hole-force-ctr*
              (setq ctr1 (hole:proj *hole-force-ctr* p1a dir)))
            ((= mode "F")
              (setq nearEnd (if (< (distance pk p1a) (distance pk p1b)) p1a p1b))
              (setq ctr1 (mapcar '+ nearEnd
                          (hole:scl (if (equal nearEnd p1a) dir
                                      (hole:scl dir -1.0))
                                    (+ d half)))))
            (t
              (setq ctr1 (mapcar '+ p1a (hole:scl dir (/ len 2.0))))))

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
            (t
              (setq capLay (cdr (assoc 8 (entget (car seg1)))))
              (if (equal (car seg1) (car seg2))
                (hole:split-pl-two (car seg1) (cadr seg1) b1a b1b (cadr seg2) b2a b2b)
                (progn (hole:splitseg seg1 b1a b1b)
                       (hole:splitseg seg2 b2a b2b)))
              (setq osm (getvar "OSMODE") clay (getvar "CLAYER"))
              (setvar "OSMODE" 0)
              (setvar "CLAYER" capLay)
              (command-s "_.LINE" b1a b2a "")
              (command-s "_.LINE" b1b b2b "")
              (setvar "CLAYER" clay)
              (setvar "OSMODE" osm)
              (princ "\nHole cut.")
              (if post (apply post (list b1a b1b b2a b2b))))))))))

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

;; -- HH corner hole (HHX) --------------------------------------------
;; Corner-anchored: click corner point, then a point on each adjoining
;; wall to define arm length. On each wall, everything from the corner
;; up to the arm end is removed and a single cap is drawn at the arm end.

;; Trim a LINE so only the portion on the arm-outer side of capPt remains.
;; Direction "positive" = along +armDir. LWPOLYLINE walls fall back to
;; hole:split-line-half equivalent (not implemented — LINE walls only).
(defun hhx:trim-half (ent capPt armDir / d s e ps pe)
  (setq d (entget ent))
  (cond
    ((/= (cdr (assoc 0 d)) "LINE")
      (princ "\nHHX supports LINE walls only for the trim step."))
    (t
      (setq s (cdr (assoc 10 d)) e (cdr (assoc 11 d))
            ps (+ (* (- (car s) (car capPt)) (car armDir))
                  (* (- (cadr s) (cadr capPt)) (cadr armDir)))
            pe (+ (* (- (car e) (car armDir)) 0)  ; recomputed below
                  0))
      (setq pe (+ (* (- (car e) (car capPt)) (car armDir))
                  (* (- (cadr e) (cadr capPt)) (cadr armDir))))
      (cond
        ((and (<= ps 0) (<= pe 0)) (entdel ent))
        ((and (>= ps 0) (>= pe 0)) nil)
        ((< ps 0) (entmod (subst (cons 10 capPt) (assoc 10 d) d)))
        (t        (entmod (subst (cons 11 capPt) (assoc 11 d) d)))))))

(defun hhx:cut-arm (wallEnt refPt armEnd / seg1 seg2 s1 e1 dir len
                                            s2 e2 cap1 cap2 lay
                                            partnerEnt osm clay armDir)
  (setq seg1 (hole:picksegment wallEnt armEnd)
        s1 (nth 2 seg1) e1 (nth 3 seg1)
        dir (mapcar '- e1 s1) len (distance s1 e1)
        dir (list (/ (car dir) len) (/ (cadr dir) len) 0.0)
        seg2 (hole:findpartner seg1 armEnd dir))
  (cond
    ((null seg2) (princ "\nNo parallel partner for wall."))
    (t
      (setq s2 (nth 2 seg2) e2 (nth 3 seg2)
            partnerEnt (car seg2)
            cap1 (hole:proj armEnd s1 dir)
            cap2 (hole:proj armEnd s2 dir))
      ;; Arm direction (from corner outward) projected on wall dir
      (setq armDir
        (if (> (+ (* (- (car armEnd) (car refPt)) (car dir))
                  (* (- (cadr armEnd) (cadr refPt)) (cadr dir))) 0)
          dir
          (list (- (car dir)) (- (cadr dir)) 0.0)))
      (hhx:trim-half wallEnt   cap1 armDir)
      (hhx:trim-half partnerEnt cap2 armDir)
      (setq lay (cdr (assoc 8 (entget wallEnt)))
            osm (getvar "OSMODE") clay (getvar "CLAYER"))
      (setvar "OSMODE" 0) (setvar "CLAYER" lay)
      (command-s "_.LINE" cap1 cap2 "")
      (setvar "CLAYER" clay) (setvar "OSMODE" osm))))
;; Given wall ent and two points, return (center-on-wall-line . width)
;; measured along the wall direction (ignoring perpendicular offset).
(defun hhx:span (ent p1 p2 / d seg s e dir len k1 k2 km)
  (setq d (entget ent))
  (setq seg (if (= (cdr (assoc 0 d)) "LWPOLYLINE")
              (hole:picksegment ent p2)
              (list ent -1 (cdr (assoc 10 d)) (cdr (assoc 11 d)))))
  (setq s (nth 2 seg) e (nth 3 seg)
        dir (mapcar '- e s) len (distance s e)
        dir (list (/ (car dir) len) (/ (cadr dir) len) 0.0)
        k1 (+ (* (- (car p1) (car s)) (car dir))
              (* (- (cadr p1) (cadr s)) (cadr dir)))
        k2 (+ (* (- (car p2) (car s)) (car dir))
              (* (- (cadr p2) (cadr s)) (cadr dir)))
        km (/ (+ k1 k2) 2.0))
  (list (mapcar '+ s (hole:scl dir km)) (abs (- k2 k1))))

(defun hhx:wall-at (pt / eps ss)
  (setq eps 1.0
        ss (ssget "_C"
             (list (- (car pt) eps) (- (cadr pt) eps))
             (list (+ (car pt) eps) (+ (cadr pt) eps))
             '((0 . "LINE,LWPOLYLINE"))))
  (if ss (ssname ss 0)))

;; Delete wall stubs past the corner: any LINE/LWPOLYLINE whose full extent
;; sits within `radius` of refPt (short leftover past the corner).
(defun hhx:kill-stubs (refPt radius layer / ss n i e d ptype verts len ok)
  (setq ss (ssget "_X"
             (list '(0 . "LINE,LWPOLYLINE") (cons 8 layer)))
        n (if ss (sslength ss) 0) i 0)
  (while (< i n)
    (setq e (ssname ss i) d (entget e) ptype (cdr (assoc 0 d)) ok nil)
    (cond
      ((= ptype "LINE")
        (if (and (< (distance (cdr (assoc 10 d)) refPt) radius)
                 (< (distance (cdr (assoc 11 d)) refPt) radius))
          (setq ok t)))
      ((= ptype "LWPOLYLINE")
        (setq verts (hole:pl-verts d))
        (if (and verts (< (length verts) 12))
          (progn
            (setq ok t)
            (foreach v verts
              (if (>= (distance v refPt) radius) (setq ok nil)))))))
    (if ok (entdel e))
    (setq i (1+ i))))

(defun c:HHX ( / refPt p1 p2 ent1 ent2 cmde)
  (setq refPt (getpoint "\nCorner point: "))
  (if refPt
    (setq p1 (getpoint refPt
      "\nOuter end along wall 1 (click or type distance): ")))
  (if p1
    (setq p2 (getpoint refPt
      "\nOuter end along wall 2 (click or type distance): ")))
  (cond
    ((or (null refPt) (null p1) (null p2)) (princ "\nCancelled."))
    ((or (null (setq ent1 (hhx:wall-at p1)))
         (null (setq ent2 (hhx:wall-at p2))))
      (princ "\nNo wall (LINE/LWPOLYLINE) found under one of the arm ends."))
    (t
      (setq cmde (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (command-s "_.UNDO" "_BE")
      (hhx:cut-arm ent1 refPt p1)
      (hhx:cut-arm ent2 refPt p2)
      (command-s "_.UNDO" "_E")
      (setvar "CMDECHO" cmde)
      (princ "\nCorner hole cut.")))
  (princ))

;; ===================================================================
;; CW - Change Width of a placed AKD door/window and adjust its hole.
;; ===================================================================

(defun cw:read-xd (ent / d xd)
  (if (= (type ent) 'ENAME)
    (progn
      (setq d (entget ent (list "ADOOR" "AWIN"))
            xd (if (listp d) (cdr (assoc -3 d))))
      (cond
        ((and (listp xd) (cdr (assoc "ADOOR" xd)))
          (list 'door (cdr (assoc "ADOOR" xd))))
        ((and (listp xd) (cdr (assoc "AWIN" xd)))
          (list 'win (cdr (assoc "AWIN" xd))))))))

;; Given a pick point, find the nearest tagged AKD door/window entity.
(defun cw:nearest-tagged (pk / app ss n i e xd mid d best bestInfo p2d)
  (if (and (listp pk) (>= (length pk) 2)
           (numberp (car pk)) (numberp (cadr pk)))
    (progn
      (setq best 1e99 bestInfo nil
            p2d (list (car pk) (cadr pk) 0.0))
      (foreach app (list "ADOOR" "AWIN")
        (setq ss (ssget "_X" (list (list -3 (list app))))
              n (if ss (sslength ss) 0) i 0)
        (while (< i n)
          (setq e (ssname ss i)
                xd (cdr (assoc app (cdr (assoc -3
                      (entget e (list app))))))
                mid (if (listp xd) (cdr (assoc 1011 xd))))
          (if (and (listp mid) (numberp (car mid)) (numberp (cadr mid)))
            (progn
              (setq d (distance p2d mid))
              (if (< d best)
                (setq best d
                      bestInfo (list
                        (if (= app "ADOOR") 'door 'win) xd e app)))))
          (setq i (1+ i))))
      bestInfo)))

;; Return a representative point of an entity for nearest-tagged lookup.
(defun cw:get-pk-of (ent / d)
  (setq d (entget ent))
  (cond ((cdr (assoc 10 d))) ((cdr (assoc 11 d))) (t '(0.0 0.0 0.0))))

;; Pick a door/window: pre-selected first, otherwise entsel.
(defun cw:acquire (prompt / preSet preSs ent pk sel)
  (setq preSet (ssgetfirst) preSs (cadr preSet))
  (cond
    ((and preSs (> (sslength preSs) 0))
      (setq ent (ssname preSs 0) pk (cw:get-pk-of ent))
      (sssetfirst nil nil)
      (list ent pk))
    (t
      (setq sel (entsel prompt))
      (if (and sel (listp sel) (= (type (car sel)) 'ENAME)) sel))))

;; List every entity in a named group by walking the ACAD_GROUP dictionary.
(defun cw:group-ents (gname / nod gd data)
  (setq nod (namedobjdict))
  (if (setq gd (dictsearch nod "ACAD_GROUP"))
    (if (setq data (dictsearch (cdr (assoc -1 gd)) gname))
      (mapcar 'cdr (vl-remove-if-not '(lambda (p) (= (car p) 340)) data)))))

(defun cw:scale (v s) (mapcar '(lambda (x) (* x s)) v))
(defun cw:add (a b) (mapcar '+ a b))
(defun cw:sub (a b) (mapcar '- a b))
(defun cw:mid-pt (a b) (mapcar '(lambda (x y) (/ (+ x y) 2.0)) a b))
(defun cw:eq (a b) (equal a b 1e-3))

;; Return LINE entities in the drawing whose midpoint equals target.
(defun cw:find-cap (target / ss n i e d a b out)
  (setq ss (ssget "_X" '((0 . "LINE"))) n (if ss (sslength ss) 0) i 0 out nil)
  (while (< i n)
    (setq e (ssname ss i) d (entget e)
          a (cdr (assoc 10 d)) b (cdr (assoc 11 d)))
    (if (cw:eq (cw:mid-pt a b) target) (setq out e))
    (setq i (1+ i)))
  out)

;; Move a matching endpoint of ent from oldPt to newPt (LINE or LWPOLYLINE).
(defun cw:move-endpoint (ent oldPt newPt / d hit newd)
  (setq d (entget ent))
  (cond
    ((= "LINE" (cdr (assoc 0 d)))
      (cond
        ((cw:eq (cdr (assoc 10 d)) oldPt)
          (entmod (subst (cons 10 newPt) (assoc 10 d) d)) t)
        ((cw:eq (cdr (assoc 11 d)) oldPt)
          (entmod (subst (cons 11 newPt) (assoc 11 d) d)) t)))
    ((= "LWPOLYLINE" (cdr (assoc 0 d)))
      (setq hit nil
            newd (mapcar
                   '(lambda (x)
                      (cond
                        ((and (= (car x) 10) (not hit)
                              (cw:eq (list (car (cdr x)) (cadr (cdr x)) 0.0)
                                     oldPt))
                          (setq hit t)
                          (cons 10 (list (car newPt) (cadr newPt))))
                        (t x)))
                   d))
      (if hit (entmod newd)) hit)))

;; Move a wall-stub endpoint at oldPt to newPt. Excludes the cap line itself.
(defun cw:move-wall-end (oldPt newPt except / ss n i e)
  (setq ss (ssget "_X" '((0 . "LINE,LWPOLYLINE")))
        n (if ss (sslength ss) 0) i 0)
  (while (< i n)
    (setq e (ssname ss i))
    (if (/= e except) (cw:move-endpoint e oldPt newPt))
    (setq i (1+ i))))

;; Resize one cap (a LINE entity): move its endpoints and the two wall stubs.
(defun cw:resize-cap (cap dv / d a b a2 b2)
  (setq d (entget cap)
        a (cdr (assoc 10 d)) b (cdr (assoc 11 d))
        a2 (cw:add a dv) b2 (cw:add b dv))
  (cw:move-wall-end a a2 cap)
  (cw:move-wall-end b b2 cap)
  (entmod (subst (cons 10 a2) (assoc 10 d)
            (subst (cons 11 b2) (assoc 11 d) d))))

(defun cw:delete-group (gname app / e)
  (foreach e (cw:group-ents gname) (if e (entdel e)))
  (foreach lbl (_labels-for-gname gname
                 (if (= app "ADOOR") "ADOORLBL" "AWINLBL"))
    (entdel lbl)))


(defun c:CW ( / picked ent pk xd1 info kind xd oldW mid vdir gname typ div
                 newW halfOld halfNew capA capB eA eB basePt inp offset
                 midNew newCapA newCapB shiftA shiftB p1new p2new cmde)
  (setq picked (cw:acquire "\nSelect door or window to resize: "))
  (cond
    ((null picked) (princ "\nCancelled."))
    ((progn
       (setq ent (car picked) pk (cadr picked))
       (setq xd1 (cw:read-xd ent))
       (setq info (if xd1 xd1 (cw:nearest-tagged pk)))
       (null info))
      (princ "\nNo AKD door or window found."))
    (t
      (setq kind (car info) xd (cadr info)
            oldW (cdr (assoc 1040 xd))
            mid  (cdr (assoc 1011 xd))
            vdir (cdr (assoc 1013 xd))
            gname (_extract-gname xd))
      (if (eq kind 'door)
        (setq typ (cond ((= (cdr (assoc 1070 xd)) 2) "D")
                        ((= (cdr (assoc 1070 xd)) 3) "G")
                        ((= (cdr (assoc 1070 xd)) 4) "P")
                        (t "S"))
              div (cdr (assoc 1071 xd)))
        (setq div (cdr (assoc 1070 xd))))
      (if (eq typ "P")
        (progn (princ "\nCW cannot resize pocket doors — delete and redraw.") (exit)))
      (initget "Base")
      (setq inp (getdist mid
        (strcat "\n[Resize | Type: " (if (eq kind 'door) "Door" "Window")
                " | Current: " (rtos oldW 2 2)
                "] New width or [Base] <" (rtos oldW 2 2) ">: ")))
      (cond
        ((eq inp "Base")
          (setq basePt (getpoint "\nBase point (stays fixed): "))
          (if basePt
            (setq newW (getdist basePt "\nNew width: "))))
        ((numberp inp) (setq newW inp)))
      (cond
        ((or (null newW) (<= newW 0)) (princ "\nCancelled."))
        (t
          (setq halfOld (/ oldW 2.0) halfNew (/ newW 2.0)
                capA (cw:sub mid (cw:scale vdir halfOld))
                capB (cw:add mid (cw:scale vdir halfOld))
                eA (cw:find-cap capA) eB (cw:find-cap capB))
          (cond
            ((or (null eA) (null eB))
              (princ "\nCap lines not found — cannot resize hole."))
            (t
              (if basePt
                (progn
                  (setq offset (+ (* (- (car basePt) (car mid)) (car vdir))
                                  (* (- (cadr basePt) (cadr mid))
                                     (cadr vdir))))
                  (if (>= offset 0)
                    (setq midNew (cw:sub basePt (cw:scale vdir halfNew)))
                    (setq midNew (cw:add basePt (cw:scale vdir halfNew)))))
                (setq midNew mid))
              (setq newCapA (cw:sub midNew (cw:scale vdir halfNew))
                    newCapB (cw:add midNew (cw:scale vdir halfNew))
                    shiftA (cw:sub newCapA capA)
                    shiftB (cw:sub newCapB capB))
              (setq cmde (getvar "CMDECHO"))
              (setvar "CMDECHO" 0)
              (command-s "_.UNDO" "_BE")
              (cw:resize-cap eA shiftA)
              (cw:resize-cap eB shiftB)
              (cw:delete-group gname (if (eq kind 'door) "ADOOR" "AWIN"))
              (cond
                ((eq kind 'door)
                  (setq *door-type* typ)
                  (if (= typ "G") (setq *slide-div* div)))
                (t (setq *win-div* div)))
              (setq p1new (cw:sub midNew (cw:scale vdir halfNew))
                    p2new (cw:add midNew (cw:scale vdir halfNew)))
              (if (eq kind 'door)
                (akd:place-door p1new p2new)
                (akd:place-window p1new p2new))
              (command-s "_.UNDO" "_E")
              (setvar "CMDECHO" cmde)
              (princ (strcat "\nResized from " (rtos oldW 2 2)
                             " to " (rtos newW 2 2) "."))))))))
  (princ))

;; ===================================================================
;; EW - Erase a placed AKD door/window and repair the wall hole.
;; ===================================================================

(defun ew:has-end (ent pt / d verts)
  (setq d (entget ent))
  (cond
    ((= "LINE" (cdr (assoc 0 d)))
      (or (cw:eq (cdr (assoc 10 d)) pt)
          (cw:eq (cdr (assoc 11 d)) pt)))
    ((= "LWPOLYLINE" (cdr (assoc 0 d)))
      (setq verts (hole:pl-verts d))
      (or (cw:eq (car verts) pt)
          (cw:eq (last verts) pt)))))

(defun ew:find-stub (pt except layer / ss n i e best)
  (setq ss (ssget "_X" (list '(0 . "LINE,LWPOLYLINE") (cons 8 layer)))
        n (if ss (sslength ss) 0) i 0 best nil)
  (while (< i n)
    (setq e (ssname ss i))
    (if (and (not best) (not (member e except)) (ew:has-end e pt))
      (setq best e))
    (setq i (1+ i)))
  best)

(defun ew:merge-line (s1 s2 pt1 pt2 / d1 d2 lay a1 b1 a2 b2 o1 o2)
  (setq d1 (entget s1) d2 (entget s2)
        lay (cdr (assoc 8 d1))
        a1 (cdr (assoc 10 d1)) b1 (cdr (assoc 11 d1))
        a2 (cdr (assoc 10 d2)) b2 (cdr (assoc 11 d2))
        o1 (if (cw:eq a1 pt1) b1 a1)
        o2 (if (cw:eq a2 pt2) b2 a2))
  (entdel s1) (entdel s2)
  (entmakex (list '(0 . "LINE") (cons 8 lay)
                  (cons 10 o1) (cons 11 o2))))

(defun ew:merge-pl (s1 s2 pt1 pt2 / d1 d2 lay v1 v2 merged closed)
  (setq d1 (entget s1) d2 (entget s2)
        lay (cdr (assoc 8 d1))
        v1 (hole:pl-verts d1) v2 (hole:pl-verts d2))
  (if (cw:eq (car v1) pt1) (setq v1 (reverse v1)))
  (if (cw:eq (last v2) pt2) (setq v2 (reverse v2)))
  (setq v1 (reverse (cdr (reverse v1)))
        v2 (cdr v2)
        merged (append v1 v2)
        closed (cw:eq (car merged) (last merged)))
  (if closed (setq merged (reverse (cdr (reverse merged)))))
  (entdel s1) (entdel s2)
  (hole:make-pl merged closed lay))

(defun ew:merge (s1 s2 pt1 pt2)
  (if (and (= "LINE" (cdr (assoc 0 (entget s1))))
           (= "LINE" (cdr (assoc 0 (entget s2)))))
    (ew:merge-line s1 s2 pt1 pt2)
    (ew:merge-pl   s1 s2 pt1 pt2)))

;; Close a wrap-around polyline: drop first + last vertex (both cap pts), close.
(defun ew:close-pl (ent pt1 pt2 / d verts lay newVerts)
  (setq d (entget ent)
        verts (hole:pl-verts d)
        lay (cdr (assoc 8 d))
        newVerts (reverse (cdr (reverse (cdr verts)))))
  (entdel ent)
  (if (>= (length newVerts) 2)
    (hole:make-pl newVerts t lay)))

(defun ew:rejoin (s1 s2 pt1 pt2 label)
  (cond
    ((and s1 s2 (not (eq s1 s2))) (ew:merge s1 s2 pt1 pt2))
    ((and s1 (ew:has-end s1 pt1) (ew:has-end s1 pt2)) (ew:close-pl s1 pt1 pt2))
    (t (princ (strcat "\n" label " stubs not paired — gap left.")))))

(defun ew:do-one (info / kind xd oldW mid vdir gname app halfOld
                          capA capB eA eB dA dB aw1 aw2 bw1 bw2
                          s1a s1b s2a s2b wallLay)
  (setq kind (car info) xd (cadr info)
        oldW (cdr (assoc 1040 xd))
        mid  (cdr (assoc 1011 xd))
        vdir (cdr (assoc 1013 xd))
        gname (_extract-gname xd)
        app (if (eq kind 'door) "ADOOR" "AWIN")
        halfOld (/ oldW 2.0)
        capA (cw:sub mid (cw:scale vdir halfOld))
        capB (cw:add mid (cw:scale vdir halfOld))
        eA (cw:find-cap capA) eB (cw:find-cap capB))
  (cond
    ((not (and eA eB))
      (princ (strcat "\n[" (if gname gname "?")
                     "] caps not found — deleting group only."))
      (cw:delete-group gname app))
    (t
      (setq dA (entget eA) dB (entget eB)
            aw1 (cdr (assoc 10 dA)) aw2 (cdr (assoc 11 dA))
            bw1 (cdr (assoc 10 dB)) bw2 (cdr (assoc 11 dB))
            wallLay (cdr (assoc 8 dA)))
      (setq s1a (ew:find-stub aw1 (list eA eB) wallLay))
      (setq s1b (ew:find-stub bw1 (list eA eB s1a) wallLay))
      (setq s2a (ew:find-stub aw2 (list eA eB s1a s1b) wallLay))
      (setq s2b (ew:find-stub bw2 (list eA eB s1a s1b s2a) wallLay))
      (entdel eA) (entdel eB)
      (ew:rejoin s1a s1b aw1 bw1 "Wall1")
      (ew:rejoin s2a s2b aw2 bw2 "Wall2")
      (cw:delete-group gname app))))

;; ---- AKD door/window group lookup (avoid nearest-tagged mis-hits) --
(defun ew:akd-groups ( / gd res nm)
  (setq gd (dictsearch (namedobjdict) "ACAD_GROUP") nm nil res nil)
  (foreach p gd
    (cond
      ((= (car p) 3) (setq nm (cdr p)))
      ((= (car p) 350)
        (if (and nm
                 (or (wcmatch (strcase nm) "AWIN*") (wcmatch (strcase nm) "ADOOR*"))
                 (not (wcmatch (strcase nm) "*LBL*")))
          (setq res (cons (cons nm
                     (mapcar 'cdr (vl-remove-if-not
                        '(lambda (x) (= (car x) 340)) (entget (cdr p)))))
                    res)))
        (setq nm nil))))
  res)

(defun ew:group-of-ent (e groups / found)
  (setq found nil)
  (foreach g groups
    (if (and (not found) (member e (cdr g))) (setq found g)))
  found)

(defun ew:info-from-group (g / found)
  (setq found nil)
  (foreach m (cdr g)
    (if (and (not found) (entget m))
      (setq found (cw:read-xd m))))
  found)

;; ---- AKDColumn (AKCOL*) group erase support ------------------------
;; ponytail: erase-only. We delete the AKCOL group + AWALL POINTs in its
;; bbox, but do NOT rejoin the walls the column split. Undo/redraw if needed.

(defun col:akcol-groups ( / gd res nm)
  (setq gd (dictsearch (namedobjdict) "ACAD_GROUP") nm nil res nil)
  (foreach p gd
    (cond
      ((= (car p) 3) (setq nm (cdr p)))
      ((= (car p) 350)
        (if (and nm (wcmatch (strcase nm) "AKCOL*"))
          (setq res (cons (cons nm
                     (mapcar 'cdr (vl-remove-if-not
                        '(lambda (x) (= (car x) 340)) (entget (cdr p)))))
                    res)))
        (setq nm nil))))
  res)

(defun col:pl-bbox (ent / d verts xs ys)
  (setq d (entget ent)
        verts (mapcar 'cdr (vl-remove-if-not
                 '(lambda (x) (= (car x) 10)) d)))
  (if verts
    (progn (setq xs (mapcar 'car verts) ys (mapcar 'cadr verts))
           (list (list (apply 'min xs) (apply 'min ys))
                 (list (apply 'max xs) (apply 'max ys))))))

(defun col:pt-in-bb (pt bb)
  (and pt bb
       (>= (car pt)  (caar bb)) (<= (car pt)  (car (cadr bb)))
       (>= (cadr pt) (cadar bb)) (<= (cadr pt) (cadr (cadr bb)))))

(defun col:point-of (e / d)
  (setq d (entget e))
  (cond ((cdr (assoc 10 d))) ((cdr (assoc 11 d))) (t nil)))

;; Return (gname . members) for AKCOL group that owns ent, else nil.
(defun col:group-of-ent (e / groups found)
  (setq groups (col:akcol-groups) found nil)
  (foreach g groups
    (if (and (not found) (member e (cdr g))) (setq found g)))
  found)

;; Given a point, find AKCOL group whose S-COLUMN polyline bbox contains it.
(defun col:group-at-point (pt / groups found bb)
  (setq groups (col:akcol-groups) found nil)
  (foreach g groups
    (if (not found)
      (foreach m (cdr g)
        (if (and (not found) (entget m)
                 (= (cdr (assoc 0 (entget m))) "LWPOLYLINE")
                 (= (cdr (assoc 8 (entget m))) "S-COLUMN"))
          (progn (setq bb (col:pl-bbox m))
                 (if (col:pt-in-bb pt bb) (setq found g)))))))
  found)

;; Resolve a picked entity to an AKCOL group, else nil.
(defun col:resolve (e / d lay)
  (setq d (entget e) lay (cdr (assoc 8 d)))
  (cond
    ((= lay "S-COLUMN") (col:group-of-ent e))
    ((= lay "A-WALL-DATA") (col:group-at-point (col:point-of e)))
    (t nil)))

;; Erase every member of the AKCOL group + AWALL POINTs on A-WALL-DATA
;; whose coord lies inside the group's S-COLUMN polyline bbox.
(defun col:erase-group (g / members bb ss n i e)
  (setq members (cdr g) bb nil)
  (foreach m members
    (if (and (not bb) (entget m)
             (= (cdr (assoc 0 (entget m))) "LWPOLYLINE")
             (= (cdr (assoc 8 (entget m))) "S-COLUMN"))
      (setq bb (col:pl-bbox m))))
  (foreach m members (if (entget m) (entdel m)))
  (if bb
    (progn
      (setq ss (ssget "_X" (list '(8 . "A-WALL-DATA") '(-3 ("AWALL"))))
            n (if ss (sslength ss) 0) i 0)
      (while (< i n)
        (setq e (ssname ss i))
        (if (and (entget e) (col:pt-in-bb (col:point-of e) bb))
          (entdel e))
        (setq i (1+ i))))))

(defun c:EW ( / preSet preSs ss n i e info gn seen infos cmde count
              cg colgs colcount akdGroups g)
  (setq preSet (ssgetfirst) preSs (cadr preSet))
  (cond
    ((and preSs (> (sslength preSs) 0))
      (setq ss preSs) (sssetfirst nil nil))
    (t
      (princ "\nSelect doors/windows to remove: ")
      (setq ss (ssget))))
  (cond
    ((null ss) (princ "\nNothing selected."))
    (t
      (setq n (sslength ss) i 0 seen nil infos nil colgs nil)
      (setq akdGroups (ew:akd-groups))
      (while (< i n)
        (setq e (ssname ss i))
        (setq cg (col:resolve e))
        (cond
          (cg
            (if (not (member (car cg) (mapcar 'car colgs)))
              (setq colgs (cons cg colgs))))
          (t
            (setq g (ew:group-of-ent e akdGroups))
            (cond
              (g (setq info (ew:info-from-group g)))
              (t
                (setq info (cw:read-xd e))
                (if (and (null info) (= n 1))
                  (setq info (cw:nearest-tagged (cw:get-pk-of e))))))
            (if info
              (progn
                (setq gn (_extract-gname (cadr info)))
                (if (and gn (not (member gn seen)))
                  (setq seen (cons gn seen)
                        infos (cons info infos)))))))
        (setq i (1+ i)))
      (cond
        ((and (null infos) (null colgs))
          (princ "\nNo AKD doors/windows/columns in selection."))
        (t
          (setq cmde (getvar "CMDECHO"))
          (setvar "CMDECHO" 0)
          (command-s "_.UNDO" "_BE")
          (setq count 0 colcount 0)
          (foreach cg colgs
            (col:erase-group cg)
            (setq colcount (1+ colcount)))
          (foreach info infos
            (ew:do-one info)
            (setq count (1+ count)))
          (command-s "_.UNDO" "_E")
          (setvar "CMDECHO" cmde)
          (princ (strcat "\n" (itoa count) " door/win, "
                         (itoa colcount) " column(s) removed."))))))
  (princ))

;; ===================================================================
;; RH - Repair Hole. Pick two cap lines; merges wall stubs back.
;; ===================================================================

(defun rh:find-stub (pt except / ss n i e best)
  (setq ss (ssget "_X" '((0 . "LINE,LWPOLYLINE")))
        n (if ss (sslength ss) 0) i 0 best nil)
  (while (and (< i n) (not best))
    (setq e (ssname ss i))
    (if (and (not (member e except)) (ew:has-end e pt)) (setq best e))
    (setq i (1+ i)))
  best)

(defun rh:pair-b (stub near b1 b2 / d ptype verts far dx dy len d1 d2 o1 o2)
  (cond
    ((null stub) b1)
    (t
      (setq d (entget stub) ptype (cdr (assoc 0 d)))
      (cond
        ((= ptype "LINE")
          (setq far (if (cw:eq near (cdr (assoc 10 d)))
                      (cdr (assoc 11 d)) (cdr (assoc 10 d)))))
        (t
          (setq verts (hole:pl-verts d)
                far (if (cw:eq near (car verts)) (last verts) (car verts)))))
      (setq dx (- (car near) (car far))
            dy (- (cadr near) (cadr far))
            len (sqrt (+ (* dx dx) (* dy dy))))
      (if (zerop len) b1
        (progn
          (setq dx (/ dx len) dy (/ dy len)
                d1 (mapcar '- b1 near) d2 (mapcar '- b2 near)
                o1 (abs (- (* (car d1) dy) (* (cadr d1) dx)))
                o2 (abs (- (* (car d2) dy) (* (cadr d2) dx))))
          (if (< o1 o2) b1 b2))))))

(defun c:RH ( / capA capB dA dB a1 a2 b1 b2
                sa1 sa2 sb1 sb2 pA1 pA2 stubB1 stubB2 cmde)
  (setq capA (car (entsel "\nPick FIRST cap line: ")))
  (setq capB (if capA (car (entsel "\nPick OPPOSITE cap line: "))))
  (cond
    ((or (null capA) (null capB)) (princ "\nCancelled."))
    (t
      (setq cmde (getvar "CMDECHO")) (setvar "CMDECHO" 0)
      (command-s "_.UNDO" "_BE")
      (setq dA (entget capA) dB (entget capB)
            a1 (cdr (assoc 10 dA)) a2 (cdr (assoc 11 dA))
            b1 (cdr (assoc 10 dB)) b2 (cdr (assoc 11 dB))
            sa1 (rh:find-stub a1 (list capA capB))
            sa2 (rh:find-stub a2 (list capA capB))
            sb1 (rh:find-stub b1 (list capA capB))
            sb2 (rh:find-stub b2 (list capA capB))
            pA1 (rh:pair-b sa1 a1 b1 b2)
            pA2 (if (equal pA1 b1) b2 b1)
            stubB1 (if (equal pA1 b1) sb1 sb2)
            stubB2 (if (equal pA1 b1) sb2 sb1))
      (entdel capA) (entdel capB)
      (ew:rejoin sa1 stubB1 a1 pA1 "Face 1")
      (ew:rejoin sa2 stubB2 a2 pA2 "Face 2")
      (command-s "_.UNDO" "_E")
      (setvar "CMDECHO" cmde)
      (princ "\nHole repaired.")))
  (princ))

;; ===================================================================
;; DDW — thicken a line drawing into door/window frames + mullions
;; ===================================================================
;; Pick outer closed polyline + interior mullion lines.
;;   - Outer polyline offsets INWARD by fw (frame band).
;;   - Interior lines are replaced by TWO offset lines fw/2 each side of
;;     centerline, extended to the inner boundary (clean butt joint).
;; ponytail: mullion-mullion crossings are NOT auto-filleted. If two
;; mullions cross, run FILLET R=0 manually on the four offset pairs.

(defun ddw:perp (p1 p2 d / dx dy L)
  (setq dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1))
        L  (sqrt (+ (* dx dx) (* dy dy))))
  (if (> L 1e-9)
    (list (* (- dy) (/ d L)) (* dx (/ d L)) 0.0)
    '(0.0 0.0 0.0)))

(defun ddw:add (a b) (list (+ (car a) (car b)) (+ (cadr a) (cadr b)) 0.0))
(defun ddw:sub (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b)) 0.0))

(defun c:DDW ( / bnd inPt fw ss n i e d p1 p2 v lay innerB cmde os offA offB)
  (initget 6)
  (setq fw (getdist (strcat "\nFrame width <" (rtos *cfg-win-fw* 2 2) ">: ")))
  (if (null fw) (setq fw *cfg-win-fw*))
  (princ "\nSelect outer boundary (closed polyline): ")
  (setq bnd (car (entsel)))
  (cond
    ((or (null bnd)
         (/= (cdr (assoc 0 (entget bnd))) "LWPOLYLINE"))
      (princ "\nNeed a closed LWPOLYLINE."))
    (t
      (setq inPt (getpoint "\nPick a point INSIDE the boundary (offset direction): "))
      (princ "\nSelect interior mullion lines (Enter for none): ")
      (setq ss (ssget '((0 . "LINE"))))
      (setq cmde (getvar "CMDECHO") os (getvar "OSMODE"))
      (setvar "CMDECHO" 0) (setvar "OSMODE" 0)
      (command-s "_.UNDO" "_BE")
      (command-s "_.OFFSET" fw bnd inPt "")
      (setq innerB (entlast))
      (if ss
        (progn
          (setq n (sslength ss) i 0)
          (while (< i n)
            (setq e   (ssname ss i)
                  d   (entget e)
                  p1  (cdr (assoc 10 d))
                  p2  (cdr (assoc 11 d))
                  lay (cdr (assoc 8 d))
                  v   (ddw:perp p1 p2 (/ fw 2.0)))
            (_mkline (ddw:add p1 v) (ddw:add p2 v) (cons lay 256))
            (setq offA (entlast))
            (_mkline (ddw:sub p1 v) (ddw:sub p2 v) (cons lay 256))
            (setq offB (entlast))
            ;; extend both ends of both offset lines to innerB
            (command-s "_.EXTEND" innerB "" p1 p2 offA offB "")
            (entdel e)
            (setq i (1+ i)))))
      (command-s "_.UNDO" "_E")
      (setvar "OSMODE" os) (setvar "CMDECHO" cmde)
      (princ "\nDDW done.")))
  (princ))

;; ===================================================================
;; SET — interactive customization of *cfg-* globals
;; ===================================================================
(setq *cfg-set-cats*
  '(("Window" (("Fw" *cfg-win-fw* R) ("Fd" *cfg-win-fd* R) ("Wall" *cfg-win-wall-off* R)))
    ("Door"   (("Fw" *cfg-door-fw* R) ("Fd" *cfg-door-fd* R) ("Panel" *cfg-door-panel-t* R)))
    ("Slide"  (("Fw" *cfg-slide-fw* R) ("Panel" *cfg-slide-p* R) ("Ext" *cfg-slide-ext* R) ("Wall" *cfg-slide-wall-off* R)))
    ("Pocket" (("Opening" *cfg-pd-opening* R) ("Jamb" *cfg-pd-jamb* R) ("Jambh" *cfg-pd-jamb-h* R)
               ("Grey" *cfg-pd-grey-t* R) ("Leaf" *cfg-pd-leaf-t* R) ("Stud" *cfg-pd-stud-t* R)
               ("Gyp" *cfg-pd-gyp-t* R) ("Studw" *cfg-pd-stud-w* R) ("Lip" *cfg-pd-lipext* R)
               ("Groove" *cfg-pd-groove* R)))
    ("Label"  (("Hex" *cfg-lbl-hex-r* R) ("Cir" *cfg-lbl-cir-r* R) ("Height" *cfg-lbl-h* R)
               ("Offwin" *cfg-lbl-off-win* R) ("Offdoor" *cfg-lbl-off-door* R)))
    ("Table"  (("Titleh" *cfg-tbl-titleH* R) ("Hdrh" *cfg-tbl-hdrH* R) ("Rowh" *cfg-tbl-rowH* R)
               ("Txth" *cfg-tbl-txtH* R) ("Titletxth" *cfg-tbl-titleTxtH* R)))
    ("Layer"  (("Winframe" *cfg-win-frame* L) ("Winglass" *cfg-win-glass* L) ("Winwall" *cfg-win-wall* L)
               ("Doorframe" *cfg-door-frame* L) ("Doorpanel" *cfg-door-panel* L) ("Doorarc" *cfg-door-arc* L)
               ("Doorwall" *cfg-door-wall* L) ("Lblshape" *cfg-lbl-shape* L) ("Lbltext" *cfg-lbl-text* L)
               ("Dimtext" *cfg-dim-text* L) ("Pdgrey" *cfg-pd-grey* L)))))

(defun set:joinkw (lst / s)
  (setq s "")
  (foreach e lst (setq s (strcat s (if (= s "") "" " ") (car e))))
  s)

(defun set:cli (/ catkws cat items itemkws var entry sym typ cur newv newL newC)
  (setq catkws (set:joinkw *cfg-set-cats*))
  (initget catkws)
  (setq cat (getkword (strcat "\nCategory [" catkws "]: ")))
  (if cat
    (progn
      (setq items (cadr (assoc cat *cfg-set-cats*))
            itemkws (set:joinkw items))
      (initget itemkws)
      (setq var (getkword (strcat "\n" cat " [" itemkws "]: ")))
      (if var
        (progn
          (setq entry (assoc var items) sym (cadr entry) typ (caddr entry) cur (eval sym))
          (cond
            ((= typ 'R)
             (setq newv (getreal (strcat "\n" var " [" (rtos cur 2 2) "]: ")))
             (if newv (progn (set sym newv)
                             (princ (strcat "\n" (vl-symbol-name sym) " = " (rtos newv 2 2))))))
            ((= typ 'L)
             (setq newL (getstring T (strcat "\n" var " layer [" (car cur) "]: ")))
             (if (= newL "") (setq newL (car cur)))
             (setq newC (getint (strcat "\n" var " color [" (itoa (cdr cur)) "]: ")))
             (if (null newC) (setq newC (cdr cur)))
             (set sym (cons newL newC))
             (princ (strcat "\n" (vl-symbol-name sym) " = \"" newL "\" color " (itoa newC)))))))))
  (princ))

;; -------- DCL pop-up version --------
(setq *set:dlg-cat* nil *set:dlg-var* nil)

(defun set:dlg-cur ( / items entry sym typ cur)
  (setq items (cadr (assoc *set:dlg-cat* *cfg-set-cats*))
        entry (assoc *set:dlg-var* items)
        sym   (cadr entry) typ (caddr entry) cur (eval sym))
  (cond
    ((= typ 'R)
      (set_tile "cur" (rtos cur 2 2))
      (set_tile "val" (rtos cur 2 2))
      (set_tile "lyr" "") (set_tile "clr" "")
      (mode_tile "val" 0) (mode_tile "lyr" 1) (mode_tile "clr" 1)
      (mode_tile "val" 2))
    ((= typ 'L)
      (set_tile "cur" (strcat (car cur) " / " (itoa (cdr cur))))
      (set_tile "val" "")
      (set_tile "lyr" (car cur)) (set_tile "clr" (itoa (cdr cur)))
      (mode_tile "val" 1) (mode_tile "lyr" 0) (mode_tile "clr" 0)
      (mode_tile "lyr" 2))))

(defun set:dlg-fill-vars ( / items)
  (setq items (cadr (assoc *set:dlg-cat* *cfg-set-cats*)))
  (start_list "var") (foreach it items (add_list (car it))) (end_list)
  (setq *set:dlg-var* (car (car items)))
  (set_tile "var" "0")
  (set:dlg-cur))

(defun set:dlg-cat-changed (idx)
  (setq *set:dlg-cat* (car (nth (atoi idx) *cfg-set-cats*)))
  (set:dlg-fill-vars))

(defun set:dlg-var-changed (idx / items)
  (setq items (cadr (assoc *set:dlg-cat* *cfg-set-cats*)))
  (setq *set:dlg-var* (car (nth (atoi idx) items)))
  (set:dlg-cur))

(defun set:dlg-apply ( / items entry sym typ vs lyr clr)
  (setq items (cadr (assoc *set:dlg-cat* *cfg-set-cats*))
        entry (assoc *set:dlg-var* items)
        sym (cadr entry) typ (caddr entry))
  (cond
    ((= typ 'R)
      (setq vs (get_tile "val"))
      (if (and vs (/= vs "") (distof vs))
        (progn (set sym (distof vs))
               (princ (strcat "\n" (vl-symbol-name sym) " = " (rtos (distof vs) 2 2))))))
    ((= typ 'L)
      (setq lyr (get_tile "lyr") clr (get_tile "clr"))
      (if (and lyr (/= lyr "") clr (/= clr ""))
        (progn (set sym (cons lyr (atoi clr)))
               (princ (strcat "\n" (vl-symbol-name sym) " = \"" lyr "\" color " clr)))))))

(defun set:write-dcl ( / p f)
  (setq p (vl-filename-mktemp "akdset" nil ".dcl") f (open p "w"))
  (foreach ln
    '("akd_set : dialog { label = \"AKDDoorWin Settings\"; spacer;"
      " : row {"
      "  : boxed_column { label = \"Category\";"
      "     : list_box { key = \"cat\"; width = 16; height = 14; } }"
      "  : boxed_column { label = \"Setting\";"
      "     : list_box { key = \"var\"; width = 18; height = 14; } }"
      "  : boxed_column { label = \"Value\";"
      "     : text { label = \"Current:\"; }"
      "     : text { key = \"cur\"; width = 22; } spacer;"
      "     : edit_box { label = \"New value:\"; key = \"val\"; width = 18; } spacer;"
      "     : text { label = \"For Layer settings:\"; }"
      "     : edit_box { label = \"Layer name:\"; key = \"lyr\"; width = 18; }"
      "     : edit_box { label = \"Color (1-256):\"; key = \"clr\"; width = 18; } }"
      " }"
      " spacer; ok_cancel; }")
    (write-line ln f))
  (close f)
  p)

(defun c:SET ( / dcl_id ok dclfile)
  (setq dclfile (findfile "AKDDoorWin.dcl"))
  (if (null dclfile) (setq dclfile (set:write-dcl)))
  (cond
    ((null dclfile) (set:cli))
    (t
      (setq dcl_id (load_dialog dclfile))
      (cond
        ((or (not dcl_id) (< dcl_id 0)) (set:cli))
        ((not (new_dialog "akd_set" dcl_id))
          (unload_dialog dcl_id) (set:cli))
        (t
          (start_list "cat")
          (foreach c *cfg-set-cats* (add_list (car c)))
          (end_list)
          (setq *set:dlg-cat* (car (car *cfg-set-cats*)))
          (set_tile "cat" "0")
          (set:dlg-fill-vars)
          (action_tile "cat" "(set:dlg-cat-changed $value)")
          (action_tile "var" "(set:dlg-var-changed $value)")
          (action_tile "accept" "(set:dlg-apply) (done_dialog 1)")
          (action_tile "cancel" "(done_dialog 0)")
          (setq ok (start_dialog))
          (unload_dialog dcl_id)))))
  (princ))

;; Optional per-user overrides: place AKDDoorWin.cfg alongside the .lsp
;; with (setq *cfg-...* ...) lines. Loaded last so it wins over defaults.
(if (findfile "AKDDoorWin.cfg")
    (load (findfile "AKDDoorWin.cfg")))

(princ (strcat "\nAKDDoorWin loaded. AD/AW/ACW=hole+door/win/curtain, ADD/AWW/ACWW=2-click, AXW/HHX=corner win/hole, CW=resize, EW=erase+repair, SET=settings. ["
               (hole:getm) " W=" (rtos (hole:getw) 2 2)
               " G=" (rtos (hole:getg) 2 2) "]"))
(princ)
