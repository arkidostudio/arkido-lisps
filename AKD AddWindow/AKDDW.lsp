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
(setq *cfg-lbl-text*      '("X-TAGS & SYMBOLS" . 2))  ; text (yellow)

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
                    (cons 1000 (strcat "G:" (if gname gname "")))))))))

(defun _tag-winlbl (ent gname)
  (regapp "AWINLBL")
  (entmod
    (append (entget ent)
      (list (list -3 (list "AWINLBL"
                     (cons 1000 "AWINLBL")
                     (cons 1000 (strcat "G:" (if gname gname "")))))))))

(defun c:AW ( / fw fd os p1 p2 ang v perp p1i p2i
                 nd n step i pc edges e1 e2 cmde
                 ents width mid gname )
  (setq fw *cfg-win-fw* fd *cfg-win-fd* os *cfg-win-wall-off*)
  (while
    (progn
      (initget "D")
      (setq p1 (getpoint
                 (strcat "\nFirst window point or [D=divisions] (current: "
                         (itoa *win-div*) "): ")))
      (if (eq p1 "D")
        (progn
          (initget 6)
          (setq nd (getint
                     (strcat "\nNumber of divisions <" (itoa *win-div*) ">: ")))
          (if nd (setq *win-div* nd))
          t))))
  (if p1 (setq p2 (getpoint p1 "\nSecond window point: ")))
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
      (setq edges (list p1i))
      (if (> *win-div* 1)
        (progn
          (setq n    (1- *win-div*)
                step (/ (distance p1i p2i) (float *win-div*))
                i    1)
          (while (<= i n)
            (setq pc (_add p1i (_scale v (* step i))))
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
        (setq edges (cddr edges)))
      (setq ents (cons (_mkline (_add p1 (_scale perp os))
                                (_add p2 (_scale perp os)) *cfg-win-wall*) ents))
      (setq ents (cons (_mkline (_sub p1 (_scale perp os))
                                (_sub p2 (_scale perp os)) *cfg-win-wall*) ents))
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
                                keys key kmap hex txt lg)
  (regapp "AWIN")
  (regapp "AWINLBL")
  (if (null ss) (setq ss (ssget "_X" '((-3 ("AWIN"))))))
  (if (null ss) (progn (princ "\nNo windows found.") (exit)))
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
          gn  (_extract-gname xd))
    (if (and w mid vd
             (or (null batch) (= (fix bt) batch)))
      (setq items (cons (list w dv mid vd gn sd e) items)))
    (setq i (1+ i)))
  (foreach it items
    (setq gn (nth 4 it))
    (if gn (foreach lbl (_labels-for-gname gn "AWINLBL") (entdel lbl))))
  (setq keys nil)
  (foreach it items
    (setq key (list (fix (+ 0.5 (car it))) (cadr it)))
    (if (not (member key keys)) (setq keys (cons key keys))))
  (setq keys (vl-sort keys
    '(lambda (a b)
       (cond ((> (car a) (car b)) t)
             ((< (car a) (car b)) nil)
             (t (> (cadr a) (cadr b)))))))
  (setq n 1 kmap nil)
  (foreach k keys (setq kmap (cons (cons k n) kmap) n (1+ n)))
  (setq lh *cfg-lbl-h* off *cfg-lbl-off-win*)
  (foreach it items
    (setq w   (car   it) dv  (cadr it) mid (caddr it) vd (cadddr it)
          gn  (nth 4 it) sd (cond ((nth 5 it)) (1))
          key (cdr (assoc (list (fix (+ 0.5 w)) dv) kmap))
          ang (atan (cadr vd) (car vd))
          pt  (list (+ (car mid) (* (- (sin ang)) off sd))
                    (+ (cadr mid) (* (cos ang) off sd)) 0.0))
    (setq hex (_mkhex pt *cfg-lbl-hex-r* *cfg-lbl-shape*)
          txt (_mktext pt lh 0.0 (strcat "W" (itoa key)) *cfg-lbl-text*))
    (_tag-winlbl hex gn) (_tag-winlbl txt gn)
    (_tag-lblnum (nth 6 it) "AWIN" (strcat "W" (itoa key)))
    (setq lg (_uniqname "AWINLBL"))
    (_mkgroup lg (list hex txt)))
  (princ (strcat "\nRenumbered " (itoa (length items))
                 " window(s) into " (itoa (length keys)) " type(s)."))
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
                    (cons 1070 (cond ((eq typ "D") 2) ((eq typ "G") 3) (1)))
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
(defun c:AD ( / fw fd pt-thk p1 p2 ang v perp mid width
                  fx fy g m mv mx my c ghost-fn
                  cmde ents gname old-err tk )
  (setq old-err *error*
        *error* (lambda (msg) (redraw) (setq *error* old-err) (princ)))
  (setq fw *cfg-door-fw* fd *cfg-door-fd* pt-thk *cfg-door-panel-t*)
  (while
    (progn
      (initget "S D G P")
      (setq p1 (getpoint
                 (strcat "\nFirst door point or [Single/Double/sliGing/Panels] ("
                         *door-type*
                         (if (eq *door-type* "G")
                           (strcat ", " (itoa *slide-div*) "p") "")
                         "): ")))
      (cond
        ((eq p1 "S") (setq *door-type* "S") t)
        ((eq p1 "D") (setq *door-type* "D") t)
        ((eq p1 "G")
         (setq *door-type* "G")
         (initget 6)
         (setq tk (getint
                    (strcat "\nSliding panels <" (itoa *slide-div*) ">: ")))
         (if tk (setq *slide-div* tk)) t)
        ((eq p1 "P")
         (initget 6)
         (setq tk (getint
                    (strcat "\nSliding panels <" (itoa *slide-div*) ">: ")))
         (if tk (setq *slide-div* tk)) t))))
  (if p1 (setq p2 (getpoint p1 "\nSecond door point: ")))
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
        ((null fx) (princ "\nCancelled."))
        (t
          (setvar "CMDECHO" 0)
          (command "_.UNDO" "_BE")
          (setq ents (cons (_rect p1 (_add p1 (_scale v fw)) perp fd *cfg-door-frame*) ents))
          (setq ents (cons (_rect p2 (_sub p2 (_scale v fw)) perp fd *cfg-door-frame*) ents))
          (cond
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
  (if (null ss) (progn (princ "\nNo doors found.") (exit)))
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
                 " door(s) into " (itoa (length keys)) " type(s)."))
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
