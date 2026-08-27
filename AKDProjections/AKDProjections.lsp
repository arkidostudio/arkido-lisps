;; AKDProjections.lsp  -  Arkido Elevation / Section generator
;; Reads AWIN/ADOOR xdata (AKDDoorWin) and AWALL xdata (AKDWall/WW) and
;; projects elevations / sections from the plan.
;;
;; Commands:
;;   DE   Opening elevation - pick a single tagged window/door.
;;   WE   View elevation    - pick two points along the elevation face;
;;                            mouse-flip picks viewer side. Draws all
;;                            walls parallel to that face (within
;;                            *dwe-far-tol* perpendicular) with their
;;                            openings, ground + roof slabs.
;;   SECT Section           - pick two points for cut line; crosses all
;;                            tagged walls, solid-fills cut wall bodies
;;                            with sill/head voids where the cut passes
;;                            through openings.
;;   QQ   Repeat last ELEV or SECT.

;; ---- session defaults ---------------------------------------------
(if (null *dwe-win-h*)    (setq *dwe-win-h*    1500.0))
(if (null *dwe-win-sill*) (setq *dwe-win-sill*  900.0))
(if (null *dwe-door-h*)   (setq *dwe-door-h*   2100.0))
(if (null *dwe-offset*)   (setq *dwe-offset*   3000.0))
(if (null *dwe-gnd-ext*)  (setq *dwe-gnd-ext*   500.0))
(if (null *dwe-txt-h*)    (setq *dwe-txt-h*     150.0))
(if (null *dwe-wall-h*)   (setq *dwe-wall-h*   2700.0))
(if (null *dwe-fe-mode*)  (setq *dwe-fe-mode*  "Elev"))  ; Elev | Sect
(if (null *dwe-corr*)     (setq *dwe-corr*      500.0))  ; corridor half-width for opening pickup
(if (null *dwe-slab-t*)   (setq *dwe-slab-t*    150.0))  ; ground/roof slab thickness in section
(if (null *dwe-far-tol*)  (setq *dwe-far-tol*  5000.0))  ; how far behind cut to scan for visible walls
(setq *dwe-face-tol* 300.0)  ; slop past wall thk/2 for face-of-wall match (always refresh)
(if (null *dwe-dim-on*) (setq *dwe-dim-on* nil))  ; SECT/SEA dimensions off by default

(defun c:DIMTOG ()
  (setq *dwe-dim-on* (not *dwe-dim-on*))
  (princ (strcat "\nSECT/SEA dimensions: " (if *dwe-dim-on* "ON" "OFF")))
  (princ))

;; ---- layers (edit these to remap output layers) --------------
;; Format: '("LAYER-NAME" . default-color-if-created).
;; Entities are drawn ByLayer, so layer color/linetype/lineweight
;; wins if the layer already exists in the drawing.
(setq *dwe-lyr-hatch* '("X-HATCH"          . 8))  ; cut wall body, slab fills
(setq *dwe-lyr-elv1*  '("ELV-1"            . 7))  ; cut wall outlines
(setq *dwe-lyr-elv2*  '("ELV-2"            . 3))  ; bg wall outline, cut/elev opening frames
(setq *dwe-lyr-elv3*  '("ELV-3"            . 4))  ; opening face detail, mullions, door panel divisions
(setq *dwe-lyr-elv5*  '("ELV-5"            . 5))  ; slab outlines, ground line
(setq *dwe-lyr-sect*  '("X-TAGS & SYMBOLS" . 1))  ; persistent section line
(setq *dwe-lyr-frame* '("0"                . 4))  ; temp section frame (cyan + HIDDEN)
(setq *dwe-lyr-anno*  '("X-ANNO"           . 2))  ; DE opening dimension text

;; Component -> layer map. Change the RHS to remap a single component.
(setq *dwe-lyr-cut-fill*    *dwe-lyr-hatch*)      ; cut wall body hatch
(setq *dwe-lyr-cut-out*     *dwe-lyr-elv1*)       ; cut wall outline + sill/head strips
(setq *dwe-lyr-bg-wall*     *dwe-lyr-elv2*)       ; background wall outline
(setq *dwe-lyr-win*         *dwe-lyr-elv2*)       ; window frame outline
(setq *dwe-lyr-glass*       *dwe-lyr-elv3*)       ; mullions/glass divisions
(setq *dwe-lyr-door*        *dwe-lyr-elv2*)       ; door frame outline
(setq *dwe-lyr-panel*       *dwe-lyr-elv3*)       ; door panel divisions
(setq *dwe-lyr-slab-fill*   *dwe-lyr-hatch*)      ; slab hatch
(setq *dwe-lyr-slab-out*    *dwe-lyr-elv5*)       ; slab outline
(setq *dwe-lyr-gnd*         *dwe-lyr-elv5*)       ; ground reference line
(setq *dwe-lyr-txt*         *dwe-lyr-anno*)       ; DE dimension text
;; legacy aliases used by generic helpers
(setq *dwe-lyr-wall*        *dwe-lyr-elv2*)
(setq *dwe-lyr-fill*        *dwe-lyr-hatch*)

;; ---- helpers ------------------------------------------------------
(defun _dwe-add (a b) (mapcar '+ a b))
(defun _dwe-scl (a k) (mapcar '(lambda (x) (* x k)) a))

(defun _dwe-lyr (spec / n cmde)
  (setq n (car spec))
  (if (null (tblsearch "LAYER" n))
    (progn
      (setq cmde (getvar "CMDECHO")) (setvar "CMDECHO" 0)
      (command "_.-layer" "_M" n "_C" (itoa (cdr spec)) "" "")
      (setvar "CMDECHO" cmde)))
  n)

(defun _dwe-line (a b spec)
  (entmake (list (cons 0 "LINE")
                 (cons 8 (_dwe-lyr spec))
                 (cons 10 (list (car a) (cadr a) 0.0))
                 (cons 11 (list (car b) (cadr b) 0.0)))))

;; Load a linetype from acad.lin/acadiso.lin if not present.
(defun _dwe-ltype (lt / cmde)
  (if (null (tblsearch "LTYPE" lt))
    (progn
      (setq cmde (getvar "CMDECHO")) (setvar "CMDECHO" 0)
      (command "_.-linetype" "_L" lt "acadiso.lin" "")
      (if (null (tblsearch "LTYPE" lt))
        (command "_.-linetype" "_L" lt "acad.lin" ""))
      (setvar "CMDECHO" cmde)))
  lt)

(defun _dwe-line-lt (a b spec lt)
  (_dwe-ltype lt)
  (entmake (list (cons 0 "LINE")
                 (cons 8 (_dwe-lyr spec))
                 (cons 6 lt)
                 (cons 10 (list (car a) (cadr a) 0.0))
                 (cons 11 (list (car b) (cadr b) 0.0)))))

(defun _dwe-rect (bl xh yh w h spec / br tr tl)
  (setq br (_dwe-add bl (_dwe-scl xh w))
        tr (_dwe-add br (_dwe-scl yh h))
        tl (_dwe-add bl (_dwe-scl yh h)))
  (_dwe-plrect bl br tr tl spec))

(defun _dwe-text (p xh h str spec / ang)
  (setq ang (atan (cadr xh) (car xh)))
  (entmake (list (cons 0 "TEXT")
                 (cons 8 (_dwe-lyr spec))
                 (cons 10 (list (car p) (cadr p) 0.0))
                 (cons 40 h)
                 (cons 1 str)
                 (cons 50 ang)
                 (cons 72 1)     ; center
                 (cons 73 2)     ; top
                 (cons 11 (list (car p) (cadr p) 0.0)))))

;; ---- xdata read ---------------------------------------------------
(defun _dwe-xd (ent app) (cdr (assoc app (cdr (assoc -3 (entget ent (list app)))))))

(defun _dwe-lblnum (xd / it out)
  (foreach it xd
    (if (and (= (car it) 1000)
             (>= (strlen (cdr it)) 2)
             (= (substr (cdr it) 1 2) "L:"))
      (setq out (substr (cdr it) 3))))
  out)

;; ---- ghost + flip -------------------------------------------------
(defun _dwe-grseg (a b) (grdraw a b 1 -1))

(defun _dwe-ghost (mid xh yh w h off / bl br tr tl)
  (setq bl (_dwe-add (_dwe-add mid (_dwe-scl yh off)) (_dwe-scl xh (- (/ w 2.0))))
        br (_dwe-add bl (_dwe-scl xh w))
        tr (_dwe-add br (_dwe-scl yh h))
        tl (_dwe-add bl (_dwe-scl yh h)))
  (_dwe-grseg bl br) (_dwe-grseg br tr)
  (_dwe-grseg tr tl) (_dwe-grseg tl bl))

;; grread loop: cursor position across wall flips side. Returns 1.0 or -1.0.
;; Click to accept, Esc to abort (returns nil).
(defun _dwe-pick-side (mid vdir perp w h / side xh yh g m mv my ok abort)
  (setq side 1.0 xh vdir abort nil ok nil
        yh (_dwe-scl perp side))
  (_dwe-ghost mid xh yh w h *dwe-offset*)
  (princ "\nMove mouse to flip side, click to place: ")
  (while (not (or ok abort))
    (setq g (vl-catch-all-apply 'grread (list t 13 0)))
    (cond
      ((vl-catch-all-error-p g) (setq abort t))
      ((= (car g) 5)
       (setq m  (cadr g)
             mv (mapcar '- m mid)
             my (+ (* (car mv) (car perp)) (* (cadr mv) (cadr perp)))
             side (if (< my 0) -1.0 1.0)
             yh (_dwe-scl perp side))
       (redraw)
       (_dwe-ghost mid xh yh w h *dwe-offset*))
      ((= (car g) 3) (setq ok t))
      ((and (= (car g) 2) (= (cadr g) 27)) (setq abort t))))
  (redraw)
  (if abort nil side))

;; ---- window elevation ---------------------------------------------
(defun _dwe-win (xd / w div side mid vdir perp xh yh base sill h i x)
  (setq w    (cdr (assoc 1040 xd))
        div  (cond ((cdr (assoc 1070 xd))) (1))
        mid  (cdr (assoc 1011 xd))
        vdir (cdr (assoc 1013 xd))
        perp (list (- (cadr vdir)) (car vdir) 0.0)
        xh   vdir
        h    *dwe-win-h*
        sill *dwe-win-sill*
        side (_dwe-pick-side mid vdir perp w (+ sill h)))
  (if (null side) (exit))
  (setq yh   (_dwe-scl perp side)
        base (_dwe-add mid (_dwe-scl yh *dwe-offset*)))
  ;; ground
  (_dwe-line (_dwe-add base (_dwe-scl xh (- 0 (/ w 2.0) *dwe-gnd-ext*)))
             (_dwe-add base (_dwe-scl xh (+ (/ w 2.0) *dwe-gnd-ext*)))
             *dwe-lyr-gnd*)
  ;; frame at sill
  (setq base (_dwe-add base (_dwe-scl yh sill)))
  (setq base (_dwe-add base (_dwe-scl xh (- (/ w 2.0)))))
  (_dwe-rect base xh yh w h *dwe-lyr-win*)
  ;; mullions
  (setq i 1)
  (while (< i div)
    (setq x (* (/ w (float div)) i))
    (_dwe-line (_dwe-add base (_dwe-scl xh x))
               (_dwe-add base (_dwe-add (_dwe-scl xh x) (_dwe-scl yh h)))
               *dwe-lyr-glass*)
    (setq i (1+ i)))
  ;; label under ground
  (_dwe-text (_dwe-add (_dwe-add mid (_dwe-scl yh *dwe-offset*))
                       (_dwe-scl yh (- 0 (* 1.5 *dwe-txt-h*))))
             xh *dwe-txt-h*
             (strcat (rtos w 2 0) " x " (rtos h 2 0)
                     "  SILL " (rtos sill 2 0))
             *dwe-lyr-txt*))

;; ---- door elevation -----------------------------------------------
(defun _dwe-door (xd / w typ dv side mid vdir perp xh yh base h i x)
  (setq w    (cdr (assoc 1040 xd))
        typ  (cond ((cdr (assoc 1070 xd))) (1))
        dv   (cond ((cdr (assoc 1071 xd))) (1))
        mid  (cdr (assoc 1011 xd))
        vdir (cdr (assoc 1013 xd))
        perp (list (- (cadr vdir)) (car vdir) 0.0)
        xh   vdir
        h    *dwe-door-h*
        side (_dwe-pick-side mid vdir perp w h))
  (if (null side) (exit))
  (setq yh   (_dwe-scl perp side)
        base (_dwe-add mid (_dwe-scl yh *dwe-offset*)))
  ;; ground
  (_dwe-line (_dwe-add base (_dwe-scl xh (- 0 (/ w 2.0) *dwe-gnd-ext*)))
             (_dwe-add base (_dwe-scl xh (+ (/ w 2.0) *dwe-gnd-ext*)))
             *dwe-lyr-gnd*)
  (setq base (_dwe-add base (_dwe-scl xh (- (/ w 2.0)))))
  (_dwe-rect base xh yh w h *dwe-lyr-door*)
  (cond
    ;; double: split leaves at center
    ((= typ 2)
     (_dwe-line (_dwe-add base (_dwe-scl xh (/ w 2.0)))
                (_dwe-add base (_dwe-add (_dwe-scl xh (/ w 2.0)) (_dwe-scl yh h)))
                *dwe-lyr-panel*))
    ;; sliding: dv panels
    ((= typ 3)
     (setq i 1)
     (while (< i dv)
       (setq x (* (/ w (float dv)) i))
       (_dwe-line (_dwe-add base (_dwe-scl xh x))
                  (_dwe-add base (_dwe-add (_dwe-scl xh x) (_dwe-scl yh h)))
                  *dwe-lyr-panel*)
       (setq i (1+ i)))))
  (_dwe-text (_dwe-add (_dwe-add mid (_dwe-scl yh *dwe-offset*))
                       (_dwe-scl yh (- 0 (* 1.5 *dwe-txt-h*))))
             xh *dwe-txt-h*
             (strcat (rtos w 2 0) " x " (rtos h 2 0))
             *dwe-lyr-txt*))

;; ---- main command -------------------------------------------------
(defun _dwe-prompt ()
  (strcat "\nSelect window/door "
          "[winH/dorH/Sill/Offset/Gnd/Txt]"
          "  winH=" (rtos *dwe-win-h* 2 0)
          " dorH=" (rtos *dwe-door-h* 2 0)
          " sill=" (rtos *dwe-win-sill* 2 0)
          " off="  (rtos *dwe-offset* 2 0)
          ": "))

(defun _dwe-getd (msg cur / v)
  (setq v (getdist (strcat "\n" msg " <" (rtos cur 2 0) ">: ")))
  (if v v cur))

(defun c:DE ( / pk ent xd done)
  (setq done nil)
  (while (not done)
    (initget "winH dorH Sill Offset Gnd Txt")
    (setq pk (entsel (_dwe-prompt)))
    (cond
      ((null pk) (setq done t))
      ((eq pk "winH")   (setq *dwe-win-h*    (_dwe-getd "Window height" *dwe-win-h*)))
      ((eq pk "dorH")   (setq *dwe-door-h*   (_dwe-getd "Door height"   *dwe-door-h*)))
      ((eq pk "Sill")   (setq *dwe-win-sill* (_dwe-getd "Window sill"   *dwe-win-sill*)))
      ((eq pk "Offset") (setq *dwe-offset*   (_dwe-getd "Offset from plan" *dwe-offset*)))
      ((eq pk "Gnd")    (setq *dwe-gnd-ext*  (_dwe-getd "Ground extension" *dwe-gnd-ext*)))
      ((eq pk "Txt")    (setq *dwe-txt-h*    (_dwe-getd "Text height"      *dwe-txt-h*)))
      ((listp pk)
       (setq ent (car pk))
       (cond
         ((setq xd (_dwe-xd ent "AWIN"))  (_dwe-win xd))
         ((setq xd (_dwe-xd ent "ADOOR")) (_dwe-door xd))
         (t (princ "\nNot a tagged AKDDoorWin entity."))))))
  (princ))

;; ====================================================================
;;                  F E   -   F A C E   E L E V A T I O N
;; ====================================================================
;; ponytail: axis-aligned rect only; skewed walls need direction from openings.

(defun _dwe-solid (a b c d spec)
  ;; SOLID vertex order is a Z: 10=bl 11=br 12=tl 13=tr
  (entmake (list (cons 0 "SOLID")
                 (cons 8 (_dwe-lyr spec))
                 (cons 10 (list (car a) (cadr a) 0.0))
                 (cons 11 (list (car b) (cadr b) 0.0))
                 (cons 12 (list (car d) (cadr d) 0.0))
                 (cons 13 (list (car c) (cadr c) 0.0)))))

;; Point-in-frame test. Frame is (xmin ymin xmax ymax).
(defun _dwe-in-frame (p fr)
  (and (>= (car p) (car fr)) (<= (car p) (caddr fr))
       (>= (cadr p) (cadr fr)) (<= (cadr p) (cadddr fr))))

;; Closed LWPOLYLINE rectangle through 4 corners (a b c d in order).
(defun _dwe-plrect (a b c d spec)
  (entmake (list (cons 0 "LWPOLYLINE")
                 (cons 100 "AcDbEntity")
                 (cons 8 (_dwe-lyr spec))
                 (cons 100 "AcDbPolyline")
                 (cons 90 4)
                 (cons 70 1)
                 (cons 10 (list (car a) (cadr a)))
                 (cons 10 (list (car b) (cadr b)))
                 (cons 10 (list (car c) (cadr c)))
                 (cons 10 (list (car d) (cadr d))))))

;; Arrow at `mid` pointing in yh direction, size sz. grdraw only.
(defun _dwe-arrow (mid xh yh sz / tip l r)
  (setq tip (_dwe-add mid (_dwe-scl yh sz))
        l   (_dwe-add tip (_dwe-add (_dwe-scl xh (- (/ sz 3.0)))
                                    (_dwe-scl yh (- (/ sz 3.0)))))
        r   (_dwe-add tip (_dwe-add (_dwe-scl xh (/ sz 3.0))
                                    (_dwe-scl yh (- (/ sz 3.0))))))
  (_dwe-grseg mid tip)
  (_dwe-grseg tip l)
  (_dwe-grseg tip r))

;; Ghost rectangle: bl + xh*len wide, + yh*h tall.
(defun _dwe-rect-ghost (bl xh yh len h / br tr tl)
  (setq br (_dwe-add bl (_dwe-scl xh len))
        tr (_dwe-add br (_dwe-scl yh h))
        tl (_dwe-add bl (_dwe-scl yh h)))
  (_dwe-grseg bl br) (_dwe-grseg br tr)
  (_dwe-grseg tr tl) (_dwe-grseg tl bl))

;; Two-step pick: (1) arrow tracks cursor, click locks view side;
;; (2) placement rubber-band ghost, constrained perpendicular to view line.
;; Returns (side . bl) or nil.
(defun _dwe-pick-view (p1 vdir perp len wall-h
                       / mid sz side lock-side place-side yh g m mv my off bl abort ok)
  (setq mid   (_dwe-add p1 (_dwe-scl vdir (/ len 2.0)))
        sz    (max 300.0 (/ len 8.0))
        side  1.0
        yh    (_dwe-scl perp side)
        abort nil ok nil)
  ;; arrow points opposite drawing side (toward what is being viewed)
  (_dwe-arrow mid vdir yh sz)
  (princ "\nMove cursor to flip arrow (view side), click to lock: ")
  (while (not (or ok abort))
    (setq g (vl-catch-all-apply 'grread (list t 13 0)))
    (cond
      ((vl-catch-all-error-p g) (setq abort t))
      ((= (car g) 5)
       (setq m  (cadr g)
             mv (mapcar '- m mid)
             my (+ (* (car mv) (car perp)) (* (cadr mv) (cadr perp)))
             side (if (< my 0) -1.0 1.0)
             yh   (_dwe-scl perp side))
       (redraw)
       (_dwe-arrow mid vdir yh sz))
      ((= (car g) 3) (setq ok t))
      ((and (= (car g) 2) (= (cadr g) 27)) (setq abort t))))
  (redraw)
  (if abort
    nil
    (progn
      (setq lock-side (- side)
            place-side lock-side
            bl (getpoint "\nPick bottom-left basepoint: "))
      (if bl
        (cons place-side bl)
        nil))))

;; Collect AWIN/ADOOR entities whose midpoint sits on the wall line segment
;; from wstart along wdir for `len`, within ± *dwe-corr* perpendicular.
;; Returns list of (kind xd along) sorted by along.
(defun _dwe-on-wall (wstart wdir len perp / out ss i e xd mid kind
                     dx dy along across)
  (setq out nil)
  (foreach app '("AWIN" "ADOOR")
    (regapp app)
    (setq ss (ssget "_X" (list (list -3 (list app)))))
    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (setq e   (ssname ss i)
                xd  (_dwe-xd e app)
                mid (cdr (assoc 1011 xd)))
          (if mid
            (progn
              (setq dx     (- (car mid) (car wstart))
                    dy     (- (cadr mid) (cadr wstart))
                    along  (+ (* dx (car wdir)) (* dy (cadr wdir)))
                    across (abs (+ (* dx (car perp)) (* dy (cadr perp)))))
              (if (and (>= along 0.0) (<= along len)
                       (<= across *dwe-corr*))
                (setq kind (if (equal app "AWIN") 'win 'door)
                      out  (cons (list kind xd along) out)))))
          (setq i (1+ i))))))
  (vl-sort out '(lambda (a b) (< (caddr a) (caddr b)))))

;; Draw one opening in the wall face at local along-wall position "along-mid".
;; Returns (x0 x1 sill head) — extent along wall + vertical band it occupies,
;; measured in local coordinates (from bl of wall rect).
(defun _dwe-fe-open (kind xd bl xh yh along-mid wall-h / w x0 x1 sill head p
                     div dv typ i x)
  (setq w (cdr (assoc 1040 xd))
        x0 (- along-mid (/ w 2.0))
        x1 (+ along-mid (/ w 2.0)))
  (cond
    ((eq kind 'win)
     (setq div  (cond ((cdr (assoc 1070 xd))) (1))
           sill *dwe-win-sill*
           head (+ sill *dwe-win-h*)
           p    (_dwe-add (_dwe-add bl (_dwe-scl xh x0)) (_dwe-scl yh sill)))
     (_dwe-rect p xh yh w *dwe-win-h* *dwe-lyr-win*)
     (setq i 1)
     (while (< i div)
       (setq x (* (/ w (float div)) i))
       (_dwe-line (_dwe-add p (_dwe-scl xh x))
                  (_dwe-add p (_dwe-add (_dwe-scl xh x) (_dwe-scl yh *dwe-win-h*)))
                  *dwe-lyr-glass*)
       (setq i (1+ i))))
    (t
     (setq typ  (cond ((cdr (assoc 1070 xd))) (1))
           dv   (cond ((cdr (assoc 1071 xd))) (1))
           sill 0.0
           head *dwe-door-h*
           p    (_dwe-add bl (_dwe-scl xh x0)))
     (_dwe-rect p xh yh w *dwe-door-h* *dwe-lyr-door*)
     (cond
       ((= typ 2)
        (_dwe-line (_dwe-add p (_dwe-scl xh (/ w 2.0)))
                   (_dwe-add p (_dwe-add (_dwe-scl xh (/ w 2.0)) (_dwe-scl yh *dwe-door-h*)))
                   *dwe-lyr-panel*))
       ((= typ 3)
        (setq i 1)
        (while (< i dv)
          (setq x (* (/ w (float dv)) i))
          (_dwe-line (_dwe-add p (_dwe-scl xh x))
                     (_dwe-add p (_dwe-add (_dwe-scl xh x) (_dwe-scl yh *dwe-door-h*)))
                     *dwe-lyr-panel*)
          (setq i (1+ i)))))))
  (list x0 x1 sill head))

;; Fill wall band with SOLID quads around openings (Section mode).
;; opens = list of (x0 x1 sill head) sorted by x0.
(defun _dwe-fe-fill (bl xh yh len wall-h opens / cursor o x0 x1 sill head
                     _quad p1 p2 p3 p4)
  (defun _quad (a b h1 h2)
    (setq p1 (_dwe-add (_dwe-add bl (_dwe-scl xh a)) (_dwe-scl yh h1))
          p2 (_dwe-add (_dwe-add bl (_dwe-scl xh b)) (_dwe-scl yh h1))
          p3 (_dwe-add (_dwe-add bl (_dwe-scl xh b)) (_dwe-scl yh h2))
          p4 (_dwe-add (_dwe-add bl (_dwe-scl xh a)) (_dwe-scl yh h2)))
    (_dwe-solid p1 p2 p3 p4 *dwe-lyr-fill*))
  (setq cursor 0.0)
  (foreach o opens
    (setq x0 (car o) x1 (cadr o) sill (caddr o) head (cadddr o))
    ;; solid wall strip before this opening
    (if (> x0 cursor) (_quad cursor x0 0.0 wall-h))
    ;; strip below opening (sill wall)
    (if (> sill 0.0) (_quad x0 x1 0.0 sill))
    ;; strip above opening (head wall)
    (if (< head wall-h) (_quad x0 x1 head wall-h))
    (setq cursor x1))
  (if (< cursor len) (_quad cursor len 0.0 wall-h)))

;; Draw a face elevation from wall p1->p2 viewed from `side` (+/-1 across perp),
;; using current *dwe-fe-mode* and *dwe-wall-h*.
(defun _dwe-fe-draw (p1 p2 side / len wdir perp wstart mid xh yh bl opens dxv dyv)
  (setq dxv (- (car p2)  (car p1))
        dyv (- (cadr p2) (cadr p1))
        len (distance p1 p2))
  (if (< len 1.0) (progn (princ "\nWall too short.") (exit)))
  (setq wdir   (list (/ dxv len) (/ dyv len) 0.0)
        perp   (list (- (cadr wdir)) (car wdir) 0.0)
        wstart p1
        mid    (_dwe-add wstart (_dwe-scl wdir (/ len 2.0)))
        xh     wdir
        yh     (_dwe-scl perp (float side))
        bl     (_dwe-add (_dwe-add mid (_dwe-scl yh *dwe-offset*))
                         (_dwe-scl xh (- (/ len 2.0))))
        opens  (_dwe-on-wall wstart wdir len perp))
  (_dwe-rect bl xh yh len *dwe-wall-h* *dwe-lyr-wall*)
  (setq opens (mapcar
    '(lambda (o) (_dwe-fe-open (car o) (cadr o) bl xh yh (caddr o) *dwe-wall-h*))
    opens))
  (if (equal *dwe-fe-mode* "Sect")
    (_dwe-fe-fill bl xh yh len *dwe-wall-h* opens))
  (_dwe-line (_dwe-add bl (_dwe-scl xh (- *dwe-gnd-ext*)))
             (_dwe-add bl (_dwe-scl xh (+ len *dwe-gnd-ext*)))
             *dwe-lyr-gnd*)
  (princ (strcat "\nFace elevation placed ("
                 (if (equal *dwe-fe-mode* "Sect") "section" "elevation")
                 ", " (itoa (length opens)) " openings).")))

;; ====================================================================
;;    E L E V   /   S E C T   -   read from tagged AWALL entities
;; ====================================================================

;; Return (thk h base p1 p2) for a tagged wall entity, or nil.
(defun _dwe-wall-info (ent / xd nums pts)
  (setq xd (_dwe-xd ent "AWALL") nums nil pts nil)
  (if xd
    (progn
      (foreach it xd
        (cond ((= (car it) 1040) (setq nums (cons (cdr it) nums)))
              ((= (car it) 1011) (setq pts  (cons (cdr it) pts)))))
      (setq nums (reverse nums) pts (reverse pts))
      (if (and (>= (length nums) 3) (>= (length pts) 2))
        (list (car nums) (cadr nums) (caddr nums)
              (car pts)  (cadr pts))))))

;; Segment-segment intersection in 2D. Returns intersection point or nil.
(defun _dwe-seg-int (a b c d / rx ry sx sy rxs qx qy tt uu)
  (setq rx  (- (car b) (car a))  ry (- (cadr b) (cadr a))
        sx  (- (car d) (car c))  sy (- (cadr d) (cadr c))
        rxs (- (* rx sy) (* ry sx)))
  (if (equal rxs 0.0 1e-9)
    nil
    (progn
      (setq qx (- (car c) (car a))
            qy (- (cadr c) (cadr a))
            tt (/ (- (* qx sy) (* qy sx)) rxs)
            uu (/ (- (* qx ry) (* qy rx)) rxs))
      (if (and (>= tt 0.0) (<= tt 1.0) (>= uu 0.0) (<= uu 1.0))
        (list (+ (car a) (* tt rx)) (+ (cadr a) (* tt ry)) 0.0)))))

;; Distance from point p to segment a-b (both 2D).
(defun _dwe-pt-seg-dist (p a b / dxv dyv l2 tt qx qy)
  (setq dxv (- (car b) (car a))
        dyv (- (cadr b) (cadr a))
        l2  (+ (* dxv dxv) (* dyv dyv)))
  (if (< l2 1e-9)
    (distance p a)
    (progn
      (setq tt (/ (+ (* (- (car p) (car a)) dxv)
                     (* (- (cadr p) (cadr a)) dyv)) l2))
      (setq tt (max 0.0 (min 1.0 tt))
            qx (+ (car a)  (* tt dxv))
            qy (+ (cadr a) (* tt dyv)))
      (distance p (list qx qy 0.0)))))

;; Find AWALL nearest to click point; returns wall-info list or nil.
(defun _dwe-wall-nearest (pt / ss i e info best-d best-info d)
  (setq best-d 1e99 best-info nil)
  (regapp "AWALL")
  (setq ss (ssget "_X" (list (list -3 (list "AWALL")))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) info (_dwe-wall-info e))
        (if info
          (progn
            (setq d (_dwe-pt-seg-dist pt (cadddr info) (nth 4 info)))
            (if (< d best-d)
              (setq best-d d best-info info))))
        (setq i (1+ i)))))
  best-info)

;; ---- WE (view elevation) --------------------------------------------
;; Scan AWALL entities parallel-ish to view-dir and within view-tol
;; perpendicular of the view line through p1 along view-dir.
;; Returns list of (along1 along2 h base) sorted by along1.
(defun _dwe-view-walls (p1 view-dir view-perp view-tol
                        / ss i e info wp1 wp2 thk h base L wdir wdot
                          dv1 dv2 al1 al2 ac1 ac2 out)
  (setq out nil)
  (regapp "AWALL")
  (setq ss (ssget "_X" (list (list -3 (list "AWALL")))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) info (_dwe-wall-info e))
        (if info
          (progn
            (setq thk (car info) h (cadr info) base (caddr info)
                  wp1 (cadddr info) wp2 (nth 4 info)
                  L    (distance wp1 wp2)
                  wdir (list (/ (- (car wp2) (car wp1)) L)
                             (/ (- (cadr wp2) (cadr wp1)) L) 0.0)
                  wdot (abs (+ (* (car wdir) (car view-dir))
                               (* (cadr wdir) (cadr view-dir))))
                  dv1  (mapcar '- wp1 p1)
                  dv2  (mapcar '- wp2 p1)
                  al1  (+ (* (car dv1) (car view-dir))  (* (cadr dv1) (cadr view-dir)))
                  al2  (+ (* (car dv2) (car view-dir))  (* (cadr dv2) (cadr view-dir)))
                  ac1  (+ (* (car dv1) (car view-perp)) (* (cadr dv1) (cadr view-perp)))
                  ac2  (+ (* (car dv2) (car view-perp)) (* (cadr dv2) (cadr view-perp))))
            (if (and (> wdot 0.85)                        ; roughly parallel
                     (<= (abs (/ (+ ac1 ac2) 2.0))       ; wall centerline within
                         (+ (/ thk 2.0) *dwe-face-tol*))) ; thk/2+slop of view line
              ;; Extend along-range by thk/2 at each end so the wall's outer
              ;; face (not its centerline) reaches the perpendicular wall's
              ;; face at corners.
              (setq out (cons (list (- (min al1 al2) (/ thk 2.0))
                                    (+ (max al1 al2) (/ thk 2.0))
                                    h base) out)))))
        (setq i (1+ i)))))
  (vl-sort out '(lambda (a b) (< (car a) (car b)))))

;; Scan all openings within view-tol perpendicular of the view line.
;; Returns (kind along w sill head) list.
(defun _dwe-view-openings (p1 view-dir view-perp view-tol
                           / ss i e xd mid w dv along across
                             kind sill head out)
  (setq out nil)
  (foreach app '("AWIN" "ADOOR")
    (regapp app)
    (setq ss (ssget "_X" (list (list -3 (list app)))))
    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (setq e   (ssname ss i)
                xd  (_dwe-xd e app)
                mid (cdr (assoc 1011 xd))
                w   (cdr (assoc 1040 xd)))
          (if (and mid w)
            (progn
              (setq dv     (mapcar '- mid p1)
                    along  (+ (* (car dv) (car view-dir))  (* (cadr dv) (cadr view-dir)))
                    across (+ (* (car dv) (car view-perp)) (* (cadr dv) (cadr view-perp))))
              (if (< (abs across) view-tol)
                (progn
                  (setq kind (if (equal app "AWIN") 'win 'door))
                  (if (eq kind 'win)
                    (setq sill *dwe-win-sill* head (+ sill *dwe-win-h*))
                    (setq sill 0.0            head *dwe-door-h*))
                  (setq out (cons (list kind along w sill head) out))))))
          (setq i (1+ i))))))
  out)

(defun c:WE ( / p1 p2 L dxv dyv view-dir view-perp view-mid side pick-lock pick-place yh xh bl slab-a slab-b
                walls opens min-a max-a total-len max-h view-tol
                wseg wa wb wh p w-along w-len
                o k ac ww sill head
                gl gr gl2 gr2 rl rr rl2 rr2 roof-y)
  (setq *dwe-last-cmd* 'c:WE)
  (setq p1 (getpoint "\nView plane first point (along the elevation face): "))
  (if (null p1) (exit))
  (setq p2 (getpoint p1 "\nView plane second point: "))
  (if (null p2) (exit))
  (setq L (distance p1 p2))
  (if (< L 1.0) (progn (princ "\nView line too short.") (exit)))
  (setq dxv       (- (car p2) (car p1))
        dyv       (- (cadr p2) (cadr p1))
        view-dir  (list (/ dxv L) (/ dyv L) 0.0)
        view-perp (list (- (cadr view-dir)) (car view-dir) 0.0)
        view-tol  *dwe-far-tol*
        walls (_dwe-view-walls p1 view-dir view-perp view-tol)
        opens (_dwe-view-openings p1 view-dir view-perp view-tol))
  ;; Clip to picked view-line window [0, L]. Walls fully outside are dropped;
  ;; walls that straddle are clipped to the window.
  (setq walls
    (vl-remove nil
      (mapcar
        '(lambda (wseg / a b h base ca cb)
           (setq a (car wseg) b (cadr wseg) h (caddr wseg) base (cadddr wseg)
                 ca (max a 0.0)
                 cb (min b L))
           (if (< ca cb) (list ca cb h base)))
        walls)))
  (setq opens
    (vl-remove-if
      '(lambda (o) (or (< (cadr o) 0.0) (> (cadr o) L)))
      opens))
  (if (null walls)
    (progn (princ (strcat "\nNo tagged walls on the view line (face-tol="
                          (rtos *dwe-face-tol* 2 0)
                          "). Pick along a wall face, or (setq *dwe-face-tol* N)."))
           (exit)))
  (setq max-h 0.0)
  (foreach wseg walls (setq max-h (max max-h (caddr wseg))))
  (setq total-len L
        min-a    0.0
        side (_dwe-pick-view p1 view-dir view-perp L max-h))
  (if (null side) (exit))
  (setq bl   (cdr side)
        side (car side))
  ;; Upright output: draw in world axes anchored at bl.
  (setq xh '(1.0 0.0 0.0)
        yh '(0.0 1.0 0.0))
  ;; Slab extents: flush with outermost walls picked up.
  (setq slab-a 1e99 slab-b -1e99)
  (foreach wseg walls
    (setq slab-a (min slab-a (car wseg))
          slab-b (max slab-b (cadr wseg))))
  ;; Ground slab
  (setq gl  (_dwe-add bl (_dwe-scl xh slab-a))
        gr  (_dwe-add bl (_dwe-scl xh slab-b))
        gl2 (_dwe-add gl (_dwe-scl yh (- *dwe-slab-t*)))
        gr2 (_dwe-add gr (_dwe-scl yh (- *dwe-slab-t*))))
  (_dwe-solid gl2 gr2 gr gl *dwe-lyr-slab-fill*)
  (_dwe-plrect gl2 gr2 gr gl *dwe-lyr-slab-out*)
  ;; Roof slab
  (setq roof-y max-h
        rl  (_dwe-add gl (_dwe-scl yh roof-y))
        rr  (_dwe-add gr (_dwe-scl yh roof-y))
        rl2 (_dwe-add rl (_dwe-scl yh *dwe-slab-t*))
        rr2 (_dwe-add rr (_dwe-scl yh *dwe-slab-t*)))
  (_dwe-solid rl rr rr2 rl2 *dwe-lyr-slab-fill*)
  (_dwe-plrect rl rr rr2 rl2 *dwe-lyr-slab-out*)
  ;; Ground line (may extend past plan for context)
  (_dwe-line (_dwe-add bl (_dwe-scl xh (- *dwe-gnd-ext*)))
             (_dwe-add bl (_dwe-scl xh (+ total-len *dwe-gnd-ext*)))
             *dwe-lyr-gnd*)
  ;; Wall outlines (view elevation = background walls)
  (foreach wseg walls
    (setq wa (car wseg) wb (cadr wseg) wh (caddr wseg)
          w-along (- wa min-a)
          w-len   (- wb wa)
          p (_dwe-add bl (_dwe-scl xh w-along)))
    (_dwe-rect p xh yh w-len wh *dwe-lyr-bg-wall*))
  ;; Openings
  (foreach o opens
    (setq k    (car o)
          ac   (- (cadr o) min-a)
          ww   (caddr o)
          sill (cadddr o)
          head (nth 4 o)
          p    (_dwe-add (_dwe-add bl (_dwe-scl xh (- ac (/ ww 2.0))))
                         (_dwe-scl yh sill)))
    (_dwe-rect p xh yh ww (- head sill)
               (if (eq k 'win) *dwe-lyr-win* *dwe-lyr-door*)))
  (princ (strcat "\nView elevation placed ("
                 (itoa (length walls)) " walls, "
                 (itoa (length opens)) " openings)."))
  (princ))

;; ---- SECT -----------------------------------------------------------
;; Scan all tagged walls, find those the cut segment crosses.
;; Returns list of hits: (along-cut thk h base sill-head-or-nil)
(defun _dwe-sect-scan (c1 c2 sect-dir frame / ss i e info wp1 wp2 thk h base
                       ip dv along oh out L wdir perp opens o w along-o kind
                       ipdv ipalong sill head)
  (setq out nil)
  (regapp "AWALL")
  (setq ss (ssget "_X" (list (list -3 (list "AWALL")))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) info (_dwe-wall-info e))
        (if info
          (progn
            (setq thk (car info) h (cadr info) base (caddr info)
                  wp1 (cadddr info) wp2 (nth 4 info)
                  ip  (_dwe-seg-int c1 c2 wp1 wp2))
            (if ip
              (progn
                (setq dv    (mapcar '- ip c1)
                      along (+ (* (car dv) (car sect-dir))
                               (* (cadr dv) (cadr sect-dir))))
                ;; check for opening on this wall at ip
                (setq L    (distance wp1 wp2)
                      wdir (list (/ (- (car wp2) (car wp1)) L)
                                 (/ (- (cadr wp2) (cadr wp1)) L) 0.0)
                      perp (list (- (cadr wdir)) (car wdir) 0.0)
                      opens (_dwe-on-wall wp1 wdir L perp)
                      ipdv  (mapcar '- ip wp1)
                      ipalong (+ (* (car ipdv) (car wdir))
                                 (* (cadr ipdv) (cadr wdir)))
                      oh nil)
                (foreach o opens
                  (setq kind (car o) along-o (caddr o)
                        w (cdr (assoc 1040 (cadr o))))
                  (if (and (>= ipalong (- along-o (/ w 2.0)))
                           (<= ipalong (+ along-o (/ w 2.0))))
                    (progn
                      (if (eq kind 'win)
                        (setq sill *dwe-win-sill* head (+ sill *dwe-win-h*))
                        (setq sill 0.0 head *dwe-door-h*))
                      (setq oh (list sill head)))))
                (setq out (cons (list along thk h base oh) out))))))
        (setq i (1+ i)))))
  (reverse out))

(defun _dwe-sect-maxh (hits / mx)
  (setq mx 0.0)
  (foreach hit hits
    (setq mx (max mx (+ (cadddr hit) (caddr hit)))))
  mx)

;; Scan all AWIN/ADOOR openings on the FAR side of the cut (opposite viewer).
;; Returns list of (kind along-cut w sill head) for elevation drawing.
(defun _dwe-sect-far-openings (c1 sect-dir sect-perp side frame
                               / ss i e xd mid w dv along across
                                 kind sill head out)
  (setq out nil)
  (foreach app '("AWIN" "ADOOR")
    (regapp app)
    (setq ss (ssget "_X" (list (list -3 (list app)))))
    (if ss
      (progn
        (setq i 0)
        (repeat (sslength ss)
          (setq e   (ssname ss i)
                xd  (_dwe-xd e app)
                mid (cdr (assoc 1011 xd))
                w   (cdr (assoc 1040 xd)))
          (if (and mid w)
            (progn
              (setq dv     (mapcar '- mid c1)
                    along  (+ (* (car dv) (car sect-dir))  (* (cadr dv) (cadr sect-dir)))
                    across (+ (* (car dv) (car sect-perp)) (* (cadr dv) (cadr sect-perp))))
              ;; far side of cut and inside frame
              (if (and (< (* across side) 0.0)
                       (_dwe-in-frame mid frame))
                (progn
                  (setq kind (if (equal app "AWIN") 'win 'door))
                  (if (eq kind 'win)
                    (setq sill *dwe-win-sill* head (+ sill *dwe-win-h*))
                    (setq sill 0.0            head *dwe-door-h*))
                  (setq out (cons (list kind along w sill head across) out))))))
          (setq i (1+ i))))))
  out)

;; Walls fully on far side of cut, parallel-ish to sect-dir, inside frame.
;; Returns list of (along-min along-max h base) for elevation outlines.
(defun _dwe-sect-bg-walls (c1 sect-dir sect-perp side frame
                           / ss i e info wp1 wp2 thk h base L wdir wdot
                             mid dv al1 al2 ac out)
  (setq out nil)
  (regapp "AWALL")
  (setq ss (ssget "_X" (list (list -3 (list "AWALL")))))
  (if ss
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) info (_dwe-wall-info e))
        (if info
          (progn
            (setq thk  (car info) h (cadr info) base (caddr info)
                  wp1  (cadddr info) wp2 (nth 4 info)
                  L    (distance wp1 wp2)
                  wdir (list (/ (- (car wp2) (car wp1)) L)
                             (/ (- (cadr wp2) (cadr wp1)) L) 0.0)
                  wdot (abs (+ (* (car wdir) (car sect-dir))
                               (* (cadr wdir) (cadr sect-dir))))
                  mid  (list (/ (+ (car wp1) (car wp2)) 2.0)
                             (/ (+ (cadr wp1) (cadr wp2)) 2.0) 0.0)
                  dv   (mapcar '- mid c1)
                  ac   (+ (* (car dv) (car sect-perp)) (* (cadr dv) (cadr sect-perp)))
                  al1  (+ (* (- (car wp1) (car c1)) (car sect-dir))
                          (* (- (cadr wp1) (cadr c1)) (cadr sect-dir)))
                  al2  (+ (* (- (car wp2) (car c1)) (car sect-dir))
                          (* (- (cadr wp2) (cadr c1)) (cadr sect-dir))))
            (if (and (> wdot 0.85)
                     (< (* ac side) 0.0)
                     (_dwe-in-frame mid frame))
              (setq out (cons (list (min al1 al2)
                                    (max al1 al2)
                                    h base ac thk) out)))))
        (setq i (1+ i)))))
  out)

(defun _dwe-sect-render (bl c1 sect-dir sect-perp sect-len side hits max-h frame
                         / xh yh hit ac thk hh bs oh sill head slab-a slab-b bg-walls bw p a1 a2 flip-total
                           wbl wbr wtl wtr far-opens fo k w p
                           gl gr gr2 gl2 rl rr rr2 rl2 roof-y base-y)
  ;; Upright output: draw in world axes anchored at bl.
  (setq xh '(1.0 0.0 0.0)
        yh '(0.0 1.0 0.0)
        base-y 0.0
        roof-y max-h)
  ;; Slab extents: flush with outermost cut walls (ac +/- thk/2).
  (setq slab-a 1e99 slab-b -1e99)
  (foreach hit hits
    (setq ac (car hit) thk (cadr hit)
          slab-a (min slab-a (- ac (/ thk 2.0)))
          slab-b (max slab-b (+ ac (/ thk 2.0)))))
  ;; Ground slab
  (setq gl  (_dwe-add bl (_dwe-scl xh slab-a))
        gr  (_dwe-add bl (_dwe-scl xh slab-b))
        gl2 (_dwe-add gl (_dwe-scl yh (- *dwe-slab-t*)))
        gr2 (_dwe-add gr (_dwe-scl yh (- *dwe-slab-t*))))
  (_dwe-solid gl2 gr2 gr gl *dwe-lyr-slab-fill*)
  (_dwe-plrect gl2 gr2 gr gl *dwe-lyr-slab-out*)
  ;; Roof slab (above max wall)
  (setq rl  (_dwe-add gl (_dwe-scl yh roof-y))
        rr  (_dwe-add gr (_dwe-scl yh roof-y))
        rl2 (_dwe-add rl (_dwe-scl yh *dwe-slab-t*))
        rr2 (_dwe-add rr (_dwe-scl yh *dwe-slab-t*)))
  (_dwe-solid rl rr rr2 rl2 *dwe-lyr-slab-fill*)
  (_dwe-plrect rl rr rr2 rl2 *dwe-lyr-slab-out*)
  ;; ground line (may extend for context)
  (_dwe-line (_dwe-add bl (_dwe-scl xh (- slab-a *dwe-gnd-ext*)))
             (_dwe-add bl (_dwe-scl xh (+ slab-b *dwe-gnd-ext*)))
             *dwe-lyr-gnd*)
  ;; Vertical cut: mirror horizontal axis so viewer's perspective matches
  ;; (west view stops appearing flipped). Mirror is done in-place on data;
  ;; slab range [slab-a, slab-b] is symmetric under this mirror.
  (setq flip-total (+ slab-a slab-b))
  (if (> (abs (cadr sect-dir)) (abs (car sect-dir)))
    (setq hits
      (mapcar '(lambda (h)
                 (cons (- flip-total (car h)) (cdr h))) hits)))
  ;; Painter's order: bg walls first (farthest), then far-openings, then cut
  ;; walls last so cut walls visually obstruct anything behind them.
  (setq bg-walls (_dwe-sect-bg-walls c1 sect-dir sect-perp side frame))
  (if (> (abs (cadr sect-dir)) (abs (car sect-dir)))
    (setq bg-walls
      (mapcar '(lambda (bw)
                 (list (- flip-total (cadr bw)) (- flip-total (car bw))
                       (caddr bw) (cadddr bw) (nth 4 bw) (nth 5 bw)))
              bg-walls)))
  (foreach bw bg-walls
    (setq a1 (max (car bw) slab-a)
          a2 (min (cadr bw) slab-b))
    (if (> a2 a1)
      (progn
        (setq p (_dwe-add bl (_dwe-scl xh a1)))
        (_dwe-rect p xh yh (- a2 a1) (caddr bw) *dwe-lyr-bg-wall*))))
  (setq far-opens (_dwe-sect-far-openings c1 sect-dir sect-perp side frame))
  (if (> (abs (cadr sect-dir)) (abs (car sect-dir)))
    (setq far-opens
      (mapcar '(lambda (fo)
                 (list (car fo) (- flip-total (cadr fo)) (caddr fo)
                       (cadddr fo) (nth 4 fo) (nth 5 fo)))
              far-opens)))
  (foreach fo far-opens
    (setq k    (car fo)
          ac   (cadr fo)
          w    (caddr fo)
          sill (cadddr fo)
          head (nth 4 fo)
          o-across (nth 5 fo)
          p    (_dwe-add (_dwe-add bl (_dwe-scl xh (- ac (/ w 2.0))))
                         (_dwe-scl yh sill)))
    ;; keep only openings that sit on a bg wall: along-range inside wall
    ;; AND perpendicular offset within wall thk/2 + 50mm slop.
    (if (vl-some
          '(lambda (bw)
             (and (>= (- ac (/ w 2.0)) (car bw))
                  (<= (+ ac (/ w 2.0)) (cadr bw))
                  (<= (abs (- o-across (nth 4 bw)))
                      (+ (/ (nth 5 bw) 2.0) 50.0))))
          bg-walls)
      (_dwe-rect p xh yh w (- head sill)
                 (if (eq k 'win) *dwe-lyr-win* *dwe-lyr-door*))))
  (foreach hit hits
    (setq ac  (car hit)   thk (cadr hit) hh (caddr hit)
          bs  (cadddr hit) oh (nth 4 hit)
          wbl (_dwe-add (_dwe-add bl (_dwe-scl xh (- ac (/ thk 2.0))))
                        (_dwe-scl yh bs))
          wbr (_dwe-add wbl (_dwe-scl xh thk))
          wtl (_dwe-add wbl (_dwe-scl yh hh))
          wtr (_dwe-add wbr (_dwe-scl yh hh)))
    (if oh
      (progn
        (setq sill (car oh) head (cadr oh))
        (if (> sill 0.0)
          (progn
            (_dwe-solid wbl wbr
              (_dwe-add wbr (_dwe-scl yh sill))
              (_dwe-add wbl (_dwe-scl yh sill))
              *dwe-lyr-cut-fill*)
            (_dwe-plrect wbl wbr
              (_dwe-add wbr (_dwe-scl yh sill))
              (_dwe-add wbl (_dwe-scl yh sill))
              *dwe-lyr-cut-out*)))
        (if (< head hh)
          (progn
            (_dwe-solid
              (_dwe-add wbl (_dwe-scl yh head))
              (_dwe-add wbr (_dwe-scl yh head))
              wtr wtl
              *dwe-lyr-cut-fill*)
            (_dwe-plrect
              (_dwe-add wbl (_dwe-scl yh head))
              (_dwe-add wbr (_dwe-scl yh head))
              wtr wtl
              *dwe-lyr-cut-out*))))
      (progn
        (_dwe-solid wbl wbr wtr wtl *dwe-lyr-cut-fill*)
        (_dwe-plrect wbl wbr wtr wtl *dwe-lyr-cut-out*)))
    (_dwe-plrect wbl wbr wtr wtl *dwe-lyr-cut-out*))
  (if *dwe-dim-on* (_dwe-dim-chain bl max-h)))

;; ---- vertical dimension chain on the left of an elevation ----------
(defun _dwe-dedup (lst / out)
  (foreach v lst
    (if (or (null out) (> (abs (- v (car out))) 1.0))
      (setq out (cons v out))))
  (reverse out))

(defun _dwe-dim-chain (bl max-h / xoff dim-x tot-x levels y1 y2 cmde)
  (setq xoff 600.0
        dim-x (- (car bl) xoff)
        tot-x (- dim-x xoff)
        levels (vl-sort (list (- *dwe-slab-t*)
                              0.0
                              *dwe-win-sill*
                              (+ *dwe-win-sill* *dwe-win-h*)
                              *dwe-door-h*
                              max-h
                              (+ max-h *dwe-slab-t*)) '<)
        levels (_dwe-dedup levels)
        y1 (car levels)
        cmde (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (_dwe-lyr *dwe-lyr-anno*)
  (foreach y2 (cdr levels)
    (command-s "_.dimlinear"
      (list (car bl) (+ (cadr bl) y1) 0.0)
      (list (car bl) (+ (cadr bl) y2) 0.0)
      (list dim-x (+ (cadr bl) (/ (+ y1 y2) 2.0)) 0.0))
    (setq y1 y2))
  (command-s "_.dimlinear"
    (list (car bl) (+ (cadr bl) (car levels)) 0.0)
    (list (car bl) (+ (cadr bl) (car (reverse levels))) 0.0)
    (list tot-x (+ (cadr bl) (/ (+ (car levels) (car (reverse levels))) 2.0)) 0.0))
  (setvar "CMDECHO" cmde))

(defun c:SECT ( / *error* c1 c2 f1 f2 frame frame-ent dxv dyv sect-len sect-dir sect-perp hits sect-mid
                  max-h side)
  (setq *dwe-last-cmd* 'c:SECT)
  (defun *error* (msg)
    (if frame-ent (entdel frame-ent))
    (if msg (princ (strcat "\n" msg))))
  (setq f1 (getpoint "\nFrame corner 1 (defines section scope): "))
  (if (null f1) (exit))
  (setq f2 (getcorner f1 "\nFrame corner 2: "))
  (if (null f2) (exit))
  (setq frame (list (min (car f1) (car f2)) (min (cadr f1) (cadr f2))
                    (max (car f1) (car f2)) (max (cadr f1) (cadr f2))))
  ;; Persistent frame polyline (dashed cyan) — kept as scope record.
  (_dwe-lyr *dwe-lyr-frame*)
  (_dwe-ltype "DASHED")
  (entmake (list (cons 0 "LWPOLYLINE")
                 (cons 100 "AcDbEntity")
                 (cons 8 (car *dwe-lyr-frame*))
                 (cons 6 "DASHED")
                 (cons 100 "AcDbPolyline")
                 (cons 90 4) (cons 70 1)
                 (cons 10 (list (car frame) (cadr frame)))
                 (cons 10 (list (caddr frame) (cadr frame)))
                 (cons 10 (list (caddr frame) (cadddr frame)))
                 (cons 10 (list (car frame) (cadddr frame)))))
  (setq frame-ent (entlast))
  (setq c1 (getpoint "\nCut line first point: "))
  (if (null c1) (exit))
  (setq c2 (getpoint c1 "\nCut line second point: "))
  (if (null c2) (exit))
  (setq dxv (- (car c2) (car c1))
        dyv (- (cadr c2) (cadr c1))
        sect-len (distance c1 c2))
  (if (< sect-len 1.0) (progn (princ "\nCut too short.") (exit)))
  (setq sect-dir  (list (/ dxv sect-len) (/ dyv sect-len) 0.0)
        sect-perp (list (- (cadr sect-dir)) (car sect-dir) 0.0)
        hits (_dwe-sect-scan c1 c2 sect-dir frame))
  (if (null hits)
    (progn (princ "\nNo tagged walls crossed. Draw walls with WW first.")
           (exit)))
  (setq max-h (_dwe-sect-maxh hits)
        side (_dwe-pick-view c1 sect-dir sect-perp sect-len max-h))
  (if side
    (_dwe-sect-render (cdr side) c1 sect-dir sect-perp sect-len
                      (car side) hits max-h frame))
  ;; Persistent section line (DASHED) as record of scope.
  (_dwe-line-lt c1 c2 *dwe-lyr-sect* "DASHED")
  ;; Frame kept visible as scope record.
  (setq frame-ent nil)
  (princ (strcat "\nSection placed (" (itoa (length hits)) " walls crossed)."))
  (princ))

(defun c:SE () (c:SECT))

;; ---- SEA - N/S/E/W set from one center point -----------------------
(defun _dwe-sea-symbol (cen sz)
  (_dwe-line (list (car cen) (- (cadr cen) sz) 0.0)
             (list (car cen) (+ (cadr cen) sz) 0.0) *dwe-lyr-sect*)
  (_dwe-line (list (- (car cen) sz) (cadr cen) 0.0)
             (list (+ (car cen) sz) (cadr cen) 0.0) *dwe-lyr-sect*)
  (_dwe-text (list (car cen) (+ (cadr cen) sz (* 0.5 sz)) 0.0) '(1.0 0.0 0.0) (* 0.6 sz) "N" *dwe-lyr-sect*)
  (_dwe-text (list (car cen) (- (cadr cen) sz (* 0.5 sz)) 0.0) '(1.0 0.0 0.0) (* 0.6 sz) "S" *dwe-lyr-sect*)
  (_dwe-text (list (+ (car cen) sz (* 0.5 sz)) (cadr cen) 0.0) '(1.0 0.0 0.0) (* 0.6 sz) "E" *dwe-lyr-sect*)
  (_dwe-text (list (- (car cen) sz (* 0.5 sz)) (cadr cen) 0.0) '(1.0 0.0 0.0) (* 0.6 sz) "W" *dwe-lyr-sect*))

(defun _dwe-sea-width (hits / a b)
  (setq a 1e99 b -1e99)
  (foreach h hits
    (setq a (min a (- (car h) (/ (cadr h) 2.0)))
          b (max b (+ (car h) (/ (cadr h) 2.0)))))
  (- b a))

(defun _dwe-sea-title (bl w str)
  (_dwe-text (list (+ (car bl) (/ w 2.0))
                   (- (cadr bl) (* *dwe-slab-t* 2.0) *dwe-txt-h*) 0.0)
             '(1.0 0.0 0.0) *dwe-txt-h* str *dwe-lyr-anno*))

(defun c:SEA ( / *error* f1 f2 frame frame-ent cen bl-place gap
                  ns-c1 ns-c2 ew-c1 ew-c2 hits-ns hits-ew mh-ns mh-ew
                  wns wew bln bls ble blw)
  (setq *dwe-last-cmd* 'c:SEA)
  (defun *error* (msg) (if msg (princ (strcat "\n" msg))))
  (setq f1 (getpoint "\nFrame corner 1: "))
  (if (null f1) (exit))
  (setq f2 (getcorner f1 "\nFrame corner 2: "))
  (if (null f2) (exit))
  (setq frame (list (min (car f1) (car f2)) (min (cadr f1) (cadr f2))
                    (max (car f1) (car f2)) (max (cadr f1) (cadr f2))))
  (_dwe-lyr *dwe-lyr-frame*)
  (_dwe-ltype "DASHED")
  (entmake (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity")
                 (cons 8 (car *dwe-lyr-frame*)) (cons 6 "DASHED")
                 (cons 100 "AcDbPolyline") (cons 90 4) (cons 70 1)
                 (cons 10 (list (car frame) (cadr frame)))
                 (cons 10 (list (caddr frame) (cadr frame)))
                 (cons 10 (list (caddr frame) (cadddr frame)))
                 (cons 10 (list (car frame) (cadddr frame)))))
  (setq cen (getpoint "\nPick center point (N/S/E/W origin): "))
  (if (null cen) (exit))
  (_dwe-sea-symbol cen 500.0)
  (setq ew-c1 (list (car frame)   (cadr cen) 0.0)
        ew-c2 (list (caddr frame) (cadr cen) 0.0)
        ns-c1 (list (car cen) (cadr frame)    0.0)
        ns-c2 (list (car cen) (cadddr frame) 0.0))
  (_dwe-line-lt ew-c1 ew-c2 *dwe-lyr-sect* "DASHED")
  (_dwe-line-lt ns-c1 ns-c2 *dwe-lyr-sect* "DASHED")
  (setq hits-ew (_dwe-sect-scan ew-c1 ew-c2 '(1.0 0.0 0.0) frame)
        hits-ns (_dwe-sect-scan ns-c1 ns-c2 '(0.0 1.0 0.0) frame))
  (if (or (null hits-ew) (null hits-ns))
    (progn (princ "\nCut lines didn't cross tagged walls both ways.") (exit)))
  (setq mh-ew (_dwe-sect-maxh hits-ew)
        mh-ns (_dwe-sect-maxh hits-ns)
        wew (_dwe-sea-width hits-ew)
        wns (_dwe-sea-width hits-ns)
        gap 3500.0)
  (setq bl-place (getpoint "\nPick bottom-left placement point: "))
  (if (null bl-place) (exit))
  (setq bln bl-place
        bls (list (+ (car bln) wew gap) (cadr bln) 0.0)
        ble (list (+ (car bls) wew gap) (cadr bln) 0.0)
        blw (list (+ (car ble) wns gap) (cadr bln) 0.0))
  ;; N view: horizontal cut, side = -1 picks north walls
  (_dwe-sect-render bln ew-c1 '(1.0 0.0 0.0) '(0.0 1.0 0.0)
                    (distance ew-c1 ew-c2) -1.0 hits-ew mh-ew frame)
  (_dwe-sea-title bln wew "NORTH")
  (_dwe-sect-render bls ew-c1 '(1.0 0.0 0.0) '(0.0 1.0 0.0)
                    (distance ew-c1 ew-c2) 1.0 hits-ew mh-ew frame)
  (_dwe-sea-title bls wew "SOUTH")
  (_dwe-sect-render ble ns-c1 '(0.0 1.0 0.0) '(-1.0 0.0 0.0)
                    (distance ns-c1 ns-c2) 1.0 hits-ns mh-ns frame)
  (_dwe-sea-title ble wns "EAST")
  (_dwe-sect-render blw ns-c1 '(0.0 1.0 0.0) '(-1.0 0.0 0.0)
                    (distance ns-c1 ns-c2) -1.0 hits-ns mh-ns frame)
  (_dwe-sea-title blw wns "WEST")
  (princ "\n4 elevations placed (N S E W).")
  (princ))

;; ---- QQ - replay last ELEV/SECT ------------------------------------
(defun c:QQ ( )
  (if *dwe-last-cmd*
    (apply *dwe-last-cmd* nil)
    (princ "\nNo last command to repeat."))
  (princ))

;; ---- DESET - settings (heights, colors, layers) --------------------
(defun _dwe-getr (prm def / v)
  (setq v (getreal (strcat "\n" prm " <" (rtos def 2 1) ">: ")))
  (if v v def))

(defun _dwe-getk (prm def / v)
  (setq v (getstring T (strcat "\n" prm " <" def ">: ")))
  (if (= v "") def v))

(defun _dwe-geti (prm def / v)
  (initget 4)
  (setq v (getint (strcat "\n" prm " <" (itoa def) ">: ")))
  (if v v def))

(defun _dwe-set-spec (sym label / cur name col)
  (setq cur (eval sym) name (car cur) col (cdr cur))
  (setq name (_dwe-getk (strcat label " layer name") name))
  (setq col  (_dwe-geti (strcat label " color (ACI 1-255)") col))
  (set sym (cons name col)))

;; ---- DCL dialog ---------------------------------------------------
(defun _dwe-dcl-put (key val) (set_tile key val))
(defun _dwe-dcl-h (key def) (atof (get_tile key)))
(defun _dwe-dcl-s (key def) (get_tile key))
(defun _dwe-dcl-i (key def / v) (setq v (atoi (get_tile key))) (if (and (>= v 1) (<= v 255)) v def))

(defun c:DESETD ( / f id res)
  (setq f (or *dwe-dcl-path* (findfile "AKDProjections.dcl")))
  (if (null f)
    (progn
      (princ "\nAKDProjections.dcl not on support path — locate it once:")
      (setq f (getfiled "Locate AKDProjections.dcl" "" "dcl" 8))
      (if f (setq *dwe-dcl-path* f) (exit))))
  (setq id (load_dialog f))
  (if (not (new_dialog "akd_deset" id)) (progn (unload_dialog id) (exit)))
  (mapcar '_dwe-dcl-put
    '("win_h" "win_sill" "door_h" "wall_h" "slab_t" "gnd_ext")
    (mapcar '(lambda (x) (rtos x 2 1))
      (list *dwe-win-h* *dwe-win-sill* *dwe-door-h* *dwe-wall-h* *dwe-slab-t* *dwe-gnd-ext*)))
  (foreach pr '(("hatch" . *dwe-lyr-hatch*) ("elv1" . *dwe-lyr-elv1*)
                ("elv2" . *dwe-lyr-elv2*)   ("elv3" . *dwe-lyr-elv3*)
                ("elv5" . *dwe-lyr-elv5*)   ("sect" . *dwe-lyr-sect*)
                ("frame" . *dwe-lyr-frame*) ("anno" . *dwe-lyr-anno*))
    (set_tile (strcat "n_" (car pr)) (car (eval (cdr pr))))
    (set_tile (strcat "c_" (car pr)) (itoa (cdr (eval (cdr pr))))))
  (action_tile "accept"
    (strcat
      "(setq *dwe-win-h* (atof (get_tile \"win_h\"))"
      " *dwe-win-sill* (atof (get_tile \"win_sill\"))"
      " *dwe-door-h*   (atof (get_tile \"door_h\"))"
      " *dwe-wall-h*   (atof (get_tile \"wall_h\"))"
      " *dwe-slab-t*   (atof (get_tile \"slab_t\"))"
      " *dwe-gnd-ext*  (atof (get_tile \"gnd_ext\")))"
      "(foreach pr '((\"hatch\" . *dwe-lyr-hatch*) (\"elv1\" . *dwe-lyr-elv1*)"
      " (\"elv2\" . *dwe-lyr-elv2*) (\"elv3\" . *dwe-lyr-elv3*)"
      " (\"elv5\" . *dwe-lyr-elv5*) (\"sect\" . *dwe-lyr-sect*)"
      " (\"frame\" . *dwe-lyr-frame*) (\"anno\" . *dwe-lyr-anno*))"
      " (set (cdr pr) (cons (get_tile (strcat \"n_\" (car pr)))"
      "                     (max 1 (min 255 (atoi (get_tile (strcat \"c_\" (car pr)))))))))"
      "(done_dialog 1)"))
  (setq res (start_dialog))
  (unload_dialog id)
  (if (= res 1)
    (setq *dwe-lyr-cut-fill* *dwe-lyr-hatch* *dwe-lyr-cut-out* *dwe-lyr-elv1*
          *dwe-lyr-bg-wall* *dwe-lyr-elv2*   *dwe-lyr-win* *dwe-lyr-elv2*
          *dwe-lyr-glass* *dwe-lyr-elv3*     *dwe-lyr-door* *dwe-lyr-elv2*
          *dwe-lyr-panel* *dwe-lyr-elv3*     *dwe-lyr-slab-fill* *dwe-lyr-hatch*
          *dwe-lyr-slab-out* *dwe-lyr-elv5*  *dwe-lyr-gnd* *dwe-lyr-elv5*
          *dwe-lyr-txt* *dwe-lyr-anno*       *dwe-lyr-wall* *dwe-lyr-elv2*
          *dwe-lyr-fill* *dwe-lyr-hatch*))
  (princ (if (= res 1) "\nSettings saved." "\nCancelled."))
  (princ))

(defun c:DESET ( / k)
  (while
    (progn
      (initget "Heights Colors Layers Dialog Quit")
      (setq k (getkword "\nDESET [Heights/Colors/Layers/Dialog/Quit] <Quit>: "))
      (cond
        ((= k "Heights")
         (setq *dwe-win-h*    (_dwe-getr "Window height"    *dwe-win-h*)
               *dwe-win-sill* (_dwe-getr "Window sill"      *dwe-win-sill*)
               *dwe-door-h*   (_dwe-getr "Door height"      *dwe-door-h*)
               *dwe-wall-h*   (_dwe-getr "Wall height"      *dwe-wall-h*)
               *dwe-slab-t*   (_dwe-getr "Slab thickness"   *dwe-slab-t*)
               *dwe-gnd-ext*  (_dwe-getr "Ground extension" *dwe-gnd-ext*))
         T)
        ((or (= k "Colors") (= k "Layers"))
         (_dwe-set-spec '*dwe-lyr-hatch* "Cut hatch")
         (_dwe-set-spec '*dwe-lyr-elv1*  "Cut outline (ELV-1)")
         (_dwe-set-spec '*dwe-lyr-elv2*  "BG wall / frames (ELV-2)")
         (_dwe-set-spec '*dwe-lyr-elv3*  "Mullions / panels (ELV-3)")
         (_dwe-set-spec '*dwe-lyr-elv5*  "Slab / ground (ELV-5)")
         (_dwe-set-spec '*dwe-lyr-sect*  "Section line")
         (_dwe-set-spec '*dwe-lyr-frame* "Section frame")
         (_dwe-set-spec '*dwe-lyr-anno*  "Annotation")
         ;; refresh aliases
         (setq *dwe-lyr-cut-fill* *dwe-lyr-hatch*
               *dwe-lyr-cut-out*  *dwe-lyr-elv1*
               *dwe-lyr-bg-wall*  *dwe-lyr-elv2*
               *dwe-lyr-win*      *dwe-lyr-elv2*
               *dwe-lyr-glass*    *dwe-lyr-elv3*
               *dwe-lyr-door*     *dwe-lyr-elv2*
               *dwe-lyr-panel*    *dwe-lyr-elv3*
               *dwe-lyr-slab-fill* *dwe-lyr-hatch*
               *dwe-lyr-slab-out* *dwe-lyr-elv5*
               *dwe-lyr-gnd*      *dwe-lyr-elv5*
               *dwe-lyr-txt*      *dwe-lyr-anno*
               *dwe-lyr-wall*     *dwe-lyr-elv2*
               *dwe-lyr-fill*     *dwe-lyr-hatch*)
         T)
        ((= k "Dialog") (c:DESETD) nil)
        (T nil))))
  (princ "\nSettings saved for session."))

(princ "\nAKDProjections loaded.  Commands: DE, WE, SECT/SE, SEA, QQ, DESET/DESETD, DIMTOG (dims off)")
(princ)
