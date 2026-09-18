;;; ============================================================
;;; ASECauto.lsp - ASECA: automatic section analysis
;;; REQUIRES ASEC.lsp loaded first (geometry, records and generator live there).
;;; AutoCAD for Mac compatible. No VLA/VLAX/ActiveX/XData. Source is read-only.
;;;
;;; USAGE
;;;   ASECA
;;;   1. Select section line, viewing side, projection depth, GF reference point
;;;      (same prompts and orientation rules as ASEC).
;;;   2. Each floor is analysed automatically from layers + geometry:
;;;        CUT WALLS        wall-layer LINEs crossing the floor's section, paired
;;;                         in U order (plus faces broken by an opening block)
;;;        CUT OPENINGS     door/window INSERTs hosted by a cut wall AND crossed
;;;        PROJECTED WALLS  wall-layer LINEs inside the view boundary, chained into
;;;                         logical faces, paired only when partners are unique
;;;        PROJ. OPENINGS   remaining INSERTs in the boundary, hosted by exactly one
;;;                         finite projected wall
;;;      Anything ambiguous becomes UNRESOLVED (never guessed).
;;;   3. Review: temporary labels C#, CD#, CW#, P#, PD#, PW#, ?# and a summary.
;;;        Generate  accept this floor (slab prompt as in ASEC)
;;;        Edit      Remove / Add (guided ASEC collectors) / Ignore unresolved
;;;        Guided    redo this floor with guided ASEC
;;;        Cancel    exit
;;;   4. Upper floors: matching reference point, same analysis. Then roof,
;;;      insertion point and the shared ASEC generator.
;;;
;;; SETTINGS (session globals)
;;;   *aseca-wall-layers* *aseca-door-layers* *aseca-window-layers*   (case-insensitive)
;;;   *aseca-min-wall-thickness* 50  *aseca-max-wall-thickness* 600
;;;   *aseca-collinear-tolerance* 5  *aseca-small-gap-tolerance* 50
;;;   *aseca-min-face-length* 300 (shorter LINEs are jamb returns/nibs, not wall faces)
;;;   Projected openings in view but in no projected wall are kept unhosted.
;;;   *asec-wall-warning-thickness* 500 (cut walls above it are MEDIUM, not rejected)
;;;   *aseca-debug* nil
;;;
;;; CONFIDENCE
;;;   HIGH    direct cut crossings / projected faces with closing lines at both ends
;;;   MEDIUM  cut face recovered through an opening gap, thick wall, uncertain end
;;;   UNRESOLVED  odd crossings, out-of-range pairing, face with several partners,
;;;               unreadable block jambs, multiple/no hosts, overlapping openings
;;; ============================================================

(if (null *aseca-wall-layers*) (setq *aseca-wall-layers* '("A-WALL")))
(if (null *aseca-door-layers*) (setq *aseca-door-layers* '("A-DOOR")))
(if (null *aseca-window-layers*) (setq *aseca-window-layers* '("A-WINDOW")))
(if (null *aseca-min-wall-thickness*) (setq *aseca-min-wall-thickness* 50.0))
(if (null *aseca-max-wall-thickness*) (setq *aseca-max-wall-thickness* 600.0))
(if (null *aseca-collinear-tolerance*) (setq *aseca-collinear-tolerance* 5.0))
(if (null *aseca-small-gap-tolerance*) (setq *aseca-small-gap-tolerance* 50.0))
(if (null *aseca-min-face-length*) (setq *aseca-min-face-length* 300.0))   ; shorter = jamb return / nib
(if (null *asec-wall-warning-thickness*) (setq *asec-wall-warning-thickness* 500.0))

(defun aseca:dbg (s) (if *aseca-debug* (princ s)) T)

;;; ---------- dependency check ----------
(setq aseca:required
  '(asec:get-master-section asec:get-view-direction asec:normalize-section-orientation
    asec:get-projection-boundary asec:getpt asec:get-config asec:floor-section
    asec:make-floor-ctx asec:make-floor-record asec:add-floor asec:generate
    asec:collect-walls asec:collect-openings asec:collect-projected-walls
    asec:get-block-data asec:get-block-opening-span asec:make-opening
    asec:validate-wall-openings asec:wall-strip asec:strip-offsets
    asec:point-in-wall-strip asec:section-crosses-opening asec:on-section-p
    asec:validate-projected-wall-faces asec:projected-wall-auto-extent
    asec:make-projected-wall asec:projected-host-candidates asec:classify-in-view
    asec:entity-bounds-overlap-view-p asec:section-coordinate asec:station-on-section
    asec:get-floor-slab asec:label asec:clear-labels asec:sort asec:unit asec:perp
    asec:error asec:cleanup asec:floor-name asec:layer-visible-p))

(defun aseca:core-loaded-p (/ miss)
  (foreach f aseca:required (if (not (eval f)) (setq miss T)))
  (not miss)
)

;;; ---------- small utilities ----------
(defun aseca:layer-p (lay lst) (member (strcase lay) (mapcar 'strcase lst)))

(defun aseca:join (lst sep / s)
  (foreach x lst (setq s (if s (strcat s sep x) x)))
  (if s s "")
)

;;; escape wildcard characters so layer names match literally in ssget filters
(defun aseca:wc-escape (s / i c out)
  (setq i 1 out "")
  (while (<= i (strlen s))
    (setq c (substr s i 1) i (1+ i)
          out (strcat out (if (member c '("#" "@" "." "*" "?" "~" "[" "]" "`")) (strcat "`" c) c))))
  out
)

;;; enames of TYP on LAYERS in the current space (database filter, no screen dependency)
(defun aseca:ss-list (typ layers / ss i out)
  (setq ss (ssget "_X" (list (cons 0 typ)
                             (cons 8 (aseca:join (mapcar 'aseca:wc-escape layers) ","))
                             (cons 410 (getvar "CTAB"))))
        i 0)
  (while (and ss (< i (sslength ss)))
    ;; "_X" also returns entities on off/frozen layers: hidden layers are never analysed
    (if (asec:layer-visible-p (asec:get 8 (entget (ssname ss i))))
      (setq out (cons (ssname ss i) out)))
    (setq i (1+ i)))
  out
)

;;; (ename a b) for a LINE
(defun aseca:line (e / ed)
  (setq ed (entget e))
  (list e (asec:2d (asec:get 10 ed)) (asec:2d (asec:get 11 ed)))
)

(defun aseca:unres (reason pt)
  (aseca:dbg (strcat "\nUNRESOLVED: " reason))
  (list (cons 'reason reason) (cons 'pt pt))
)

(defun aseca:memq (x lst / r) (foreach y lst (if (eq x y) (setq r T))) r)

(defun aseca:parallel-p (d1 d2) (<= (abs (asec:cross d1 d2)) *asec-parallel-tolerance*))

(defun aseca:mid (a b) (asec:v* (asec:v+ a b) 0.5))

(defun aseca:uv->plan (u v ctx)
  (asec:v+ (asec:v+ (car (asec:get 'se ctx)) (asec:v* (asec:get 'dir ctx) u))
           (asec:v* (asec:get 'vd ctx) v))
)

;;; ---------- blocks ----------
;;; ((kind . "DOOR"|"WINDOW") (bd . block data) (jm . jambs|nil)) for every opening INSERT
(defun aseca:collect-blocks (/ out bd)
  (foreach e (aseca:ss-list "INSERT" (append *aseca-door-layers* *aseca-window-layers*))
    (setq bd (asec:get-block-data e)
          out (cons (list (cons 'kind (if (aseca:layer-p (asec:get 'layer bd) *aseca-door-layers*)
                                        "DOOR" "WINDOW"))
                          (cons 'bd bd)
                          (cons 'jm (asec:get-block-opening-span bd)))
                    out)))
  out
)

;;; ---------- cut walls ----------
;;; crossings (u pt (a b) enames source), sorted by U and merged within collinear tolerance
(defun aseca:cut-crossings (se dir lines blocks / out ip gap jm j1 w n width hd ta tb tlo thi)
  (setq gap *aseca-small-gap-tolerance*)
  (foreach ln lines
    (if (and (> (distance (cadr ln) (caddr ln)) asec:tol)
             (setq ip (inters (car se) (cadr se) (cadr ln) (caddr ln) T)))
      (setq out (cons (list (asec:station-on-section (asec:2d ip) (car se) dir) (asec:2d ip)
                            (cdr ln) (list (car ln)) "direct")
                      out))))
  ;; face broken by an opening: a parallel wall segment ends at a jamb and its
  ;; supporting line crosses the section inside that opening's jamb span
  (foreach ob blocks
    (if (setq jm (asec:get 'jm ob))
      (progn
        (setq j1 (car jm) width (caddr jm) hd (cadddr jm)
              w (asec:unit (asec:v- (cadr jm) j1)) n (asec:perp w))
        (foreach ln lines
          (if (and (> (distance (cadr ln) (caddr ln)) asec:tol)
                   (aseca:parallel-p (asec:unit (asec:v- (caddr ln) (cadr ln))) w)
                   (<= (abs (asec:dot (asec:v- (cadr ln) j1) n)) (+ hd *aseca-max-wall-thickness*))
                   (not (inters (car se) (cadr se) (cadr ln) (caddr ln) T))
                   (setq ta (asec:dot (asec:v- (cadr ln) j1) w)
                         tb (asec:dot (asec:v- (caddr ln) j1) w)
                         tlo (min ta tb) thi (max ta tb))
                   (or (<= (abs thi) gap) (<= (abs (- tlo width)) gap))
                   (setq ip (inters (car se) (cadr se) (cadr ln) (caddr ln) nil))
                   (asec:on-section-p (setq ip (asec:2d ip)) se)
                   (<= (- gap) (asec:dot (asec:v- ip j1) w) (+ width gap)))
            (setq out (cons (list (asec:station-on-section ip (car se) dir) ip
                                  (cdr ln) (list (car ln)) "opening-gap")
                            out)))))))
  (setq out (aseca:merge-crossings (asec:sort out '(lambda (p q) (< (car p) (car q))))))
  (foreach x out
    (aseca:dbg (strcat "\nCUT FACE  U = " (rtos (car x) 2 2) "  " (nth 4 x) "  layers/handles:"
                       (apply 'strcat (mapcar '(lambda (e) (strcat " " (asec:get 8 (entget e)) "/" (asec:get 5 (entget e))))
                                              (cadddr x))))))
  out
)

;;; same face seen twice (collinear pieces / duplicates): keep one, prefer direct, merge enames
(defun aseca:merge-crossings (xs / out base)
  (foreach x xs
    (if (and out (<= (- (car x) (car (car out))) *aseca-collinear-tolerance*))
      (setq base (if (= (nth 4 x) "direct") x (car out))
            out  (cons (list (car base) (cadr base) (caddr base)
                             (append (cadddr x) (cadddr (car out))) (nth 4 base))
                       (cdr out)))
      (setq out (cons x out))))
  (reverse out)
)

;;; (walls unresolved) - pairs 1-2, 3-4 ... ; any bad pair leaves ALL walls unresolved
(defun aseca:pair-cut-walls (xs / walls a b thk bad)
  (cond
    ((null xs) (list nil nil))
    ((= (rem (length xs) 2) 1)
     (list nil (list (aseca:unres (strcat (itoa (length xs))
                                          " cut wall-face crossings found; an even number is required")
                                  (cadr (car xs))))))
    (T
     (while xs
       (setq a (car xs) b (cadr xs) xs (cddr xs) thk (- (car b) (car a)))
       (aseca:dbg (strcat "\nCUT PAIR  U " (rtos (car a) 2 2) " -> " (rtos (car b) 2 2)
                          "  thickness " (rtos thk 2 2)))
       (if (or (< thk *aseca-min-wall-thickness*) (> thk *aseca-max-wall-thickness*))
         (setq bad (cadr a))
         (setq walls (cons (list (cons 'p1 (cadr a)) (cons 'p2 (cadr b))
                                 (cons 's1 (car a)) (cons 's2 (car b))
                                 (cons 'f1 (strcat "auto " (nth 4 a))) (cons 'f2 (strcat "auto " (nth 4 b)))
                                 (cons 'l1 (caddr a)) (cons 'l2 (caddr b))
                                 (cons 'umin (car a)) (cons 'umax (car b)) (cons 'thk thk)
                                 (cons 'enames (append (cadddr a) (cadddr b)))
                                 (cons 'conf (if (and (= (nth 4 a) "direct") (= (nth 4 b) "direct")
                                                      (<= thk *asec-wall-warning-thickness*))
                                               "HIGH" "MEDIUM")))
                           walls))))
     (if bad
       (list nil (list (aseca:unres "cut wall pairing gives a thickness outside the automatic range" bad)))
       (list (reverse walls) nil))))
)

;;; ---------- cut openings ----------
(defun aseca:section-through-opening-p (jm ctx / se w ip)
  (setq se (asec:get 'se ctx) w (asec:unit (asec:v- (cadr jm) (car jm))))
  (and (setq ip (inters (car se) (cadr se) (car jm) (cadr jm) nil))
       (asec:on-section-p (setq ip (asec:2d ip)) se)
       (<= (- asec:tol) (asec:dot (asec:v- ip (car jm)) w) (+ (caddr jm) asec:tol)))
)

;;; block insertion near this floor's section or inside its view boundary
(defun aseca:block-near-view-p (ob ctx / se uv vb mt)
  (setq se (asec:get 'se ctx) vb (asec:get 'view ctx) mt *aseca-max-wall-thickness*
        uv (asec:section-coordinate (asec:get 'ins (asec:get 'bd ob)) (car se) (asec:get 'dir ctx) (asec:get 'vd ctx)))
  (and (<= (- mt) (car uv) (+ (asec:get 'umax vb) mt))
       (<= (- mt) (cadr uv) (+ (asec:get 'vfar vb) mt)))
)

;;; (cutdoors cutwins not-cut-blocks unresolved)
(defun aseca:classify-cut-openings (blocks walls ctx / cd cw rest un jm bd kind mid k hosts wl rec)
  (foreach ob blocks
    (setq jm (asec:get 'jm ob) bd (asec:get 'bd ob) kind (asec:get 'kind ob) hosts nil k 0)
    (cond
      ((null jm)
       (if (aseca:block-near-view-p ob ctx)
         (setq un (cons (aseca:unres (strcat (strcase kind T) " jamb geometry could not be read ("
                                             (asec:get 'name bd) ")")
                                     (asec:get 'ins bd))
                        un))
         (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " ignored: unreadable jambs, far from view"))))
      (T
       (setq mid (aseca:mid (car jm) (cadr jm)))
       (foreach w walls
         (setq k (1+ k))
         (if (and (asec:wall-strip w)
                  (asec:point-in-wall-strip (asec:strip-offsets mid (asec:wall-strip w)) (cadddr jm))
                  (asec:section-crosses-opening jm w))
           (setq hosts (cons k hosts))))
       (cond
         ((> (length hosts) 1)
          (setq un (cons (aseca:unres (strcat (strcase kind T) " matches several cut walls") mid) un)))
         ((= (length hosts) 1)
          (setq wl  (nth (1- (car hosts)) walls)
                rec (cons (cons 'conf "HIGH")
                          (asec:make-opening kind "CUT" bd jm (car hosts)
                                             (asec:get 'umin wl) (asec:get 'umax wl) ctx nil)))
          (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " -> CUT in C" (itoa (car hosts))))
          (cond
            ((asec:validate-wall-openings rec (append cd cw))
             (setq un (cons (aseca:unres (strcat "cut " (strcase kind T) " overlaps another cut opening") mid) un)))
            ((= kind "DOOR") (setq cd (cons rec cd)))
            (T (setq cw (cons rec cw)))))
         ((aseca:section-through-opening-p jm ctx)
          (setq un (cons (aseca:unres (strcat "section crosses " (strcase kind T) " but no cut wall hosts it") mid) un)))
         (T (setq rest (cons ob rest)))))))
  (list (reverse cd) (reverse cw) (reverse rest) un)
)

;;; ---------- projected walls ----------
(defun aseca:seg-interval (ln origin d / ta tb)
  (setq ta (asec:dot (asec:v- (cadr ln) origin) d) tb (asec:dot (asec:v- (caddr ln) origin) d))
  (list (min ta tb) (max ta tb) (car ln))
)

;;; gap [g0,g1] along a face is explained by a parallel opening block spanning it
(defun aseca:gap-explained-p (g0 g1 origin d n blocks / jm hit ja jb gap)
  (setq gap *aseca-small-gap-tolerance*)
  (foreach ob blocks
    (if (and (setq jm (asec:get 'jm ob))
             (aseca:parallel-p (asec:unit (asec:v- (cadr jm) (car jm))) d)
             (<= (abs (asec:dot (asec:v- (car jm) origin) n)) (+ (cadddr jm) *aseca-max-wall-thickness*))
             (setq ja (asec:dot (asec:v- (car jm) origin) d) jb (asec:dot (asec:v- (cadr jm) origin) d))
             (<= (min ja jb) (+ g0 gap))
             (>= (max ja jb) (- g1 gap)))
      (setq hit T)))
  hit
)

(defun aseca:make-face (cur origin d)
  (list (cons 'a (asec:v+ origin (asec:v* d (car cur))))
        (cons 'b (asec:v+ origin (asec:v* d (cadr cur))))
        (cons 'dir d) (cons 'len (- (cadr cur) (car cur)))
        (cons 'segs (caddr cur)))
)

;;; parallel + collinear LINEs grouped into logical faces, split at gaps that are
;;; neither small nor explained by an opening. Source lines are never joined.
(defun aseca:chain-faces (cand blocks / ln d n grp rest cur faces)
  (while cand
    (setq ln (car cand) cand (cdr cand)
          d (asec:unit (asec:v- (caddr ln) (cadr ln))) n (asec:perp d)
          grp (list ln) rest nil cur nil)
    (foreach o cand
      (if (and (aseca:parallel-p (asec:unit (asec:v- (caddr o) (cadr o))) d)
               (<= (abs (asec:dot (asec:v- (cadr o) (cadr ln)) n)) *aseca-collinear-tolerance*))
        (setq grp (cons o grp))
        (setq rest (cons o rest))))
    (setq cand (reverse rest))
    (foreach sg (asec:sort (mapcar '(lambda (g) (aseca:seg-interval g (cadr ln) d)) grp)
                           '(lambda (p q) (< (car p) (car q))))
      (if (and cur
               (or (<= (- (car sg) (cadr cur)) *aseca-small-gap-tolerance*)
                   (aseca:gap-explained-p (cadr cur) (car sg) (cadr ln) d n blocks)))
        (setq cur (list (car cur) (max (cadr cur) (cadr sg)) (cons (caddr sg) (caddr cur))))
        (progn
          (if cur (setq faces (cons (aseca:make-face cur (cadr ln) d) faces)))
          (setq cur (list (car sg) (cadr sg) (list (caddr sg)))))))
    (if cur (setq faces (cons (aseca:make-face cur (cadr ln) d) faces))))
  (foreach f faces
    (aseca:dbg (strcat "\nLOGICAL FACE  " (asec:pt-str (asec:get 'a f)) " -> " (asec:pt-str (asec:get 'b f))
                       "  segments " (itoa (length (asec:get 'segs f)))
                       "  layers/handles:"
                       (apply 'strcat (mapcar '(lambda (e) (strcat " " (asec:get 8 (entget e)) "/" (asec:get 5 (entget e))))
                                              (asec:get 'segs f))))))
  faces
)

;;; faces that could be the other side of the same wall
(defun aseca:face-partners (f faces / out sep g0 g1 ov)
  (foreach g faces
    (if (and (not (eq f g))
             (aseca:parallel-p (asec:get 'dir f) (asec:get 'dir g))
             (setq sep (abs (asec:dot (asec:v- (asec:get 'a g) (asec:get 'a f)) (asec:perp (asec:get 'dir f)))))
             (>= sep *aseca-min-wall-thickness*)
             (<= sep *aseca-max-wall-thickness*)
             (setq g0 (asec:dot (asec:v- (asec:get 'a g) (asec:get 'a f)) (asec:get 'dir f))
                   g1 (asec:dot (asec:v- (asec:get 'b g) (asec:get 'a f)) (asec:get 'dir f))
                   ov (- (min (asec:get 'len f) (max g0 g1)) (max 0.0 (min g0 g1))))
             (>= ov (* 0.5 (min (asec:get 'len f) (asec:get 'len g)))))
      (setq out (cons g out))))
  out
)

(defun aseca:partners-of (f pl / r) (foreach e pl (if (eq (car e) f) (setq r (cdr e)))) r)

;;; partners all lie on one support line without overlapping each other
;;; (e.g. an outer wall face opposite two inner faces split by a T-junction)
(defun aseca:collinear-disjoint-p (ps / ok d a0 a1 b0 b1)
  (setq ok T)
  (foreach g ps
    (foreach h ps
      (if (and ok (not (eq g h)))
        (progn
          (setq d (asec:get 'dir g)
                a0 0.0 a1 (asec:get 'len g)
                b0 (asec:dot (asec:v- (asec:get 'a h) (asec:get 'a g)) d)
                b1 (asec:dot (asec:v- (asec:get 'b h) (asec:get 'a g)) d))
          (if (or (not (aseca:parallel-p d (asec:get 'dir h)))
                  (> (abs (asec:dot (asec:v- (asec:get 'a h) (asec:get 'a g)) (asec:perp d)))
                     *aseca-collinear-tolerance*)
                  (> (- (min a1 (max b0 b1)) (max a0 (min b0 b1))) *aseca-collinear-tolerance*))
            (setq ok nil))))))
  ok
)

;;; portion of face f opposite face g, as (a b) on f's support line
(defun aseca:clip-face-to (f g / d t0 t1)
  (setq d (asec:get 'dir f)
        t0 (max 0.0 (min (asec:dot (asec:v- (asec:get 'a g) (asec:get 'a f)) d)
                         (asec:dot (asec:v- (asec:get 'b g) (asec:get 'a f)) d)))
        t1 (min (asec:get 'len f) (max (asec:dot (asec:v- (asec:get 'a g) (asec:get 'a f)) d)
                                       (asec:dot (asec:v- (asec:get 'b g) (asec:get 'a f)) d))))
  (list (asec:v+ (asec:get 'a f) (asec:v* d t0)) (asec:v+ (asec:get 'a f) (asec:v* d t1)))
)

;;; A jamb return at a door/window hole crosses both faces and looks like a wall end.
;;; If an opening block starts at a wall end, the wall continues over it: extend the end
;;; to the far jamb, then look for a real closing line there. ext = (e0 e1 m0 m1 confident)
(defun aseca:extend-over-openings (ext f1 f2 gg blocks / d n off origin e0 e1 m0 m1 c0 c1 x0 x1 i changed
                                       jm ja jb lo hi lat gap)
  (setq d (car gg) n (cadr gg) off (caddr gg) origin (car f1) gap *aseca-small-gap-tolerance*
        e0 (car ext) e1 (cadr ext) m0 (caddr ext) m1 (cadddr ext) i 0 changed T)
  (while (and changed (< i 5))
    (setq changed nil i (1+ i))
    (foreach ob blocks
      (if (and (setq jm (asec:get 'jm ob))
               (aseca:parallel-p (asec:unit (asec:v- (cadr jm) (car jm))) d)
               (setq lat (- (asec:dot (asec:v- (aseca:mid (car jm) (cadr jm)) origin) n) (* 0.5 off)))
               (<= (abs lat) (+ (cadddr jm) (abs off)))
               (setq ja (asec:dot (asec:v- (car jm) origin) d)
                     jb (asec:dot (asec:v- (cadr jm) origin) d)
                     lo (min ja jb) hi (max ja jb)))
        (cond
          ((and (<= (abs (- lo e1)) gap) (> hi (+ e1 asec:tol)))
           (setq e1 hi x1 T changed T))
          ((and (<= (abs (- hi e0)) gap) (< lo (- e0 asec:tol)))
           (setq e0 lo x0 T changed T))))))
  (if x0 (setq c0 (asec:find-projected-wall-closing-line f1 f2 d n off e0 T)
               m0 (if c0 "OPENING+CLOSING" "OPENING") e0 (if c0 c0 e0)))
  (if x1 (setq c1 (asec:find-projected-wall-closing-line f1 f2 d n off e1 nil)
               m1 (if c1 "OPENING+CLOSING" "OPENING") e1 (if c1 c1 e1)))
  (list e0 e1 m0 m1
        (and (if x0 c0 (= m0 "CLOSING-LINES")) (if x1 c1 (= m1 "CLOSING-LINES"))))
)

;;; projected wall record (with conf/view-rel/segs) from face lines f1 f2, or nil
(defun aseca:build-projected-wall (f1 f2 segs ctx blocks / gg ext rec vb)
  (setq vb (asec:get 'view ctx) gg (asec:validate-projected-wall-faces f1 f2))
  (if (/= gg 'bad)
    (progn
      (setq ext (aseca:extend-over-openings (asec:projected-wall-auto-extent f1 f2 gg) f1 f2 gg blocks))
      (aseca:dbg (strcat "\nPROJECTED PAIR  " (asec:pt-str (car f1)) " / " (asec:pt-str (car f2))
                         "  ends " (caddr ext) " / " (cadddr ext)
                         "  owned segments " (itoa (length segs))))
      (if (> (- (cadr ext) (car ext)) asec:tol)
        (progn
          (setq rec (asec:make-projected-wall f1 f2 gg (list (car ext) (cadr ext) (caddr ext) (cadddr ext)) ctx))
          (append (list (cons 'conf (if (nth 4 ext) "HIGH" "MEDIUM"))
                        (cons 'view-rel (asec:classify-in-view
                                          (list (asec:get 'u1 rec) (asec:get 'v1 rec))
                                          (list (asec:get 'u2 rec) (asec:get 'v2 rec)) vb))
                        (cons 'segs segs))
                  rec)))))
)

(defun aseca:face-line (f) (list (asec:get 'a f) (asec:get 'b f)))

;;; (pwalls unresolved). A face pairs with its unique partner; one face opposite several
;;; collinear non-overlapping faces (T-junction) pairs with each of them. Faces shorter
;;; than *aseca-min-face-length* (jamb returns, nibs) are not wall-face candidates.
(defun aseca:discover-projected-walls (lines excl blocks ctx / se dir vd vb cand faces pl used un pws
                                             ps g rec)
  (setq se (asec:get 'se ctx) dir (asec:get 'dir ctx) vd (asec:get 'vd ctx) vb (asec:get 'view ctx))
  (foreach ln lines
    (if (and (not (member (car ln) excl))
             (> (distance (cadr ln) (caddr ln)) asec:tol)
             (asec:entity-bounds-overlap-view-p (cadr ln) (caddr ln) (car se) dir vd vb))
      (setq cand (cons ln cand))))
  (foreach f (aseca:chain-faces (reverse cand) blocks)
    (if (>= (asec:get 'len f) *aseca-min-face-length*) (setq faces (cons f faces))))
  (foreach f faces (setq pl (cons (cons f (aseca:face-partners f faces)) pl)))
  (foreach f faces
    (setq ps (aseca:partners-of f pl))
    (cond
      ((aseca:memq f used))
      ((and (> (length ps) 1) (aseca:collinear-disjoint-p ps))
       (aseca:dbg (strcat "\nT-JUNCTION FACE  " (asec:pt-str (asec:get 'a f)) "  opposite "
                          (itoa (length ps)) " collinear faces"))
       (setq used (cons f used))
       (foreach g ps
         (if (and (not (aseca:memq g used))
                  (= (length (aseca:partners-of g pl)) 1))
           (progn
             (setq used (cons g used))
             (if (setq rec (aseca:build-projected-wall (aseca:clip-face-to f g) (aseca:face-line g)
                                                       (append (asec:get 'segs f) (asec:get 'segs g)) ctx blocks))
               (setq pws (cons rec pws)))))))
      ((> (length ps) 1)
       (setq un (cons (aseca:unres (strcat "wall face has " (itoa (length ps)) " possible partner faces")
                                   (asec:get 'a f))
                      un)))
      ((and (= (length ps) 1)
            (setq g (car ps))
            (= (length (aseca:partners-of g pl)) 1)
            (not (aseca:memq g used)))
       (setq used (cons f (cons g used)))
       (if (setq rec (aseca:build-projected-wall (aseca:face-line f) (aseca:face-line g)
                                                 (append (asec:get 'segs f) (asec:get 'segs g)) ctx blocks))
         (setq pws (cons rec pws))))))
  (list (reverse pws) un)
)

;;; ---------- projected openings ----------
;;; (projdoors projwins unresolved)
(defun aseca:classify-projected-openings (blocks pws ctx / se dir vd vb pd pw un jm bd kind uv0 uv1 rec
                                               cands pwl tl)
  (setq se (asec:get 'se ctx) dir (asec:get 'dir ctx) vd (asec:get 'vd ctx) vb (asec:get 'view ctx)
        tl *asec-wall-host-tolerance*)
  (foreach ob blocks
    (setq jm (asec:get 'jm ob) bd (asec:get 'bd ob) kind (asec:get 'kind ob)
          uv0 (asec:section-coordinate (car jm) (car se) dir vd)
          uv1 (asec:section-coordinate (cadr jm) (car se) dir vd))
    (cond
      ((= (asec:classify-in-view uv0 uv1 vb) "OUTSIDE")                 ; not in view: ignore
       (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " ignored: outside view boundary  V = "
                          (rtos (cadr uv0) 2 0) " / " (rtos (cadr uv1) 2 0)
                          "  (vFar " (rtos (asec:get 'vfar vb) 2 0) ")")))
      ((< (abs (- (car uv1) (car uv0))) asec:tol)                        ; edge-on: invisible
       (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " ignored: edge-on to the section")))
      (T
       (setq rec (asec:make-opening kind "PROJECTED" bd jm nil (min (car uv0) (car uv1))
                                    (max (car uv0) (car uv1)) ctx (if (= kind "DOOR") "SINGLE"))
             cands (asec:projected-host-candidates jm pws))
       (cond
         ((> (length cands) 1)
          (setq un (cons (aseca:unres (strcat "projected " (strcase kind T) " matches several projected walls")
                                      (aseca:mid (car jm) (cadr jm)))
                         un)))
         ;; in view but in no projected wall (e.g. door between a wall end and another wall):
         ;; keep it unhosted, as guided ASEC's Keep does
         ((null cands)
          (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " -> PROJECTED unhosted"))
          (setq rec (cons (cons 'conf "MEDIUM") rec))
          (if (= kind "DOOR") (setq pd (cons rec pd)) (setq pw (cons rec pw))))
         ((and (setq pwl (nth (1- (caar cands)) pws))
               (>= (asec:get 'umin rec) (- (asec:get 'umin pwl) tl))
               (<= (asec:get 'umax rec) (+ (asec:get 'umax pwl) tl)))
          (aseca:dbg (strcat "\nOPENING " (asec:get 'name bd) " -> PROJECTED in P" (itoa (caar cands))))
          (setq rec (append (list (cons 'phost (caar cands)) (cons 'conf "HIGH")) rec))
          (if (= kind "DOOR") (setq pd (cons rec pd)) (setq pw (cons rec pw))))
         (T
          (setq un (cons (aseca:unres (strcat "projected " (strcase kind T) " exceeds its host wall extent")
                                      (aseca:mid (car jm) (cadr jm)))
                         un)))))))
  (list (reverse pd) (reverse pw) un)
)

;;; ---------- review ----------
(defun aseca:conf (r) (cond ((asec:get 'conf r)) ("GUIDED")))

(defun aseca:label-ops (ops pre ctx / k)
  (setq k 0)
  (foreach o ops
    (setq k (1+ k))
    (asec:label (aseca:uv->plan (asec:get 'u o) (asec:get 'v o) ctx) (strcat pre (itoa k)) (asec:get 'se ctx)))
)

(defun aseca:label-all (walls cd cw pws pd pw un ctx / se k)
  (asec:clear-labels)
  (setq se (asec:get 'se ctx) k 0)
  (foreach w walls
    (setq k (1+ k))
    (asec:label (aseca:mid (asec:get 'p1 w) (asec:get 'p2 w)) (strcat "C" (itoa k)) se))
  (aseca:label-ops cd "CD" ctx)
  (aseca:label-ops cw "CW" ctx)
  (setq k 0)
  (foreach w pws
    (setq k (1+ k))
    (asec:label (aseca:mid (asec:get 'cstart w) (asec:get 'cend w)) (strcat "P" (itoa k)) se))
  (aseca:label-ops pd "PD" ctx)
  (aseca:label-ops pw "PW" ctx)
  (setq k 0)
  (foreach u un
    (setq k (1+ k))
    (if (asec:get 'pt u) (asec:label (asec:get 'pt u) (strcat "?" (itoa k)) se)))
)

(defun aseca:print-ops (ops pre hostkey hostpre / k)
  (setq k 0)
  (foreach o ops
    (setq k (1+ k))
    (princ (strcat "\n  " pre (itoa k) "  width " (rtos (asec:get 'width o) 2 0)
                   (if (asec:get hostkey o) (strcat "  -> " hostpre (itoa (asec:get hostkey o))) "  unhosted")
                   "  " (aseca:conf o))))
)

(defun aseca:print-summary (i walls cd cw pws pd pw un / k)
  (princ (strcat "\n\nASECA - " (asec:floor-name i) " ANALYSIS"))
  (princ "\nCUT WALLS")
  (setq k 0)
  (foreach w walls
    (setq k (1+ k))
    (princ (strcat "\n  C" (itoa k) "  " (rtos (asec:get 'thk w) 2 0) "  " (aseca:conf w))))
  (princ "\nCUT OPENINGS")
  (aseca:print-ops cd "CD" 'host "C")
  (aseca:print-ops cw "CW" 'host "C")
  (princ "\nPROJECTED WALLS")
  (setq k 0)
  (foreach w pws
    (setq k (1+ k))
    (princ (strcat "\n  P" (itoa k) "  " (rtos (asec:get 'thk w) 2 0)
                   "  length " (rtos (- (asec:get 't1 w) (asec:get 't0 w)) 2 0)
                   "  " (aseca:conf w)
                   (if (asec:get 'view-rel w) (strcat "  " (asec:get 'view-rel w)) ""))))
  (princ "\nPROJECTED OPENINGS")
  (aseca:print-ops pd "PD" 'phost "P")
  (aseca:print-ops pw "PW" 'phost "P")
  (princ "\nUNRESOLVED")
  (setq k 0)
  (foreach u un
    (setq k (1+ k))
    (princ (strcat "\n  ?" (itoa k) "  " (asec:get 'reason u))))
  (if (null un) (princ "\n  none"))
)

(defun aseca:drop-nth (lst n / i out)
  (setq i 0)
  (foreach x lst (setq i (1+ i)) (if (/= i n) (setq out (cons x out))))
  (reverse out)
)

(defun aseca:without (key o / out)
  (foreach x o (if (not (eq (car x) key)) (setq out (cons x out))))
  (reverse out)
)

;;; after removing wall n: openings on n are dropped (drop T) or unhosted, higher hosts shift down
(defun aseca:rehost (ops key n drop / out h)
  (foreach o ops
    (setq h (asec:get key o))
    (cond
      ((or (null h) (< h n)) (setq out (cons o out)))
      ((= h n) (if (not drop) (setq out (cons (aseca:without key o) out))))
      (T (setq out (cons (subst (cons key (1- h)) (assoc key o) o) out)))))
  (reverse out)
)

(defun aseca:valid-n (n lst)
  (if (and n (<= 1 n (length lst))) T (progn (princ "\nInvalid number.") nil))
)

;;; ---------- floor analysis + review ----------
(defun aseca:analyse-floor (i m vd vb gfref cfg / fs ref se dir lines blocks xs pr walls excl ctx
                                cut prj pro cd cw pws pd pw un r res kind n)
  (setq fs (asec:floor-section i m vd vb gfref nil) ref (car fs) se (cadr fs) dir (caddr m))
  (princ (strcat "\nASECA - analysing " (asec:floor-name i) "..."))
  (setq lines  (mapcar 'aseca:line (aseca:ss-list "LINE" *aseca-wall-layers*))
        blocks (aseca:collect-blocks)
        xs     (aseca:cut-crossings se dir lines blocks)
        pr     (aseca:pair-cut-walls xs)
        walls  (car pr)
        un     (cadr pr)
        excl   (apply 'append (mapcar 'cadddr xs))   ; cut faces are never projected faces
        ctx    (asec:make-floor-ctx i se m vd vb cfg walls)
        cut    (aseca:classify-cut-openings blocks walls ctx)
        cd     (car cut)
        cw     (cadr cut)
        un     (append un (cadddr cut))
        prj    (aseca:discover-projected-walls lines excl blocks ctx)
        pws    (car prj)
        un     (append un (cadr prj))
        pro    (aseca:classify-projected-openings (caddr cut) pws ctx)
        pd     (car pro)
        pw     (cadr pro)
        un     (append un (caddr pro)))
  (while (not res)
    (setq ctx (asec:make-floor-ctx i se m vd vb cfg walls))
    (aseca:label-all walls cd cw pws pd pw un ctx)
    (aseca:print-summary i walls cd cw pws pd pw un)
    (initget "Generate Edit Guided Cancel")
    (setq r (getkword (strcat "\n[Generate/Edit/Guided/Cancel] <" (if un "Edit" "Generate") ">: ")))
    (if (null r) (setq r (if un "Edit" "Generate")))
    (cond
      ((= r "Cancel") (exit))
      ((= r "Guided")
       (asec:clear-labels)
       (setq res (asec:add-floor i m vd vb gfref cfg ref)))
      ((= r "Generate")
       (asec:clear-labels)
       (setq res (asec:make-floor-record i ref se ctx pws cd cw pd pw (asec:get-floor-slab i se m walls))))
      (T
       (initget "Remove Add Ignore Back")
       (setq r (getkword "\nEdit [Remove/Add/Ignore/Back] <Back>: "))
       (cond
         ((= r "Ignore") (setq un nil) (princ "\nUnresolved items ignored."))
         ((member r '("Remove" "Add"))
          (initget "CutWall CutDoor CutWindow ProjectedWall ProjectedDoor ProjectedWindow")
          (setq kind (getkword (strcat "\n" r " [CutWall/CutDoor/CutWindow/ProjectedWall/ProjectedDoor/ProjectedWindow]: ")))
          (if (= r "Remove")
            (progn
              (setq n (getint "\nNumber to remove: "))
              (cond
                ((= kind "CutWall")
                 (if (aseca:valid-n n walls)
                   (setq walls (aseca:drop-nth walls n) cd (aseca:rehost cd 'host n T) cw (aseca:rehost cw 'host n T))))
                ((= kind "CutDoor") (if (aseca:valid-n n cd) (setq cd (aseca:drop-nth cd n))))
                ((= kind "CutWindow") (if (aseca:valid-n n cw) (setq cw (aseca:drop-nth cw n))))
                ((= kind "ProjectedWall")
                 (if (aseca:valid-n n pws)
                   (setq pws (aseca:drop-nth pws n) pd (aseca:rehost pd 'phost n nil) pw (aseca:rehost pw 'phost n nil))))
                ((= kind "ProjectedDoor") (if (aseca:valid-n n pd) (setq pd (aseca:drop-nth pd n))))
                ((= kind "ProjectedWindow") (if (aseca:valid-n n pw) (setq pw (aseca:drop-nth pw n))))))
            ;; Add = the guided ASEC collectors, appended to the automatic records
            (progn
              (asec:clear-labels)
              (cond
                ((= kind "CutWall") (setq walls (append walls (asec:collect-walls se dir))))
                ((= kind "CutDoor")
                 (setq cd (append cd (asec:collect-openings "DOOR" "CUT" (cons (cons 'prior (append cd cw)) ctx)))))
                ((= kind "CutWindow")
                 (setq cw (append cw (asec:collect-openings "WINDOW" "CUT" (cons (cons 'prior (append cd cw)) ctx)))))
                ((= kind "ProjectedWall") (setq pws (append pws (asec:collect-projected-walls ctx))))
                ((= kind "ProjectedDoor")
                 (setq pd (append pd (asec:collect-openings "DOOR" "PROJECTED" (cons (cons 'pwalls pws) ctx)))))
                ((= kind "ProjectedWindow")
                 (setq pw (append pw (asec:collect-openings "WINDOW" "PROJECTED" (cons (cons 'pwalls pws) ctx)))))))))))))
  res
)

;;; ---------- command ----------
(defun c:ASECA (/ m vd vb gfref cfg floors i)
  (if (not (aseca:core-loaded-p))
    (princ "\nASEC core is not loaded.\nLoad ASEC.lsp before running ASECA.")
    (progn
      (setq asec:old-error *error* *error* asec:error
            asec:undo-open nil asec:cmdecho nil asec:temp nil
            aseca:old-closing-layers *asec-closing-layers*
            *asec-closing-layers* *aseca-wall-layers*)   ; wall ends only from wall-layer LINEs
      (setq m     (asec:get-master-section)
            vd    (asec:get-view-direction m)
            m     (asec:normalize-section-orientation m vd)
            vb    (asec:get-projection-boundary m vd)
            gfref (asec:getpt "\nPick Ground Floor reference point: ")
            cfg   (asec:get-config))
      (aseca:dbg (strcat "\nVIEW BOUNDARY  U 0 -> " (rtos (asec:get 'umax vb) 2 2)
                         "  V 0 -> " (rtos (asec:get 'vfar vb) 2 2)))
      (setq floors (list (aseca:analyse-floor 0 m vd vb gfref cfg)) i 0)
      (while (progn (initget "Yes No")
                    (= (getkword "\nAdd upper floor? [Yes/No] <No>: ") "Yes"))
        (setq i (1+ i)
              floors (cons (aseca:analyse-floor i m vd vb gfref cfg) floors)))
      (asec:generate m vd vb cfg floors)
      (setq *asec-closing-layers* aseca:old-closing-layers)
      (asec:cleanup)
      (setq *error* asec:old-error)))
  (princ)
)

(princ "\nASECauto loaded. Type ASECA to run (requires ASEC.lsp).")
(princ)
