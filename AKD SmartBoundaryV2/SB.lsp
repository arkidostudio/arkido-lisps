;;; SB.lsp - SMART BOUNDARY (AutoCAD for Mac, pure AutoLISP, no ActiveX)
;;; Room boundary from wall-layer geometry, bridging plausible door openings.
;;; Source geometry is never edited; bridges exist only in the in-memory network.
;;; Command: SB

;;; ================================================================
;;; Tunables
;;; ================================================================
(setq *sb-debug*               nil)
(setq *sb-debug-draw*          nil)    ; T: draw E#/G#/U#/face/probes on X-SB-DEBUG per pick (SBDEBUGCLEAR removes)
(setq *sb-point-tolerance*     0.5)    ; drawing units: points closer than this are one vertex
(setq *sb-angle-tolerance*     3.0)    ; degrees: allowed misalignment of a bridge vs wall direction
(setq *sb-max-candidate-count* 5000)   ; safety cap on aligned candidate pairs
(setq *sb-max-alternates*      12)     ; pass 2: conflicting candidates retried per failed pick
(setq *sb-unresolved-factor*   4.0)    ; aligned gaps up to Max Bridge x this are tracked as "unresolved"
(setq *sb-arc-step*            (/ pi 12)) ; chord angle for ARC / bulge approximation

;;; ================================================================
;;; Small helpers
;;; ================================================================
(defun sb:dbg (s) (if *sb-debug* (princ (strcat "\n  " s))))
(defun sb:r (x) (rtos x 2 1))
(defun sb:pt (p) (strcat "(" (sb:r (car p)) " " (sb:r (cadr p)) ")"))
(defun sb:2d (p) (list (car p) (cadr p)))
(defun sb:sub (a b) (list (- (car a) (car b)) (- (cadr a) (cadr b))))
(defun sb:dot (a b) (+ (* (car a) (car b)) (* (cadr a) (cadr b))))
(defun sb:cross (a b) (- (* (car a) (cadr b)) (* (cadr a) (car b))))
(defun sb:len (v) (sqrt (sb:dot v v)))
(defun sb:dist (a b) (sb:len (sb:sub a b)))
(defun sb:unit (v / l) (setq l (sb:len v)) (list (/ (car v) l) (/ (cadr v) l)))
(defun sb:ang (a b) (atan (- (cadr b) (cadr a)) (- (car b) (car a))))
(defun sb:angdeg (u v) (* (/ 180.0 pi) (atan (abs (sb:cross u v)) (sb:dot u v))))
(defun sb:drop-nth (i lst / k out)
  (setq k 0)
  (foreach x lst (if (/= k i) (setq out (cons x out))) (setq k (1+ k)))
  (reverse out)
)

;;; ================================================================
;;; Geometry collection (LINE, LWPOLYLINE incl. bulges, ARC -> straight segments)
;;; Curves are chorded; add real curve edges here later.
;;; ================================================================
(defun sb:arc-pts (cen r a0 sweep / n k out)
  (setq n (max 2 (fix (+ 0.5 (/ (abs sweep) *sb-arc-step*)))) k 0)
  (repeat (1+ n)
    (setq out (cons (polar cen (+ a0 (* sweep (/ k (float n)))) r) out) k (1+ k)))
  (reverse out)
)

(defun sb:pts->segs (pts / out)
  (while (cdr pts)
    (setq out (cons (list (sb:2d (car pts)) (sb:2d (cadr pts))) out) pts (cdr pts)))
  out
)

(defun sb:bulge-segs (p q b / th c u h cen pts)
  (if (< (abs b) 0.000001)
    (list (list p q))
    (progn
      (setq th  (* 4.0 (atan b))
            c   (sb:dist p q)
            u   (sb:unit (sb:sub q p))
            h   (/ (* 0.5 c (cos (/ th 2.0))) (sin (/ th 2.0)))
            cen (list (- (* 0.5 (+ (car p) (car q))) (* h (cadr u)))
                      (+ (* 0.5 (+ (cadr p) (cadr q))) (* h (car u))))
            pts (sb:arc-pts cen (sb:dist cen p) (sb:ang cen p) th))
      (sb:pts->segs (append (reverse (cdr (reverse pts))) (list q)))
    )
  )
)

;; Segments are (p q handle); the handle is only used for debug output.
(defun sb:tag (segs h) (mapcar '(lambda (s) (list (car s) (cadr s) h)) segs))

;; Z is dropped (sb:2d) for every entity: the solver is strictly 2D plan.
(defun sb:collect-wall-geometry (layer / ss i e d typ h segs pts el nz a0 a1 skipped nl np npc na nbul zs)
  (setq ss (ssget "_X" (list (cons 8 layer) (cons 410 (getvar "CTAB"))))
        i 0 nl 0 np 0 npc 0 na 0 nbul 0 skipped "")
  (if ss
    (repeat (sslength ss)
      (setq e (ssname ss i) d (entget e) typ (cdr (assoc 0 d)) h (cdr (assoc 5 d)) i (1+ i))
      (cond
        ((= typ "LINE")
         (setq nl (1+ nl) zs (cons (cadddr (assoc 10 d)) (cons (cadddr (assoc 11 d)) zs))
               segs (cons (list (sb:2d (cdr (assoc 10 d))) (sb:2d (cdr (assoc 11 d))) h) segs)))
        ((= typ "LWPOLYLINE")
         (setq np (1+ np)
               el (cond ((cdr (assoc 38 d))) (0.0))
               zs (cons el zs)
               nz (if (and (assoc 210 d) (< (cadddr (assoc 210 d)) 0)) -1.0 1.0)
               pts nil)
         (foreach x d
           (cond
             ((= (car x) 10)
              (setq pts (cons (list (sb:2d (trans (list (cadr x) (caddr x) el) e 0)) 0.0) pts)))
             ((and (= (car x) 42) pts)
              (if (/= 0.0 (cdr x)) (setq nbul (1+ nbul)))
              (setq pts (cons (list (caar pts) (* nz (cdr x))) (cdr pts))))
           )
         )
         (setq pts (reverse pts))
         (if (and pts (= 1 (logand 1 (cdr (assoc 70 d)))))
           (setq npc (1+ npc) pts (append pts (list (list (caar pts) 0.0)))))
         (while (cdr pts)
           (setq segs (append (sb:tag (sb:bulge-segs (caar pts) (caadr pts) (cadar pts)) h) segs)
                 pts  (cdr pts))))
        ((= typ "ARC")
         (setq na (1+ na) zs (cons (cadddr (assoc 10 d)) zs)
               a0 (cdr (assoc 50 d)) a1 (cdr (assoc 51 d)))
         (if (< a1 a0) (setq a1 (+ a1 pi pi)))
         (setq segs (append
                      (sb:tag (sb:pts->segs
                        (mapcar '(lambda (p) (trans p e 0))
                                (sb:arc-pts (cdr (assoc 10 d)) (cdr (assoc 40 d)) a0 (- a1 a0)))) h)
                      segs)))
        (T (if (not (wcmatch skipped (strcat "* " typ "*"))) (setq skipped (strcat skipped " " typ))))
      )
    )
  )
  (if *sb-debug*
    (progn
      (sb:dbg (strcat "WALL COLLECTION  layer " layer ": " (itoa nl) " LINE, " (itoa np) " LWPOLYLINE ("
                      (itoa npc) " closed, " (itoa nbul) " bulged segments), " (itoa na) " ARC"))
      (if (/= skipped "") (sb:dbg (strcat "  Unsupported entity types ignored (incl. blocks):" skipped)))
      (if zs (sb:dbg (strcat "  Source Z range: " (rtos (apply 'min zs) 2 6) " .. " (rtos (apply 'max zs) 2 6)
                             "  (flattened to 2D for analysis)")))))
  segs
)

;;; ================================================================
;;; Network normalization
;;; ================================================================
;; Hits of segment p-q against p2-q2: (list tsOnFirst tsOnSecond), parameters 0..1.
(defun sb:segment-intersection (p q p2 q2 tol / r s rl sl den w tt uu et eu ta tb)
  (setq r (sb:sub q p) s (sb:sub q2 p2) rl (sb:len r) sl (sb:len s)
        den (sb:cross r s) w (sb:sub p2 p) et (/ tol rl) eu (/ tol sl))
  (if (> (abs den) (* 0.000001 rl sl))
    (progn
      (setq tt (/ (sb:cross w s) den) uu (/ (sb:cross w r) den))
      (if (and (>= tt (- et)) (<= tt (+ 1.0 et)) (>= uu (- eu)) (<= uu (+ 1.0 eu)))
        (list (list (min 1.0 (max 0.0 tt))) (list (min 1.0 (max 0.0 uu))))))
    (if (< (/ (abs (sb:cross w r)) rl) tol)          ; collinear overlap
      (progn
        (foreach x (list p2 q2)
          (setq tt (/ (sb:dot (sb:sub x p) r) (* rl rl)))
          (if (and (> tt 0.0) (< tt 1.0)) (setq ta (cons tt ta))))
        (foreach x (list p q)
          (setq uu (/ (sb:dot (sb:sub x p2) s) (* sl sl)))
          (if (and (> uu 0.0) (< uu 1.0)) (setq tb (cons uu tb))))
        (list ta tb)))
  )
)

;; Split every segment at every intersection / touch. Sweep on min-x.
(defun sb:split-segments (segs tol / i ents srt a b rest h hits out p r prev pt)
  (setq i 0)
  (foreach s segs
    (setq p (car s) r (cadr s)
          ents (cons (list i p r (min (car p) (car r)) (max (car p) (car r))
                           (min (cadr p) (cadr r)) (max (cadr p) (cadr r))) ents)
          hits (cons (cons i 0.0) (cons (cons i 1.0) hits))
          i (1+ i)))
  (setq srt (vl-sort ents '(lambda (a b) (< (nth 3 a) (nth 3 b)))))
  (while srt
    (setq a (car srt) rest (cdr srt))
    (while (and rest (<= (nth 3 (car rest)) (+ (nth 4 a) tol)))
      (setq b (car rest))
      (if (and (<= (nth 5 b) (+ (nth 6 a) tol)) (<= (nth 5 a) (+ (nth 6 b) tol)))
        (progn
          (setq h (sb:segment-intersection (nth 1 a) (nth 2 a) (nth 1 b) (nth 2 b) tol))
          (foreach x (car h)  (setq hits (cons (cons (car a) x) hits)))
          (foreach x (cadr h) (setq hits (cons (cons (car b) x) hits)))))
      (setq rest (cdr rest)))
    (setq srt (cdr srt)))
  (setq hits (vl-sort hits '(lambda (a b) (if (= (car a) (car b)) (< (cdr a) (cdr b)) (< (car a) (car b))))))
  (setq i 0)
  (foreach s segs
    (setq p (car s) r (sb:sub (cadr s) p) prev nil)
    (while (and hits (= (caar hits) i))
      (setq pt   (list (+ (car p) (* (cdar hits) (car r))) (+ (cadr p) (* (cdar hits) (cadr r))))
            hits (cdr hits))
      (cond ((null prev) (setq prev pt))
            ((> (sb:dist prev pt) tol) (setq out (cons (list prev pt) out) prev pt))))
    (setq i (1+ i)))
  out
)

;; Merge endpoints within tol into vertices. Returns (verts edges), edges = (i j) i<j unique.
;; ponytail: x-band sweep degrades if thousands of points share one x; grid hashing if that bites.
(defun sb:normalize-network (pieces tol / k items active nid verts assign found keep i j edges out ncol)
  (setq k 0 ncol 0)
  (foreach pc pieces
    (setq items (cons (list (car pc) (* 2 k)) (cons (list (cadr pc) (1+ (* 2 k))) items)) k (1+ k)))
  (setq items (vl-sort items '(lambda (a b) (< (caar a) (caar b)))) nid 0)
  (foreach it items
    (setq keep nil found nil)
    (foreach a active
      (if (>= (car (cadr a)) (- (caar it) tol))
        (progn
          (setq keep (cons a keep))
          (if (and (not found) (<= (sb:dist (cadr a) (car it)) tol)) (setq found (car a))))))
    (setq active keep)
    (if (not found)
      (setq found nid active (cons (list nid (car it)) active) verts (cons (car it) verts) nid (1+ nid)))
    (setq assign (cons (cons (cadr it) found) assign)))
  (setq assign (vl-sort assign '(lambda (a b) (< (car a) (car b)))))
  (while assign
    (setq i (cdar assign) j (cdadr assign) assign (cddr assign))
    (if (/= i j) (setq edges (cons (if (< i j) (list i j) (list j i)) edges)) (setq ncol (1+ ncol))))
  (setq edges (vl-sort edges '(lambda (a b) (if (= (car a) (car b)) (< (cadr a) (cadr b)) (< (car a) (car b))))))
  (foreach e edges (if (not (equal e (car out))) (setq out (cons e out))))
  (sb:dbg (strcat "NETWORK NORMALIZATION: " (itoa (- (length edges) (length out)))
                  " duplicate/overlapping edge pieces merged, " (itoa ncol) " near-zero pieces collapsed"))
  (list (reverse verts) (reverse out))
)

;; Per-vertex neighbor lists sorted CCW by angle.
(defun sb:adjacency (verts edges / half pa pb out k nb)
  (foreach e edges
    (setq pa (nth (car e) verts) pb (nth (cadr e) verts)
          half (cons (list (car e) (sb:ang pa pb) (cadr e))
                     (cons (list (cadr e) (sb:ang pb pa) (car e)) half))))
  (setq half (vl-sort half '(lambda (a b) (if (= (car a) (car b)) (< (cadr a) (cadr b)) (< (car a) (car b))))))
  (setq k 0)
  (repeat (length verts)
    (setq nb nil)
    (while (and half (= (caar half) k))
      (setq nb (cons (caddr (car half)) nb) half (cdr half)))
    (setq out (cons (reverse nb) out) k (1+ k)))
  (reverse out)
)

;;; ================================================================
;;; Gap candidates
;;; ================================================================
;; Walk straight from `from` through `to` and onward along collinear edges. Returns (endVertex length).
(defun sb:straight-end (verts adj from to cosT / d cur nxt len guard)
  (setq d (sb:unit (sb:sub (nth to verts) (nth from verts)))
        cur to len (sb:dist (nth from verts) (nth to verts)) guard 0 nxt T)
  (while (and nxt (< guard 10000))
    (setq nxt nil guard (1+ guard))
    (foreach n (nth cur adj)
      (if (and (not nxt) (> (sb:dot (sb:unit (sb:sub (nth n verts) (nth cur verts))) d) cosT)) (setq nxt n)))
    (if nxt (setq len (+ len (sb:dist (nth cur verts) (nth nxt verts))) cur nxt)))
  (list cur len)
)

(defun sb:on-seg-p (p s tol / r l t0)
  (setq r (sb:sub (cadr s) (car s)) l (sb:len r))
  (and (> l 0.0)
       (< (/ (abs (sb:cross r (sb:sub p (car s)))) l) tol)
       (>= (setq t0 (/ (sb:dot r (sb:sub p (car s))) l)) (- tol))
       (<= t0 (+ l tol)))
)

;; Debug only: original wall segment (p q handle) containing pa-pb.
(defun sb:source (pa pb segs / hit s)
  (while (and segs (not hit))
    (setq s (car segs) segs (cdr segs))
    (if (and (sb:on-seg-p pa s *sb-point-tolerance*) (sb:on-seg-p pb s *sb-point-tolerance*)) (setq hit s)))
  hit
)

;; Port = open wall direction at a vertex of degree 1-2: (vertex neighbor dir point id).
;; The wall arriving from neighbor continues nowhere past the vertex.
;; Jamb returns are excluded: a straight chain shorter than the two faces it joins, both faces
;; turning the same way (U-shape). Its continuation points across the room, not along the
;; interrupted wall face; the face's own port at the same corner carries the opening.
(defun sb:ports (verts adj segs / cosT k id pk d ok w x ch wd lg1 lg2 src out)
  (setq cosT (cos (* *sb-angle-tolerance* (/ pi 180.0))) k 0 id 0)
  (sb:dbg "Open endpoints detected:")
  (foreach nb adj
    (if (<= 1 (length nb) 2)
      (progn
        (setq pk (nth k verts))
        (foreach u nb
          (setq d (sb:unit (sb:sub pk (nth u verts))) ok T)
          (foreach w nb
            (if (> (sb:dot (sb:unit (sb:sub (nth w verts) pk)) d) cosT) (setq ok nil)))
          (if (and ok (= (length nb) 2))
            (progn
              (setq w  (if (= u (car nb)) (cadr nb) (car nb))
                    wd (sb:unit (sb:sub (nth w verts) pk))
                    ch (sb:straight-end verts adj k u cosT)
                    x  nil)
              (foreach n (nth (car ch) adj)
                (if (> (sb:dot (sb:unit (sb:sub (nth n verts) (nth (car ch) verts))) wd) cosT) (setq x n)))
              (if x
                (progn
                  (setq lg1 (cadr (sb:straight-end verts adj k w cosT))
                        lg2 (cadr (sb:straight-end verts adj (car ch) x cosT)))
                  (if (< (cadr ch) (min lg1 lg2))
                    (progn
                      (setq ok nil)
                      (sb:dbg (strcat "  jamb-return direction ignored at " (sb:pt pk) " (return "
                                      (sb:r (cadr ch)) " < faces " (sb:r (min lg1 lg2)) ")"))))))))
          (if ok
            (progn
              (setq id (1+ id) out (cons (list k u d pk id) out))
              (if *sb-debug*
                (progn
                  (setq src (sb:source (nth u verts) pk segs))
                  (sb:dbg (strcat "ENDPOINT E" (itoa id) "  Point: " (sb:pt pk) "  Degree: " (itoa (length nb))
                                  "  Incoming direction: " (sb:r (* (/ 180.0 pi) (atan (cadr d) (car d)))) " deg"
                                  "  Source entity: " (sb:ent-desc src)
                                  (if src (strcat "  Source segment: " (sb:pt (car src)) " -> " (sb:pt (cadr src))) ""))))))))))
    (setq k (1+ k)))
  out
)

;; T if segment pa-pb is touched/crossed in its interior by any (p q) in segs.
(defun sb:crosses-p (pa pb segs tol / et hit s h)
  (setq et (/ tol (sb:dist pa pb)))
  (while (and segs (not hit))
    (setq s (car segs) segs (cdr segs))
    (if (and (<= (min (car (car s)) (car (cadr s))) (+ (max (car pa) (car pb)) tol))
             (>= (max (car (car s)) (car (cadr s))) (- (min (car pa) (car pb)) tol))
             (<= (min (cadr (car s)) (cadr (cadr s))) (+ (max (cadr pa) (cadr pb)) tol))
             (>= (max (cadr (car s)) (cadr (cadr s))) (- (min (cadr pa) (cadr pb)) tol)))
      (progn
        (setq h (sb:segment-intersection pa pb (car s) (cadr s) tol))
        (foreach x (car h) (if (and (> x et) (< x (- 1.0 et))) (setq hit T))))))
  hit
)

(defun sb:score-gap (dist angA angB maxb)
  (- 1.0 (* 0.5 (/ dist maxb)) (* 0.25 (/ (+ angA angB) *sb-angle-tolerance*)))
)

;; Direction error within tolerance, and sideways offset no larger than the angle tolerance
;; allows over Max Bridge (keeps long "unresolved" gaps from matching merely offset walls).
(defun sb:accept-gap-p (angA angB dist maxb)
  (and (<= angA *sb-angle-tolerance*) (<= angB *sb-angle-tolerance*)
       (<= (* dist (sin (* (/ pi 180.0) (max angA angB))))
           (+ *sb-point-tolerance* (* maxb (sin (* (/ pi 180.0) *sb-angle-tolerance*))))))
)

(defun sb:gline (gid a b dist angA angB)
  (strcat "CANDIDATE G" (itoa gid) "  E" (itoa (nth 4 a)) " -> E" (itoa (nth 4 b))
          "  Distance: " (sb:r dist) "  Direction error A: " (sb:r angA) " deg  B: " (sb:r angB) " deg")
)

;; Returns (candidates unresolved).
;; candidate  = (score portA portB dist angA angB gid)
;; unresolved = (vertexA vertexB dist gid)
(defun sb:find-gap-candidates (ports wsegs maxb / tol window srt a b rest v dist u angA angB cands unres n gid)
  (setq tol *sb-point-tolerance* window (* maxb *sb-unresolved-factor*) n 0 gid 0
        srt (vl-sort ports '(lambda (a b) (< (car (nth 3 a)) (car (nth 3 b))))))
  (sb:dbg "Gap candidates:")
  (while srt
    (setq a (car srt) rest (cdr srt))
    (while (and rest (<= (- (car (nth 3 (car rest))) (car (nth 3 a))) window))
      (setq b (car rest) rest (cdr rest))
      (if (/= (car a) (car b))
        (progn
          (setq v (sb:sub (nth 3 b) (nth 3 a)) dist (sb:len v))
          (if (and (> dist tol) (<= dist window))
            (progn
              (setq u    (list (/ (car v) dist) (/ (cadr v) dist))
                    angA (sb:angdeg (nth 2 a) u)
                    angB (sb:angdeg (nth 2 b) (list (- (car u)) (- (cadr u)))))
              (cond
                ((> dist (+ maxb tol))
                 (if (sb:accept-gap-p angA angB dist maxb)
                   (progn
                     (setq gid (1+ gid))
                     (if (sb:crosses-p (nth 3 a) (nth 3 b) wsegs tol)
                       (sb:dbg-g gid a b (strcat (sb:gline gid a b dist angA angB) "  Crosses wall: YES  Decision: REJECT  Reason: distance exceeds Max Bridge; crosses wall geometry"))
                       (progn
                         (setq unres (cons (list (car a) (car b) dist gid) unres))
                         (sb:dbg-g gid a b (strcat (sb:gline gid a b dist angA angB) "  Crosses wall: NO  Decision: REJECT  Reason: distance exceeds Max Bridge (unresolved opening)")))))))
                ((not (sb:accept-gap-p angA angB dist maxb))
                 (if (and (< angA 45.0) (< angB 45.0))
                   (progn
                     (setq gid (1+ gid))
                     (sb:dbg-g gid a b (strcat (sb:gline gid a b dist angA angB) "  Decision: REJECT  Reason: direction error or sideways offset exceeds tolerance")))))
                ((sb:crosses-p (nth 3 a) (nth 3 b) wsegs tol)
                 (setq gid (1+ gid))
                 (sb:dbg-g gid a b (strcat (sb:gline gid a b dist angA angB) "  Crosses wall: YES  Decision: REJECT  Reason: crosses wall geometry")))
                (T
                 (setq gid (1+ gid) n (1+ n)
                       cands (cons (list (sb:score-gap dist angA angB maxb) a b dist angA angB gid) cands))
                 (sb:dbg-g gid a b (strcat (sb:gline gid a b dist angA angB) "  Crosses wall: NO  Geometry: PASS  Score: "
                                 (rtos (car (car cands)) 2 2) "  (to matching)"))
                 (if (> n *sb-max-candidate-count*)
                   (progn (princ "\nSB: candidate limit reached; remaining gaps ignored.")
                          (setq rest nil srt (list nil)))))
              )))))
    )
    (setq srt (cdr srt)))
  (list cands unres)
)

(defun sb:cand-edge (c) (list (car (nth 1 c)) (car (nth 2 c))))
(defun sb:cand-mid (c) (mapcar '(lambda (x y) (* 0.5 (+ x y))) (nth 3 (nth 1 c)) (nth 3 (nth 2 c))))

;; Greedy by score: each port used once, no crossing bridges.
;; Returns (accepted alternates); alternates are geometry-valid candidates lost to a conflict.
(defun sb:select-bridges (cands / tol used acc alts bsegs ia ib pa pb hit)
  (setq tol *sb-point-tolerance*)
  (sb:dbg "Bridge matching:")
  (foreach c (vl-sort cands '(lambda (a b) (> (car a) (car b))))
    (setq ia (nth 4 (nth 1 c)) ib (nth 4 (nth 2 c)) pa (nth 3 (nth 1 c)) pb (nth 3 (nth 2 c)) hit nil)
    (cond
      ((setq hit (cond ((assoc ia used)) ((assoc ib used))))
       (setq alts (cons c alts))
       (sb:dbg-g (nth 6 c) (nth 1 c) (nth 2 c) (strcat "CANDIDATE G" (itoa (nth 6 c)) "  Decision: REJECT  Reason: endpoint E" (itoa (car hit))
                       " already consumed by better candidate G" (itoa (cdr hit)))))
      ((progn (foreach s bsegs (if (and (not hit) (sb:crosses-p pa pb (list s) tol)) (setq hit (caddr s)))) hit)
       (setq alts (cons c alts))
       (sb:dbg-g (nth 6 c) (nth 1 c) (nth 2 c) (strcat "CANDIDATE G" (itoa (nth 6 c)) "  Decision: REJECT  Reason: bridge crossing with G" (itoa hit))))
      (T
       (setq used  (cons (cons ia (nth 6 c)) (cons (cons ib (nth 6 c)) used))
             bsegs (cons (list pa pb (nth 6 c)) bsegs)
             acc   (cons c acc))
       (sb:dbg-g (nth 6 c) (nth 1 c) (nth 2 c) (strcat "CANDIDATE G" (itoa (nth 6 c)) "  A: " (sb:pt pa) "  B: " (sb:pt pb)
                       "  Distance: " (sb:r (nth 3 c)) "  Score: " (rtos (car c) 2 2) "  Decision: ACCEPT")))
    ))
  (list (reverse acc) alts)
)

;; Full analysis model.
;; net = (0 verts  1 edges+bridges  2 adjacency  3 unresolved  4 bridgeEdges  5 segcount
;;        6 wallEdges  7 accepted  8 alternates  9 ports  10 debugLog  11 segs)
(defun sb:build-virtual-network (segs maxb / tol nw verts edges ports cr sel bedges all i nz ns sb-log)
  (setq tol *sb-point-tolerance* i 0 nz 0 ns 0)
  (foreach s segs
    (cond ((<= (sb:dist (car s) (cadr s)) tol) (setq nz (1+ nz)))
          (T (if (< (sb:dist (car s) (cadr s)) (* 10.0 tol)) (setq ns (1+ ns)))
             (setq nw (cons s nw)))))
  (sb:dbg (strcat "SB NETWORK DEBUG\n  Wall segments: " (itoa (length nw)) "  zero-length ignored: " (itoa nz)
                  "  very short (<" (sb:r (* 10.0 tol)) ") kept: " (itoa ns)))
  (if (and *sb-debug* (<= (length nw) 300))
    (foreach s nw
      (sb:dbg (strcat "  S" (itoa (setq i (1+ i))) " " (sb:pt (car s)) " -> " (sb:pt (cadr s)) "  " (sb:ent-desc s)))))
  (setq cr    (sb:normalize-network (sb:split-segments nw tol) tol)
        verts (car cr) edges (cadr cr))
  (sb:dbg (strcat "Network: " (itoa (length verts)) " vertices, " (itoa (length edges)) " edges"))
  (setq ports  (sb:ports verts (sb:adjacency verts edges) nw)
        cr     (sb:find-gap-candidates ports
                                       (mapcar '(lambda (e) (list (nth (car e) verts) (nth (cadr e) verts))) edges)
                                       maxb)
        sel    (sb:select-bridges (car cr))
        bedges (mapcar '(lambda (c) (sb:cand-edge c)) (car sel))
        all    (append edges bedges))
  (sb:dbg (strcat "Virtual bridges: " (itoa (length bedges))
                  "  Unresolved aligned gaps: " (itoa (length (cadr cr)))))
  (list verts all (sb:adjacency verts all) (cadr cr) bedges (length nw) edges (car sel) (cadr sel)
        ports (reverse sb-log) nw)
)

;;; ================================================================
;;; Region solving
;;; ================================================================
;; Walk a face keeping it on the left (next = clockwise neighbor of the reverse edge).
(defun sb:trace-face (u v adj guard / su sv out nb p w n)
  (setq su u sv v n 0)
  (while (progn
           (setq out (cons u out) nb (nth v adj) p (vl-position u nb)
                 w (nth (if (= p 0) (1- (length nb)) (1- p)) nb)
                 u v v w n (1+ n))
           (and (< n guard) (not (and (= u su) (= v sv))))))
  (if (< n guard) (reverse out))
)

(defun sb:area (pts / a q)
  (setq a 0.0 q (last pts))
  (foreach p pts (setq a (+ a (sb:cross q p)) q p))
  (/ a 2.0)
)

(defun sb:point-in-region-p (pt pts / in q)
  (setq q (last pts))
  (foreach p pts
    (if (and (not (eq (> (cadr p) (cadr pt)) (> (cadr q) (cadr pt))))
             (< (car pt) (+ (car p) (/ (* (- (cadr pt) (cadr p)) (- (car q) (car p))) (- (cadr q) (cadr p))))))
      (setq in (not in)))
    (setq q p))
  in
)

;; Cast a ray +X from pt; the nearest crossing whose left face is bounded and contains pt wins.
;; ponytail: islands (columns) inside a room are not subtracted.
(defun sb:find-regions (verts edges adj pt / xs pa pb x ids res pts guard ar in)
  (setq guard (+ 2 (* 2 (length edges))))
  (foreach e edges
    (setq pa (nth (car e) verts) pb (nth (cadr e) verts))
    (if (not (eq (> (cadr pa) (cadr pt)) (> (cadr pb) (cadr pt))))
      (progn
        (setq x (+ (car pa) (/ (* (- (cadr pt) (cadr pa)) (- (car pb) (car pa))) (- (cadr pb) (cadr pa)))))
        (if (> x (car pt)) (setq xs (cons (list x (car e) (cadr e)) xs))))))
  (setq xs (vl-sort xs '(lambda (a b) (< (car a) (car b)))))
  (sb:dbg (strcat "Ray hits: " (itoa (length xs))))
  (while (and xs (not res))
    (setq pa (nth (cadar xs) verts) pb (nth (caddar xs) verts)
          ids (if (> (sb:cross (sb:sub pb pa) (sb:sub pt pa)) 0.0)
                (sb:trace-face (cadar xs) (caddar xs) adj guard)
                (sb:trace-face (caddar xs) (cadar xs) adj guard)))
    (sb:dbg (strcat "  hit x=" (sb:r (car (car xs))) " on " (sb:pt pa) "-" (sb:pt pb)))
    (setq xs (cdr xs))
    (if ids
      (progn
        (setq pts (mapcar '(lambda (i) (nth i verts)) ids) ar (sb:area pts) in (sb:point-in-region-p pt pts))
        (sb:dbg (strcat "  face walk: " (itoa (length ids)) " vertices, area " (sb:r ar)
                        ", contains pick: " (if in "YES" "NO")
                        (cond ((and (> ar 0.0) in) "  -> candidate region")
                              ((<= ar 0.0) "  -> outer/hole face, skipped")
                              (T "  -> does not contain pick, skipped"))))
        (if (and (> ar 0.0) in) (setq res ids)))
      (sb:dbg "  face walk aborted (guard exceeded)")))
  res
)

;; Remove spikes (dangling stubs), duplicates and collinear vertices.
(defun sb:clean-ring (ids verts / tol changed n i pts a b c l)
  (setq tol *sb-point-tolerance* changed T)
  (while (and changed (> (length ids) 2))
    (setq changed nil n (length ids) i 0)
    (while (and (not changed) (< i n))
      (if (or (= (nth i ids) (nth (rem (1+ i) n) ids))
              (= (nth (rem (+ i n -1) n) ids) (nth (rem (1+ i) n) ids)))
        (setq ids (sb:drop-nth i ids) changed T))
      (setq i (1+ i))))
  (setq pts (mapcar '(lambda (i) (nth i verts)) ids) changed T)
  (while (and changed (> (length pts) 2))
    (setq changed nil n (length pts) i 0)
    (while (and (not changed) (< i n))
      (setq a (nth (rem (+ i n -1) n) pts) b (nth i pts) c (nth (rem (1+ i) n) pts) l (sb:dist a c))
      (if (or (< l tol) (< (/ (abs (sb:cross (sb:sub c a) (sb:sub b a))) l) tol))
        (setq pts (sb:drop-nth i pts) changed T))
      (setq i (1+ i))))
  pts
)

;; One solve over a given edge set. Returns (pts key) or (nil message).
(defun sb:solve-region (verts edges adj unres pt maxb cands / ids best bd d m pts)
  (setq ids (sb:find-regions verts edges adj pt))
  (if (and *sb-debug* ids) (sb:face-report ids verts unres cands))
  (cond
    ((null ids)
     (sb:dbg "Stage failed: region search (no bounded face encloses the pick point).")
     (foreach g unres
       (setq m (mapcar '(lambda (x y) (* 0.5 (+ x y))) (nth (car g) verts) (nth (cadr g) verts))
             d (sb:dist m pt))
       (if (or (null bd) (< d bd)) (setq bd d best g)))
     (list nil
       (strcat "Unable to determine a reliable room boundary."
         (if best
           (strcat "\nPossible unresolved opening: " (rtos (caddr best) 2 0)
                   " (Max Bridge " (rtos maxb 2 0) ").\nIncrease Max Bridge or inspect wall geometry.")
           "\nNo aligned opening found near this room; inspect wall geometry for gaps or misaligned ends."))))
    ((progn
       (foreach g unres
         (if (and (member (car g) ids) (member (cadr g) ids) (or (null best) (< (caddr g) (caddr best))))
           (setq best g)))
       best)
     (sb:dbg (strcat "Stage failed: leak validation (region contains unresolved opening G" (itoa (cadddr best))
                     " " (sb:pt (nth (car best) verts)) "-" (sb:pt (nth (cadr best) verts)) ")."))
     (list nil
       (strcat "Unable to determine a reliable room boundary."
               "\nRegion leaks through an opening of " (rtos (caddr best) 2 0)
               " (Max Bridge " (rtos maxb 2 0) ").\nIncrease Max Bridge or inspect wall geometry.")))
    ((< (length (setq pts (sb:clean-ring ids verts))) 3)
     (sb:dbg "Stage failed: simplification (degenerate ring).")
     (list nil "Unable to determine a reliable room boundary (degenerate region)."))
    (T (list pts (vl-sort ids '(lambda (a b) (< a b)))))
  )
)

;; PASS 2: swap in geometry-valid candidates that lost matching to a conflict, nearest the pick first.
(defun sb:second-pass (net pt maxb first / verts alts n res keep c edges r conflicts ports)
  (setq verts (car net) n 0
        alts  (vl-sort (nth 8 net) '(lambda (a b) (< (sb:dist (sb:cand-mid a) pt) (sb:dist (sb:cand-mid b) pt)))))
  (sb:dbg (strcat "PASS 2: " (itoa (length alts)) " conflicting candidate(s) to reconsider."))
  (while (and alts (not res) (< n *sb-max-alternates*))
    (setq c (car alts) alts (cdr alts) n (1+ n) keep nil conflicts ""
          ports (list (nth 4 (nth 1 c)) (nth 4 (nth 2 c))))
    (foreach k (nth 7 net)
      (if (or (member (nth 4 (nth 1 k)) ports) (member (nth 4 (nth 2 k)) ports)
              (sb:crosses-p (nth 3 (nth 1 c)) (nth 3 (nth 2 c))
                            (list (list (nth 3 (nth 1 k)) (nth 3 (nth 2 k)))) *sb-point-tolerance*))
        (setq conflicts (strcat conflicts " G" (itoa (nth 6 k))))
        (setq keep (cons k keep))))
    (setq edges (append (nth 6 net) (mapcar '(lambda (k) (sb:cand-edge k)) (cons c keep)))
          r     (sb:solve-region verts edges (sb:adjacency verts edges) (nth 3 net) pt maxb (cons c keep)))
    (sb:dbg (strcat "PASS 2 try G" (itoa (nth 6 c)) " replacing" (if (= conflicts "") " nothing" conflicts)
                    ": " (if (car r) "VALID REGION" "no valid region")))
    (if (car r) (setq res r)))
  (cond (res) (first))
)

;; Returns (pts key) on success, (nil message) on failure. pt in WCS.
(defun sb:solve-pick (net pt maxb / r s)
  (sb:dbg (strcat "\nSB ROOM SOLVE DEBUG\n  Pick: " (sb:pt pt)))
  (if *sb-debug*
    (progn
      (setq s "")
      (foreach c (nth 7 net) (setq s (strcat s " G" (itoa (nth 6 c)))))
      (sb:dbg (strcat "Accepted bridges:" (if (= s "") " none" s)))
      (setq s "")
      (foreach c (nth 8 net) (setq s (strcat s " G" (itoa (nth 6 c)))))
      (sb:dbg (strcat "Rejected in matching:" (if (= s "") " none" s)))
      (setq s "")
      (foreach g (nth 3 net) (setq s (strcat s " G" (itoa (cadddr g)) "(" (sb:r (caddr g)) ")")))
      (sb:dbg (strcat "Unresolved openings:" (if (= s "") " none" s)))
      (sb:dbg "PASS 1:")))
  (setq r (sb:solve-region (car net) (cadr net) (caddr net) (nth 3 net) pt maxb (nth 7 net)))
  (if (and (null (car r)) (nth 8 net)) (setq r (sb:second-pass net pt maxb r)))
  (if (and *sb-debug* (null (car r))) (sb:local-report net pt maxb))
  (sb:dbg (strcat "Final result: " (if (car r) (strcat "room boundary, " (itoa (length (car r))) " vertices") "FAILED")))
  r
)
;;; ================================================================
;;; Debug reporting and drawing (only active with *sb-debug* / *sb-debug-draw*)
;;; ================================================================
(defun sb:mid (a b) (list (* 0.5 (+ (car a) (car b))) (* 0.5 (+ (cadr a) (cadr b)))))

;; Print a numbered-candidate line and keep it for the per-pick local report.
;; sb-log is a local of sb:build-virtual-network (dynamic scope).
(defun sb:dbg-g (gid a b s)
  (sb:dbg s)
  (if *sb-debug* (setq sb-log (cons (list gid (nth 3 a) (nth 3 b) s) sb-log)))
)

(defun sb:ent-desc (seg / e)
  (if (and seg (caddr seg) (setq e (handent (caddr seg))))
    (strcat (cdr (assoc 0 (entget e))) " handle " (caddr seg))
    "entity ?")
)

(defun sb:spike-p (v ids / n i hit)
  (setq n (length ids) i 0)
  (repeat n
    (if (and (= (nth i ids) v) (= (nth (rem (+ i n -1) n) ids) (nth (rem (1+ i) n) ids))) (setq hit T))
    (setq i (1+ i)))
  hit
)

;; Nearest edge (wall or bridge) hit by a ray from pt at angle ang: (distance edge) or nil.
(defun sb:probe (net pt ang / far q bd best h d)
  (setq far 100000000.0 q (list (+ (car pt) (* far (cos ang))) (+ (cadr pt) (* far (sin ang)))))
  (foreach e (cadr net)
    (setq h (sb:segment-intersection pt q (nth (car e) (car net)) (nth (cadr e) (car net)) *sb-point-tolerance*))
    (if (car h)
      (progn
        (setq d (* far (car (car h))))
        (if (and (> d *sb-point-tolerance*) (or (null bd) (< d bd))) (setq bd d best e)))))
  (if bd (list bd best))
)

(defun sb:edge-desc (e net / lab)
  (foreach c (nth 7 net)
    (if (and (member (car e) (sb:cand-edge c)) (member (cadr e) (sb:cand-edge c)))
      (setq lab (strcat "VIRTUAL G" (itoa (nth 6 c)) " (" (sb:r (nth 3 c)) ")"))))
  (cond (lab)
        ((strcat "WALL " (sb:ent-desc (sb:source (nth (car e) (car net)) (nth (cadr e) (car net)) (nth 11 net))))))
)

;; Face found: edge sequence, which bridges it uses, and how each unresolved gap relates to it.
(defun sb:face-report (ids verts unres cands / n i a b lab used fpts fsegs onA onB pa pb other)
  (setq n (length ids) i 0 other 0 fpts (mapcar '(lambda (k) (nth k verts)) ids))
  (sb:dbg (strcat "SELECTED FACE: " (itoa n) " edges (closed, contains pick; before validation/simplification)"))
  (repeat n
    (setq a (nth i ids) b (nth (rem (1+ i) n) ids) lab nil)
    (foreach c cands
      (if (and (not lab) (member a (sb:cand-edge c)) (member b (sb:cand-edge c))) (setq lab c)))
    (if lab (setq used (cons (nth 6 lab) used)))
    (setq fsegs (cons (list (nth a verts) (nth b verts)) fsegs))
    (sb:dbg (strcat "  " (itoa (1+ i)) "  "
                    (if lab (strcat "VIRTUAL G" (itoa (nth 6 lab)) " - " (sb:r (nth 3 lab))) "WALL")
                    "  " (sb:pt (nth a verts)) " -> " (sb:pt (nth b verts))))
    (setq i (1+ i)))
  (sb:dbg "Bridges in this solve:")
  (foreach c cands
    (sb:dbg (strcat "  G" (itoa (nth 6 c)) "  " (sb:pt (nth 3 (nth 1 c))) " -> " (sb:pt (nth 3 (nth 2 c)))
                    "  Distance " (sb:r (nth 3 c)) "  Used by selected face: " (if (member (nth 6 c) used) "YES" "NO"))))
  (foreach g unres
    (setq onA (member (car g) ids) onB (member (cadr g) ids) pa (nth (car g) verts) pb (nth (cadr g) verts))
    (if (or onA onB)
      (sb:dbg (strcat "UNRESOLVED G" (itoa (cadddr g)) "  " (sb:pt pa) " -> " (sb:pt pb) "  Distance " (sb:r (caddr g))
                      "\n    A on selected face: " (if onA "YES" "NO") "  B on selected face: " (if onB "YES" "NO")
                      "\n    A dangling end in face: " (if (sb:spike-p (car g) ids) "YES" "NO")
                      "  B dangling end in face: " (if (sb:spike-p (cadr g) ids) "YES" "NO")
                      "\n    Gap midpoint inside face: " (if (sb:point-in-region-p (sb:mid pa pb) fpts) "YES" "NO")
                      "  Gap crosses face edges: " (if (sb:crosses-p pa pb fsegs *sb-point-tolerance*) "YES" "NO")
                      "\n    Used to reject region: " (if (and onA onB) "YES" "NO")))
      (setq other (1+ other))))
  (sb:dbg (strcat "Unresolved openings not touching this face: " (itoa other)))
)

;; No face: probe 4 directions, list open endpoints and their candidate decisions near the pick.
(defun sb:local-report (net pt maxb / verts x0 y0 x1 y1 pr rad pts)
  (setq verts (car net) x0 (car (car verts)) x1 x0 y0 (cadr (car verts)) y1 y0)
  (foreach v verts
    (setq x0 (min x0 (car v)) x1 (max x1 (car v)) y0 (min y0 (cadr v)) y1 (max y1 (cadr v))))
  (sb:dbg "EXPECTED LOCAL ENCLOSURE (no closed face contains the pick)")
  (sb:dbg (strcat "Network extents: (" (sb:r x0) " " (sb:r y0) ") - (" (sb:r x1) " " (sb:r y1) ")  Pick inside extents: "
                  (if (and (<= x0 (car pt) x1) (<= y0 (cadr pt) y1)) "YES" "NO")))
  (foreach dir (list (list 0.0 "+X") (list (* 0.5 pi) "+Y") (list pi "-X") (list (* 1.5 pi) "-Y"))
    (if (setq pr (sb:probe net pt (car dir)))
      (progn
        (setq rad (if rad (max rad (car pr)) (car pr)))
        (sb:dbg (strcat "Probe " (cadr dir) ": " (sb:edge-desc (cadr pr) net) " at " (sb:r (car pr)) "  "
                        (sb:pt (nth (car (cadr pr)) verts)) "-" (sb:pt (nth (cadr (cadr pr)) verts)))))
      (sb:dbg (strcat "Probe " (cadr dir) ": NOTHING - no wall or bridge in this direction"))))
  (setq rad (if rad (+ rad maxb) (* 2.0 maxb)))
  (sb:dbg (strcat "Open endpoints within " (sb:r rad) " of pick:"))
  (foreach p (nth 9 net)
    (if (<= (sb:dist (nth 3 p) pt) rad)
      (progn
        (setq pts (cons (nth 3 p) pts))
        (sb:dbg (strcat "  E" (itoa (nth 4 p)) " " (sb:pt (nth 3 p)) "  direction "
                        (sb:r (* (/ 180.0 pi) (atan (cadr (nth 2 p)) (car (nth 2 p))))) " deg  "
                        (sb:ent-desc (sb:source (nth (cadr p) verts) (nth 3 p) (nth 11 net))))))))
  (sb:dbg "Candidates involving those endpoints:")
  (foreach g (nth 10 net)
    (if (or (member (cadr g) pts) (member (caddr g) pts)) (sb:dbg (strcat "  " (cadddr g)))))
)

(defun sb:dent (l) (entmake (cons (car l) (cons '(8 . "X-SB-DEBUG") (cdr l)))))
(defun sb:dline (p q c)
  (sb:dent (list '(0 . "LINE") (cons 62 c) (list 10 (car p) (cadr p) 0.0) (list 11 (car q) (cadr q) 0.0))))
(defun sb:dcircle (p r c)
  (sb:dent (list '(0 . "CIRCLE") (cons 62 c) (list 10 (car p) (cadr p) 0.0) (cons 40 r))))
(defun sb:dtext (p s c h)
  (sb:dent (list '(0 . "TEXT") (cons 62 c) (list 10 (car p) (cadr p) 0.0) (cons 40 h) (cons 1 s))))

(defun sb:debug-clear (/ ss i)
  (setq i 0)
  (if (setq ss (ssget "_X" '((8 . "X-SB-DEBUG"))))
    (repeat (sslength ss) (entdel (ssname ss i)) (setq i (1+ i))))
  i
)

;; E# cyan, accepted G# green, unresolved red, pick yellow, selected face magenta, probes yellow/red.
(defun sb:debug-draw (net pt res maxb / h pr q verts)
  (sb:debug-clear)
  (sb:ensure-layer "X-SB-DEBUG" 2)
  (setq h (* 0.05 maxb) verts (car net))
  (foreach p (nth 9 net)
    (sb:dcircle (nth 3 p) (* 0.15 h) 4)
    (sb:dtext (nth 3 p) (strcat "E" (itoa (nth 4 p))) 4 (* 0.5 h)))
  (foreach c (nth 7 net)
    (sb:dline (nth 3 (nth 1 c)) (nth 3 (nth 2 c)) 3)
    (sb:dtext (sb:cand-mid c) (strcat "G" (itoa (nth 6 c)) " " (rtos (nth 3 c) 2 0)) 3 h))
  (foreach g (nth 3 net)
    (sb:dline (nth (car g) verts) (nth (cadr g) verts) 1)
    (sb:dtext (sb:mid (nth (car g) verts) (nth (cadr g) verts))
              (strcat "U G" (itoa (cadddr g)) " " (rtos (caddr g) 2 0)) 1 h))
  (sb:dcircle pt h 2)
  (if (car res)
    (sb:dent (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 62 6)
                           (cons 90 (length (car res))) '(70 . 1) '(43 . 0.0))
                     (mapcar '(lambda (p) (list 10 (car p) (cadr p))) (car res))))
    (foreach a (list 0.0 (* 0.5 pi) pi (* 1.5 pi))
      (setq pr (sb:probe net pt a)
            q  (list (+ (car pt) (* (if pr (car pr) (* 3.0 maxb)) (cos a)))
                     (+ (cadr pt) (* (if pr (car pr) (* 3.0 maxb)) (sin a)))))
      (sb:dline pt q (if pr 2 1))
      (if (not pr) (sb:dtext q "NO HIT" 1 h))))
  (princ "\nSB debug graphics drawn on layer X-SB-DEBUG (SBDEBUGCLEAR removes them).")
)

(defun c:SBDEBUGCLEAR ()
  (princ (strcat "\nSB debug entities removed: " (itoa (sb:debug-clear))))
  (princ)
)

;;; ================================================================
;;; Settings (persisted with setenv)
;;; ================================================================
(defun sb:get-settings (k def) (cond ((getenv (strcat "AKD_SB_" k))) (def)))
(defun sb:put-setting (k v) (setenv (strcat "AKD_SB_" k) v))
(defun sb:maxb () (atof (sb:get-settings "MaxBridge" "1200")))
(defun sb:hatch-on () (= "1" (sb:get-settings "Hatch" "1")))
(defun sb:transp () (atoi (sb:get-settings "Transparency" "50")))
(defun sb:blayer () (sb:get-settings "BoundaryLayer" "X-ROOM-BOUNDARY"))
(defun sb:hlayer () (sb:get-settings "HatchLayer" "X-ROOM-HATCH"))

(defun sb:set-wall-layer (/ e lay)
  (setvar "ERRNO" 0)
  (while (and (not (setq e (entsel "\nSelect a wall object: "))) (= (getvar "ERRNO") 7))
    (setvar "ERRNO" 0))
  (if e
    (progn
      (setq lay (cdr (assoc 8 (entget (car e)))))
      (sb:put-setting "WallLayer" lay)
      (princ (strcat "\nWall layer set to: " lay))
      lay))
)

(defun sb:status ()
  (princ (strcat "\nWall Layer: " (sb:get-settings "WallLayer" "<not set>")
                 "\nMax Bridge: " (rtos (sb:maxb) 2 0)
                 "\nHatch: " (if (sb:hatch-on) (strcat "ON (" (itoa (sb:transp)) "% transparency)") "OFF")))
)

;; Returns T when the network must be rebuilt.
(defun sb:settings (/ kw v rebuild)
  (while
    (progn
      (initget "Wall Bridge Hatch Transparency Boundarylayer hatchLayer eXit")
      (setq kw (getkword "\nSettings [Wall/Bridge/Hatch/Transparency/Boundarylayer/hatchLayer/eXit] <eXit>: "))
      (and kw (/= kw "eXit")))
    (cond
      ((= kw "Wall") (if (sb:set-wall-layer) (setq rebuild T)))
      ((= kw "Bridge")
       (initget 6)
       (if (setq v (getdist (strcat "\nMax Bridge <" (rtos (sb:maxb) 2 0) ">: ")))
         (progn (sb:put-setting "MaxBridge" (rtos v 2 4)) (setq rebuild T))))
      ((= kw "Hatch") (sb:put-setting "Hatch" (if (sb:hatch-on) "0" "1")))
      ((= kw "Transparency")
       (initget 4)
       (if (setq v (getint (strcat "\nHatch transparency 0-90 <" (itoa (sb:transp)) ">: ")))
         (sb:put-setting "Transparency" (itoa (min 90 v)))))
      ((= kw "Boundarylayer")
       (setq v (getstring T (strcat "\nBoundary layer <" (sb:blayer) ">: ")))
       (if (and (/= v "") (snvalid v)) (sb:put-setting "BoundaryLayer" v)))
      ((= kw "hatchLayer")
       (setq v (getstring T (strcat "\nHatch layer <" (sb:hlayer) ">: ")))
       (if (and (/= v "") (snvalid v)) (sb:put-setting "HatchLayer" v)))
    )
    (sb:status))
  rebuild
)

;;; ================================================================
;;; Output
;;; ================================================================
(defun sb:ensure-layer (name col)
  (if (not (tblsearch "LAYER" name))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                   (cons 2 name) '(70 . 0) (cons 62 col) '(6 . "Continuous"))))
)

(defun sb:create-boundary (pts)
  (sb:ensure-layer (sb:blayer) 7)
  (if (entmake (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 (sb:blayer))
                             '(100 . "AcDbPolyline") (cons 90 (length pts)) '(70 . 1))
                       (mapcar '(lambda (p) (list 10 (car p) (cadr p))) pts)))
    (entlast))
)

(defun sb:create-hatch (pts color / base)
  (sb:ensure-layer (sb:hlayer) 8)
  (setq base (append (list '(100 . "AcDbHatch") '(10 0.0 0.0 0.0) '(210 0.0 0.0 1.0) '(2 . "SOLID")
                           '(70 . 1) '(71 . 0) '(91 . 1) '(92 . 7) '(72 . 0) '(73 . 1) (cons 93 (length pts)))
                     (mapcar '(lambda (p) (list 10 (car p) (cadr p))) pts)
                     (list '(97 . 0) '(75 . 0) '(76 . 1) '(98 . 1) '(10 0.0 0.0 0.0))))
  (cond
    ((entmake (append (list '(0 . "HATCH") '(100 . "AcDbEntity") (cons 8 (sb:hlayer)) (cons 62 color)
                            (cons 440 (+ 33554432 (fix (* 2.55 (- 100 (sb:transp)))))))
                      base))
     (entlast))
    ((entmake (append (list '(0 . "HATCH") '(100 . "AcDbEntity") (cons 8 (sb:hlayer)) (cons 62 color)) base))
     (princ "\nNote: hatch transparency not accepted; hatch created opaque.")
     (entlast))
  )
)

(setq *sb-palette* '(1 2 3 4 5 6 30 40 90 130 150 170 200 230))
(defun sb:pick-color (last / c)
  (setq c (nth (rem (getvar "MILLISECS") (length *sb-palette*)) *sb-palette*))
  (if (= c last) (nth (rem (1+ (vl-position c *sb-palette*)) (length *sb-palette*)) *sb-palette*) c)
)

;;; ================================================================
;;; Command
;;; ================================================================
(defun sb:load-network (/ lay segs net)
  (setq lay (sb:get-settings "WallLayer" nil))
  (if (and lay (not (tblsearch "LAYER" lay)))
    (progn (princ (strcat "\nWall layer \"" lay "\" no longer exists.")) (setq lay nil)))
  (if (not lay) (progn (princ "\nWall layer not set.") (setq lay (sb:set-wall-layer))))
  (cond
    ((not lay) nil)
    ((null (setq segs (sb:collect-wall-geometry lay)))
     (princ (strcat "\nNo LINE/LWPOLYLINE/ARC geometry on layer " lay ".")) nil)
    (T
     (princ "\nAnalyzing wall network...")
     (setq net (sb:build-virtual-network segs (sb:maxb)))
     (princ (strcat " " (itoa (nth 5 net)) " segments, " (itoa (length (nth 4 net))) " virtual bridges."))
     net)
  )
)

(defun sb:cleanup (oldecho inundo)
  (if inundo (command-s "_.UNDO" "_E"))
  (if oldecho (setvar "CMDECHO" oldecho))
)

(defun c:SB (/ *error* oldecho inundo net pt res count keys quit ent color)
  (setq count 0 oldecho (getvar "CMDECHO"))
  (defun *error* (msg)
    (sb:cleanup oldecho inundo)
    (if (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*EXIT*")) (princ (strcat "\nSB error: " msg)))
    (princ (strcat "\n" (itoa count) " room boundaries created."))
    (princ))
  (setvar "CMDECHO" 0)
  (if (not (sb:get-settings "WallLayer" nil)) (progn (princ "\nWall layer not set.") (sb:set-wall-layer)))
  (sb:status)
  (setq net (sb:load-network))
  (while (not quit)
    (initget "Settings Refresh Done")
    (setq pt (getpoint "\nPick inside room or [Settings/Refresh/Done]: "))
    (cond
      ((or (null pt) (= pt "Done")) (setq quit T))
      ((= pt "Settings") (if (sb:settings) (setq net (sb:load-network) keys nil)))
      ((= pt "Refresh") (setq net (sb:load-network) keys nil))
      ((null net) (princ "\nNo wall network. Use Settings to pick a wall layer, or Refresh."))
      ((progn
         (setq res (sb:solve-pick net (sb:2d (trans pt 1 0)) (sb:maxb)))
         (if *sb-debug-draw* (sb:debug-draw net (sb:2d (trans pt 1 0)) res (sb:maxb)))
         (not (car res)))
       (princ (strcat "\n" (cadr res))))
      ((member (cadr res) keys) (princ "\nThis room was already processed."))
      (T
       (command-s "_.UNDO" "_BE")
       (setq inundo T)
       (if (setq ent (sb:create-boundary (car res)))
         (progn
           (setq count (1+ count) keys (cons (cadr res) keys))
           (if (sb:hatch-on)
             (if (sb:create-hatch (car res) (setq color (sb:pick-color color)))
               nil
               (princ "\nHatch creation failed; boundary kept.")))
           (princ (strcat "\nRoom " (itoa count) " created.")))
         (princ "\nBoundary entity could not be created (entmake rejected the polyline)."))
       (command-s "_.UNDO" "_E")
       (setq inundo nil))
    ))
  (sb:cleanup oldecho nil)
  (princ (strcat "\n" (itoa count) " room boundaries created."))
  (princ)
)

(princ "\nSB loaded. Command: SB")
(princ)
