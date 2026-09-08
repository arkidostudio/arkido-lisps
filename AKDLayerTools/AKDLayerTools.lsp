; ============================================================
; AKDLayerTools.lsp
; Layer utilities in one file:
;   ER1   Set Current Layer     (preset list, re-runs last draw cmd)
;   ERS   Select By Layer       (grab all objects on a layer)
;   ERT   Move To Layer         (move picked objects to a layer)
;   ERD   Isolate Layer         (pick → isolate its layer; run again to restore)
;   ERDD  Isolate Objects       (pick → hide the rest; run again to unisolate)
;   ERF   Layer Off (pick)      (pick object → turn its layer off; repeats)
;   ERA   Layers All On/Thaw    (restore visibility + unisolate)
;   ERAF  Turn Off All But Current
;   ERL   Lock Layer (pick)     (pick object → lock its layer; repeats)
;   ERU   Unlock Layer (pick)   (pick object → unlock its layer; repeats)
;   ERSC  Show all shortcuts
; Self-contained: no .dcl or config files needed.
; ============================================================

;; ---- Edit your ER1 preset layers here ----
(setq *ER1_Layers*
  '(
    "A-WALL"
    "A-DOOR"
    "A-WINDOW"
    "S-BEAM"
    "X-DIMS"
    "P-HATCH"
    "Z-TITLE"
  )
)

; ============================================================
; ERS - Select By Layer
; ============================================================

(defun c:ERS (/ laynames rec fn f dcl_id idx choice mode ss
                pt1 pt2 presel preList i e appendSel)

  ;; Capture any current grip-selection BEFORE the dialog clears it
  (setq presel (ssget "_I") preList nil)
  (if presel
    (progn
      (setq i 0)
      (repeat (sslength presel)
        (setq preList (cons (ssname presel i) preList))
        (setq i (1+ i)))))

  (setq laynames '())
  (setq rec (tblnext "LAYER" T))
  (while rec
    (setq laynames (cons (cdr (assoc 2 rec)) laynames))
    (setq rec (tblnext "LAYER")))
  (setq laynames (acad_strlsort laynames))

  (setq fn (vl-filename-mktemp "ers.dcl"))
  (setq f (open fn "w"))
  (write-line "ers_dialog : dialog { label = \"Select By Layer\";" f)
  (write-line " : list_box { key = \"lst\"; width = 40; height = 18; allow_accept = true; }" f)
  (write-line " : button { key = \"append\"; label = \"Append: OFF\"; }" f)
  (write-line " : row {" f)
  (write-line "   : button { key = \"all\";    label = \"All (Drawing)\"; is_default = true; }" f)
  (write-line "   : button { key = \"window\"; label = \"By Selection\"; }" f)
  (write-line "   : button { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; }" f)
  (write-line " } }" f)
  (close f)

  (setq dcl_id (load_dialog fn))
  (if (not (new_dialog "ers_dialog" dcl_id)) (exit))

  (start_list "lst")
  (mapcar 'add_list laynames)
  (end_list)

  (setq idx "0")
  (if (and *ers:last-layer* (member *ers:last-layer* laynames))
    (setq idx (itoa (- (length laynames)
                       (length (member *ers:last-layer* laynames))))))

  (setq appendSel (if *ers:last-append* "1" "0"))
  (set_tile "append" (if (= appendSel "1") "Append: ON" "Append: OFF"))

  (action_tile "append"
    "(setq appendSel (if (= appendSel \"1\") \"0\" \"1\"))(set_tile \"append\" (if (= appendSel \"1\") \"Append: ON\" \"Append: OFF\"))")
  (action_tile "lst"    "(setq idx $value)(if (= $reason 4) (done_dialog 3))")
  (action_tile "all"    "(done_dialog 1)")
  (action_tile "window" "(done_dialog 2)")
  (action_tile "cancel" "(done_dialog 0)")

  (set_tile "lst" idx)
  (mode_tile "lst" 2)

  (setq mode (start_dialog))
  (setq *ers:last-append* (= appendSel "1"))
  (unload_dialog dcl_id)
  (vl-file-delete fn)

  (if (> mode 0)
    (progn
      (setq choice (nth (atoi idx) laynames))
      (setq *ers:last-layer* choice)

      (cond
        ((= mode 1)
         (setq ss (ssget "X" (list (cons 8 choice)))))
        ((= mode 2)
         (setq pt1 (getpoint (strcat "\nFirst corner (layer: " choice "): ")))
         (if pt1 (setq pt2 (getcorner pt1 "\nOpposite corner: ")))
         (if (and pt1 pt2)
           (setq ss (ssget "C" pt1 pt2 (list (cons 8 choice))))))
        ((= mode 3)
         (if preList
           (progn
             (setq ss (ssadd))
             (foreach e preList
               (if (and (entget e)
                        (= (cdr (assoc 8 (entget e))) choice))
                 (ssadd e ss)))
             (if (zerop (sslength ss)) (setq ss nil)))
           (setq ss (ssget "X" (list (cons 8 choice)))))))

      (if (and (= appendSel "1") preList)
        (progn
          (if (not ss) (setq ss (ssadd)))
          (foreach e preList
            (if (entget e) (ssadd e ss)))))

      (if ss
        (progn
          (sssetfirst nil ss)
          (princ (strcat "\nSelected " (itoa (sslength ss))
                         " object(s) on layer: " choice
                         (if (= appendSel "1") " (appended)" ""))))
        (princ (strcat "\nNo objects found on layer: " choice)))))
  (princ)
)

; ============================================================
; ER1 - Set Current Layer (preset list) + re-fire last draw cmd
; ============================================================

(defun c:ER1 (/ fn f dcl_id idx selected lastcmd)

  (if (not *ER1_Layers*)
    (progn (alert "No layers defined in *ER1_Layers*.") (exit)))

  (setq fn (vl-filename-mktemp "er1.dcl"))
  (setq f (open fn "w"))
  (write-line "er1_dialog : dialog { label = \"Set Current Layer\";" f)
  (write-line " : list_box { key = \"lst\"; width = 30; height = 15; allow_accept = true; }" f)
  (write-line " spacer; ok_cancel; }" f)
  (close f)

  (setq dcl_id (load_dialog fn))
  (if (not (new_dialog "er1_dialog" dcl_id)) (exit))

  (start_list "lst")
  (mapcar 'add_list *ER1_Layers*)
  (end_list)

  (setq idx "0")
  (if (and *er1:last-layer* (member *er1:last-layer* *ER1_Layers*))
    (setq idx (itoa (- (length *ER1_Layers*)
                       (length (member *er1:last-layer* *ER1_Layers*))))))

  (action_tile "lst"    "(setq idx $value)(if (= $reason 4) (done_dialog 1))")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (set_tile "lst" idx)
  (mode_tile "lst" 2)

  (if (= (start_dialog) 1)
    (setq selected (nth (atoi idx) *ER1_Layers*)))

  (unload_dialog dcl_id)
  (vl-file-delete fn)

  (if selected
    (progn
      (setq *er1:last-layer* selected)
      (setvar "CLAYER" selected)
      (princ (strcat "\nCurrent layer set to: " selected))
      (setq lastcmd (getvar "CMDNAMES"))
      (cond
        ((wcmatch lastcmd "*PLINE*")   (command "_.PLINE"))
        ((wcmatch lastcmd "*LINE*")    (command "_.LINE"))
        ((wcmatch lastcmd "*ARC*")     (command "_.ARC"))
        ((wcmatch lastcmd "*CIRCLE*")  (command "_.CIRCLE"))
        ((wcmatch lastcmd "*RECTANG*") (command "_.RECTANG"))
        ((wcmatch lastcmd "*POLYGON*") (command "_.POLYGON"))
        ((wcmatch lastcmd "*MTEXT*")   (command "_.MTEXT"))
        ((wcmatch lastcmd "*TEXT*")    (command "_.TEXT"))
        ((wcmatch lastcmd "*HATCH*")   (command "_.HATCH"))
        (T (command "_.LINE")))))
  (princ)
)

; ============================================================
; ERT - Move Selected Objects To Layer (all drawing layers)
; ============================================================

(defun c:ERT (/ layer_list rec fn f dcl_id idx selected ss i ent)

  (setq layer_list '())
  (setq rec (tblnext "LAYER" T))
  (while rec
    (setq layer_list (cons (cdr (assoc 2 rec)) layer_list))
    (setq rec (tblnext "LAYER")))
  (setq layer_list (acad_strlsort layer_list))

  (if (not layer_list)
    (progn (alert "No layers in drawing.") (exit)))

  (setq fn (vl-filename-mktemp "ert.dcl"))
  (setq f (open fn "w"))
  (write-line "ert_dialog : dialog { label = \"Move To Layer\";" f)
  (write-line " : text { label = \"Double-click to apply\"; }" f)
  (write-line " : list_box { key = \"lst\"; width = 30; height = 12; allow_accept = true; }" f)
  (write-line " spacer; ok_cancel; }" f)
  (close f)

  (setq dcl_id (load_dialog fn))
  (if (not (new_dialog "ert_dialog" dcl_id)) (exit))

  (start_list "lst")
  (mapcar 'add_list layer_list)
  (end_list)

  (setq idx "0")
  (if (and *ert:last-layer* (member *ert:last-layer* layer_list))
    (setq idx (itoa (- (length layer_list)
                       (length (member *ert:last-layer* layer_list))))))

  (action_tile "lst"    "(setq idx $value)(if (= $reason 4) (done_dialog 1))")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (set_tile "lst" idx)
  (mode_tile "lst" 2)

  (if (= (start_dialog) 1)
    (setq selected (nth (atoi idx) layer_list)))

  (unload_dialog dcl_id)
  (vl-file-delete fn)

  (if selected
    (progn
      (setq *ert:last-layer* selected)
      (prompt (strcat "\nMoving to layer: " selected))
      (setq ss (ssget))
      (if ss
        (progn
          (akd:undo-begin)
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (entmod (subst (cons 8 selected)
                           (assoc 8 (entget ent))
                           (entget ent)))
            (setq i (1+ i)))
          (command-s "_.REGEN")
          (sssetfirst nil nil)
          (akd:undo-end)
          (princ (strcat "\nMoved " (itoa (sslength ss))
                         " object(s) to layer: " selected)))
        (princ "\nNo objects selected."))))
  (princ)
)

; ============================================================
; Helpers
; ============================================================

(defun akd:all-layers (/ rec out)
  (setq out '() rec (tblnext "LAYER" T))
  (while rec
    (setq out (cons (cdr (assoc 2 rec)) out))
    (setq rec (tblnext "LAYER")))
  out
)

(defun akd:layer-cmd (op layer)
  (command "_.-LAYER" op layer "")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
)

;; Undo grouping — one BEGIN/END pair per command so a single U reverses it.
(defun akd:undo-begin () (command-s "_.UNDO" "_BEGIN"))
(defun akd:undo-end   () (command-s "_.UNDO" "_END"))

;; Preselection: return the pickfirst set or nil. Callers clear it after use.
(defun akd:presel ( / ss)
  (if (setq ss (ssget "_I")) ss))

;; Unique layer names from an ss, excluding the current layer if skip-current.
(defun akd:ss-layers (ss skip-current / i ent lay cur out)
  (setq cur (getvar "CLAYER") out '() i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
          lay (cdr (assoc 8 (entget ent))))
    (if (and (not (member lay out))
             (or (not skip-current) (/= lay cur)))
      (setq out (cons lay out)))
    (setq i (1+ i)))
  out)

; ============================================================
; ERDD - Isolate Objects: hide everything except the picked
;        objects (regardless of layer). Run again to unisolate.
;        Wraps native ISOLATEOBJECTS / UNISOLATEOBJECTS.
; ============================================================

(defun c:ERDD (/ ss)
  (if *erd:isolated*
    (progn
      (akd:undo-begin)
      (command-s "_.UNISOLATEOBJECTS")
      (akd:undo-end)
      (setq *erd:isolated* nil)
      (princ "\nObjects unhidden."))
    (progn
      (prompt "\nPick objects to isolate. Enter when done.")
      (if (setq ss (ssget))
        (progn
          (akd:undo-begin)
          (command-s "_.ISOLATEOBJECTS" ss "")
          (akd:undo-end)
          (setq *erd:isolated* T)
          (princ "\nObjects isolated. Run ERDD again to unisolate."))
        (princ "\nNo objects picked."))))
  (princ)
)

; ============================================================
; ERD - Isolate the picked object's LAYER (LAYISO/LAYUNISO).
;       Run again to restore.
; ============================================================

(defun c:ERD (/ ss e)
  (if *erd:layer-isolated*
    (progn
      (akd:undo-begin)
      (command-s "_.LAYUNISO")
      (akd:undo-end)
      (setq *erd:layer-isolated* nil)
      (princ "\nLayers restored."))
    (progn
      (setq ss (akd:presel))
      (if ss
        (progn
          (akd:undo-begin)
          (command-s "_.LAYISO" ss "")
          (akd:undo-end)
          (sssetfirst nil nil)
          (setq *erd:layer-isolated* T)
          (princ "\nLayer(s) isolated. Run ERD again to restore."))
        (progn
          (setq e (entsel "\nPick object on layer to isolate: "))
          (if e
            (progn
              (akd:undo-begin)
              (command-s "_.LAYISO" e "")
              (akd:undo-end)
              (setq *erd:layer-isolated* T)
              (princ "\nLayer isolated. Run ERD again to restore."))
            (princ "\nNo object picked."))))))
  (princ)
)

; ============================================================
; ERF - Turn off the layer of a picked object. Repeatable.
; ============================================================

(defun c:ERF (/ ss e ent lay cur)
  (setq cur (getvar "CLAYER"))
  (akd:undo-begin)
  (if (setq ss (akd:presel))
    (progn
      (foreach lay (akd:ss-layers ss T)
        (akd:layer-cmd "_OFF" lay)
        (princ (strcat "\nLayer OFF: " lay)))
      (sssetfirst nil nil))
    (progn
      (prompt "\nPick object(s) — layer gets turned off. Enter to exit.")
      (while (setq e (entsel "\nPick object: "))
        (setq ent (car e)
              lay (cdr (assoc 8 (entget ent))))
        (cond
          ((= lay cur)
           (princ (strcat "\nSkip: " lay " is the current layer.")))
          (T
           (akd:layer-cmd "_OFF" lay)
           (princ (strcat "\nLayer OFF: " lay)))))))
  (akd:undo-end)
  (princ)
)

; ============================================================
; ERA - Thaw + turn on all layers (recover from ERD/ERF/ERAF)
; ============================================================

(defun c:ERA ()
  (akd:undo-begin)
  (command "_.-LAYER" "_THAW" "*" "_ON" "*" "")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (if *erd:isolated*
    (progn
      (command-s "_.UNISOLATEOBJECTS")
      (setq *erd:isolated* nil)))
  (if *erd:layer-isolated*
    (progn
      (command-s "_.LAYUNISO")
      (setq *erd:layer-isolated* nil)))
  (akd:undo-end)
  (princ "\nAll layers ON, thawed, objects & layers restored.")
  (princ)
)

; ============================================================
; ERAF - Turn OFF all layers except current
; ============================================================

(defun c:ERAF (/ cur)
  (setq cur (getvar "CLAYER"))
  (akd:undo-begin)
  (foreach lay (akd:all-layers)
    (if (/= lay cur) (akd:layer-cmd "_OFF" lay)))
  (akd:undo-end)
  (princ (strcat "\nAll layers off except: " cur))
  (princ)
)

; ============================================================
; ERL - Lock layers by picking objects. Repeatable.
; ============================================================

(defun c:ERL (/ ss e ent lay cur done)
  (setq cur (getvar "CLAYER") done nil)
  (akd:undo-begin)
  (cond
    ((setq ss (akd:presel))
     (foreach lay (akd:ss-layers ss T)
       (akd:layer-cmd "_LOCK" lay)
       (princ (strcat "\nLocked: " lay)))
     (sssetfirst nil nil)
     (setq done T)))
  (if (not done)
    (prompt "\nPick object(s) — layer gets locked. [A]=All except current. Enter to exit."))
  (while (not done)
    (initget "All")
    (setq e (entsel "\nPick object or [All]: "))
    (cond
      ((= e "All")
       (foreach lay (akd:all-layers)
         (if (/= lay cur) (akd:layer-cmd "_LOCK" lay)))
       (princ (strcat "\nLocked all layers except: " cur))
       (setq done T))
      ((null e)
       (setq done T))
      (T
       (setq ent (car e)
             lay (cdr (assoc 8 (entget ent))))
       (cond
         ((= lay cur)
          (princ (strcat "\nSkip: " lay " is the current layer.")))
         (T
          (akd:layer-cmd "_LOCK" lay)
          (princ (strcat "\nLocked: " lay)))))))
  (akd:undo-end)
  (princ)
)

; ============================================================
; ERU - Unlock layers by picking objects. Repeatable.
; ============================================================

(defun c:ERU (/ ss e ent lay done)
  (setq done nil)
  (akd:undo-begin)
  (cond
    ((setq ss (akd:presel))
     (foreach lay (akd:ss-layers ss nil)
       (akd:layer-cmd "_UNLOCK" lay)
       (princ (strcat "\nUnlocked: " lay)))
     (sssetfirst nil nil)
     (setq done T)))
  (if (not done)
    (prompt "\nPick object(s) — layer gets unlocked. [A]=All layers. Enter to exit."))
  (while (not done)
    (initget "All")
    (setq e (entsel "\nPick object or [All]: "))
    (cond
      ((= e "All")
       (command "_.-LAYER" "_UNLOCK" "*" "")
       (while (> (getvar "CMDACTIVE") 0) (command ""))
       (princ "\nAll layers unlocked.")
       (setq done T))
      ((null e)
       (setq done T))
      (T
       (setq ent (car e)
             lay (cdr (assoc 8 (entget ent))))
       (akd:layer-cmd "_UNLOCK" lay)
       (princ (strcat "\nUnlocked: " lay)))))
  (akd:undo-end)
  (princ)
)

; ============================================================
; EREX - Export Layer List (prompts TXT or JSON)   [Mac-safe, no VLA]
;   Group filters aren't reachable in pure LISP on Mac AutoCAD, so
;   groups are derived from name prefix (before "-").
; ERIM - Import layers from a TXT produced by EREX (pipe-delimited).
;   Creates missing layers; sets color/linetype/lineweight/plot/on/frozen/lock.
; ============================================================

(defun akd:lw-str (lw)
  (cond ((null lw) "Default") ((= lw -1) "ByLayer") ((= lw -2) "ByBlock")
        ((= lw -3) "Default")
        (T (strcat (rtos (/ lw 100.0) 2 2) "mm"))))

(defun akd:parse-lw (s / n)
  (cond ((= s "Default") -3) ((= s "ByLayer") -1) ((= s "ByBlock") -2)
        (T (setq n (atof s)) (fix (+ 0.5 (* n 100))))))

(defun akd:tobool (s) (or (= (strcase s) "TRUE") (= s "1") (= (strcase s) "T")))

(defun akd:json-esc (s / i c r)
  (setq i 1 r "")
  (while (<= i (strlen s))
    (setq c (substr s i 1))
    (setq r (strcat r (cond ((= c "\"") "\\\"") ((= c "\\") "\\\\") (T c))))
    (setq i (1+ i)))
  r)

(defun akd:pad (s n / L)
  (setq s (vl-princ-to-string s) L (strlen s))
  (if (>= L n) (strcat s " ")
    (strcat s (akd:spaces (- n L)))))
(defun akd:spaces (n / r) (setq r "") (repeat n (setq r (strcat r " "))) r)

(defun akd:layer-group-fallback (name / p)
  (setq p (vl-string-search "-" name))
  (if p (substr name 1 p) "(none)"))

;; Iterate layer table via tblnext → list of layer DXF records.
(defun akd:layer-records ( / rec out)
  (setq out '() rec (tblnext "LAYER" T))
  (while rec
    (setq out (cons rec out))
    (setq rec (tblnext "LAYER")))
  (reverse out))

;; Locate the LAYER table's extension dictionary (entget list) or nil.
(defun akd:layer-tbl-xdict ( / lay0 tblE xH)
  (setq lay0 (tblobjname "LAYER" (cdr (assoc 2 (tblnext "LAYER" T)))))
  (if lay0
    (progn
      (setq tblE (cdr (assoc 330 (entget lay0))))
      (if tblE (setq xH (cdr (assoc 360 (entget tblE)))))
      (if xH (entget xH)))))

;; Try each known filter-dict key; return the filter dict entget list, or nil.
(defun akd:filter-dict ( / xd keys k sub)
  (setq xd (akd:layer-tbl-xdict))
  (setq keys '("ACAD_LAYERFILTERS" "AcLyDictionary" "ACAD_LAYERSTATES"))
  (if xd
    (progn
      (foreach k keys
        (if (and (null sub) (dictsearch (cdr (assoc -1 xd)) k))
          (setq sub (dictsearch (cdr (assoc -1 xd)) k))))
      sub)))

;; Read filter dict → ((filter-name layer-wildcard) ...).
;; AutoCAD Mac stores each as an XRECORD; 1st code-1 is the name,
;; 2nd code-1 is the layer-name wildcard pattern.
(defun akd:read-filters ( / fd p name fEnt ones out)
  (setq out '())
  (setq fd (akd:filter-dict))
  (if fd
    (progn
      (setq p fd name nil)
      (while p
        (cond
          ((= (car (car p)) 3) (setq name (cdr (car p))))
          ((and name (= (car (car p)) 350))
           (setq fEnt (vl-catch-all-apply '(lambda () (entget (cdr (car p))))))
           (if (not (vl-catch-all-error-p fEnt))
             (progn
               (setq ones (mapcar 'cdr (vl-remove-if-not
                                         '(lambda (kv) (= (car kv) 1)) fEnt)))
               (if (>= (length ones) 2)
                 (setq out (cons (list name (nth 1 ones)) out)))))
           (setq name nil)))
        (setq p (cdr p)))))
  (reverse out))

;; Diagnostic: dump xdict, filter dict, and DXF of the first filter entry.
(defun c:ERGDBG ( / xd fd first firstE)
  (setq xd (akd:layer-tbl-xdict))
  (princ "\n--- LAYER table xdict ---")
  (if xd (foreach kv xd
           (if (member (car kv) '(3 350 360))
             (princ (strcat "\n  " (itoa (car kv)) " : " (vl-princ-to-string (cdr kv)))))))
  (setq fd (akd:filter-dict))
  (princ "\n--- Filter dict entries ---")
  (if fd
    (progn
      (foreach kv fd
        (if (member (car kv) '(3 350))
          (princ (strcat "\n  " (itoa (car kv)) " : " (vl-princ-to-string (cdr kv))))))
      ;; Find first 350 and dump its entget
      (foreach kv fd
        (if (and (null first) (= (car kv) 350)) (setq first (cdr kv))))
      (if first
        (progn
          (princ "\n--- Full DXF of first filter entry ---")
          (setq firstE (entget first))
          (foreach kv firstE
            (princ (strcat "\n  " (itoa (car kv)) " : " (vl-princ-to-string (cdr kv)))))))))
  (princ))

(defun c:EREX (/ ans ext fn fp rows row rec name col flags lw pl ltype
                  groups g lst rr grp filters used f ln line)
  (initget "T J")
  (setq ans (getkword "\nExport format [Txt/Json] <T>: "))
  (if (null ans) (setq ans "T"))
  (setq ext (if (= ans "J") "json" "txt"))
  (setq fn (getfiled "Save Layer List" "layers" ext 1))
  (if (null fn) (progn (princ "\nCancelled.") (exit)))

  (setq rows '())
  (foreach rec (akd:layer-records)
    (setq name  (cdr (assoc 2 rec))
          col   (cdr (assoc 62 rec))
          flags (cdr (assoc 70 rec))
          ltype (cdr (assoc 6 rec))
          lw    (cdr (assoc 370 rec))
          pl    (cdr (assoc 290 rec)))
    (setq row
      (list
        (cons "name"       name)
        (cons "color"      (itoa (abs col)))
        (cons "linetype"   (if ltype ltype "Continuous"))
        (cons "lineweight" (akd:lw-str lw))
        (cons "on"         (if (minusp col) "false" "true"))
        (cons "frozen"     (if (= 1 (logand flags 1)) "true" "false"))
        (cons "locked"     (if (= 4 (logand flags 4)) "true" "false"))
        (cons "plottable"  (if (or (null pl) (= pl 1)) "true" "false"))))
    (setq rows (cons row rows)))
  (setq rows (reverse rows))

  ;; Groups: real ACAD_LAYERFILTERS group filters if any, else prefix
  (setq groups '())
  (setq filters (akd:read-filters))
  (cond
    (filters
     (setq used '())
     (foreach f filters
       (setq lst '())
       (foreach rr rows
         (if (wcmatch (cdr (assoc "name" rr)) (cadr f))
           (progn (setq lst (cons rr lst))
                  (setq used (cons (cdr (assoc "name" rr)) used)))))
       (if lst (setq groups (cons (cons (car f) (reverse lst)) groups))))
     (setq groups (reverse groups))
     (setq lst '())
     (foreach rr rows
       (if (not (member (cdr (assoc "name" rr)) used))
         (setq lst (cons rr lst))))
     (if lst (setq groups (append groups (list (cons "(ungrouped)" (reverse lst)))))))
    (T
     (foreach rr rows
       (setq g (akd:layer-group-fallback (cdr (assoc "name" rr))))
       (if (setq lst (assoc g groups))
         (setq groups (subst (cons g (cons rr (cdr lst))) lst groups))
         (setq groups (cons (list g rr) groups))))
     (setq groups (mapcar '(lambda (x) (cons (car x) (reverse (cdr x)))) groups))))

  (setq fp (open fn "w"))
  (cond
    ((= ans "J")
     (write-line "{" fp)
     (write-line "  \"groups\": {" fp)
     (setq lst groups)
     (while lst
       (setq grp (car lst))
       (write-line (strcat "    \"" (akd:json-esc (car grp)) "\": ["
                    (apply 'strcat
                      (akd:join
                        (mapcar '(lambda (r) (strcat "\"" (akd:json-esc (cdr (assoc "name" r))) "\""))
                                (cdr grp))
                        ", "))
                    "]"
                    (if (cdr lst) "," ""))
                  fp)
       (setq lst (cdr lst)))
     (write-line "  }," fp)
     (write-line "  \"layers\": [" fp)
     (setq lst rows)
     (while lst
       (setq rr (car lst))
       (write-line "    {" fp)
       (write-line
         (strcat
           "      \"name\": \""       (akd:json-esc (cdr (assoc "name" rr)))       "\", "
           "\"color\": \""             (cdr (assoc "color" rr))                     "\", "
           "\"linetype\": \""          (akd:json-esc (cdr (assoc "linetype" rr)))   "\", "
           "\"lineweight\": \""        (cdr (assoc "lineweight" rr))                "\", "
           "\"on\": "                  (cdr (assoc "on" rr))                        ", "
           "\"frozen\": "              (cdr (assoc "frozen" rr))                    ", "
           "\"locked\": "              (cdr (assoc "locked" rr))                    ", "
           "\"plottable\": "           (cdr (assoc "plottable" rr)))
         fp)
       (write-line (if (cdr lst) "    }," "    }") fp)
       (setq lst (cdr lst)))
     (write-line "  ]" fp)
     (write-line "}" fp))
    (T
     (write-line "# AutoCAD Layer Export" fp)
     (write-line (strcat "# Total layers: " (itoa (length rows))) fp)
     (write-line "# Fields: name|color|linetype|lineweight|on|frozen|locked|plottable|group" fp)
     (write-line "" fp)
     (foreach grp groups
       (write-line (strcat "# === Group: " (car grp) " ===") fp)
       (foreach rr (cdr grp)
         (write-line
           (strcat
             (cdr (assoc "name" rr)) "|"
             (cdr (assoc "color" rr)) "|"
             (cdr (assoc "linetype" rr)) "|"
             (cdr (assoc "lineweight" rr)) "|"
             (cdr (assoc "on" rr)) "|"
             (cdr (assoc "frozen" rr)) "|"
             (cdr (assoc "locked" rr)) "|"
             (cdr (assoc "plottable" rr)) "|"
             (car grp))
           fp))
       (write-line "" fp))))
  (close fp)
  (princ (strcat "\nExported " (itoa (length rows)) " layers, "
                 (itoa (length groups)) " groups → " fn))
  (princ))

(defun akd:join (lst sep / r)
  (if (null lst) '("")
    (cons (car lst)
          (apply 'append
            (mapcar '(lambda (x) (list sep x)) (cdr lst))))))

;; --- Split "a|b|c" -> ("a" "b" "c") ---
(defun akd:split (s sep / p out)
  (setq out '())
  (while (setq p (vl-string-search sep s))
    (setq out (cons (substr s 1 p) out))
    (setq s (substr s (+ p 1 (strlen sep)))))
  (setq out (cons s out))
  (reverse out))

; ============================================================
; ERIM - Import layers from a TXT or JSON produced by EREX.
;   Format is chosen by file extension.
; ============================================================

;; Apply one parsed row via entmake/entmod on the LAYER record — no prompts.
(defun akd:apply-row (row / lname ltype lw col flags plot existed e)
  (setq lname (cdr (assoc "name" row)))
  (if (or (null lname) (= lname "")) nil
    (progn
      (setq existed (tblsearch "LAYER" lname))
      (setq col (atoi (cdr (assoc "color" row))))
      (if (zerop col) (setq col 7))
      (if (not (akd:tobool (cdr (assoc "on" row)))) (setq col (- col)))
      (setq ltype (cdr (assoc "linetype" row)))
      (if (or (null ltype) (not (tblsearch "LTYPE" ltype))) (setq ltype "Continuous"))
      (setq lw (akd:parse-lw (cdr (assoc "lineweight" row))))
      (setq flags (+ (if (akd:tobool (cdr (assoc "frozen" row))) 1 0)
                     (if (akd:tobool (cdr (assoc "locked" row))) 4 0)))
      (setq plot (if (akd:tobool (cdr (assoc "plottable" row))) 1 0))
      (if (null existed)
        (entmake
          (list '(0 . "LAYER")
                '(100 . "AcDbSymbolTableRecord")
                '(100 . "AcDbLayerTableRecord")
                (cons 2 lname)
                (cons 70 flags)
                (cons 62 col)
                (cons 6 ltype)
                (cons 370 lw)
                (cons 290 plot)))
        (progn
          (setq e (entget (tblobjname "LAYER" lname)))
          (setq e (akd:dxf-put e 70  flags))
          (setq e (akd:dxf-put e 62  col))
          (setq e (akd:dxf-put e 6   ltype))
          (setq e (akd:dxf-put e 370 lw))
          (setq e (akd:dxf-put e 290 plot))
          (entmod e)))
      (if existed 'updated 'created))))

;; subst or add a DXF pair keyed by group code
(defun akd:dxf-put (e code val / old)
  (if (setq old (assoc code e))
    (subst (cons code val) old e)
    (append e (list (cons code val)))))

;; Extract "key" value from a JSON fragment; strips surrounding quotes if any.
(defun akd:json-get (s key / p q e v)
  (setq p (vl-string-search (strcat "\"" key "\":") s))
  (if p
    (progn
      (setq q (+ p (strlen key) 3))
      ;; skip whitespace and optional opening quote
      (while (member (substr s (1+ q) 1) '(" " "\t")) (setq q (1+ q)))
      (if (= (substr s (1+ q) 1) "\"")
        (progn
          (setq q (1+ q))
          (setq e (vl-string-search "\"" s q))
          (if e (setq v (substr s (1+ q) (- e q)))))
        (progn
          ;; bare token until , or }
          (setq e q)
          (while (and (< e (strlen s))
                      (not (member (substr s (1+ e) 1) '("," "}" " " "\n" "\r" "\t"))))
            (setq e (1+ e)))
          (setq v (substr s (1+ q) (- e q)))))
      v)))

(defun akd:row-from-json (s)
  (list
    (cons "name"       (akd:json-get s "name"))
    (cons "color"      (akd:json-get s "color"))
    (cons "linetype"   (akd:json-get s "linetype"))
    (cons "lineweight" (akd:json-get s "lineweight"))
    (cons "on"         (akd:json-get s "on"))
    (cons "frozen"     (akd:json-get s "frozen"))
    (cons "locked"     (akd:json-get s "locked"))
    (cons "plottable"  (akd:json-get s "plottable"))))

(defun akd:row-from-txt (line / p)
  (setq p (akd:split line "|"))
  (if (>= (length p) 8)
    (list
      (cons "name"       (nth 0 p))
      (cons "color"      (nth 1 p))
      (cons "linetype"   (nth 2 p))
      (cons "lineweight" (nth 3 p))
      (cons "on"         (nth 4 p))
      (cons "frozen"     (nth 5 p))
      (cons "locked"     (nth 6 p))
      (cons "plottable"  (nth 7 p)))))

(defun c:ERIM (/ fn fp line ext buf row result created updated)
  (setq fn (getfiled "Import Layer List" "" "" 0))
  (if (null fn) (progn (princ "\nCancelled.") (exit)))
  (setq ext (strcase (vl-filename-extension fn)))
  (setq fp (open fn "r") created 0 updated 0)
  (akd:undo-begin)
  (cond
    ;; --- JSON: buffer between { and } as one row ---
    ((= ext ".JSON")
     (setq buf nil)
     (while (setq line (read-line fp))
       (setq line (vl-string-trim " \t\r\n" line))
       (cond
         ((vl-string-search "{" line)
          (if (/= line "{") (setq buf (substr line (+ 1 (vl-string-search "{" line))))
                            (setq buf "")))
         ((vl-string-search "}" line)
          (if buf
            (progn
              (setq row (akd:row-from-json buf))
              (if (and row (cdr (assoc "name" row)))
                (progn
                  (setq result (akd:apply-row row))
                  (if (eq result 'created) (setq created (1+ created))
                    (if (eq result 'updated) (setq updated (1+ updated))))))
              (setq buf nil))))
         (buf (setq buf (strcat buf " " line))))))
    ;; --- TXT: pipe-delimited, one row per line ---
    (T
     (while (setq line (read-line fp))
       (setq line (vl-string-trim " \t\r\n" line))
       (if (and (/= line "") (/= (substr line 1 1) "#"))
         (progn
           (setq row (akd:row-from-txt line))
           (if (and row (cdr (assoc "name" row)))
             (progn
               (setq result (akd:apply-row row))
               (if (eq result 'created) (setq created (1+ created))
                 (if (eq result 'updated) (setq updated (1+ updated)))))))))))
  (close fp)
  (akd:undo-end)
  (princ (strcat "\nImport done. Created: " (itoa created)
                 "  Updated: " (itoa updated)
                 "  (Group filters unchanged — recreate in Layer Manager if needed.)"))
  (princ))

; ============================================================
; ERSC - Show shortcuts
; ============================================================

(defun c:ERSC ()
  (prompt "\n================ AKDLayerTools ================")
  (prompt "\n ER1   Set Current Layer (preset list)")
  (prompt "\n ERS   Select By Layer")
  (prompt "\n ERT   Move Selected Objects To Layer")
  (prompt "\n ERD   Isolate picked object's LAYER (toggle)")
  (prompt "\n ERDD  Isolate picked OBJECTS (toggle)")
  (prompt "\n ERF   Turn OFF picked object's layer (loop)")
  (prompt "\n ERA   All layers ON + Thawed + Unisolate")
  (prompt "\n ERAF  Turn OFF all layers except current")
  (prompt "\n ERL   Lock picked object's layer (loop)")
  (prompt "\n ERU   Unlock picked object's layer (loop)")
  (prompt "\n EREX  Export layer list + groups (TXT or JSON)")
  (prompt "\n ERIM  Import layers from a TXT export")
  (prompt "\n ERSC  Show this list")
  (prompt "\n===============================================")
  (princ)
)

(princ "\nAKDLayerTools loaded. Type ERSC for the shortcut list.")
(princ)
