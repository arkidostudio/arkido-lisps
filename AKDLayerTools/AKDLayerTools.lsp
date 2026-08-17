; ============================================================
; AKDLayerTools.lsp
; Layer utilities in one file:
;   ER1   Set Current Layer     (preset list, re-runs last draw cmd)
;   ERS   Select By Layer       (grab all objects on a layer)
;   ERT   Move To Layer         (move picked objects to a layer)
;   ERD   Isolate Objects       (pick → hide the rest; run again to unisolate)
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
          (setq i 0)
          (repeat (sslength ss)
            (setq ent (ssname ss i))
            (entmod (subst (cons 8 selected)
                           (assoc 8 (entget ent))
                           (entget ent)))
            (setq i (1+ i)))
          (command "_.REGEN")
          (sssetfirst nil nil)
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

; ============================================================
; ERD - Isolate Objects: hide everything except the picked
;       objects (regardless of layer). Run again to unisolate.
;       Wraps native ISOLATEOBJECTS / UNISOLATEOBJECTS.
; ============================================================

(defun c:ERD (/ ss)
  (if *erd:isolated*
    (progn
      (command "_.UNISOLATEOBJECTS")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (setq *erd:isolated* nil)
      (princ "\nObjects unhidden."))
    (progn
      (prompt "\nPick objects to isolate. Enter when done.")
      (if (setq ss (ssget))
        (progn
          (command "_.ISOLATEOBJECTS" ss "")
          (while (> (getvar "CMDACTIVE") 0) (command ""))
          (setq *erd:isolated* T)
          (princ "\nObjects isolated. Run ERD again to unisolate."))
        (princ "\nNo objects picked."))))
  (princ)
)

; ============================================================
; ERF - Turn off the layer of a picked object. Repeatable.
; ============================================================

(defun c:ERF (/ e ent lay cur)
  (setq cur (getvar "CLAYER"))
  (prompt "\nPick object(s) — layer gets turned off. Enter to exit.")
  (while (setq e (entsel "\nPick object: "))
    (setq ent (car e)
          lay (cdr (assoc 8 (entget ent))))
    (cond
      ((= lay cur)
       (princ (strcat "\nSkip: " lay " is the current layer.")))
      (T
       (akd:layer-cmd "_OFF" lay)
       (princ (strcat "\nLayer OFF: " lay)))))
  (princ)
)

; ============================================================
; ERA - Thaw + turn on all layers (recover from ERD/ERF/ERAF)
; ============================================================

(defun c:ERA ()
  (command "_.-LAYER" "_THAW" "*" "_ON" "*" "")
  (while (> (getvar "CMDACTIVE") 0) (command ""))
  (if *erd:isolated*
    (progn
      (command "_.UNISOLATEOBJECTS")
      (while (> (getvar "CMDACTIVE") 0) (command ""))
      (setq *erd:isolated* nil)))
  (princ "\nAll layers ON, thawed, objects unhidden.")
  (princ)
)

; ============================================================
; ERAF - Turn OFF all layers except current
; ============================================================

(defun c:ERAF (/ cur)
  (setq cur (getvar "CLAYER"))
  (foreach lay (akd:all-layers)
    (if (/= lay cur) (akd:layer-cmd "_OFF" lay)))
  (princ (strcat "\nAll layers off except: " cur))
  (princ)
)

; ============================================================
; ERL - Lock layers by picking objects. Repeatable.
; ============================================================

(defun c:ERL (/ e ent lay cur done)
  (setq cur (getvar "CLAYER") done nil)
  (prompt "\nPick object(s) — layer gets locked. [A]=All except current. Enter to exit.")
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
  (princ)
)

; ============================================================
; ERU - Unlock layers by picking objects. Repeatable.
; ============================================================

(defun c:ERU (/ e ent lay done)
  (setq done nil)
  (prompt "\nPick object(s) — layer gets unlocked. [A]=All layers. Enter to exit.")
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
  (princ)
)

; ============================================================
; ERSC - Show shortcuts
; ============================================================

(defun c:ERSC ()
  (prompt "\n================ AKDLayerTools ================")
  (prompt "\n ER1   Set Current Layer (preset list)")
  (prompt "\n ERS   Select By Layer")
  (prompt "\n ERT   Move Selected Objects To Layer")
  (prompt "\n ERD   Isolate picked objects (toggle)")
  (prompt "\n ERF   Turn OFF picked object's layer (loop)")
  (prompt "\n ERA   All layers ON + Thawed + Unisolate")
  (prompt "\n ERAF  Turn OFF all layers except current")
  (prompt "\n ERL   Lock picked object's layer (loop)")
  (prompt "\n ERU   Unlock picked object's layer (loop)")
  (prompt "\n ERSC  Show this list")
  (prompt "\n===============================================")
  (princ)
)

(princ "\nAKDLayerTools loaded. Type ERSC for the shortcut list.")
(princ)
