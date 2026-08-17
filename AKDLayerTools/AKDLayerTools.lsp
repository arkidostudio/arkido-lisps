; ============================================================
; AKDLayerTools.lsp
; Three layer utilities in one file:
;   ERC = Set Current Layer    (preset list, re-runs last draw cmd)
;   ERS = Select By Layer      (grab all objects on a layer)
;   ERT = Move To Layer        (move picked objects to a layer)
; Self-contained: no .dcl or config files needed.
; ============================================================

;; ---- Edit your ERC preset layers here ----
(setq *ERC_Layers*
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
; ERC - Set Current Layer (preset list) + re-fire last draw cmd
; ============================================================

(defun c:ERC (/ fn f dcl_id idx selected lastcmd)

  (if (not *ERC_Layers*)
    (progn (alert "No layers defined in *ERC_Layers*.") (exit)))

  (setq fn (vl-filename-mktemp "erc.dcl"))
  (setq f (open fn "w"))
  (write-line "erc_dialog : dialog { label = \"Set Current Layer\";" f)
  (write-line " : list_box { key = \"lst\"; width = 30; height = 15; allow_accept = true; }" f)
  (write-line " spacer; ok_cancel; }" f)
  (close f)

  (setq dcl_id (load_dialog fn))
  (if (not (new_dialog "erc_dialog" dcl_id)) (exit))

  (start_list "lst")
  (mapcar 'add_list *ERC_Layers*)
  (end_list)

  (setq idx "0")
  (if (and *erc:last-layer* (member *erc:last-layer* *ERC_Layers*))
    (setq idx (itoa (- (length *ERC_Layers*)
                       (length (member *erc:last-layer* *ERC_Layers*))))))

  (action_tile "lst"    "(setq idx $value)(if (= $reason 4) (done_dialog 1))")
  (action_tile "accept" "(done_dialog 1)")
  (action_tile "cancel" "(done_dialog 0)")

  (set_tile "lst" idx)
  (mode_tile "lst" 2)

  (if (= (start_dialog) 1)
    (setq selected (nth (atoi idx) *ERC_Layers*)))

  (unload_dialog dcl_id)
  (vl-file-delete fn)

  (if selected
    (progn
      (setq *erc:last-layer* selected)
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

(princ "\nAKDLayerTools loaded. Commands: ERC  ERS  ERT")
(princ)
