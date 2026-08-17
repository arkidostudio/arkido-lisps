; ==========================================================
; AKDPickArea  (command: AA)
; Arkido Area Picker
;
; Features
; ✓ Continuous pick mode
; ✓ Saved settings
; ✓ Units: MM2 / M2 / FT2
; ✓ Scale: None / Standard / Custom
; ✓ Undo last
; ✓ Label memory
; ✓ Add Area from existing text
; ✓ Clipboard mode (manual copy display)
; ✓ Final MTEXT output
; ✓ Cleaned / streamlined code
; ==========================================================

(defun c:AA
(
 / *error*
   ent ed typ area total count picked hist hilite
   unit mode factor denom
   oldunit oldmode oldval
   cmd done clipOnly
   handle rec
   labelTxt pre num
   result txtUnit finalTxt
   pt hgt sty
   raw numstr val i ch
)

  ;; ------------------------------------------------
  ;; Helpers
  ;; ------------------------------------------------
  (defun ap-convert (val u)
    (cond
      ((= u "MM2") val)
      ((= u "M2")  (/ val 1000000.0))
      ((= u "FT2") (/ val 92903.04))
    )
  )

  (defun ap-suffix (u)
    (cond
      ((= u "MM2") " SQMM")
      ((= u "M2")  " SQM")
      ((= u "FT2") " SQFT")
    )
  )

  (defun ap-display-total ()
    (ap-convert (/ total (* factor factor)) unit)
  )

  (defun ap-clear ()
    (foreach x hilite (redraw x 4))
    (command "_.REGEN")
  )

  (defun ap-status ()
    (prompt
      (strcat
        "\n[Total: "
        (rtos (ap-display-total) 2 2)
        " "
        unit
        " | Count: "
        (itoa count)
        (if (/= labelTxt "")
          (strcat " | Label: " labelTxt)
          ""
        )
        "]"
      )
    )
  )

  ;; ------------------------------------------------
  ;; Error Handler
  ;; ------------------------------------------------
  (defun *error* (msg)
    (ap-clear)
    (if (and msg (/= msg "Function cancelled"))
      (prompt (strcat "\nError: " msg))
    )
    (princ)
  )

  ;; ------------------------------------------------
  ;; Load Settings
  ;; ------------------------------------------------
  (setq oldunit (getenv "AREAPICK_UNIT"))
  (setq oldmode (getenv "AREAPICK_MODE"))
  (setq oldval  (getenv "AREAPICK_VAL"))

  (if (null oldunit) (setq oldunit "M2"))
  (if (null oldmode) (setq oldmode "None"))
  (if (null oldval)  (setq oldval "1"))

  (setq unit oldunit
        mode oldmode
        factor 1.0
        labelTxt "")

  (cond
    ((= mode "Standard")
      (setq factor (/ 1.0 (atof oldval))))
    ((= mode "Custom")
      (setq factor (atof oldval)))
  )

  ;; ------------------------------------------------
  ;; Init
  ;; ------------------------------------------------
  (setq total 0.0
        count 0
        picked '()
        hist '()
        hilite '()
        done nil
        clipOnly nil)

  ;; ------------------------------------------------
  ;; Header
  ;; ------------------------------------------------
  (prompt "\n================================================")
  (prompt "\nAKDPickArea (AA)")
  (prompt
    (strcat
      "\nUnit: " unit
      " | Mode: " mode
      " | Scale: " oldval
    )
  )
  (prompt "\n")
  (prompt "\nPick")
  (prompt "\nSettings")
  (prompt "\nLabel")
  (prompt "\nUndo")
  (prompt "\nAdd Area")
  (prompt "\nClipboard")
  (prompt "\nDone")
  (prompt "\n================================================")

  ;; ------------------------------------------------
  ;; Main Loop
  ;; ------------------------------------------------
  (while (not done)

    (ap-status)

    (initget "P S B U A C D Pick Settings Label Undo AddArea Clipboard Done")
    (setq cmd (getkword "\nAction [Pick/Settings/Label/Undo/AddArea/Clipboard/Done]: "))

    ;; shortcuts
    (if (= cmd "P") (setq cmd "Pick"))
    (if (= cmd "S") (setq cmd "Settings"))
    (if (= cmd "B") (setq cmd "Label"))
    (if (= cmd "U") (setq cmd "Undo"))
    (if (= cmd "A") (setq cmd "AddArea"))
    (if (= cmd "C") (setq cmd "Clipboard"))
    (if (= cmd "D") (setq cmd "Done"))

    ;; ============================================
    ;; DONE
    ;; ============================================
    (if (= cmd "Done")
      (setq done T)
    )

    ;; ============================================
    ;; PICK
    ;; ============================================
    (if (or (null cmd) (= cmd "Pick"))
      (progn
        (prompt "\nPick Mode - Press ENTER when done.")

        (while (setq ent (car (entsel "\nSelect polyline: ")))

          (setq handle (cdr (assoc 5 (entget ent))))

          (if (member handle picked)
            (prompt "\n⚠ Already selected.")
            (progn
              (setq typ (cdr (assoc 0 (entget ent))))

              (if (or (= typ "LWPOLYLINE") (= typ "POLYLINE"))
                (progn
                  (redraw ent 3)
                  (command "_.AREA" "_O" ent)
                  (setq area (getvar "AREA"))

                  (setq total (+ total area))
                  (setq count (+ count 1))
                  (setq picked (cons handle picked))
                  (setq hist (cons (list ent handle area) hist))
                  (setq hilite (cons ent hilite))

                  (prompt
                    (strcat
                      "\n✓ Added #"
                      (itoa count)
                      " | Total = "
                      (rtos (ap-display-total) 2 2)
                    )
                  )
                )
                (prompt "\n✖ Not a polyline.")
              )
            )
          )
        )
      )
    )

    ;; ============================================
    ;; SETTINGS
    ;; ============================================
    (if (= cmd "Settings")
      (progn
        (initget "MM2 M2 FT2")
        (setq unit (getkword (strcat "\nUnit [MM2/M2/FT2] <" unit ">: ")))
        (if (null unit) (setq unit oldunit))

        (initget "None Standard Custom")
        (setq mode (getkword (strcat "\nMode [None/Standard/Custom] <" mode ">: ")))
        (if (null mode) (setq mode oldmode))

        (setq factor 1.0)

        (if (= mode "Standard")
          (progn
            (setq denom (getreal (strcat "\nScale denominator (1:x) <" oldval ">: ")))
            (if (null denom) (setq denom (atof oldval)))
            (if (<= denom 0) (setq denom 1))
            (setq factor (/ 1.0 denom))
            (setq oldval (rtos denom 2 6))
          )
        )

        (if (= mode "Custom")
          (progn
            (setq factor (getreal (strcat "\nCustom factor <" oldval ">: ")))
            (if (null factor) (setq factor (atof oldval)))
            (if (<= factor 0) (setq factor 1))
            (setq oldval (rtos factor 2 6))
          )
        )

        (if (= mode "None")
          (setq oldval "1")
        )

        (setenv "AREAPICK_UNIT" unit)
        (setenv "AREAPICK_MODE" mode)
        (setenv "AREAPICK_VAL" oldval)

        (prompt "\n✓ Settings saved.")
      )
    )

    ;; ============================================
    ;; LABEL
    ;; ============================================
    (if (= cmd "Label")
      (progn
        (initget "D W C")
        (setq pre (getkword "\nLabel [D/W/C] <W>: "))
        (if (null pre) (setq pre "W"))

        (cond
          ((= pre "D")
            (setq num (getstring "\nDoor #: "))
            (setq labelTxt (strcat "D" num))
          )
          ((= pre "W")
            (setq num (getstring "\nWindow #: "))
            (setq labelTxt (strcat "W" num))
          )
          ((= pre "C")
            (setq labelTxt (getstring T "\nCustom label: "))
          )
        )

        (prompt (strcat "\n✓ Label = " labelTxt))
      )
    )

    ;; ============================================
    ;; UNDO
    ;; ============================================
    (if (= cmd "Undo")
      (progn
        (if hist
          (progn
            (setq rec (car hist))
            (setq hist (cdr hist))

            (setq ent    (nth 0 rec))
            (setq handle (nth 1 rec))
            (setq area   (nth 2 rec))

            (setq total (- total area))
            (setq count (- count 1))
            (setq picked (vl-remove handle picked))
            (setq hilite (vl-remove ent hilite))

            (redraw ent 4)
            (prompt "\n↺ Last removed.")
          )
          (prompt "\nNothing to undo.")
        )
      )
    )

    ;; ============================================
    ;; ADD AREA
    ;; ============================================
    (if (= cmd "AddArea")
      (progn
        (prompt "\nSelect text objects. ENTER when done.")

        (while (setq ent (car (entsel "\nSelect text: ")))

          (setq ed  (entget ent))
          (setq typ (cdr (assoc 0 ed)))

          (if (or (= typ "TEXT") (= typ "MTEXT"))
            (progn
              (setq raw (cdr (assoc 1 ed)))
              (setq raw (vl-string-subst " " "\\P" raw))

              (setq numstr "")
              (setq i 1)

              (while (and (<= i (strlen raw)) (= numstr ""))
                (setq ch (substr raw i 1))

                (if (or (and (>= ch "0") (<= ch "9")) (= ch "."))
                  (progn
                    (while (and (<= i (strlen raw))
                                (or
                                  (and (>= (substr raw i 1) "0")
                                       (<= (substr raw i 1) "9"))
                                  (= (substr raw i 1) ".")
                                )
                           )
                      (setq numstr (strcat numstr (substr raw i 1)))
                      (setq i (+ i 1))
                    )
                  )
                )
                (setq i (+ i 1))
              )

              (if (/= numstr "")
                (progn
                  (setq val (atof numstr))

                  (cond
                    ((= unit "M2")  (setq val (* val 1000000.0)))
                    ((= unit "FT2") (setq val (* val 92903.04)))
                  )

                  (setq val (* val (* factor factor)))
                  (setq total (+ total val))

                  (prompt
                    (strcat "\n✓ Added area: " numstr)
                  )
                )
                (prompt "\nNo numeric value found.")
              )
            )
            (prompt "\nNot text.")
          )
        )
      )
    )

    ;; ============================================
    ;; CLIPBOARD
    ;; ============================================
    (if (= cmd "Clipboard")
      (progn
        (prompt "\n====================")
        (prompt "\nCOPY VALUE:")
        (prompt (strcat "\n" (rtos (ap-display-total) 2 2)))
        (prompt "\nCmd+C")
        (prompt "\n====================")

        (setq clipOnly T)
        (setq done T)
      )
    )

  ) ;; end while

  ;; ------------------------------------------------
  ;; Cleanup
  ;; ------------------------------------------------
  (ap-clear)

  ;; ------------------------------------------------
  ;; Final Output
  ;; ------------------------------------------------
  (setq result  (ap-display-total))
  (setq txtUnit (ap-suffix unit))

  (if (and (> result 0) (not clipOnly))
    (progn
      (setq pt  (getpoint "\nPick bottom-right insertion point: "))
      (setq hgt (getvar "TEXTSIZE"))
      (setq sty (getvar "TEXTSTYLE"))

      (setq finalTxt
        (if (/= labelTxt "")
          (strcat labelTxt "\\P" (rtos result 2 2) txtUnit)
          (strcat (rtos result 2 2) txtUnit)
        )
      )

      (command
        "_.MTEXT"
        pt
        "_J" "BR"
        "_H" hgt
        "_W" 0
        finalTxt
        ""
      )

      (prompt "\n✓ Right-justified result placed.")
    )
  )

  (princ)
)