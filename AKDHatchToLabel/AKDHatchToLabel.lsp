(defun c:HATX
  (/ f line p key val readRooms
     room-list room-shortcuts room-names keyword-list keyword-prompt pair parts
     target-layer room-col area-col
     unit1 unit2 dec1 dec2 conv1 conv2
     th-room th-area spacing fontname bold uppercase
     ent txtent area sqm val1 val2 pt line1 line2 txt mode room input roomtxt
     old-type old-fac settings-path lisp-full-path
     ss i e ed oldstr newstr newroom pos)

  ;; ==================================================
  ;; HELPERS
  ;; ==================================================

  (defun _split (str delim / pos out)
    (setq out '())
    (while (setq pos (vl-string-search delim str))
      (setq out (append out (list (substr str 1 pos))))
      (setq str (substr str (+ pos (strlen delim) 1)))
    )
    (append out (list str))
  )

  (defun _join (lst sep / r)
    (if lst
      (progn
        (setq r (car lst))
        (foreach x (cdr lst)
          (setq r (strcat r sep x)))
        r)
      "")
  )

  (defun _find-room (kw names keys / i ans)
    (setq i 0 ans nil)
    (repeat (length keys)
      (if (= (strcase kw) (strcase (nth i keys)))
        (setq ans (nth i names)))
      (setq i (1+ i)))
    ans
  )

  (defun _parse-unit (s / a)
    (setq a (_split s "|"))
    (list
      (if (car a) (car a) "0")
      (if (cadr a) (atoi (cadr a)) 2)
    )
  )

  (defun _factor (u)
    (cond
      ((= (strcase u) "SQM")   1.0)
      ((= (strcase u) "SQFT")  10.7639)
      ((= (strcase u) "HA")    0.0001)
      ((= (strcase u) "ACRE")  0.000247105)
      ((= (strcase u) "0")     0.0)
      (T 1.0)
    )
  )

  (defun _uline (u v dec col h)
    (if (= (strcase u) "0")
      nil
      (strcat
        "{\\C" col ";\\H" (rtos h 2 0) ";"
        (rtos v 2 dec) " " (strcase u)
        "}"
      )
    )
  )

  ;; ==================================================
  ;; FIND SETTINGS FILE
  ;; ==================================================

  (setq lisp-full-path (findfile "AKDHatchToLabel.lsp"))

  (if lisp-full-path
    (setq settings-path
      (strcat (vl-filename-directory lisp-full-path) "/AKDHatchToLabel_Settings.txt"))
    (setq settings-path "AKDHatchToLabel_Settings.txt")
  )

  ;; ==================================================
  ;; DEFAULTS
  ;; ==================================================

  (setq target-layer "0")
  (setq room-col "256")
  (setq area-col "2")

  (setq room-list "BED=Bedroom,BAL=Balcony,TOI=Toilet")

  (setq unit1 "SQM")
  (setq unit2 "SQFT")
  (setq dec1 2)
  (setq dec2 0)

  (setq conv1 nil)
  (setq conv2 nil)

  (setq th-room 150)
  (setq th-area 100)
  (setq spacing 0.55)

  (setq fontname "Arial")
  (setq bold "Yes")
  (setq uppercase "Yes")

  (if (not *HATX-LAST-ROOM*)
    (setq *HATX-LAST-ROOM* nil))

  ;; ==================================================
  ;; READ SETTINGS (UPGRADED MULTILINE PARSER)
  ;; ==================================================

  (setq f (open settings-path "r"))
  (if f
    (progn
      (setq room-list "")
      (setq readRooms nil)

      (while (setq line (read-line f))

        (if (and (/= line "") (/= (substr line 1 1) "#"))

          (progn

            ;; Rooms block mode
            (if readRooms
              (progn
                (if (vl-string-search ":" line)
                  (setq readRooms nil)
                  (progn
                    (if (= room-list "")
                      (setq room-list line)
                      (setq room-list (strcat room-list "," line))
                    )
                  )
                )
              )
            )

            ;; Normal settings
            (if (not readRooms)
              (progn
                (setq p (vl-string-search ":" line))

                (if p
                  (progn
                    (setq key (substr line 1 p))
                    (setq val (substr line (+ p 2)))

                    (cond
                      ((= key "Layer")          (setq target-layer val))
                      ((= key "RoomColor")      (setq room-col val))
                      ((= key "AreaColor")      (setq area-col val))

                      ((= key "Rooms")
                       (setq room-list "")
                       (if (/= val "") (setq room-list val))
                       (setq readRooms T)
                      )

                      ((= key "Units1")
                       (setq val (_parse-unit val))
                       (setq unit1 (car val))
                       (setq dec1 (cadr val)))

                      ((= key "Units2")
                       (setq val (_parse-unit val))
                       (setq unit2 (car val))
                       (setq dec2 (cadr val)))

                      ((= key "Conv1")
                       (if (/= val "") (setq conv1 (atof val))))

                      ((= key "Conv2")
                       (if (/= val "") (setq conv2 (atof val))))

                      ((= key "TextHeightRoom") (setq th-room (atof val)))
                      ((= key "TextHeightArea") (setq th-area (atof val)))
                      ((= key "LineSpacing")    (setq spacing (atof val)))

                      ((= key "Font")           (setq fontname val))
                      ((= key "Bold")           (setq bold val))
                      ((= key "Uppercase")      (setq uppercase val))
                    )
                  )
                )
              )
            )
          )
        )
      )
      (close f)
    )
  )

  ;; ==================================================
  ;; BUILD ROOM LIST
  ;; ==================================================

  (setq room-shortcuts '())
  (setq room-names '())

  (foreach pair (_split room-list ",")
    (setq parts (_split pair "="))

    (if (= (length parts) 2)
      (progn
        (setq room-shortcuts (append room-shortcuts (list (car parts))))
        (setq room-names     (append room-names (list (cadr parts))))
      )
      (progn
        (setq room-shortcuts (append room-shortcuts (list pair)))
        (setq room-names     (append room-names (list pair)))
      )
    )
  )

  (setq keyword-list room-shortcuts)
  (setq keyword-prompt (_join keyword-list "/"))

  (if *HATX-LAST-ROOM*
    (setq room *HATX-LAST-ROOM*)
    (setq room (car room-names))
  )

  ;; ==================================================
  ;; SAVE VARS
  ;; ==================================================

  (setq old-type (getvar "TSPACETYPE"))
  (setq old-fac (getvar "TSPACEFAC"))

  ;; ==================================================
  ;; MODE
  ;; ==================================================

  (initget "SelectHatch RoomName")
  (setq mode
    (getkword
      "\nChoose option [SelectHatch/RoomName] <SelectHatch>: "
    )
  )

  (if (= mode "RoomName")
    (progn
      (initget (_join keyword-list " "))
      (setq input
        (getkword
          (strcat "\nChoose room [" keyword-prompt "]: ")
        )
      )
      (setq room (_find-room input room-names keyword-list))
      (setq *HATX-LAST-ROOM* room)
    )
  )

  ;; ==================================================
  ;; OBJECT SELECTION
  ;; ==================================================

  (setq ent nil)
  (setq txtent nil)

  (setq ss (ssget "_I"))

  (if ss
    (progn
      (setq i 0)
      (while (< i (sslength ss))
        (setq e (ssname ss i))

        (cond
          ((= (cdr (assoc 0 (entget e))) "HATCH")
           (if (not ent) (setq ent e)))

          ((or
             (= (cdr (assoc 0 (entget e))) "MTEXT")
             (= (cdr (assoc 0 (entget e))) "TEXT"))
           (if (not txtent) (setq txtent e)))
        )

        (setq i (1+ i))
      )
    )
  )

  ;; ==================================================
  ;; EDIT EXISTING TEXT
  ;; ==================================================

  (if txtent
    (progn
      (initget (_join keyword-list " "))
      (setq input
        (getkword
          (strcat "\nChoose new room [" keyword-prompt "]: ")
        )
      )

      (setq room (_find-room input room-names keyword-list))
      (setq *HATX-LAST-ROOM* room)

      (setq ed (entget txtent))
      (setq oldstr (cdr (assoc 1 ed)))

      (setq roomtxt room)

      (if (= (strcase uppercase) "YES")
        (setq roomtxt (strcase roomtxt)))

      (setq newroom
        (strcat
          "{\\C" room-col ";"
          "\\f" fontname
          (if (= (strcase bold) "YES") "|b1" "")
          ";\\H" (rtos th-room 2 0) ";"
          roomtxt
          "}"
        )
      )

      (setq pos (vl-string-search "\\P" oldstr))

      (if pos
        (setq newstr
          (strcat newroom (substr oldstr (+ pos 1))))
        (setq newstr newroom))

      (entmod
        (subst
          (cons 1 newstr)
          (assoc 1 ed)
          ed))
      (entupd txtent)
    )

    ;; ==================================================
    ;; NORMAL HATCH PROCESS
    ;; ==================================================

    (progn
      ;; --------------------------------------
      ;; Batch hatch selection
      ;; --------------------------------------
      (if (not ent)
        (setq ss (ssget '((0 . "HATCH"))))
        (progn
          (setq ss (ssadd))
          (ssadd ent ss)
        )
      )

      (if ss
        (progn
          (setq i 0)
          (setq roomCounts '()) ;; association list: (("Bedroom" . 2) ...)

          (while (< i (sslength ss))
            (setq ent (ssname ss i))

            ;; --------------------------------------
            ;; Ask room
            ;; --------------------------------------
            (initget (_join keyword-list " "))
            (setq input
              (getkword
                (strcat "\nChoose room for hatch "
                        (itoa (1+ i))
                        " ["
                        keyword-prompt
                        "]: ")
              )
            )

            (if input
              (setq room (_find-room input room-names keyword-list))
            )

            (if room
              (setq *HATX-LAST-ROOM* room)
            )

            ;; --------------------------------------
            ;; Number duplicate rooms
            ;; --------------------------------------
            (setq item (assoc room roomCounts))

            (if item
              (progn
                (setq num (1+ (cdr item)))
                (setq roomCounts
                  (subst (cons room num) item roomCounts))
              )
              (progn
                (setq num 1)
                (setq roomCounts
                  (cons (cons room 1) roomCounts))
              )
            )

            (setq roomtxt (strcat room " " (itoa num)))

            ;; Uppercase option
            (if (= (strcase uppercase) "YES")
              (setq roomtxt (strcase roomtxt))
            )

            ;; --------------------------------------
            ;; Area calc
            ;; --------------------------------------
            (command "_.AREA" "_O" ent)
            (setq area (getvar "AREA"))
            (setq sqm (/ area 1000000.0))

            (if conv1
              (setq val1 (* sqm conv1))
              (setq val1 (* sqm (_factor unit1)))
            )

            (if conv2
              (setq val2 (* sqm conv2))
              (setq val2 (* sqm (_factor unit2)))
            )

            (setq line1 (_uline unit1 val1 dec1 area-col th-area))
            (setq line2 (_uline unit2 val2 dec2 area-col th-area))

            ;; --------------------------------------
            ;; Build text
            ;; --------------------------------------
            (setq txt
              (strcat
                "{\\C" room-col ";"
                "\\f" fontname
                (if (= (strcase bold) "YES") "|b1" "")
                ";\\H" (rtos th-room 2 0) ";"
                roomtxt
                "}"
              )
            )

            (if line1
              (setq txt (strcat txt "\\P" line1))
            )

            (if line2
              (setq txt (strcat txt "\\P" line2))
            )

            ;; --------------------------------------
            ;; Place text
            ;; --------------------------------------
            (setq pt
              (getpoint
                (strcat "\nClick location for "
                        roomtxt
                        ": ")
              )
            )

            (if pt
              (progn
                (if (tblsearch "LAYER" target-layer)
                  (setvar "CLAYER" target-layer)
                )

                (setvar "TSPACETYPE" 2)
                (setvar "TSPACEFAC" spacing)

                (command "_.MTEXT" pt "_J" "_MC" "_W" 0 txt "")
              )
            )

            (setq i (1+ i))
          )

          ;; restore vars
          (setvar "TSPACETYPE" old-type)
          (setvar "TSPACEFAC" old-fac)
        )
        (prompt "\nNo hatch selected.")
      )
    )
  )

  (princ)
)