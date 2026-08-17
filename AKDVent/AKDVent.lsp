;;; ================================================================
;;; AKDVent - CLEANED FINAL VERSION
;;; AutoCAD Mac Compatible
;;; Ventilation Schedule Generator
;;; ================================================================

(defun c:VE (/ cfg pt rows x y prevDivider row)

  (setq cfg (gt:get-config))
  (setq rows (gt:collect-rows))

  (if rows
    (progn
      (setq pt (getpoint "\nPick insertion point: "))

      (if pt
        (progn
          (setq x (car pt)
                y (cadr pt))

          ;; Title
          (gt:draw-title x y cfg)

          ;; Header
          (setq y (- y (cdr (assoc 'titleH cfg))))
          (gt:draw-header x y cfg)

          ;; Rows
          (setq y (- y (cdr (assoc 'headerH cfg))))
          (setq prevDivider nil)

          (foreach row rows
            (gt:draw-data-row x y row cfg prevDivider)

            (setq prevDivider
              (and (> (length row) 5)
                   (= (nth 5 row) 'DIVIDER)))

            (setq y (- y (cdr (assoc 'rowH cfg))))
          )

          ;; Bottom Border
          (gt:draw-line
            (list x y 0)
            (list (+ x (gt:table-width cfg)) y 0)
            (cdr (assoc 'frameCol cfg)))
        )
      )
    )
  )
  (princ)
)

;;; ================================================================
;;; CONFIG
;;; ================================================================

(defun gt:get-config (/ txtStyle hdrStyle)
  (setq txtStyle (getvar "TEXTSTYLE"))

  (setq hdrStyle
        (if (tblsearch "STYLE" "TB_BOLD")
          "TB_BOLD"
          txtStyle))

  (list
    (cons 'titleText "VENTILATION SCHEDULE")

    (cons 'titleH 680.0)
    (cons 'headerH 750.0)
    (cons 'rowH 550.0)

    (cons 'txtH 180.0)
    (cons 'titleTxtH 250.0)

    (cons 'col1 3200.0)
    (cons 'col2 2600.0)
    (cons 'col3 2200.0)
    (cons 'col4 3700.0)
    (cons 'col5 3700.0)

    (cons 'frameCol 3) ;; Green outer frame
    (cons 'hCol 1)     ;; Red horizontals
    (cons 'vCol 2)     ;; Yellow verticals

    (cons 'txtStyle txtStyle)
    (cons 'hdrStyle hdrStyle)
  )
)

(defun gt:table-width (cfg)
  (+ (cdr (assoc 'col1 cfg))
     (cdr (assoc 'col2 cfg))
     (cdr (assoc 'col3 cfg))
     (cdr (assoc 'col4 cfg))
     (cdr (assoc 'col5 cfg)))
)

;;; ================================================================
;;; DATA COLLECTION
;;; ================================================================

(defun gt:collect-rows (/ rows currFloor cmd fl ent raw done donePick)

  (setq rows '()
        currFloor nil
        done nil)

  (while (not done)

    (initget "SetFloor PickArea Finish")
    (setq cmd
      (getkword
        "\n[SetFloor/PickArea/Finish]: "))

    (cond

      ;; Finish
      ((or (= cmd "Finish") (= cmd nil))
       (setq done T))

      ;; ------------------------------------------------
      ;; Set Floor + Auto Pick
      ;; ------------------------------------------------
      ((= cmd "SetFloor")

       (initget "0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 T M")
       (setq fl
         (getkword
           "\n[0/1/2/3/4/5/6/7/8/9/10/11/12/13/14/15/T/M]: "))

       (if (= fl "M")
         (setq fl (getstring T "\nManual Floor Name/Number: ")))

       (if (/= fl "")
         (progn
           (setq currFloor fl)

           ;; Divider row
           (setq rows
             (append rows
               (list
                 (list (gt:floor-name currFloor)
                       "" "" "" "" 'DIVIDER))))

           (prompt
             (strcat "\nCurrent Floor: "
                     (gt:floor-name currFloor)))

           ;; Auto pick mode
           (prompt "\nPick MTEXT objects. Press Enter when done.")

           (setq donePick nil)

           (while (not donePick)

             (setq ent (entsel "\nPick MTEXT: "))

             (if ent
               (if (= (cdr (assoc 0 (entget (car ent)))) "MTEXT")
                 (progn
                   (setq raw (cdr (assoc 1 (entget (car ent)))))
                   (setq rows
                     (append rows
                       (list (gt:parse-room raw))))
                   (prompt "\nAdded room.")
                 )
                 (prompt "\nNot MTEXT.")
               )
               (setq donePick T)
             )
           )
         )
       )
      )

      ;; ------------------------------------------------
      ;; Pick Area
      ;; ------------------------------------------------
      ((= cmd "PickArea")

       (prompt "\nPick MTEXT objects. Press Enter when done.")

       (setq donePick nil)

       (while (not donePick)

         (setq ent (entsel "\nPick MTEXT: "))

         (if ent
           (if (= (cdr (assoc 0 (entget (car ent)))) "MTEXT")
             (progn
               (setq raw (cdr (assoc 1 (entget (car ent)))))
               (setq rows
                 (append rows
                   (list (gt:parse-room raw))))
               (prompt "\nAdded room.")
             )
             (prompt "\nNot MTEXT.")
           )
           (setq donePick T)
         )
       )
      )
    )
  )
  rows
)

(defun gt:floor-name (n / num)

  (cond
    ((= n "0") "GROUND FLOOR")
    ((= (strcase n) "T") "TERRACE")

    ((numberp (read n))
     (setq num (atoi n))

     (cond
       ((= num 1) "1ST FLOOR")
       ((= num 2) "2ND FLOOR")
       ((= num 3) "3RD FLOOR")
       (T (strcat n "TH FLOOR"))
     )
    )

    (T (strcase n))
  )
)

;;; ================================================================
;;; PARSER
;;; ================================================================

(defun gt:parse-room (txt / lines room numText nums sqm sqft req)

  ;; Normalize MTEXT
  (setq txt (vl-string-subst "\n" "\\P" txt))
  (setq txt (vl-string-subst "" "{" txt))
  (setq txt (vl-string-subst "" "}" txt))
  (setq txt (gt:strip-mtext txt))

  ;; Split lines
  (setq lines (gt:split-lines txt))
  (setq room (if lines (car lines) txt))

  ;; Try numbers from lines after first
  (setq numText "")
  (foreach ln (cdr lines)
    (setq numText (strcat numText " " ln)))

  (setq nums (gt:extract-numbers numText))

  ;; Fallback: search whole text
  (if (= (length nums) 0)
    (setq nums (gt:extract-numbers txt)))

  ;; Values
  (if (>= (length nums) 1)
    (setq sqm (nth 0 nums))
    (setq sqm 0.0))

  (if (>= (length nums) 2)
    (setq sqft (nth 1 nums))
    (setq sqft 0.0))

  ;; Clean room name if single-line source
  (if (= (length lines) 1)
    (progn
      (setq room txt)
      (foreach n nums
        (setq room (vl-string-subst "" (rtos n 2 2) room)))
      (setq room (vl-string-subst "" "SQM" room))
      (setq room (vl-string-subst "" "SQFT" room))
      (setq room (vl-string-trim " " room))
    )
  )

  (setq req (* sqm 0.10))

  (list room sqm "X" req 0.0)
)

;;; ================================================================
;;; DRAWING
;;; ================================================================

(defun gt:draw-title (x y cfg / w)

  (setq w (gt:table-width cfg))

  (gt:draw-line (list x y 0) (list (+ x w) y 0) (cdr (assoc 'frameCol cfg)))
  (gt:draw-line (list x y 0) (list x (- y (cdr (assoc 'titleH cfg))) 0) (cdr (assoc 'frameCol cfg)))
  (gt:draw-line (list (+ x w) y 0) (list (+ x w) (- y (cdr (assoc 'titleH cfg))) 0) (cdr (assoc 'frameCol cfg)))

  (gt:make-text
    (+ x (/ w 2.0))
    (- y 430)
    (cdr (assoc 'titleText cfg))
    (cdr (assoc 'titleTxtH cfg))
    (cdr (assoc 'hdrStyle cfg))
    "CENTER"
    7)
)

(defun gt:draw-header (x y cfg)

  (gt:draw-row
    x y
    (list
      "ROOM NAME/\nNUMBER"
      "ROOM AREAS\n(SQM)"
      "OPENING\nNUMBER"
      "REQUIRED OPENING AREA\n(SQM)"
      "DESIGNED OPENING AREA\n(SQM)")
    cfg T T (cdr (assoc 'headerH cfg)))
)

(defun gt:draw-data-row (x y row cfg prevDivider / vals)

  (if (and (> (length row) 5)
           (= (nth 5 row) 'DIVIDER))

    (gt:draw-divider-row x y (nth 0 row) cfg)

    (progn
      (setq vals
        (list
          (nth 0 row)
          (strcat (rtos (nth 1 row) 2 2) " SQM")
          (nth 2 row)
          (strcat (rtos (nth 3 row) 2 2) " SQM")
          (strcat (rtos (nth 4 row) 2 2) " SQM")))

      (gt:draw-row
        x y vals cfg nil prevDivider
        (cdr (assoc 'rowH cfg)))
    )
  )
)

(defun gt:draw-row (x y vals cfg isHeader whiteTop rowH
                      / c1 c2 c3 c4 c5 widths xpos rowCol vCol txtCol)

  (setq c1 (cdr (assoc 'col1 cfg))
        c2 (cdr (assoc 'col2 cfg))
        c3 (cdr (assoc 'col3 cfg))
        c4 (cdr (assoc 'col4 cfg))
        c5 (cdr (assoc 'col5 cfg)))

  (setq widths (list c1 c2 c3 c4 c5))

  (setq rowCol (if whiteTop 7 (cdr (assoc 'hCol cfg))))
  (setq vCol   (if isHeader 7 (cdr (assoc 'vCol cfg))))
  (setq txtCol (if isHeader 7 2))

  ;; Top horizontal
  (gt:draw-line
    (list x y 0)
    (list (+ x (gt:table-width cfg)) y 0)
    rowCol)

  ;; Verticals
  (setq xpos x)

  (gt:draw-line
    (list xpos y 0)
    (list xpos (- y rowH) 0)
    (cdr (assoc 'frameCol cfg)))

  (foreach w widths
    (setq xpos (+ xpos w))
    (gt:draw-line
      (list xpos y 0)
      (list xpos (- y rowH) 0)
      (if (= xpos (+ x (gt:table-width cfg)))
        (cdr (assoc 'frameCol cfg))
        vCol))
  )

  ;; Text
  (gt:draw-cell-text x y vals widths cfg isHeader txtCol)
)

(defun gt:draw-cell-text (x y vals widths cfg isHeader txtCol / px i w txt)

  (setq px x
        i 0)

  (foreach w widths

    (setq txt (nth i vals))

    (gt:make-text
      (if isHeader
        (+ px 120)
        (if (= i 0)
          (+ px 120)
          (+ px (/ w 2.0))))
      (- y 410)
      txt
      (cdr (assoc 'txtH cfg))
      (if isHeader
        (cdr (assoc 'hdrStyle cfg))
        (cdr (assoc 'txtStyle cfg)))
      (if (or isHeader (= i 0)) "LEFT" "CENTER")
      txtCol)

    (setq px (+ px w))
    (setq i (1+ i))
  )
)

(defun gt:draw-divider-row (x y title cfg / w h)

  (setq w (gt:table-width cfg)
        h (cdr (assoc 'rowH cfg)))

  ;; top white
  (gt:draw-line (list x y 0) (list (+ x w) y 0) 7)

  ;; bottom white
  (gt:draw-line (list x (- y h) 0) (list (+ x w) (- y h) 0) 7)

  ;; sides green
  (gt:draw-line (list x y 0) (list x (- y h) 0) (cdr (assoc 'frameCol cfg)))
  (gt:draw-line (list (+ x w) y 0) (list (+ x w) (- y h) 0) (cdr (assoc 'frameCol cfg)))

  ;; text
  (gt:make-text
    (+ x 120)
    (- y 360)
    title
    (cdr (assoc 'txtH cfg))
    (cdr (assoc 'hdrStyle cfg))
    "LEFT"
    7)
)

;;; ================================================================
;;; PRIMITIVES
;;; ================================================================

(defun gt:draw-line (p1 p2 col)
  (entmake
    (list '(0 . "LINE")
          (cons 62 col)
          (cons 10 p1)
          (cons 11 p2)))
)

(defun gt:make-text (px py str th sty just col / lines yy)

  (if (vl-string-search "\n" str)
    (progn
      (setq lines (gt:split-lines str))
      (setq yy (+ py 66.25))

      (foreach s lines
        (gt:make-text-single px yy s th sty just col)
        (setq yy (- yy 265))
      )
    )
    (gt:make-text-single px py str th sty just col))
)

(defun gt:make-text-single (px py str th sty just col)

  (if (= just "CENTER")
    (entmake
      (list '(0 . "TEXT")
            (cons 62 col)
            (cons 7 sty)
            (cons 10 (list px py 0))
            (cons 11 (list px py 0))
            (cons 72 1)
            (cons 73 0)
            (cons 40 th)
            (cons 1 str)))
    (entmake
      (list '(0 . "TEXT")
            (cons 62 col)
            (cons 7 sty)
            (cons 10 (list px py 0))
            (cons 40 th)
            (cons 1 str)))
  )
)

;;; ================================================================
;;; HELPERS
;;; ================================================================

(defun gt:split-lines (s / p out)
  (setq out '())
  (while (setq p (vl-string-search "\n" s))
    (setq out (append out (list (substr s 1 p))))
    (setq s (substr s (+ p 2))))
  (append out (list s))
)

(defun gt:strip-mtext (s / p1 p2)
  (while (vl-string-search "\\" s)
    (setq p1 (vl-string-search "\\" s)
          p2 (vl-string-search ";" s p1))
    (if (and p1 p2)
      (setq s (strcat (substr s 1 p1)
                      (substr s (+ p2 2))))
      (setq s (vl-string-subst "" "\\" s)))
  )
  s
)

(defun gt:extract-numbers (s / i ch token out)
  (setq i 1
        token ""
        out '())

  (while (<= i (strlen s))
    (setq ch (substr s i 1))

    (if (or (wcmatch ch "#") (= ch "."))
      (setq token (strcat token ch))
      (if (> (strlen token) 0)
        (progn
          (setq out (append out (list (atof token))))
          (setq token ""))))

    (setq i (1+ i))
  )

  (if (> (strlen token) 0)
    (setq out (append out (list (atof token)))))

  out
)

(princ "\nType VE to run.")
(princ)