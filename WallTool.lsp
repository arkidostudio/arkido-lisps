(defun trim-line-to-point-start (ent pt / data)
  (setq data (entget ent))
  (setq data (subst (cons 10 pt) (assoc 10 data) data))
  (entmod data)
)

(defun trim-line-to-point-end (ent pt / data)
  (setq data (entget ent))
  (setq data (subst (cons 11 pt) (assoc 11 data) data))
  (entmod data)
)

(defun c:WT ( / thick_list idx val mode offset
                p1 p2 ang dist
                a1 a2 b1 b2
                prevA1 prevA2 prevB1 prevB2
                prevL1 prevL2
                firstA1 firstB1
                l1 l2 int1 int2
                ss i other odata op1 op2 ip)

  ;; Load config
  (load (findfile "WallToolConfig.lsp"))
  (setq thick_list *WT_ThicknessList*)

  ;; Thickness
  (setq idx (getint "\nSelect thickness index: "))

  (if (and idx (>= idx 0) (< idx (length thick_list)))
    (progn
      (setq val (nth idx thick_list))

      ;; Mode
      (initget "Center Left Right")
      (setq mode (getkword "\nMode [Center/Left/Right] <Center>: "))
      (if (not mode) (setq mode "Center"))

      (cond
        ((= mode "Center") (setq offset (/ val 2.0)))
        ((= mode "Left")   (setq offset val))
        ((= mode "Right")  (setq offset (- val)))
      )

      (prompt "\nStart wall")
      (setq p1 (getpoint "\nStart point: "))

      (while p1
        (setq p2 (getpoint p1 "\nNext point: "))

        (if p2
          (progn
            (setq ang (angle p1 p2))
            (setq dist (distance p1 p2))

            ;; Offset points
            (setq a1 (polar p1 (+ ang (/ pi 2)) offset))
            (setq b1 (polar p1 (- ang (/ pi 2)) offset))

            (setq a2 (polar a1 ang dist))
            (setq b2 (polar b1 ang dist))

            ;; Store first
            (if (not firstA1)
              (progn
                (setq firstA1 a1)
                (setq firstB1 b1)
              )
            )

            ;; ---- CORNER CLEAN ----
            (if prevL1
              (progn
                (setq int1 (inters prevA1 prevA2 a1 a2 nil))
                (setq int2 (inters prevB1 prevB2 b1 b2 nil))

                (if int1
                  (progn
                    (trim-line-to-point-end prevL1 int1)
                    (setq a1 int1)
                  )
                )

                (if int2
                  (progn
                    (trim-line-to-point-end prevL2 int2)
                    (setq b1 int2)
                  )
                )
              )
            )

            ;; Draw new
            (setq l1 (entmakex (list (cons 0 "LINE") (cons 10 a1) (cons 11 a2))))
            (setq l2 (entmakex (list (cons 0 "LINE") (cons 10 b1) (cons 11 b2))))

            ;; ---- FIXED INTERSECTION LOGIC ----
            (setq ss (ssget "_X" '((0 . "LINE"))))

            (if ss
              (progn
                (setq i 0)
                (while (< i (sslength ss))
                  (setq other (ssname ss i))

                  ;; skip self + previous
                  (if (and (/= other l1) (/= other l2)
                           (/= other prevL1) (/= other prevL2))
                    (progn
                      (setq odata (entget other))
                      (setq op1 (cdr (assoc 10 odata)))
                      (setq op2 (cdr (assoc 11 odata)))

                      ;; ONLY trim NEW lines
                      (setq ip (inters a1 a2 op1 op2 T))
                      (if ip (trim-line-to-point-end l1 ip))

                      (setq ip (inters b1 b2 op1 op2 T))
                      (if ip (trim-line-to-point-end l2 ip))
                    )
                  )

                  (setq i (1+ i))
                )
              )
            )

            ;; Store prev
            (setq prevA1 a1
                  prevA2 a2
                  prevB1 b1
                  prevB2 b2
                  prevL1 l1
                  prevL2 l2)

            (setq p1 p2)
          )
          (setq p1 nil)
        )
      )

      ;; ---- CAPS ----
      (if (and firstA1 firstB1)
        (entmakex (list (cons 0 "LINE") (cons 10 firstA1) (cons 11 firstB1)))
      )

      (if (and prevA2 prevB2)
        (entmakex (list (cons 0 "LINE") (cons 10 prevA2) (cons 11 prevB2)))
      )
    )
  )

  (princ)
)