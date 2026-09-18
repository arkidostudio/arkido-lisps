; ==========================================================
; AKDPickArea  (commands: AA, AAS)
; AA  = pick/preselect polylines+hatches, place labeled MTEXT
; AAS = settings (unit, scale)
; Settings persist via env vars.
; ==========================================================

(defun ap-getset ()
  (setq ap-unit   (cond ((getenv "AREAPICK_UNIT")) ("M2"))
        ap-mode   (cond ((getenv "AREAPICK_MODE")) ("None"))
        ap-val    (cond ((getenv "AREAPICK_VAL"))  ("1"))
        ap-factor (cond ((= ap-mode "Standard") (/ 1.0 (atof ap-val)))
                        ((= ap-mode "Custom")   (atof ap-val))
                        (1.0)))
)

(defun ap-convert (v)
  (cond ((= ap-unit "MM2") v)
        ((= ap-unit "M2")  (/ v 1000000.0))
        ((= ap-unit "FT2") (/ v 92903.04))))

(defun ap-suffix ()
  (cond ((= ap-unit "MM2") " SQMM")
        ((= ap-unit "M2")  " SQM")
        ((= ap-unit "FT2") " SQFT")))

(defun ap-area (ent)
  (command "_.AREA" "_O" ent)
  (getvar "AREA"))

;; ---------- Settings command ----------
(defun c:AAS ( / u m v d)
  (ap-getset)
  (initget "MM2 M2 FT2")
  (setq u (getkword (strcat "\nUnit [MM2/M2/FT2] <" ap-unit ">: ")))
  (if u (setq ap-unit u))
  (initget "None Standard Custom")
  (setq m (getkword (strcat "\nMode [None/Standard/Custom] <" ap-mode ">: ")))
  (if m (setq ap-mode m))
  (cond
    ((= ap-mode "Standard")
      (setq d (getreal (strcat "\nScale 1:x <" ap-val ">: ")))
      (if (and d (> d 0)) (setq ap-val (rtos d 2 6))))
    ((= ap-mode "Custom")
      (setq d (getreal (strcat "\nFactor <" ap-val ">: ")))
      (if (and d (> d 0)) (setq ap-val (rtos d 2 6))))
    (T (setq ap-val "1")))
  (setenv "AREAPICK_UNIT" ap-unit)
  (setenv "AREAPICK_MODE" ap-mode)
  (setenv "AREAPICK_VAL"  ap-val)
  (prompt (strcat "\nSaved: " ap-unit " | " ap-mode " | " ap-val))
  (princ))

;; ---------- Main ----------
(defun c:AA
  ( / *error* ss i ent total area pre num label pt hgt txt)

  (defun *error* (msg)
    (if (and msg (/= msg "Function cancelled")) (prompt (strcat "\n" msg)))
    (princ))

  (ap-getset)

  ;; preselection first; else prompt with filter
  (setq ss (cond ((ssget "_I" '((0 . "LWPOLYLINE,POLYLINE,HATCH"))))
                 ((progn
                    (prompt "\nSelect polylines/hatches: ")
                    (ssget '((0 . "LWPOLYLINE,POLYLINE,HATCH")))))))

  (if (not ss)
    (progn (prompt "\nNothing selected.") (princ))
    (progn
      (setq total 0.0 i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              i   (1+ i)
              total (+ total (ap-area ent))))

      (setq area (* (ap-convert total) ap-factor ap-factor))

      (prompt (strcat "\nCount: " (itoa (sslength ss))
                      " | Area: " (rtos area 2 2) (ap-suffix)))

      ;; label
      (initget "D W C B")
      (setq pre (getkword "\nLabel [Door/Window/Custom/Blank] <B>: "))
      (cond
        ((= pre "D") (setq num (getstring "\nDoor #: ") label (strcat "D" num)))
        ((= pre "W") (setq num (getstring "\nWindow #: ") label (strcat "W" num)))
        ((= pre "C") (setq label (getstring T "\nCustom label: ")))
        (T           (setq label "")))

      ;; insertion point
      (setq pt (getpoint "\nInsertion point (bottom-right): "))
      (if pt
        (progn
          (setq hgt (getvar "TEXTSIZE")
                txt (if (= label "")
                      (strcat (rtos area 2 2) (ap-suffix))
                      (strcat label "\\P" (rtos area 2 2) (ap-suffix))))
          (command "_.MTEXT" pt "_J" "BR" "_H" hgt "_W" 0 txt "")))
      (princ))))

(princ "\nAKDPickArea loaded. AA=pick, AAS=settings.")
(princ)
