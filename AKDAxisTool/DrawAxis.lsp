;; Draw Axis (AX) - draws lines on AXIS layer with red color, PHANTOM2 linetype, LTScale 500
(defun c:AX ( / oldlayer oldcecolor oldcltype oldcltscale)
  (setq oldlayer    (getvar "CLAYER")
        oldcecolor  (getvar "CECOLOR")
        oldcltype   (getvar "CELTYPE")
        oldcltscale (getvar "CELTSCALE"))

  ;; Load PHANTOM2 linetype if not already loaded
  (if (not (tblsearch "LTYPE" "PHANTOM2"))
    (command "_.-linetype" "_Load" "PHANTOM2" "acad.lin" "")
  )

  ;; Create/set AXIS layer with red color and PHANTOM2 linetype
  (command "_.-layer" "_Make" "AXIS" "_Color" "1" "AXIS" "_Ltype" "PHANTOM2" "AXIS" "_Set" "AXIS" "")

  (setvar "CECOLOR" "BYLAYER")
  (setvar "CELTYPE" "BYLAYER")
  (setvar "CELTSCALE" 500.0)

  (command "_.line")
  (while (> (getvar "CMDACTIVE") 0)
    (command pause)
  )

  (setvar "CLAYER"    oldlayer)
  (setvar "CECOLOR"   oldcecolor)
  (setvar "CELTYPE"   oldcltype)
  (setvar "CELTSCALE" oldcltscale)
  (princ)
)

(princ "\nDraw Axis loaded. Type AX to start.")
(princ)
