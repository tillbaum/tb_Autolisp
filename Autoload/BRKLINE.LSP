;draws a break line
;
;Jeffery P Sanders
;
(defun C:BKL()
 (setvar "cmdecho" 0)
 (princ "\n \n ")
 (setq lt (getvar "ltscale")
       pt1(getpoint "\n Starting Point: ")
       pt2(getpoint pt1 "\n Ending Point: ")
       ang(angle pt1 pt2)
       mpt(polar pt1 ang (/ (distance pt1 pt2) 2.0))
 )
 (command "Pline" pt1 "WIDTH" "0" "0"
  (polar mpt (+ ang pi)(* lt 0.03125))
  (polar mpt (- ang (/ pi 2.0))(* lt 0.0625))
  (polar mpt (+ ang (/ pi 2.0))(* lt 0.0625))
  (polar mpt ang (* lt 0.03125))
  pt2 ""
 )
 (setvar "cmdecho" 1)
 (princ "\n \n ")(princ)
)
