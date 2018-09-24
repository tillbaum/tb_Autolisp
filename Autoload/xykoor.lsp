




(defun c:ptkoo (/  p x y ptcoord textloc)
(while ;start while
(setq p (getpoint "
Pick Point to Label: "))
(setq textloc (getpoint p "
Pick Text Location"))
(setq x (rtos (car p)2 4))
(setq y (rtos (cadr p)2 4))
(setq ptcoord (strcat "x=" x " " "y=" y))
(command "_leader" p textloc "" ptcoord "")
(princ)
) ;end while
)