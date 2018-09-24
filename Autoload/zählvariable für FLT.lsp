; Zählfunktion Tb
;


(defun c:zähler ()

(setq Nr 1)
(if Nr (1+ Nr ) 1)

  (setq Wort (getstring "\nWort: "))
  
	;(alert (strcat "Zähler steht auf " (rtos Zaehler 2 0)))

  (princ (strcat "zähler steht auf " (rtos Nr 2 0)))

  (princ (strcat "\nBauteil:" Wort))
(princ)
);_defun
