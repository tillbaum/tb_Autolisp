; Lisp-Programm zum Maﬂ-Absetzen (Distance)
; 1. Absetzen im Format [mm]:XXX   -- dist-mm
; 2. Absetzen im Format [cm]:XXX.X -- dist-cm
; 3. Absetzen im Format [m] :X.XXX -- dist-m
; erstellt  14.06.1997 Lieske
; erweitert 28.08.1998 Lieske
; erweitert 21.11.2000 Lieske
;
(defun c:dist-mm
  ( / ech distm distmm el ent)
  (setq ech (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n ")
  (princ "\n Messen einer Distanz mit Picken zweier Punkte")
  (princ "\n Format: xxx [mm]")
  (princ "\n Wert durch Anpicken eines vorhandenen absetzen")
  (princ "\n von tgl am 21.11.2000")
  (princ "\n ")
  (while (setq distm  (getdist "\n   First point:"))
    (setq distmm (rtos (* distm 1000) 2 0))
    (princ "\n     Die Entfernung betr‰gt [mm]: ")
    (princ distmm)
    (setq el (entsel "\n     vorhandenen Text picken...")
          ent (entget (car el))
          ent (subst (cons 1 distmm) (assoc 1 ent) ent)
    ) ;of setq
    (entmod ent)
    (princ "\n ")
  ) ;of while
  (princ " \n  Ende.\n")
  (setvar "CMDECHO" ech)
  (prin1)
) ;end.
(defun c:dist-cm
  ( / ech distm distcm el ent)
  (setq ech (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n ")
  (princ "\n Messen einer Distanz mit Picken zweier Punkte")
  (princ "\n Format: xxx.x [cm]")
  (princ "\n Wert durch Anpicken eines vorhandenen absetzen")
  (princ "\n von tgl am 14.06.1997")
  (princ "\n ")
  (while (setq distm  (getdist "\n   First point:"))
    (setq distcm (rtos (* distm 100) 2 1))
    (princ "\n     Die Entfernung betr‰gt [cm]: ")
    (princ distcm)
    (setq el (entsel "\n     vorhandenen Text picken...")
          ent (entget (car el))
          ent (subst (cons 1 distcm) (assoc 1 ent) ent)
    ) ;of setq
    (entmod ent)
    (princ "\n ")
  ) ;of while
  (princ " \n  Ende.\n")
  (setvar "CMDECHO" ech)
  (prin1)
) ;end.
(defun c:dist-m
  ( / ech distm dist_m el ent)
  (setq ech (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (princ "\n ")
  (princ "\n Messen einer Distanz mit Picken zweier Punkte")
  (princ "\n Format: x.xxx [m]")
  (princ "\n Wert durch Anpicken eines vorhandenen absetzen")
  (princ "\n von tgl am 28.08.1998")
  (princ "\n ")
  (while (setq distm  (getdist "\n   First point:"))
    (setq dist_m (rtos distm 2 3))
    (princ "\n     Die Entfernung betr‰gt [m]: ")
    (princ dist_m)
    (setq el (entsel "\n     vorhandenen Text picken...")
          ent (entget (car el))
          ent (subst (cons 1 dist_m) (assoc 1 ent) ent)
    ) ;of setq
    (entmod ent)
    (princ "\n ")
  ) ;of while
  (princ " \n  Ende.\n")
  (setvar "CMDECHO" ech)
  (prin1)
) ;end.
