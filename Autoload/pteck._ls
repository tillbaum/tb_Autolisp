

(defun test ()

  (setq Bauteilnr (strcase (getstring "\nBauteil-Nr:"))) 
  (setq Bauteilart (strcase (getstring "\nBauteil-Art: RE/L/U/K:"))) 
  (setq ptBauteil (getpoint "\nBT-Text Insertion Point")) 
  
  (if (= Bauteilnr = L) (setq pt_eck rtos((getpoint "Eckpunkt L-Profil f. SchubmPkt"))) 2 4 )
  

  (command "text" ptBauteil "0" (strcat Bauteilnr " " Bauteilart pt_eck))

  )