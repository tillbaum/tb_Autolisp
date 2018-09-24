;variablen nummerierung aus Kochbuch 

(defun zähler ()
 (setq j 1)
  (while (< j 10) 
  
  (set (read(strcat "Zahl" (itoa j))) j)
  
  (setq j (1+ j)) 
  )
)



