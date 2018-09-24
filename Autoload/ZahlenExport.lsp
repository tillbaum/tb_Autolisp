;;Zahlenexport Aufruf (zex STRING)  mit atof, TB 20.2.16
;; 


(defun zex (String)
  (if (wcmatch String "*##.#*" )(zexp2 String)
  ;else if wcmatch
  (princ "keine Zahl im String") 
));end if, end defun 

(defun zexp2 ( String / ergebnis i j )

 (setq strlength (strlen String)) 
 (setq i 1)
 (setq j 1)
  
(while (< i strlength)
  (setq ergebnis (atof String)) 
  
  (if (= ergebnis 0.0) 
   (progn 
    (setq String (substr String 2)) 
   )	;close progn
    
;else if Ergebnis \=0 
    (progn 
     (setq String (vl-string-left-trim " " String)) 
     (set (read(strcat "Zahl" (itoa j))) Ergebnis) 
     (setq lenErgebnis (1+ (strlen (rtos ergebnis 2 2)))) 
     (setq String (substr String lenergebnis)) 
    
     (setq j (1+ j)) 
    )	;close progn 
  )	;close if  
 
 (setq i (1+ i)) 
 (setq strlength (strlen String))  
);end while < string strlength
(princ (strcat "\nexportierte Zahlen:"  (itoa  (1- j)))) 
(print) 
);end defun

  
;|«Visual LISP© Format Options» 
(72 2 40 1 nil " " 60 9 0 0 0 T T nil T) 
;*** KEINEN Text unterhalb des Kommentars hinzufügen! ***|; 
