;Zahlenexport aus string mit atof,tests, läuft ohne Fehler!, by TB, 20.2.16
;aufruf: (zex string) 
;
;

(setq t1 "abc123.45 cd12.34") 
(setq t2 "  dskföasdfa3 asdf af aefa afawe fa 0 255 3") 
(setq t3 " test") 
(setq t4 "                       I: 212.57953314 entlang [0.06500000 0.54041023]")
(setq t5 "Begrenzungsrahmen:         X: -29.01359092  --  -20.97948844" )


(defun zex (String)
  (if (wcmatch String "*#.##*" )(zahlenexp String)
  ;else if wcmatch 
  (princ "keine Zahl im String") 
));end if, end defun 

(defun zahlenexp ( String / ergebnis lenergebnis )

 (setq strlength (strlen String))
 (setq i 1)
 (setq zahlenliste nil)
   
(while (< i strlength) 
  
  (setq ergebnis (atof String)) 
  (if (= ergebnis 0.0) 
   (progn 
    (setq String (substr String 2)) 
    ;(princ (strcat "\n" String)) 
   )	;close progn 

    ;else if Ergebnis \=0 
    (progn 
    ;(princ (strcat "\nrest=" String)) 
    (setq String (vl-string-left-trim " " String))  
    ;(princ (strcat "\nrest_ohne_leerzeichen=" String)) 
    
    (princ (strcat "\n" "ergebnis=" (rtos ergebnis 2 8)))
    (setq lenErgebnis (+ 1 (strlen (rtos Ergebnis 2 8)))) 
    (princ (strcat "\n" "lenErgebnis +2= " (itoa lenErgebnis)))
    (setq String (substr String lenErgebnis)) 
    (setq zahlenliste (append zahlenliste (setq zahlenliste (list Ergebnis))))
    
    (setq j (1+ j)) 
    )	;close progn 
  )	;close if 
 
 (setq i (1+ i)) 
);end while < string strlength

 (princ zahlenliste) 

(princ) 
);end defun


;|«Visual LISP© Format Options» 
(72 2 40 1 nil " " 60 9 0 0 0 T T nil T) 
;*** KEINEN Text unterhalb des Kommentars hinzufügen! ***|; 
