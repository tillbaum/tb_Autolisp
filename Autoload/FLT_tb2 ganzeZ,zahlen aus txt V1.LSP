; FLT Flächenträgkeitsmom./Schwpkt, aus masseig-Befehl
; Aufruf: flt  ; Erweiterung d. fun Schwpkt, c. by TB                          
; 
;- legt nummerierte VAR für Textzeilen aus mprdat.txt (Zeile1- ZEile x) an 
;- Zahlenexport der VAR ZeileX mit fun (ZahlEX String), keine "0"en 
;  VAR Ix, Iy, Richtung d. Hauptachsen x_I1, y_I1, area, Richtungswinkel zw Hauptachsen 
;  und Ursprungsachsen: Alpha 
;- Berechnung von alpha und Ixy aus Ix, Iy
;- Bauteilnr, BauteilART Beschriftung, Ablegung in Pkt 
;- L Profil SchubMpkt Ermittlung 
;- Schwerpkt wird mit acad-Point markiert.
;- Hauptachsen werden mit linie gezeichnet. 
;- Ablegung der VAR Ix, Iy, x_s, y_s ...... in dtext 
;
;

(defun tan (winkel) 
  (/ (sin winkel) (cos winkel))
  )

(defun REGKEY_R	(/ KEY) 
  (setq	key (vl-registry-read 
	      (strcat 
		"HKEY_CURRENT_USER\\" 
		(vlax-product-key) 
		"\\Profiles\\" 
		(getvar "CProfile") 
		"\\" 
		"General Configuration" 
		) ;_ end of strcat  
	      "TempDirectory" 
	      ) ;_ end of vl-registry-read 
	) ;_ end of setq 
  (eval key) 
  ) ;_ end of defun 


(defun savesysvar ()
  (setq cmdakt (getvar "cmdecho"))
  (setq osmakt (getvar "osmode"))
  (setq filedakt (getvar "filedia"))
  (setq layakt (getvar "clayer"))
  (setq alterr *error*)
  (setq blipakt (getvar "blipmode"))
  (setq delakt (getvar "delobj"))
  (setq lupakt (getvar "luprec"))
  (setq pdakt (getvar "pdmode"))
  )	;ende defun  

(defun restoresysvar ()
  (setvar "cmdecho" cmdakt)
  (setvar "osmode" osmakt)
  (setvar "filedia" filedakt)
  (setvar "clayer" layakt)
  (setq *error* alterr)
  (setvar "blipmode" blipakt)
  (setvar "delobj" delakt)
  (setvar "luprec" lupakt)
  (setvar "pdmode" pdakt)
  )	;ende defun 


(defun neuerr (s)	; Abfangfunktion  
  (if (/= s "Funktion abgebrochen")
    (princ (strcat "\nFehler: " s))
    )
  (if mprdat
    (close mprdat)
    )
  (command "_undo" "_BACK")
  (restoresysvar)
  (print)
  )		;ende defun

;|(DEFUN koord2 (arg2 item2 x / posi)	;Definition koord (item arg) globale Variablen, ( len pos) lokale Variablen 
  (if (setq posi (vl-string-search arg2 item2))			
      (atof (substr item2 (+ 1 x posi)))	;atof konvertiert String in Realzahl 
    );close if 	; substr "Wort" 2 
 ) ;ende defun   
|;

;Zahlenexportfunktion mit atof , "0" werden nicht exportiert! 
(defun Zahlex (String)	
  (if (wcmatch String "*#.##*" ) (zexp2 String)
  ;else if wcmatch 
  (princ "\nkeine Zahl im String") 
));end if, end defun 

(defun zexp2 ( String / ergebnis i lenergebnis )  
 (setq strlength (strlen String)) 
 (setq i 1) 
 (setq zahlenliste nil) 
   
(while (< i strlength) 
  (setq ergebnis (atof String)) 
  
  (if (= ergebnis 0.0) 
   (progn 
    (setq String (substr String 2))  
   )	;close progn 

    ;else if Ergebnis \=0 
   (progn 
    (setq String (vl-string-left-trim " " String))  
    (setq lenErgebnis (+ 2 (strlen (rtos Ergebnis 2 9)))) 
    (setq String (substr String lenErgebnis)) 

    (setq zahlenliste (append zahlenliste (setq zahlenliste (list Ergebnis)))) 
   )	;close progn 
  )	;close if  
 (setq i (1+ i))  
);end while < string strlength
(princ "\n")
(princ zahlenliste)
(princ) 
);end defun


(defun c:flt ( / mprdat  p1 datei alterr cmdakt osmakt filedakt	layakt as el kont e1
	      ptBT BTnr BTart kword xeck yeck xyeck pteck
	      x_s y_s area I1 I2 x_i1 y_i1 x_i2 y_i2 ptI1 ptI2 alpha I_xy I_xy2 Schwpt Ihaupt schwpkoo)

  (vl-load-com)
  (command "_undo" "_MARK")
  (savesysvar)
  (setvar "osmode" 0)
  (setvar "cmdecho" 0)
  (setvar "luprec" 8)
  (setvar "pdmode" 3)
  (setq *error* neuerr)
  (setq el (entlast))
  (setq as (ssadd))

  (setq p1 (getpoint "\nin die zu berechnende Fläche klicken"))
  (command "-umgrenzung" "o" "i" "j" "o" "r" "x" p1 "")
  (setq e1 (entnext el))
  (setq datei (strcat (REGKEY_R) "test.mpr"))
  (while e1
    (ssadd e1 as)
    (setq e1 (entnext e1)) 
    )
  (setq kont (ssname as 0))
  (ssdel kont as)
  (command "_massprop" "_l" "" "_Y" datei)

  (command "_.erase" "_l" "")	  ;_l steht für last

  (setq mprdat (open datei "r"))

;writing IxoSA,IyoSA... in dtext 

;Durchummerieren der Zeilen der mprdat datei, Variablen Vergebung für Zeilen:Z1-Z25
  (setq i 1)
  (while (< i 20)
    (set (read (strcat "Zeile" (itoa i))) (read-line mprdat))
    (setq i (1+ i))
   );close while
  
  (close mprdat) 
  
  (zahlex Zeile4) (setq area (car zahlenliste))
  ;UMfang, nicht benötigt (zex Zeile5) (setq umf (car zahlenliste))
  
  (zahlex Zeile8)(setq x_s (car zahlenliste)) 
  (zahlex Zeile9)(setq y_s (car zahlenliste)) 

  (zahlex Zeile16) (setq I1 (car zahlenliste))
  (setq x_I1 (cadr zahlenliste)) 
  (if (= x_I1 1)(setq y_I1 0)(setq y_I1 (caddr zahlenliste))) ;defun zex erkennt Null nicht, bei Rechtecken 
  (setq ptI1 (list (+ x_s x_I1) (+ y_s y_I1))) 

  (zahlex Zeile17) (setq I2 (car zahlenliste))
  (setq x_I2 (cadr zahlenliste))
  (if (= x_I2 1)(setq y_I2 1 x_I2 0)(setq y_I2 (caddr zahlenliste)))
  (setq ptI2 (list (+ x_s x_I2) (+ y_s y_I2))) 
				
  (setq Schwpt (list x_s y_s))		;list durch Leerzeichen getrennt

  (vl-file-delete datei)

  (command "_point" Schwpt)
  ; Hauptachsen zeichnen!!!
  (command "_point" ptI1)
  (command "_line" "_non" ptI1 "_non" Schwpt ptI2 "") 
  ;Berechnung I_xy  
  (setq alpha (atan (/ y_I1 x_I1))) 
  (setq I_xy (* (* 0.5 (/ (sin (* 2 alpha)) (cos (* 2 alpha)))) (- I2 I1 )))
  (setq I_xy2 (- (* 0.5 I2 (tan (* 2 alpha))) (* 0.5 I1 (tan (* 2 alpha)))))

  (princ (strcat "\naus Berechnung; " "\nalpha=arctan(y_I1/y_I1)=" (rtos alpha 2 6)))
  (princ "\nI_xy=")(princ I_xy)
  (princ "\nI_xy2=")(princ I_xy2) 

; counter for FLT calls
 ;(setq fcall (if fcall (1+ fcall) 1)) 
 ;(setq fcallstring (rtos fcall 2 0)) 
 ;(princ (strcat "Fcall steht auf " fcallstring)) 

  
; Beschriftung der BauteilNR und ART, als text absetzen 

(setq BTnr (strcase (getstring "\nBauteil-Nr:" ))) 
(setq BTart (strcase (getstring "\nBauteil-Art: RE/L/U/K:"))) 
(setq ptBT (getpoint "\nBT-Text Insertion Point")) 

(command "text" ptBT "0" (strcat BTnr "; " BTart "; "))  

  (restoresysvar) 

;L-Profil getpoint SchubmPunkt=ECKPUNKT  
  (if (= BTart  "L")
    (progn
      ( setq pteck (getpoint "SchubMpkt L-Profil (in Ecke)"))
      (command "_point" pteck)
    (setq xeck (rtos (car pteck)2 6))
    (setq yeck (rtos (cadr pteck)2 6))
    (setq xyeck (strcat "SMpkt_L; " xeck "; " yeck))
    ) ;end progn
;elseif
    (setq xyeck " ");kein Zeichen 
  ) ;end if 
      
  
;Abfrage NEUer Absetzpkt für IxIy_text, Überprüfung ob Absetzpunktes = nil 
  (princ "\nVAR dtext_pt= ")(princ dtext_pt) 

  (if (/= dtext_pt nil) 		;() = nil
  (if (= (setq kword (getint "\n Neuer Absetzpunkt / DELETE dtext_pt <1=Yes/0=No> "))1) 
    (setq dtext_pt nil) 
   ) ;end if 
  ) ;end if 
  (princ "\nVAR dtext_pt= ")(princ dtext_pt) 

;getting point for dtext, dtext_pt, placing it at the koordinates from the former 
  (if (= dtext_pt nil) 
    (setq dtext_pt (getpoint "\nI_x,I_y dText Insertion Point")) 
  ; else if 
    (setq dtext_pt (polar dtext_pt (* pi 1.5) (* (getvar "textsize") 0.5)); close the polar function from the insertion point 
	  ); close the setq 
  ); end if 

; Umwandeln von I_y, I_x in strings 

(setq IHaupt (strcat (rtos I1 2 8) "; " (rtos I2 2 8) "; " (rtos I_xy 2 8))) 
(setq Schwpkoo (strcat (rtos x_s 2 6) "; " (rtos y_s 2 6))) 
  
(command "text" dtext_pt "0" 
    (strcat BTnr "; " BTart "; Ix/Iy/Ixy/x_s/y_s/Area/alpha_rad; "
	    IHaupt "; " Schwpkoo "; " (rtos area 2 8) "; " (rtos alpha 2 8) "; " xyeck)) 
 (princ)
  
 (restoresysvar)
  
  );end defun 

(princ)

(prompt 
  "\n < flt > eingeben zum Starten ,Schriftstil mit Texthöhe vorher einstellen !!!
 	\n\nFLT berechnet:
	\n- Flächenträgheitsmomente Ix,Iy, Ixy (ohne Steiner Anteil)
	\n- Schwerpunkte x_s, y_s beliebiger Flächen.
	\n \n Berechnung über masseig-Befehl. Export der Zeilen der masseig-file in dtext, dtext in ZEichnung
	ablegen. text mit textout.lsp exportieren, CSV.lisp als CSV-Datei abspeichern
	\n\nFür Aussteifungsberechnung mit iMath und CALC,
	\nmit BKS KO-System verschieben, "
  ) 


 ;|«Visual LISP© Format Options» (72 2 40 1 nil " " 60 9 0 0 0 T T nil T) 
 ***Don't add text below the comment!***|;