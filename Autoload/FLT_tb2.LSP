;Aufruf: flt - FlächenträgkeitsMom. ermitteln u. in dtext absetzen (Vorlage SCHwerpkt) c. by TB  v2.1 13.01.2017                       
; 
; Berechnet die Flächenträgheitsmomente o. Steiner Anteil einer Polylinie/Region 
;und markiert ihn mit einem Punkt 
; 
; Aufruf: flt
;


(defun REGKEY_R (/ KEY) 
  (setq key (vl-registry-read
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
  )	;_ end of defun 

  ;;; Fkt sichvar
(defun sichvar () 
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

;;;Fkt restvar
(defun restvar ()
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

;;;fkt neuerr Abfangfunktion
(defun neuerr (s) 	 ; Abfangfunktion  
  (if (/= s "Funktion abgebrochen") 
    (princ (strcat "\nFehler: " s)) 
    )
  
  (if mprdat(close mprdat))  
  (command "_undo" "_BACK")  
  (restvar)  
  (print)  
 )	;ende defun

;;;FKT koord
(DEFUN koord (item arg / len pos zeich str)  		 ;Definition koord (item arg) globale Variablen, ( len pos) lokale Variablen 
  (if (setq posi					 ;var posi = position des Zeichens "X:" 
	     (vl-string-position (ascii arg) item) 	 ;ascii 
		)  				   	 ;Looks for a character with the specified ASCII code in a string, gibt Zeichenposition zurück
      (atof(substr item (+ 4 posi))) 			 ;atof konvertiert String in Realzahl 
    ) 	;close if 	; substr "Wort" 2 
   	;(print posi) 
  )	;ende defun 


(DEFUN koord2 (item2 arg2 / len pos zeich str)  	 ;Definition koord (item arg) globale Variablen, ( len pos) lokale Variablen 
  (if (setq posi					 ;var posi = position des Zeichens "X:" 
	     (vl-string-position (ascii arg2)item2)   	 ;ascii 
		)  				   	 ;Looks for a character with the specified ASCII code in a string, gibt Zeichenposition zurück
      (atof(substr item2 (+ 28 posi))) 			 ;atof konvertiert String in Realzahl 
    ) 	;close if 	; substr "Wort" 2 
   	;(print posi) 
  )	;ende defun   

 

;;;FKT flt
(defun c:flt ( / mprdat p1 xs ys datei p2 alterr cmdakt osmakt filedakt layakt 
		as el kont e1 BTart BTnr kword xeck yeck xyeck pteck ) 
  (vl-load-com) 
  (command "_undo" "_MARK")
  (sichvar)
  (setvar "osmode" 0) ;setzt objektfangpunkte außer Kraft s. Hilfe 0 alle deaktiviert
  (setvar "cmdecho" 0) 
  (setvar "luprec" 8) 
  (setvar "pdmode" 3)  ;Symbol Punkt, 3 = kreuz   (setvar "pdsize" ) steuert geom. Größe fest

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

  (repeat 3		;hiermit wird die Textzeile bestimmt !!
    (read-line mprdat) 
    (print) 
  )
  
  (setq Area (koord2 (strcase (read-line mprdat)) "Fläche:"))

  (repeat 3 		;hiermit wird die Textzeile bestimmt !!
    (read-line mprdat)
    (print) 
  )			
  
  (setq xs (koord (strcase (read-line mprdat)) "X:"))
  (setq ys (koord (strcase (read-line mprdat)) "Y:"))
  (setq Ixx (koord (strcase (read-line mprdat)) "X:"))
  (setq Iyy (koord (strcase (read-line mprdat)) "Y:"))
  (setq Ixxyy (koord (strcase (read-line mprdat)) "XY:")) 

  
  (close mprdat) 
  (setq p2 (list xs ys)) ;list durch Leerzeichen getrennt 
  (vl-file-delete datei) 
  (command "_point" p2)		
  (restvar) 	 
  (princ) 

; Calculation of moment of inertia without Steiner part

  (setq IxoSA (- Ixx (* Area (* ys ys)))) 
  (setq IyoSA (- Iyy (* Area (* xs xs)))) 
  (setq IxyoSA (+  Ixxyy (* Area xs ys)))

  
  ; counter for FLT calls 
  ; (setq fcall (if fcall (1+ fcall) 1))
  ;(setq fcallstring (rtos fcall 2 0)) 
  ;(princ (strcat "Fcall steht auf " fcallstring))

  ; Beschriftung der Bauteilnr und ART, als text absetzen
  
  (setq BTnr (strcase (getstring "\nBauteil-Nr:"))) 
  (setq BTart (strcase (getstring "\nBauteil-Art: RE/L/U/K:"))) 
  (setq ptBT (getpoint "\nBT-Text Insertion Point")) 
  
  (command "text" "s" "standard" ptBT "2.5" "0" (strcat BTnr " " BTart)) 

;L-Profil Schubmittelpkt, = ECKPUNKT
  (if (= BTart  "L")
    (progn
      ( setq pteck (getpoint "Eckpunkt L-Profil f. SchubmPkt")) 
      (command "_point" pteck) 
      
      (setq xeck (rtos (car pteck)2 6)) 
      (setq yeck (rtos (cadr pteck)2 6)) 
      (setq xyeck (strcat " Eckpt " xeck " " yeck)) 
    ) ;end progn
      (setq xyeck " ") 
  ) ;endif
      
;NEUer Absetzpkt für IxIy_text, Überprüfung des Absetzpunktes
  
  (princ "\nVAR inspt_text=  " ) (princ inspt_text) 

  (if (/= inspt_text ()) ; if var inspt ist /= ungleich nil, dann... 
	  	   
  	   (if (=  (setq kword (getint "\n Neuer Absetzpunkt / DELETE inspt_text <1=Yes/0=No> "))  1)
  	     (setq inspt_text nil))
    ) end if
  
  (princ) 	 
  (princ "\nVAR inspt_text=  " ) (princ inspt_text) 
  (princ) 

  
  ; getting point inspt_text 
   (if (= inspt_text ()) ;if var inspt ist gleich nil, dann
      (setq inspt_text (getpoint "\nI_y,I_z dText Insertion Point"))
         	; 3 Ausdruck in if else Beziehung
          (setq inspt_text 
          ;start the polar function i.e. [polar pt angle distance]
		  (polar inspt_text (* pi 1.5)     ;set the angle to go down i.e. (270 deg OR pi + pi/2 radians)
          (* (getvar "textsize") 0.5)  ;distance down to the next line is (textsize x .5) -> 0.5 durch ausprobieren ermittelt!!
	  ) ; close the polar function  from the insertion point
      ) ; close the setq 
   ) end if ;Till

  

; Umwandeln von Iy, I_x in strings
  (setq xsys (strcat (rtos xs 2 6) "; " (rtos ys 2 6) " ;" (rtos Area 2 4))) 
  (setq IxIyIxy (strcat  (rtos Ixx 2 6) "; " (rtos IxoSA 2 6) "; " (rtos IyoSA 2 6) "; " (rtos IxyoSA 2 6) ))   ;rtos Zahl 2 (für Dezimale Darstellung) 3 (Anzahl d. Stellen) Strings zu einer Stringkette

  
  (command "text" "S" "standard" inspt_text "2.5" "0" (strcat BTnr "; " BTart "; Ix/IxoSA/IyoSA/IxyoSA/xs/ys/Area; " IxIyIxy "; " xsys xyeck ))            

  (princ (strcat "\narea= " (rtos Area 2 4)))

  ;(princ (strcat "\nFcall steht auf " fcallstring))

  
  
 ; anderes inspt-text Prog aus Kochbuch (setq inspt_text '( ( / tmp) (initget 0 "Yes No") (= (getkword "Delete inspt_text <Yes>/No: ")"No")
 
  
  (print)
    			; strcat verbindet 2 oder mehr Zeichenketten 
  ) ;end defun



;set fltcall to manual value
(defun c:setfcall () (setq fcall (getint "\nfcall: "))
  (princ)
) 

(prompt "\n < flt > eingeben zum Starten 
        \n
		\nSchriftstil: Standard, Texthöhe 2.5 !!!
	\n
	\nFLT berechnet:
	\n- Flächenträgheitsmomente Ix,Iy, Ixy (ohne Steiner Anteil)
	\n- Schwerpunkte x_s, y_s beliebiger Flächen.
	\n \n Berechnung erfolgt über masseig-Befehl. Export der Zeilen der masseig-file in dtext,
    \n	dtext in ZEichnung ablegen. Text mit textout.lsp bzw CSVout.lsp exportieren
	\n\nFür Aussteifungsberechnung mit iMath und CALC,
	\nmit BKS KO-System in geeigneten Pkt des Systems verschieben, CSV.lisp als CSV-Datei abspeichern") 



  

;|«Visual LISP© Format Options» (72 2 40 1 nil "end of " 60 9 0 0 0 T T nil T) 
 ***Don't add text below the comment!***|;  