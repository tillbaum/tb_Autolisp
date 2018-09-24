; AREATill.lps - Auftruf:adt,ldt area dtext, länge dtext;  Längen/Flächen von Polilinien in DTEXT Absetzen
;
; Till Jan 2013
;!!! ACHTUNG!!! geht nur, wenn eine Schriftstil mit vorgewählter Texthöhe gewählt wurde!!!


;Länge in dtext
;
(defun c:ldt(/ laenge pt string ech)
  (setq ech (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  
  (princ "\n ")(princ "\n Längen PolyLinien-> DTEXT")
  (princ "\n verwendeter Schriftstil muss vorgewählte Texthöhe haben")
 
 (repeat 20
    (princ "\n ")(princ "\n   Pline picken:") ; Drucke ,neue Zeile 
   
    (command "_.area" "_e" pause)
    (setq laenge(getvar "perimeter")); SysVar für Pline-L„nge
  
   (setq pt(getpoint "\n Insertion Point"))

   (setq string(strcat (rtos laenge 2 3) " ,=L")) 	;2 Strings zu einer Stringkette, rtos real to string
													;,=L Kommagetrennt wegen export in csv-Datei
   (command "text" pt "" string)

   );of repeat
  
  (setvar "CMDECHO" ech)
  (princ)
) ;end.

;
; Fläche Polylinie
;

(defun c:adt(/ area pt ech string)

  ;(setq ech (getvar "CMDECHO")) ;Wozu????
  ;(setvar "CMDECHO" 0)
  
  (princ "\n ")(princ "\n Längen PolyLinien -> DTEXT")
   (princ "\n verwendeter Schriftstil muss vorgwählte Texthöhe haben")(princ "\n")
 
 (repeat 20
    (princ "\n ")(princ "\n Pline picken:")
   
    (command "_.area" "_e" pause)
    (setq area(getvar "area")); SysVar für Pline-flaeche
  
   (setq pt(getpoint "\n Insertion Point"))
   
   (setq string(strcat (rtos area 2 3) " ,=A")) ;2 Strings zu einer Stringkette

   (command "text" pt "" string)

   );of repeat
  
  ;(setvar "CMDECHO" ech)
  (princ)
) ;end.


(prompt "; Till Jan 2013 ACHTUNG!!! geht nur, wenn eine Schriftstil mit vorgewählter Texthöhe gewählt wurde!!! < SCHWPK > eingeben zum Starten")