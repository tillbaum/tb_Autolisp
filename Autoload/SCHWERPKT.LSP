; SCHwerpkt berechnen, Aufruff schwpk                                                   
;SCHwerpkt: 
;Berechnet den Schwerpkt einer Polylinie/REgion 
;und markiert ihn mit einem Punkt 
;
;Aufruf: Schwpk
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
  ) ;_ end of defun

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
 );ende defun 

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
 );ende defun 

(defun neuerr (s) 
  (if (/= s "Funktion abgebrochen") 
    (princ (strcat "\nFehler: " s)) 
    ) 
  (if mprdat(close mprdat)) 
  (command "_undo" "_BACK") 
  (restvar) 
  (princ) 
 );ende defun 

(DEFUN koord (item arg / len pos zeich str) 
  (if (setq posi(vl-string-position (ascii arg)item)) 
      (atof(substr item (+ 4 posi))) 
    ) 
 );ende defun 

(defun c:schwpk (/ mprdat p1 x y datei	p2 alterr cmdakt osmakt	filedakt layakt
		as el kont e1) 
  (vl-load-com)  
  (command "_undo" "_MARK") 
  (sichvar) 
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
  (command "_.erase" "_l" "")  
  (setq mprdat (open datei "r"))  
  (repeat 7 
    (read-line mprdat)  
    (princ)  
    ) 
  (setq x (koord (strcase (read-line mprdat))"X:")) 
  (setq y (koord (strcase (read-line mprdat))"Y:")) 
  (close mprdat) 
  (setq p2 (list x y)) 
  (vl-file-delete datei) 
  (command "_point" p2) 
  (restvar) 
  (princ) 
  ) 
(prompt "\n < SCHWPK > eingeben zum Starten") 


;|«Visual LISP© Format Options» (72 2 40 1 nil "end of " 60 9 0 0 0 T T nil T) 
 ***Don't add text below the comment!***|;  