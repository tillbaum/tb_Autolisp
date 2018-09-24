; ZahlausString von Wolfgang Sünder korriegiert von CAD Admin aus CAD Forum 					
; Aufruf: (ZexWS string T/nil T/nil);  1. T/nil für "." -PunktTrennung, T kommatrennung nil Pkttrennung		
;									   2. T/nil für "-" Zeichen Trennung 	
;														
;														
;														

(defun ZexWS (string KommaFlag NegativesVorzeichenFlag /
                        CHARACTERCODE GUELTIGFLAG Listezeichen
                        LISTESTRING RESULTLIST RESULTSUBLIST X)
 
  (setq listeZeichen 
        (apply 'append 
                (list(vl-string->list "1234567890"))
                    (if KommaFlag (vl-string->list ",")(vl-string->list ".")
		    )
                    (if NegativesVorzeichenFlag (vl-string->list "-") 
                    )
        )
        listeString (vl-string->list string))

  (mapcar
    '(lambda(CharacterCode)
            (if (member CharacterCode listeZeichen)
              (if GueltigFlag
                (setq ResultSubList (cons CharacterCode ResultSubList))
                (setq GueltigFlag 'T
                      ResultSubList (cons CharacterCode ResultSubList))
              )
              (if GueltigFlag
                (setq ResultList (cons (reverse ResultSubList)ResultList)
                      ResultSubList nil
                      GueltigFlag nil)
		)
	      )
       )
    listeString)
  (if ResultSubList
    (setq ResultList (cons (reverse ResultSubList)ResultList)))

  (princ (strcat "\n" string "\n"))
  (mapcar '(lambda(X)
              (atof(vl-string-subst "." "," (vl-list->string X)))
              )
      (reverse ResultList))
  )
