; ZahlausString von Wolfgang Sünder korriegiert von CAD Admin aus CAD Forum 					
; Aufruf: (ZexWS string T/nil T/nil);  1. T/nil für "." -PunktTrennung, T kommatrennung nil Pkttrennung		
;									   2. T/nil für "-" Zeichen Trennung 	
;														
;														
;														

(defun ZahlexWS (string NegativesVorzeichenFlag /
                        CHARACTERCODE GUELTIGFLAG ListeZahlen
                        LISTESTRING RESULTLIST RESULTSUBLIST X)
 
  (setq listeZahlen 
        (append 
              (vl-string->list "1234567890.")
	      (if NegativesVorzeichenFlag (vl-string->list "-") 
                    )
        )
        listeString (vl-string->list string))

  (mapcar
    '(lambda (CharacterCode)
            (if (member CharacterCode ListeZahlen)
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
    (setq ResultList
	   (cons (reverse ResultSubList) ResultList))
  );end if

  (princ (strcat "\n" string "\n"))
  (mapcar
    '(lambda (X)
              (atof(vl-list->string X))
              );close lambda
      (reverse ResultList)
  );close mapcar
  
  );end defun
