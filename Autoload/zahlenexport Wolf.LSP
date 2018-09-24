; ZahlausString von Wolfgang Sünder korriegiert von CAD Admin aus CAD Forum 					
; Aufruf: (ZexWS string T/nil T/nil);  1. T/nil für "." -PunktTrennung, T kommatrennung nil Pkttrennung
;            2. T/nil für "-" Zeichen Trennung 	                       
;														
;														
;														

(defun c:test1 ( / )
  (zahlexp "sdfasdfsd12,123.44sd0.000f(-3444.44)dafsdf-13.13safs" 'T)
  )
;;;Gültige Zahlen: mit Komma getrennt und mit negativem Vorzeichen
;;;=> (12.0 12.0 -13.0 13.0)

(defun c:test2( / )
  (setq t2 (zahlexp "           J: 686719.68681845 entlang0.000 [-0.52267416 0.85253253]"  nil)
  ))

;;;Gültige Zahlen: mit Punkt getrennt und ohne negativem Vorzeichen
;;;=>(12.12 13.13)

(defun c:test3( / )
  (zahlexp "sdfasdfsd12,12dafsdf-13,13safs" 'T)
  )
;;;Gültige Zahlen: mit Komma getrennt und mit negativem Vorzeichen
;;;=>(12.12 -13.13)

(defun c:test4( / )
  (zahlexp "sdfasdfsd12,12dafsdf-13,13safs" nil)
  )
;;;Gültige Zahlen: mit Punkt getrennt und mit negativem Vorzeichen
;;;=>(12.0 12.0 -13.0 13.0)


(defun zahlexp (string NegativesVorzeichenFlag /
                        CHARACTERCODE GUELTIGFLAG listezahlenCC
                        listestringCC RESULTLIST RESULTSUBLIST X)

  ; Zahlen in character Code; String in CC
  (setq listezahlenCC
        (apply 'append
                (list(vl-string->list "1234567890.-")
                      ;(if NegativesVorzeichenFlag
                      ;(vl-string->list "-")
                      ;)
                    ) ;end list
                ) ;end apply
        listestringCC (vl-string->list string)
   ) ;end setq 

  
  (mapcar
    '(lambda(CharacterCode)
            (if (member CharacterCode listezahlenCC)
              (if GueltigFlag
                (setq ResultSubList (cons CharacterCode ResultSubList))
                (setq GueltigFlag 'T
                      ResultSubList (cons CharacterCode ResultSubList))
                )
              (if GueltigFlag
                (setq ResultList (cons (reverse ResultSubList)ResultList)
                      ResultSubList nil
                      GueltigFlag nil)))

	     )
    listestringCC) ;end mapcar

  
  (if ResultSubList
    (setq ResultList (cons (reverse ResultSubList)ResultList))
  );end if

  (princ (strcat "\n" string "\n"))
  (mapcar

    '(lambda(X)
              (atof (vl-list->string X))
              )
      (reverse ResultList))
  )