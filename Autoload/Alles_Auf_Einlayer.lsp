;Platziert ausgewählte Objekte alle auf ein Layer
;
;
(defun C:einlayer (/        SS1      SS1Len   i        NumChg   EName
                  Elist    EType    ZNACZNIK MD_ERR   M:FEHLER m:farbe
                  ) ;_ end of /
 ;_ end of /
  (setq MD_ERR  *ERROR*
        *ERROR* M:FEHLER
        ) ;_ end of setq
  ;;Marke für Zurück nach Abbruch setzen
  (command "_.UNDO" "_MARK")
  (setvar "cmdecho" 0)
  (setvar "tilemode" 1)
  (command "_.-Layer" "_unlock" "*" "")
  (prompt
    "\nElemente wählen zum ändern, oder Return für alle... "
    ) ;_ end of prompt
  (setq SS1 (ssget))
  (if (null SS1)                        ;if enter, ZOOM All and choose everything
    (setq
      SS1 (ssget "X" '((-4 . "<NOT") (0 . "VIEWPORT") (-4 . "NOT>")))
      ) ;_ end of setq
    )                                   ;if
  (if ss1
    ;(setq m:farbe (acad_colordlg 256))
    (progn
      (prompt "\nObjekt für Layer picken, oder Return für Layereingabe:")
      (setq M:Lay (entsel))
      (if (null M:LAY)
        (progn
        (setq m:farbe (getstring "\nLayername: "))
        (while (= (tblsearch "LAYER" m:farbe)NIL)
          (setq m:farbe(getstring "\nDer Layername Existiert nicht. Bitte neue Eingabe: "))
          )
        )
      (setq m:farbe (cdr (assoc 8 (entget (car M:LAY)))))
        )
      )
    ) ;_ end of if
                                        ;*initialize variables
  (setq SS1Len (sslength SS1)           ;length of selection set
        i      0                        ;loop counter
        NumChg 0                        ;number changed counter
        )                               ;setq
                                        ;*do the work
  (prompt "\n  Arbeite.")
  (terpri)
  (while (< i SS1Len)                   ;while more members in the SS
    (cond
      (ZNACZNIK
       (setq ZNACZNIK NIL)
       (princ (strcat (itoa NumChg) "\r \\ "))
       )
      (t
       (setq ZNACZNIK
              (princ (strcat (itoa NumChg) "\r / "))
             ) ;_ end of setq
       )
      ) ;_ end of cond
    (setq EName (ssname SS1 i)          ;entity name
          EList (entget EName)          ;entity data list
          EType (cdr (assoc 0 EList))   ;entity type
          )                             ;setq
                                        ;*change group 10 Z coordinate to 0 for listed entity types
    (cond
      ((= EType "INSERT")
       (setq EList (zeroz 8 EList M:farbe);(zeroz 62 EList M:farbe)
                                        ;change entities in list above
             )                          ;setq
       (if (null (tblsearch "BLOCK" (cdr (assoc 2 EList))))
         (progn
           (setq EARX_BTAA (tblnext "BLOCK" T))
           (while
             (/= EARX_BTAA nil)
              (if
                (wcmatch (cdr (assoc 2 EARX_BTAA)) (cdr (assoc 2 EList)))
                 (progn
                   (setq EARX_BTAB EARX_BTAA)
                   (setq EARX_BTAA nil)
                   ) ;_ end of progn
                 (setq EARX_BTAA (tblnext "BLOCK"))
                 ) ;_ end of if
              ) ;_ end of while
           ) ;_ end of progn
         (setq EARX_BTAB (tblsearch "BLOCK" (cdr (assoc 2 EList))))
         ) ;_ end of if
       (setq EARX_BLK (cdr (assoc -2 EARX_BTAB)))
       (while EARX_BLK
         (cond
           (ZNACZNIK (setq ZNACZNIK NIL) (princ "\r \\ "))
           (t (setq ZNACZNIK (princ "\r / ")))
           ) ;_ end of cond
         (setq ELIST (entget EARX_BLK)
               EType (cdr (assoc 0 EList))
               ) ;_ end of setq
         (setq EList (zeroz 8 EList M:farbe));(zeroz 62 EList M:farbe))
         (if ELIST
           (entmod ELIST)
           ) ;_ end of if
         (setq EARX_BLK (entnext EARX_BLK))
         )                              ;ende while
       )                                ;ende insert
      (T
       (setq EList (zeroz 8 EList M:farbe));(zeroz 62 EList M:farbe)) ;(md_liste M:farbe));
       )
      )                                 ;ende cond
    (setq i (1+ i))                     ;next entity
    )                                   ;while
  (princ)
  )                                     ;defun

(prompt
  "\nProgramm zum Verändern aller ausgewählten Objekte auf einen auszuwählenden Layer.\nAufruf des Programms mit: \"einlayer\" !"
  ) ;_ end of prompt
(princ) ;_ Fängt das nil vom prompt ab

(defun M:FEHLER (MSG)
  (command "_.UNDO" "_BACK")
  (setq *ERROR* MD_ERR)
  (princ
    (strcat "Fehler! AutoCAD meldet: \"" MSG "\" als Ursache.")
    ) ;_ end of princ
  (princ)
  ) ;_ end of defun

(defun zeroz (key ZEList col / OPList NPList)
  (if (= (cdr(assoc 0 ZEList))"ATTRIB")
    (setq test nil)
    )
  (setq OPList (assoc key ZEList))
  (if (/= OPList nil)
;;;    (setq ff     (cdr (member (assoc 8 ZEList) zelist))
;;;          f      (reverse (member (assoc 8 ZEList) (reverse zelist)))
;;;          ZEList (append f (cons (cons key col) ff))
    (setq NPList (cons key col)
          ZEList (subst NPList OPList ZEList)
          ) ;_ end of setq
    (progn
      (setq md_layer_0_e
             (entget (tblobjname "layer" (cdr (assoc 8 ZEList)))
                     ) ;_ end of entget
            ) ;_ end of setq
      (setq md_layer_0_e
             (subst (cons 62 col)
                    (assoc 62 md_layer_0_e)
                    md_layer_0_e
                    ) ;_ end of subst
            ) ;_ end of setq
      (entmod md_layer_0_e)
      ) ;_ end of progn
    ) ;_ end of if
  (entmod ZEList)
   ZELIST
  )                                     ;defun




;|«Visual LISP© Format Options»
(72 2 40 1 T "end of " 60 9 0 0 0 nil T nil T)
;*** KEINEN Text unterhalb des Kommentars hinzufügen! ***|;
