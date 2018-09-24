;blockt jeden Layer mit den darauf enthaltenen Elementen einzeln als [Layername.dwg] ins gleiche Verzeichnis ab.
;exportiert jeden layer in eine einzelne Zeichnung

(defun c:layereinzel (/ flag md_layerliste filter zielverzeichniss)
  (setq MD_ERR  *ERROR*
        *ERROR* M:FEHLER
        ) ;_ end of setq
  ;;Marke für Zurück nach Abbruch setzen
  (command "_UNDO" "_MARK")
  (setq M:filedia (getvar "filedia")
   M:cmddia (getvar "cmddia")
        M:cmdecho (getvar "cmdecho"))
  (setvar "filedia" 0)
  (setvar "cmddia" 0)
  (setvar "cmdecho" 0)
  (if
    (= (member "acetutil.arx" (arx)) NIL)
     (progn
       (princ
         "\nDie Dateien werden in das Aktuelle Verzeichniss geschrieben!"
         ) ;_ end of princ
       (setq zielverzeichniss (getvar "dwgprefix"))
       (if (wcmatch (substr zielverzeichniss (strlen zielverzeichniss)1) "\\")
         (setq zielverzeichniss (substr zielverzeichniss 1(-(strlen zielverzeichniss)1)))
         )
       ) ;_ end of progn
     (setq zielverzeichniss (acet-ui-pickdir))
     ) ;_ end of if
  (layerauswahl)
  (if zielverzeichniss
    (progn
      (command "-layer" "ta" "*" "ein" "*" "ent" "*" "_set" "0" "") ;_ end of command
      (repeat (length md_layerliste)
        (if
          (setq
            filter
             (ssget
               "x"
               (list (cons 8 (nth 0 md_layerliste)) (cons 410 "Model"))
               ) ;_ end of ssget
            ) ;_ end of setq
           (progn
             (if (/= (findfile (strcat zielverzeichniss
                                       "\\"
                                       (nth 0 md_layerliste)
                                       ".dwg"
                                       ) ;_ end of strcat
                               ) ;_ end of findfile
                     NIL
                     ) ;_ end of /=
               (setq flag T)
               (setq flag nil)
               ) ;_ end of if
             (command
               "_-wblock"
               (strcat zielverzeichniss
                       "\\"
                       (nth 0 md_layerliste)
                       ".dwg"
                       ) ;_ end of strcat
               ) ;_ end of command
             (if flag
               (command "J")
               ) ;_ end of if
             (command
               ""
               "0,0"
               filter
               ""
               ) ;_ end of command
             (If (= (getvar "cmdactive") 1)
               (command)
               ) ;_ end of If
             ) ;_ end of progn
           ) ;_ end of if
        (setq md_layerliste
               (cdr md_layerliste)      ;(member (nth 1 md_layerliste) md_layerliste)
              ) ;_ end of setq
        ) ;_ end of repeat
      ) ;_ end of progn
    (princ "\nKein Verzeichniss gewählt - Ende!")
    ) ;_ end of if
  (restore-var)
  (princ)
  ) ;_ end of defun
(prompt "\nStarten mit 'Layereinzel'")

(defun md_liste (/ md_layer_0 md_layer_next)
  (setq md_layer_0 (tblnext "layer" "0"))
  (setq md_layer_0 (cdr (assoc 2 md_layer_0)))
  (setq md_layerliste nil)
  (setq md_layerliste (list md_layer_0))
  (setq md_layer_next (tblnext "layer"))
  (setq md_layer_next (cdr (assoc 2 md_layer_next)))
  (while (/= md_layer_next nil)
    (progn
      (setq md_layer_next (list md_layer_next))
      (setq md_layerliste (append md_layerliste md_layer_next))
      (setq md_layer_next (tblnext "layer"))
      (setq md_layer_next (cdr (assoc 2 md_layer_next)))
      )                                 ;ende progn
    )                                   ;ende while
  (setq md_layerliste (acad_strlsort md_layerliste))
  )                                     ;ende defun md_liste

(defun layerauswahl (/)
  (prompt
    "\nReferenzobjekt auf Layern wählen, oder Return für alle... "
    ) ;_ end of prompt
  (setq M:SS1 (ssget))
  (if (null M:SS1)                      ;if enter, ZOOM All and choose everything
    (md_liste)
    (progn
      (setq M:SS1LEN      (sslength M:SS1) ;length of selection set
            md_layerliste '()
            i             0
            ) ;_ end of setq
      (repeat M:SS1LEN
        (setq M:ENAME (ssname M:SS1 i)  ;entity name
              M:ELIST (entget M:ENAME)  ;entity data list
              M:LAY   (cdr (assoc 8 M:ELIST)) ;entity type
              )                         ;setq
        (if
          (not (member M:LAY md_layerliste))
        (setq md_layerliste (append md_layerliste (list M:LAY)))
          )
        (setq i (1+ i))
        ) ;_ end of repeat
      (setq md_layerliste (acad_strlsort md_layerliste))
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of defun


(defun M:FEHLER (S)
  (print (strcat "M:fehler " S))
  (command "_UNDO" "_BACK")
  (setq *ERROR* MD_ERR)
  (princ)
  ) ;_ end of defun

(defun restore-var ( )
  (setvar "filedia" M:filedia)
  (setvar "cmddia" M:cmddia)
  (setvar "cmdecho" M:cmdecho)
  (setq *ERROR* MD_ERR)
  )
  
;|«Visual LISP© Format Options»
(72 2 40 1 T "end of " 60 9 0 0 0 nil T nil T)
;*** KEINEN Text unterhalb des Kommentars hinzufügen! ***|;
