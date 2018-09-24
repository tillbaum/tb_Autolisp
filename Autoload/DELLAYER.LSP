;;;  This program deletes all entities on specified layers.  Wildcards can be specified.
;;;   Copyright (C) 1990 - 1993 by Autodesk, Inc.
;;;
;;;   Permission to use, copy, modify, and distribute this software
;;;   for any purpose and without fee is hereby granted, provided
;;;   that the above copyright notice appears in all copies and that
;;;   both that copyright notice and this permission notice appear in
;;;   all supporting documentation.
;;;
;;;   THIS SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED
;;;   WARRANTY.  ALL IMPLIED WARRANTIES OF FITNESS FOR ANY PARTICULAR
;;;   PURPOSE AND OF MERCHANTABILITY ARE HEREBY DISCLAIMED.
;;;
;;;   Version 2.0
;;; --------------------------------------------------------------------------;
;;;   DESCRIPTION
;;;
;;;   This program deletes all entities on specified layers.  Wildcards
;;;   can be specified.
;;;
;;;   Version 2.0 has been updated to delete entities in both paper
;;;   space and modelspace. An additional prompt has been added to
;;;   prepare a layer for purging. If desired, the layer(s) will be
;;;   thawed and turned off, to make purging possible.
;;;   Script files using earlier versions of this routine must be
;;;   updated to accomodate the extra prompt.
;;;
;;; --------------------------------------------------------------------------;

(defun dellerr (s)                    ; If an error (such as CTRL-C) occurs
                                      ; while this command is active...
  (if (/= s "Funktion abgebrochen")
    (princ (strcat "\nFehler: " s))
  )
  (setq sset_1 nil)                   ; Free selection-sets if any
  (setq sset_2 nil)
  (setvar "CMDECHO" ocmd)             ; Restore saved mode
  (setq *error* olderr)               ; Restore old *error* handler
  (princ)
)


(defun c:DELLAYER ( / sset_1 sset_2 prg num count ex)
   (setq olderr *error*
        *error* dellerr)

   (setq ocmd (getvar "CMDECHO"))
   (setvar "CMDECHO" 0)

   (setq lname (strcase (getstring "\nZu löschende(r) Layer: ")))

   ;; Get all entities on layer(s)
   (setq sset_1 (ssget "_X" (list (cons 8 lname))))

   (if sset_1
      (progn
         (initget "Ja Nein")
         (setq prg (getkword "\n\
Den(die) Layer zum bereinigen vorbreiten <J>/N:"))
         (if (= prg nil) (setq prg "Ja"))
         (setq num (sslength sset_1))

         (setq count 0)                      ;delete the entities
         (repeat (sslength sset_1)
            (entdel (ssname sset_1 count))
            (setq count (1+ count))
         )

         ;Check that everything is gone
         (if
            (ssget "_X" (list (cons 8 lname)))
            ;And if anything is left
            (progn
              (setq tm (getvar "tilemode"))
                (if (= 1 tm)
                  (setvar "tilemode" 0)
                )
                ;Go to paperspace
                (if (/= 1 (getvar "cvport"))
                 (progn
                  (princ "\nWechsle in den Papierbereich.")
                  (command "_.pspace")
                 )
                )
                ;And try again
                (setq sset_2 (ssget "_X" (list (cons 8 lname))))
                (setq count 0)
                 (repeat (sslength sset_2)
                   (entdel (ssname sset_2 count))
                   (setq count (1+ count))
                 )
              (setvar "tilemode" tm)
            )
         )

         (if (= prg "Ja")
           (progn
             ; Prep the layer for purging
             ; Turn off, thaw, and unlock layer(s)
             (setq ex (getvar "expert"))
             (setvar "expert" 5)
             (command "_.layer" "_off" lname "_thaw" lname "")
               (if (= 0 (getvar "tilemode"))
                 (command "_.vplayer" "_vpvisdflt" lname "_thaw" "_reset"
                 lname "_all" "")
               )
             (princ "\n")(princ num)(princ " Objekt(e) auf Layer ")
             (princ lname)(princ " gelöscht.")(princ "\nLayer ") (princ lname)
             (princ " ist getaut, ausgeschaltet und kann bereinigt werden.")
	     (setvar "expert" ex)
           )
           (progn
             (princ "\n")(princ num)(princ " Objekt(e) auf Layer ")
             (princ lname)(princ " gelöscht.")
           )
         )
      )

      (princ "Layer leer oder Layername ungültig.")
   )

   (setq sset_1 nil)                   ; Free selection-sets
   (setq sset_2 nil)
   (setvar "CMDECHO" ocmd)             ; Restore saved mode
   (setq *error* olderr)               ; Restore old *error* handler
   (princ)
)
(prompt "\nStarten mit: Dellayer")

