;MTEXTOUT.LSP By: Jeffery P. Sanders, changed by Tillmann Baumeister
; *.csv
;This program gets MTEXT from an AutoCAD drawing and writes it to a text file.

;define program - listing your variable names here 
; resets them to nil after the program finishes    
(defun C:MTEXTOUT(/ lts ernote filen fil eset en enlist cntr)

  ;turn echo off
  (setvar "cmdecho" 0)

  ;get ltscale (Note: ltscale should always equal dimscale)
  (setq lts(getvar "ltscale"))

  ;set the exit note to successful
  (setq ernote "\n....MTextOut Complete.")

  ;use dialog box to set file name / the 1 allows
  ;the user to type in a new file name
  ;the "txt" sets the default to be "*.txt"
  (setq filen
    (getfiled "Type or Select Text File Name" "" "csv" 1)
  )

  ;open file to write
  (if (setq fil(open filen "w"))

    ;progn necessary for multiple statements inside an if statement
    (progn

      ;if ssget returns a valid selection set
      (if (setq eset(ssget))

        ;progn necessary for multiple statements inside an if statement
        (progn

          ;set the entity counter to zero [the first entity in a set is zero]
          (setq cntr 0)

          ;step through each entity in the selection set
          (while (< cntr (sslength eset))

            ;get the entity name indexed by cntr
            (setq en(ssname eset cntr))

            ;get the DXF group codes for the entity
            (setq enlist(entget en))

            ;check the group code 0 to see if entity type = MTEXT
            (if(= "MTEXT" (cdr(assoc 0 enlist)))

              ;progn necessary for multiple statements inside an if statement
              (progn

                ;get the text string from the entity's DXF Group Code 1
                (setq str(cdr(assoc 1 enlist)))

                ;print the string to the command line
                (princ (strcat "\nOutput To File: " str))

                ;print the string to the file
                (princ (strcat "\n" str) fil)

              ) ;close the if progn

            ) ;close the if statement

            ;increment the counter to get the next entity
            (setq cntr(+ cntr 1)) 

          ) ;close the while loop

          ;close the text file
          (close fil) 

        ) ;close the if progn

        ;set the exit note as an error
        (setq ernote "\nError - No Entities Selected.")
		
      ) ; close the if statement
    ) ;close the if progn

    ;set the exit note to be an error
    (setq ernote (strcat "\nError - Could not create File: " filen))

  ) ;close the if statement

  ;turn the command echo back on
  (setvar "cmdecho" 1)

  ;print the exit note to the command line
  (princ ernote)

  ;clear the command line
  (princ "\n ")

  ;supress last echo
  (princ)

) ;close the program