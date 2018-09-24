;TEXTIN.LSP By: Jeffery P. Sanders 
;This program takes any text file and prints it in AutoCAD.

;define program - listing your variable names here 
; resets them to nil after the program finishes
(defun C:TEXTIN(/ lts ernote inspt filen fil lineone) 

  ;turn echo off
  (setvar "cmdecho" 0)

   ;get ltscale (Note: ltscale should always equal dimscale)
  (setq lts(getvar "ltscale"))

  ;set the exit note to display successful
  (setq ernote "\n....TextIn Complete.")

  ;get the text insertion point
  (setq inspt(getpoint "\nInsertion Point: "))

  ;use dialog box to get file name / the 4 allows
  ;the user to type in a new file extension
  ;the "txt" sets the default to be "*.txt"
  (setq filen
    (getfiled "Select Text File" "" "txt" 4)
  )

  ;if file exist, open file to read
  (if (setq fil(open filen "r")) 

     ;progn necessary for multiple statements inside if statement
    (progn

     ;while the line from text file does not equal nil
    (while (setq lineone(read-line fil))

       ;print the text in AutoCAD drawing
      (command "text" inspt(* lts(getvar "textsize"))0.0 lineone)

       ;move down one line each loop by resetting the insertion point
      (setq inspt

          ;start the polar function i.e. [polar pt angle distance]
        (polar

            ;from the insertion point
           inspt

            ;set the angle to go down i.e. (270 deg OR pi + pi/2 radians)
          (* pi 1.5)

            ;distance down to the next line is (textsize x 1.5) x ltscale 
           (* lts (* 1.5(getvar "textsize")))

         ) ; close the polar function

       ) ; close the setq

     ) ; close the while loop

     ;close the text file
     (close fil)

   ) ; close the if progn statement

   ;else set the exiting remark as an error
   (setq ernote (strcat "\nCannot Find File: " filen))

  ) ; close the if statement

  ;turn echo on
  (setvar "cmdecho" 1)

  ;print the exiting remark
  (princ ernote)

  (princ "\n ")    ; clear command line
  (princ)        ; no echo

)                  ; close the program

