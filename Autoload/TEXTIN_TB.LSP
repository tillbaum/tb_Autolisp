;TEXTIN.LSP By: Jeffery P. Sanders 
;This program takes any text file and prints it in AutoCAD.

;define program - listing your variable names here 
; resets them to nil after the program finishes
(defun C:TEXTIN(/ varltscale ernote inspt filen file lineone) ;inspt = insertionpoint

  ;turn echo off
  (setvar "cmdecho" 0)

   ;get ltscale (Note: ltscale should always equal dimscale)
  (setq varltscale(getvar "ltscale"))

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
  (if (setq file(open filen "r")) 
  
     ;progn necessary for multiple statements inside if statement
    (progn

     ;while the line from text file does not equal nil
    (while (setq lineone(read-line file))

       ;print the text in AutoCAD drawing 
	   
      ; old code (command "text" inspt (* varltscale(getvar "textsize"))0.0 lineone) _tb
	  
	  (command "text" inspt "0" lineone) 			;"0" ist Drehung
       ;move down one line each loop by resetting the insertion point
      (setq inspt

          ;start the polar function i.e. [polar pt angle distance]
        (polar

            ;from the insertion point
           inspt

            ;set the angle to go down i.e. (270 deg OR pi + pi/2 radians)
          (* pi 1.5)

            ;distance down to the next line is (textsize x 1.5) x ltscale, geändert tb
            (* (getvar "textsize") 0.5) ; Berechnung Zeilenabstand, 0.5 durch ausprobieren ermittelt!!

         ) ; close the polar function 

       ) ; close the setq 

     ) ; close the while loop 

     ;close the text file 
     (close file) 

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

