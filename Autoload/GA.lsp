;;;--- GA.lsp   -   GetArea
;;
;;;--- Select a spot on the interior of an enclosed area and this program
;;;    will write the Plot Number, Area, and Perimeter on that spot.
;;;
;;;
;;;--- This program uses the -bhatch command to create a hatch over the area.
;;;    It then gets a list of all of the control points for the hatch and
;;;    creates a polyline boundary (using the ENTMAKE) function.  The area
;;;    command is used on the polyline to find the area and perimeter.
;;;    No islands are taken into consideration.
;;;
;;;
;;;--- Created on 8/6/04
;;;    Copyright 2004 by JefferyPSanders.com
;;;    All rights reserved.
;;;
;;;
;;;--- Program is issued as is without gauranties of the accuracy nor of
;;;    damages resulting from use of the program.
;;;
;;;
;;;--- Tested with AutoCAD Release 14 and 2004

(defun C:GA()

  ;;;--- Turn the command echo off
  (setvar "cmdecho" 0)

  ;;;--- Inform the user of the plot number
  (princ "\n Starting Number is ")(princ (getvar "useri1"))
  (princ ".   To change this, reset the system variable USERI1")

  ;;;--- Get the interior selection point
  (setq pt(getpoint "\n Select Interior of Area : "))

  ;;;--- Create a hatch pattern in the area
  (command "_bhatch" "Advanced" "Island" "No" "Nearest" "" pt "")

  ;;;--- If the hatch pattern was created...
  (if(setq en(entlast))
    (progn

      ;;;--- Get the dxf group codes from the hatch entity
      (setq enlist(entget en))

      ;;;--- Build a list to eventually make a polyline entity
      (setq plist(list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity") (cons 100 "AcDbPolyline")))

      ;;;--- Set up an empty list, a flag, and a counter
      (setq newList(list) flag 0 cntr 0)

      ;;;--- Cycle through every item in the hatch's dxf group codes
      (while(< cntr (length enlist))

        ;;;--- Get the first item
        (setq a(nth cntr enlist))

        ;;;--- If it is the first group code 10, increment the flag and proceed
        (if (= (car a) 10)(setq flag(+ flag 1)))

        ;;;--- Skip the first group code 10 in the entity list
        (if (> flag 1)

          ;;;--- If the dxf group code number is 10, then...
          (if(= (car a) 10)
            (progn

              ;;;--- Start a temporary list holding the dxf group codes for a VERTEX entity
              ;;;    Add codes 0 and 10
              (setq tmp(list (cons 0 "VERTEX") a))

              ;;;--- In case there are bulges, look for codes 42 and 50 while the next code isn't
              ;;;    a group code 10 and there are more items in the dxf group codes to look for.
              (while(and (< cntr (length enlist))(/= (car (nth (+ cntr 1) enlist)) 10))

                ;;;--- Increment the counter to get to the 42 and 50 codes
                (setq cntr(+ cntr 1))

                ;;;--- Get the next code 
                (setq b(nth cntr enlist))

                ;;;--- If it is a 42 or 50 code...
                (if(or (= (car b) 42)(= (car b) 50))

                  ;;;--- Add it to the temporary list
                  (setq tmp(append tmp (list b)))

                )
              )

              ;;;--- Add the temporary list to the new list
              (setq newList(append newList (list tmp)))
            )
          )
        )

        ;;;--- Increment the counter to get the next group 10 code
        (setq cntr(+ cntr 1))
      )

      ;;;--- In order to close the polyline, we will need to save the start point
      ;;;    and use it later as the end point of the polyline
      (setq lastPt(car newList))

      ;;;--- Strip off the last point, which is the point selected during the hatch command
      (setq newList(reverse(cdr(reverse newList))))

      ;;;--- Delete the hatch pattern
      (entdel en)

      ;;;--- Start creating the polyline entity
      (entmake 
        (list 
          (cons 0 "POLYLINE")   ; Object type
          (cons 66 1)           ; Vertices follow
        )
      )

      ;;;--- Add each vertex to the polyline entity...
      (foreach a newList
        (entmake a )
      )

      ;;;--- Close the polyline by adding the first point
      (entmake lastPt)

      ;;;--- Finally add the SEQEND to create the polyline
      (entmake (list (cons 0 "SEQEND")))    

      ;;;--- Get the entity name of the polyline just created
      (setq en(entlast))

      ;;;--- Use the area command on the polyline
      (command "area" "Object" en)

      ;;;--- Get the Area of the Polyline
      (setq myArea(getvar "area"))    


      ;;;--- NOTE: Here would be a good place to do any manipulation of the area.
      ;;;    Such as converting it to square feet or acres.


      ;;;--- Get the perimeter of the polyline
      (setq myPerim(getvar "perimeter"))


      ;;;--- NOTE: Here would be a good place to do any manipulation of the perimeter.
      ;;;          Such as converting to feet or meters.

  
      ;;;--- Get the plot number to use from the USERI1 system variable.
      (setq myNum(getvar "useri1"))

      ;;;--- Don't start with zero, which is Autocad's default
      (if(= myNum 0)(setq myNum 1))

      ;;;--- Increment the counter before saving for next time
      (setvar "useri1" (+ myNum 1))

      ;;;--- Convert the number to a string
      (setq myNum(itoa myNum))

      ;;;--- Grab the current textsize
      (setq tht(getvar "textsize"))

      ;;;--- Get the current text style
      (setq csty(getvar "textstyle"))

      ;;;--- See if the text style has a preset height
      (if(= 0 (cdr(assoc 40(tblsearch "style" csty))))
        (progn

          ;;;--- Insert the text with a height parameter
          (command "text" "Justify" "Center" Pt tht 0 myNum)
          (command "text" "Justify" "Center" (polar Pt (* pi 1.5) (* tht 1.5)) tht 0 (strcat "AREA=" (rtos myArea 2 4)))
          (command "text" "Justify" "Center" (polar Pt (* pi 1.5) (* 2.0(* tht 1.5))) tht 0 (strcat "PERIMETER=" (rtos myPerim)))
        )
        (progn

          ;;;--- Else, Insert the text without a height parameter
          (command "text" "Justify" "Left" Pt 0 myNum)
          (command "text" "Justify" "Left" (polar Pt (* pi 1.5) (* tht 1.5)) 0 (strcat "AREA=" (rtos myArea 2 4)))
          (command "text" "Justify" "Left" (polar Pt (* pi 1.5) (* 2.0(* tht 1.5))) 0 (strcat "PERIMETER=" (rtos myPerim)))

        )
      )

      ;;;--- Delete the polyline entity
      (entdel en)
    )
    (alert "Hatch pattern could not be created.  Make sure the area is enclosed.")
  )

  ;;;--- Turn the command echo back on
  (setvar "cmdecho" 1)

  ;;;--- Suppress the last echo for a clean exit
  (princ)
)


