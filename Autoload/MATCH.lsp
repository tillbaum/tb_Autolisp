;;;--- MATCH - Draw a matchline
;;;
;;;
;;;
;;;
;;;--- Copyright 2006 by JefferyPSanders.com
;;;    All rights reserved
;;;
;;;
;;;
;;;   1/27/06


;;;--- Define an error trap

;(defun newErr(msg)
;  
;  ;;;--- Display the error message
;  (princ"\n *Error* - ")(princ msg)
;
;  ;;;--- Reset the settings
;  (if oldLay  (setvar "clayer" oldLay))
;  (if oldStyle(setvar "textstyle" oldStyle))
;  (if oldLThk (setvar "plinewid" oldLThk));
;
;  ;;;--- Undefine my error trap
;  (setq *ERROR* OrigErr)
;
;  ;;;--- Suppress the echo for a clean exit
;  (princ)
;)
;
;;;;--- Start my error trap
;(setq OrigErr *ERROR* *ERROR* newErr)




;;;--- Define the program so all variables are local

(defun C:MATCH()




  ;;;--- Function to save the dialog box settings

  (defun saveVars()
    (setq style1(atoi(get_tile "style1")))
    (setq style2(atoi(get_tile "style2")))
    (setq matchMark(get_tile "matchmark"))
    (setq sheetNum(get_tile "sheetnum"))
    (setq lineThk(distof(get_tile "linethk")))
    (setq lineLay(atoi(get_tile "linelay")))
    (setq textSiz(distof(get_tile "textsiz")))
    (setq textSty(atoi(get_tile "textsty")))
    (setq textLay(atoi(get_tile "textlay")))
    (setq scaleF(atoi(get_tile "scalef")))
    (setq overRide(atoi(get_tile "override")))
    (setq circSiz(distof(get_tile "circsiz")))
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Sort Function   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;--- Usage (sort list)

  (defun sort(alist / n)(setq lcup nil rcup nil)
    (defun cts(a b)
      (cond
        ((> a b)t)
        ((= a b )t)
        (t nil)
      )
    )
    (foreach n alist
      (while (and rcup(cts n(car rcup)))(setq lcup(cons(car rcup)lcup)rcup(cdr rcup)))
      (while (and lcup(cts(car lcup)n))(setq rcup(cons(car lcup)rcup)lcup(cdr lcup)))
      (setq rcup(cons n rcup))
    )
    (append(reverse lcup)rcup)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;  End of Sort Function  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;        Function to get table data          ;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Usage - (setq myLayers(get_table "LAYER"))

  (defun get_table (table / tmp ret)
    (setq tmp (tblnext table 't))
    (while tmp
      (setq ret (cons (cdr (assoc 2 tmp)) ret) tmp (tblnext table))
    )
    (sort ret)
  )

  ;;;;;;;;;;;;;;;;;;;;;;;     End of get table data      ;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;--- Function to allow switching styles

  (defun changeStyle(a)
    (if(= a 1)
      (progn
        (set_tile "style1" "1")
        (set_tile "style2" "0")
        (mode_tile "sheetnum" 1)
      )
      (progn
        (set_tile "style1" "0")
        (set_tile "style2" "1")
        (mode_tile "sheetnum" 0)
      )
    )
  )

  
  ;;;--- Function to enable or disable the circle size

  (defun changeCircMode()
    (if(= (get_tile "override") "1")
      (mode_tile "circsiz" 0)
      (mode_tile "circsiz" 1)      
    )
  )



  ;;;--- Function to check to see if the style has a fixed height
  ;;;    If so, disable the text height box

  (defun chkTextSizeMode()
    (setq textSty(atoi(get_tile "textsty")))
    (setq textSty(nth textSty styList))    
    ;;;--- Check to see if the style has a fixed height
    (if(setq tbl(tblsearch "STYLE" textSty)) 
      (progn
        (setq ht(cdr(assoc 40 tbl)))
        (if(= ht 0.0)
          (setq fixedHeight nil)
          (setq fixedHeight T)
        )
      )
    )
    (if fixedHeight
      (mode_tile "textsiz" 1)
      (mode_tile "textsiz" 0)
    )
  )






  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                ;;;
  ;;;  888       888          888           8888888      8888   888  ;;;
  ;;;  8888     8888         88888            888        88888  888  ;;;
  ;;;  88888   88888        888 888           888        888888 888  ;;;
  ;;;  888888 888888       888   888          888        888 888888  ;;;
  ;;;  888 88888 888      88888888888         888        888  88888  ;;;
  ;;;  888  888  888     888       888      8888888      888   8888  ;;;
  ;;;                                                                ;;;
  ;;;                                                                ;;;
  ;;;           888            888888888        888888888            ;;;
  ;;;          88888           888   888        888   888            ;;;
  ;;;         888 888          888   888        888   888            ;;;
  ;;;        888   888         888888888        888888888            ;;;
  ;;;       88888888888        888              888                  ;;;
  ;;;      888       888       888              888                  ;;;
  ;;;                                                                ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  ;;;--- Main application

  ;;;--- Get the autoCAD Version
  (setq ver(atoi(substr (getvar "acadver") 1 2)))
  (if(> ver 15)
     (setq prefx (strcat (getvar "mydocumentsprefix") "\\")) 
     (setq prefx (getvar "dwgprefix"))
  )


  ;;;--- Load the dialog box from file
  (setq dcl_id (load_dialog "MATCH.dcl"))

  ;;;--- Load the dialog definition from the file
  (if (not (new_dialog "MATCH" dcl_id))
    (progn
      (alert "Could not find the MATCH.DCL file!")
      (exit)
    )
  )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;; Build the layer and style list  ;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq layList(get_table "LAYER"))
  (setq styList(get_table "STYLE"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;; Load the list in the dialog box. ;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;--- Add the line layer list to the dialog box
  (start_list "linelay" 3)
  (mapcar 'add_list layList)
  (end_list)

  ;;;--- Add the text layer list to the dialog box
  (start_list "textlay" 3)
  (mapcar 'add_list layList)
  (end_list)

  ;;;--- Add the style list to the dialog box
  (start_list "textsty" 3)
  (mapcar 'add_list styList)
  (end_list)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       Load the images      ;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq width (dimx_tile "img1"))
  (setq height(dimy_tile "img1"))
  (start_image "img1")
  (fill_image 0 0 width height 0)
  (slide_image 0 0 width height "MATCH1.sld")
  (end_image)

  (setq width (dimx_tile "img2"))
  (setq height(dimy_tile "img2"))
  (start_image "img2")
  (fill_image 0 0 width height 0)
  (slide_image 0 0 width height "MATCH2.sld")
  (end_image)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      Load the defaults     ;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (if(findfile (strcat prefx "MATCH.DAT"))
    (progn
      (if(setq fil(open (strcat prefx "MATCH.DAT") "r"))
        (progn
          (set_tile "style1" (read-line fil))
          (set_tile "style2" (read-line fil))
          (set_tile "matchmark" (setq tmp(read-line fil)))
          (set_tile "sheetnum" (read-line fil))
          (set_tile "linethk" (rtos(distof (read-line fil))))
          (set_tile "linelay" (read-line fil))
          (set_tile "textsiz" (rtos(distof (read-line fil))))
          (set_tile "textsty" (read-line fil))                    
          (set_tile "textlay" (read-line fil))
          (set_tile "scalef" (read-line fil))
          (set_tile "override" (read-line fil))
          (set_tile "circsiz" (rtos(distof (read-line fil))))
          (close fil)
        )
      )
    )
  )

  ;;;--- Set the override circle size to enabled or disabled
  (changeCircMode)

  ;;;--- Set the sheetnum text box to be enabled or disabled
  (if(= (get_tile "style1") "1")
    (changeStyle 1)
    (changeStyle 2)
  )

  ;;;--- Check the style to see if a height is set
  (chkTextSizeMode)

  ;;;--- If an action event occurs, do this...
  (action_tile "textsty" "(chkTextSizeMode)")
  (action_tile "override" "(changeCircMode)")
  (action_tile "style1" "(changeStyle 1)")
  (action_tile "style2" "(changeStyle 2)")
  (action_tile "insert"  "(saveVars)(setq ddiag 1)(done_dialog)")
  (action_tile "cancel"  "(setq ddiag 2)(done_dialog)")

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;-- Unload the dialog box
  (unload_dialog dcl_id)

  ;;;--- Start the routine
  (if(= ddiag 1)
    (progn

      ;;;--- Turn the command echo off
      (setvar "cmdecho" 0)

      ;;;--- Save the current settings
      (setq oldLay(getvar "clayer"))
      (setq oldStyle(getvar "textstyle"))
      (setq oldLThk(getvar "_.plinewid"))

      ;;;--- Save the settings for next use
      (if(setq fil(open (strcat prefx "MATCH.DAT") "w"))
        (progn
          (princ (itoa style1) fil)
          (princ (strcat "\n" (itoa style2)) fil)
          (princ (strcat "\n" matchMark) fil)
          (princ (strcat "\n" sheetNum) fil)
          (if lineThk(princ (strcat "\n" (rtos lineThk 2 4)) fil)(princ "\n0" fil))
          (princ (strcat "\n" (itoa lineLay)) fil)
          (if textSiz(princ (strcat "\n" (rtos textSiz 2 4)) fil)(princ "\n0" fil))
          (princ (strcat "\n" (itoa textSty)) fil)
          (princ (strcat "\n" (itoa textLay)) fil) 
          (princ (strcat "\n" (itoa scaleF)) fil)
          (princ (strcat "\n" (itoa override)) fil)
          (if circSiz (princ (strcat "\n" (rtos circSiz 2 4)) fil)(princ "\n0" fil))
          (close fil)
        )
        (alert "Could not save the settings for next use.")
      )

      ;;;--- Multiply the textsize times the dimscale unless disabled
      (if(= scalef 0)(setq textSiz(* textSiz (getvar "dimscale"))))

      ;;;--- Multiply the line thickness by the ltscale unless disabled
      (if(= scalef 0)(setq lineThk(* lineThk (getvar "ltscale"))))

      ;;;--- Check the override for the circle diameter
      (if(= override 1)
        (setq circRad (/ circSiz 2.0))
        (setq circRad (* textSiz 3.5))
      )	

      ;;;--- Find the end of the polyline and center of the circles
      (setq pt1(getpoint "\n Matchline Start Point: "))
      (setq pt2(getpoint pt1 "\n Matchline End Point: "))
      (setq pta(polar pt1 (angle pt1 pt2) circRad))
      (setq ptb(polar pt2 (angle pt2 pt1) circRad))

      ;;;--- Check to see if using style2, if so, move the points
      (if(= style2 1)
        (progn
          (setq pta(polar pta (angle pt2 pt1) (* 2.0 circRad)))
          (setq ptb(polar ptb (angle pt1 pt2) (* 2.0 circRad)))
        )
      )

      ;;;--- Set the layer for the lines
      (setvar "clayer" (nth lineLay layList))

      ;;;--- Draw the match line
      (command "_.pline" pta "W" lineThk lineThk ptb "")

      ;;;--- Draw the circles on the ends of the matchline
      (command "donut" (- (* circRad 2.0) lineThk) (+ lineThk(* circRad 2.0)) pt1 pt2 "")

      ;;;--- Set the layer for the text
      (setvar "clayer" (nth textLay layList))

      ;;;--- Set the text style
      (setvar "textstyle" (nth textSty styList))

      ;;;--- Find the locations for the text based on which style is used
      (if(= style2 1)
        (progn
          (setq pt1a(polar pt1 (+ (angle pt1 pt2) (/ pi 2.0)) (/ circRad 2.0)))
          (setq pt2a(polar pt2 (+ (angle pt1 pt2) (/ pi 2.0)) (/ circRad 2.0)))
          (setq pt1b(polar pt1 (- (angle pt1 pt2) (/ pi 2.0)) (/ circRad 2.0)))
          (setq pt2b(polar pt2 (- (angle pt1 pt2) (/ pi 2.0)) (/ circRad 2.0)))
        )

        ;;;--- Else style 1 was used...
        (setq pt1a pt1 pt2a pt2)
      )

      ;;;--- Check to see if the style has a fixed height
      (if(setq tbl(tblsearch "STYLE" (nth textSty styList))) 
        (progn
          (setq ht(cdr(assoc 40 tbl)))
          (if(= ht 0.0)
            (setq fixedHeight nil)
            (setq fixedHeight T)
          )
        )
      )  
 
      ;;;--- Draw the matchline mark text
      (if fixedHeight
        (progn
          (command "text" "J" "MC" pt1a (angtos(angle pt1 pt2)) matchMark)
          (command "text" "J" "MC" pt2a (angtos(angle pt1 pt2)) matchMark)

          ;;;--- If style 2 is used, draw the sheet number text
          (if(= style2 1)
            (progn
              (command "text" "J" "MC" pt1b (angtos(angle pt1 pt2)) sheetNum)
              (command "text" "J" "MC" pt2b (angtos(angle pt1 pt2)) sheetNum)
            )
          )
        )
        (progn
          (command "text" "J" "MC" pt1a textSiz (angtos(angle pt1 pt2)) matchMark)
          (command "text" "J" "MC" pt2a textSiz (angtos(angle pt1 pt2)) matchMark)

          ;;;--- If style 2 is used, draw the sheet number text
          (if(= style2 1)
            (progn
              (command "text" "J" "MC" pt1b textSiz (angtos(angle pt1 pt2)) sheetNum)
              (command "text" "J" "MC" pt2b textSiz (angtos(angle pt1 pt2)) sheetNum)
            )
          )
        )
      )
    )
  )
  ;;;--- Turn the command echo back on
  (setvar "cmdecho" 1)

  ;;;--- Reset the settings
  (setvar "clayer" oldLay)
  (setvar "textstyle" oldStyle)
  (setvar "_.plinewid" oldLThk)


  ;;;--- Suppress the last echo for a clean exit
  (princ)
)