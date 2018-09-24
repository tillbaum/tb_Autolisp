;;;--- ETABLE - Send entity properties into a table.

;;;--- Copyright 2007 by JefferyPSanders.com.  All rights reserved.
;;;  
;;;    This program was created to extract data from entities and send
;;;    them to an autocad table entity.

;;;    The types of entities to be selected are ARC, ATTRIB, CIRCLE,
;;;    ELLIPSE, IMAGE, INSERT (Block), LINE, LWPOLYLINE, MLINE,
;;;    MTEXT, POINT, POLYLINE, SOLID, TEXT, TRACE, and XLINE.

;;;    Revisions:
;;;    
;;;      12/6/07  -  Solved problem with program crashing with variable txtSz.
;;;      12/6/07  -  Added the ability to save the last used settings
;;;
;;;

;;;    Comments:
;;;
;;;      12/5/07  -  AutoCAD 2005 crashes at the vla-addtable statement (unresolved)
;;;      12/6/07  -  Add ability to save defaults (resolved)
;;;      9/12/13  -  Added multi-language support



;;;--- Function to strip commas from a string

(defun stripCommas(a)
  (setq cnt 1)
  (while (< cnt (strlen a))
     (setq ch(substr a cnt 1))
     (if(= ch ",")
       (setq a
         (strcat
           (substr a 1 (- cnt 1))
           (substr a (+ cnt 1))
         )
       )
       (setq cnt(+ cnt 1))
     )
  )
  a
)




;;;--- Function to return an arc length

(defun getArcLen(rad sAng eAng)
  (if (< sAng eAng)
    (setq angl (abs (- sAng eAng)))
    (setq angl (- (* 2 pi) (abs (- sAng eAng))))
  )
  (rtos(setq len (* angl rad))2 4)
)





;;;--- Function to return a selection set

(defun windowedSelection(entType)
  (setq eset(ssget (list(cons 0 entType))))
)






;;;--- Function to write a label        
;;;    Parameters:                      
;;;      label - Text string            
;;;      dotPt - Arrow point for leader 
;;;      txtPt - Text Location point    

(defun writeLabel(label dotPt txtPt)

  ;;;--- Save old layer and set new layer
  (setq oldLay(getvar "clayer"))
  (setvar "clayer" layerChoice)

  ;;;--- Save the old snap mode and turn osnaps off
  (setq oldSnap(getvar "osmode"))
  (setvar "osmode" 0)

  ;;;--- Get the angle of the leader
  (setq lang(angle dotPt txtPt))

  ;;;--- Draw an ellipctical arc around the entity
  (command "_ellipse" "arc" "C"
    dotPt                                          ; center of ellipse   
    (polar dotPt (+ lang pi) (/ txtSz 2.0))        ; long axis point     
    (polar dotPt (+ lang (/ pi 2.0))(/ txtSz 4.0)) ; short axis point    
    (polar dotPt (+ lang (* pi 0.75)) txtSz)       ; start angle         
    (polar dotPt (+ lang (* pi 0.3))  txtSz)       ; end angle           
  )

  ;;;--- Get the size box required to hold the text
  (setq tb(textbox (list(cons 0 "TEXT")(cons 1 label)(cons 40 (getvar "textsize")))))
  (setq boxWidth(+ (- (car (cadr tb)) (car(car tb)))(/ txtSz 2.0)))
  (setq boxHeight(+ (- (cadr(cadr tb)) (cadr(car tb))) (/ txtSz 3.0)))

  ;;;--- Find the left right location of the box
  (if(and(> lang (* pi 0.5))(< lang (* pi 1.5)))
    (setq side "Left")
    (setq side "Right")
  )

  ;;;--- Find the up down location of the box
  (if(<= lang pi)
    (setq upDwn "Top")
    (setq upDwn "Bot")
  )

  ;;;--- Calculate the box points based on location
  (if(and(= side "Left")(= upDwn "Top"))
    (setq pta txtPt ptb (polar pta (* pi 0.5) boxHeight)
          ptc (polar ptb pi boxWidth)
          ptd (polar pta pi boxWidth)
          txtPt ptd
    )
  )
  (if(and(= side "Left")(= upDwn "Bot"))
    (setq pta txtPt ptb (polar pta pi boxWidth)
          ptc (polar ptb (* pi 1.5) boxHeight)
          ptd (polar ptc 0 boxWidth)
          txtPt ptc
    )
  )
  (if(and(= side "Right")(= upDwn "Top"))
    (setq pta txtPt ptb (polar pta 0 boxWidth)
          ptc (polar ptb (* pi 0.5) boxHeight)
          ptd (polar ptc pi boxWidth)
    )
  )
  (if(and(= side "Right")(= upDwn "Bot"))
    (setq pta txtPt ptb (polar pta (* pi 1.5) boxHeight)
          ptc (polar ptb 0 boxWidth)
          ptd (polar pta 0 boxWidth)
          txtPt ptb
    )
  )

  ;;;--- Draw a leader and box around the text label
  (command "_pline" (polar dotPt lang (/ txtSz 2.0)) "w" 0 0 pta ptb ptc ptd pta "")
  
  ;;;--- Get the current text style
  (setq csty(getvar "textstyle"))

  ;;;--- See if the text style has a preset height
  (if(= 0 (cdr(assoc 40(tblsearch "style" csty))))
    
    ;;;--- Insert the text with a height parameter
    (command "_text" (polar txtPt (* pi 0.25) (/ txtSz 4.0)) (getvar "textsize") 0 label)
      
    ;;;--- Else, Insert the text without a height parameter
    (command "_text" (polar txtPt (* pi 0.25) (/ txtSz 4.0)) 0 label)
  )


  ;;;--- Reset layer and osnaps
  (setvar "clayer" oldLay)
  (setvar "osmode" oldSnap)
)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                 ;;;
;;;                                                                                 ;;;
;;;--- Functions to get entity types                                                ;;;
;;;                                                                                 ;;;
;;;                                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;--- ARC DATA

(defun getArcData(/ eset hdrStr en enlist entLyr entPt entRad entDia entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "ARC"))
    (setq eset(ssget "X" (list (cons 0 "ARC"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "Center X" "Center Y" "Center Z"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "LineType"))))
      (if(= tog11 "1")(setq hdrStr (append hdrStr (list "Radius"))))
      (if(= tog12 "1")(setq hdrStr (append hdrStr (list "Diameter"))))
      (if(= tog18 "1")(setq hdrStr (append hdrStr (list "Length"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (setq entRad(rtos(cdr(assoc 40 enlist))2 4))
        (setq entDia(rtos(* 2 (cdr(assoc 40 enlist)))2 4))
        (setq entLen(getArcLen (cdr(assoc 40 enlist))(cdr(assoc 50 enlist))(cdr(assoc 51 enlist))))
        (if(not (setq entHan(cdr(assoc 5 enlist))))
          (setq entHan "OFF")
        )
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        (setq stpt(polar entPt (cdr(assoc 50 enlist)) (distof entRad)))
        (setq edpt(polar entPt (cdr(assoc 51 enlist)) (distof entRad)))
        (if (not c:cal)(arxload "geomcal"))
        (setq myAng (cal "ang(entPt,stpt,edpt)"))
        (setq myAng(+ (angle entPt stpt) (* pi (/ (/ myAng 2.0) 180.0))))
        (setq newPt(polar entPt myAng (distof entRad)))

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                                            ; label       
            newPt                                                                           ; arrow point 
            (polar newPt (angle entPt newpt) (* 4.0 (getvar "textsize")))                   ; text point  
          )
        )
        
        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog5  "1")(setq dStr (append dStr entPtSTr)))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty))))
        (if(= tog11 "1")(setq dStr (append dStr (list entRad))))
        (if(= tog12 "1")(setq dStr (append dStr (list entDia))))
        (if(= tog18 "1")(setq dStr (append dStr (list entLen))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))                                                               
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))
      )
    )                    
  )
  dataList
)







;;;--- ATTRIBUTE DATA

(defun getAttData(/ eset hdrStr dataList blkCntr en enlist blkType entName entPoint entRot 
                    entX entY entZ entLay attTag attVal entSty  entClr dStr group66)
  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "INSERT"))
    (setq eset(ssget "X" (list (cons 0 "INSERT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog1  "1")(setq hdrStr (append hdrStr (list "Name"))))
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog4  "1")(setq hdrStr (append hdrStr (list "Insertion X" "Insertion Y" "Insertion Z"))))
      (if(= tog7  "1")(setq hdrStr (append hdrStr (list "Tag"))))
      (if(= tog8  "1")(setq hdrStr (append hdrStr (list "Text Value"))))
      (if(= tog9  "1")(setq hdrStr (append hdrStr (list "Style"))))
      (if(= tog16 "1")(setq hdrStr (append hdrStr (list "Rotation"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up some counters
      (setq blkCntr 0 cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< blkCntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset blkCntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                      
        ;;;--- Check to see if the block's attribute flag is set
        (if(cdr(assoc 66 enlist))
          (progn
                 
            ;;;--- Get the entity name
            (setq en(entnext en))
                   
            ;;;--- Get the entity dxf group codes
            (setq enlist(entget en))
                    
            ;;;--- Get the type of block
            (setq blkType (cdr(assoc 0 enlist)))
                    
            ;;;--- If group 66 then there are attributes nested inside this block
            (setq group66(cdr(assoc 66 enlist)))
                   
            ;;;--- Loop while the type is an attribute or a nested attribute exist
            (while(or (= blkType "ATTRIB")(= group66 1))
                    
              ;;;--- Get the block type 
              (setq blkType (cdr(assoc 0 enlist)))
                          
              ;;;--- Get the block name         
              (setq entName (cdr(assoc 2 enlist)))
                       
              ;;;--- Check to see if this is an attribute or a block
              (if(= blkType "ATTRIB")
                (progn
                        
                  ;;;--- Get the insertion point
                  (setq entPoint(cdr(assoc 10 enlist)))
                       
                  ;;;--- Get the X, Y, and Z coordinates of the insertion point
                  (setq entX (rtos (car entPoint)   2 4))
                  (setq entY (rtos (cadr entPoint)  2 4))
                  (setq entZ (rtos (caddr entPoint) 2 4))
                       
                  ;;;--- Save the layer
                  (setq entLay(cdr(assoc 8 enlist)))
                       
                  ;;;--- Save the name of the attribute
                  (setq attTag(cdr(assoc 2 enlist)))
                      
                  ;;;--- Get the value of the attribute
                  (setq attVal(cdr(assoc 1 enlist))) 

                  ;;;--- Get the style of the attribute
                  (setq entSty(cdr(assoc 7 enlist))) 

                  ;;;--- Get the rotation of the attribute
                  (if(assoc 50 enlist)
                     (setq entRot(angtos(cdr(assoc 50 enlist))0 4)) 
                     (setq entRot "0.0000")
                  )

                  ;;;--- Get the handle
                  (if(not (setq entHan(cdr(assoc 5 enlist))))
                    (setq entHan "OFF")
                  )
        
                  ;;;--- Get the color of the attribute       
                  (if(cdr(assoc 62 enlist))(setq  entClr(cdr(assoc 62 enlist)))(setq  entClr "BYLAYER"))
                  (if(= 'INT (type  entClr))(setq  entClr (itoa  entClr)))

                  ;;;--- Send a label and two points to the writeLabel function
                  (if labelTog
                    (writeLabel
                      (strcat lblPrefix (itoa lblStrtNum))                          ; label       
                      entPoint                                                      ; arrow point 
                      (polar entPoint (* pi 1.25) (* 4.0 (getvar "textsize")))      ; text point  
                    )
                  )
                  
                      
                  ;;;--- Build a data string
                  (setq dStr(list))
                  (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))                  
                  (if(= tog1  "1")(setq dStr (append dStr (list entName))))
                  (if(= tog2  "1")(setq dStr (append dStr (list entLay))))
                  (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
                  (if(= tog4  "1")(setq dStr (append dStr (list entX entY entZ))))
                  (if(= tog7  "1")(setq dStr (append dStr (list attTag))))
                  (if(= tog8  "1")(setq dStr (append dStr (list attVal))))
                  (if(= tog9  "1")(setq dStr (append dStr (list entSty))))
                  (if(= tog16 "1")(setq dStr (append dStr (list entRot))))
                  (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
                  (setq dataList(append dataList(list dStr)))                                                               
                  (setq cntr (+ cntr 1))
                  (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))                    

                  ;;;--- Get the next sub-entity or nested entity as you will
                  (setq en(entnext en))
                       
                  ;;;--- Get the dxf group codes of the next sub-entity
                  (setq enlist(entget en))
                      
                  ;;;--- Get the block type of the next sub-entity
                  (setq blkType (cdr(assoc 0 enlist)))
                        
                  ;;;--- See if the dxf group code 66 exist.  if so, there are more nested attributes
                  (setq group66(cdr(assoc 66 enlist)))
                ) 
              )
            )
          )
        )
        (setq blkCntr (+ blkCntr 1))
      )
    )                    
  )
  dataList
)









;;;--- CIRCLE DATA

(defun getCirData(/ eset hdrStr en enlist entLyr entPt entRad entDia entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "CIRCLE"))
    (setq eset(ssget "X" (list (cons 0 "CIRCLE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "Center X" "Center Y" "Center Z"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "LineType"))))
      (if(= tog11 "1")(setq hdrStr (append hdrStr (list "Radius"))))
      (if(= tog12 "1")(setq hdrStr (append hdrStr (list "Diameter"))))
      (if(= tog17 "1")(setq hdrStr (append hdrStr (list "Area"))))
      (if(= tog18 "1")(setq hdrStr (append hdrStr (list "Perimeter"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (setq entRad(rtos(cdr(assoc 40 enlist))2 4))
        (setq entDia(rtos(* 2 (cdr(assoc 40 enlist)))2 4))
        (setq entLen(rtos(* pi (* 2.0 (cdr(assoc 40 enlist))))2 4))
        (setq entAre(rtos(* pi (expt (cdr(assoc 40 enlist)) 2))2 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
          (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                                    ; label       
            (polar entPt (* pi 0.25) (atof entRad))                                 ; arrow point 
            (polar entPt (* pi 0.25) (+ (atof entRad)(* 2.0 (getvar "textsize"))))  ; text point  
          )
        )
        
        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))        
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog5  "1")(setq dStr (append dStr entPtSTr)))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty))))
        (if(= tog11 "1")(setq dStr (append dStr (list entRad))))
        (if(= tog12 "1")(setq dStr (append dStr (list entDia))))
        (if(= tog17 "1")(setq dStr (append dStr (list entAre))))
        (if(= tog18 "1")(setq dStr (append dStr (list entLen))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)








;;;--- ELLIPSE DATA

(defun getEllData(/ eset hdrStr en enlist entLyr entPt entRad entDia entLty entClr dstr 
                    entRot maAxis miAxis)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "ELLIPSE"))
    (setq eset(ssget "X" (list (cons 0 "ELLIPSE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "Center X" "Center Y" "Center Z"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "LineType"))))
      (if(= tog14 "1")(setq hdrStr (append hdrStr (list "Major Axis"))))
      (if(= tog15 "1")(setq hdrStr (append hdrStr (list "Minor Axis"))))                
      (if(= tog16 "1")(setq hdrStr (append hdrStr (list "Rotation"))))             
      (if(= tog17 "1")(setq hdrStr (append hdrStr (list "Area"))))
      (if(= tog18 "1")(setq hdrStr (append hdrStr (list "Perimeter"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (setq maAxis(distance (list 0 0) (cdr(assoc 11 enlist))))
        (setq miAxis(rtos(* maAxis (cdr(assoc 40 enlist)))2 4))
        (setq maAxis(rtos maAxis 2 4))
        (command "_area" "Object" en)
        (setq entAre(rtos (getvar "area") 2 4))
        (setq entLen(rtos (getvar "perimeter")2 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
          (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the rotation of the ellipse
        (setq entRot(angtos(angle (list 0 0)(cdr(assoc 11 enlist)))0 4)) 

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                          ; label       
            (polar entPt (angtof entRot) (distof maAxis))                 ; arrow point 
            (polar                                                        ; text point  
              (polar entPt(angtof entRot)(distof maAxis))
              (angtof entRot)
              (* 4.0(getvar "textsize"))
            ) 
          )
        )

        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))       
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog5  "1")(setq dStr (append dStr entPtSTr)))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty))))
        (if(= tog14 "1")(setq dStr (append dStr (list maAxis))))
        (if(= tog15 "1")(setq dStr (append dStr (list miAxis)))) 
        (if(= tog16 "1")(setq dStr (append dStr (list entRot))))
        (if(= tog17 "1")(setq dStr (append dStr (list entAre)))) 
        (if(= tog18 "1")(setq dStr (append dStr (list entLen)))) 
        (if(= tog19 "1")(setq dStr (append dStr (list entHan)))) 
        (setq dataList(append dataList(list dStr)))                                                               
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))
      )
    )                    
  )
  dataList
)







;;;--- IMAGE DATA

(defun getImgData(/ eset hdrStr en enlist entLyr entPt entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "IMAGE"))
    (setq eset(ssget "X" (list (cons 0 "IMAGE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog4  "1")(setq hdrStr (append hdrStr (list "Insertion X" "Insertion Y" "Insertion Z"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
          (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                                    ; label       
            entPt                                                                   ; arrow point 
            (polar entPt (* pi 1.25) (* 2.0 (getvar "textsize")))                   ; text point  
          )
        )
        
        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))        
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog4  "1")(setq dStr (append dStr entPtSTr)))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))                                                               
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)







;;;--- INSERT DATA (BLOCKS)

(defun getInsData(/ eset en enlist hdrStr dataList entName entPoint entX entY entZ entLay entRot dStr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "INSERT"))
    (setq eset(ssget "X" (list (cons 0 "INSERT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog1  "1")(setq hdrStr (append hdrStr (list "Name"))))
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog4  "1")(setq hdrStr (append hdrStr (list "Insertion X" "Insertion Y" "Insertion Z"))))
      (if(= tog16 "1")(setq hdrStr (append hdrStr (list "Rotation"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up some counters
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                      
        ;;;--- Get the block name         
        (setq entName(cdr(assoc 2 enlist)))
                       
        ;;;--- Get the insertion point
        (setq entPoint(cdr(assoc 10 enlist)))
                       
        ;;;--- Get the X, Y, and Z coordinates of the insertion point
        (setq entX (rtos (car   entPoint) 2 4))
        (setq entY (rtos (cadr  entPoint) 2 4))
        (setq entZ (rtos (caddr entPoint) 2 4))
                       
        ;;;--- Save the layer
        (setq entLay(cdr(assoc 8 enlist)))
                       
        ;;;--- Get the rotation of the entity
        (if(assoc 50 enlist)
           (setq entRot(angtos(cdr(assoc 50 enlist))0 4)) 
           (setq entRot "0.0000")
        )
        
        ;;;--- Get the color of the entity       
        (if(cdr(assoc 62 enlist))(setq entClr(cdr(assoc 62 enlist)))(setq entClr "BYLAYER"))
        (if(= 'INT (type entClr))(setq entClr(itoa  entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                            ; label       
            entPoint                                                        ; arrow point 
            (polar entPoint (* pi 1.25) (* 4.0 (getvar "textsize")))        ; text point  
          )
        )        
                      
        ;;;--- Build a data string
        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))        
        (if(= tog1  "1")(setq dStr (append dStr (list entName))))
        (if(= tog2  "1")(setq dStr (append dStr (list entLay))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog4  "1")(setq dStr (append dStr (list entX entY entZ))))
        (if(= tog16 "1")(setq dStr (append dStr (list entRot))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))                                                               
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)






;;;--- LINE DATA

(defun getLinData(/ eset en enlist hdrStr entLyr entPt entPtStr entEPt entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "LINE"))
    (setq eset(ssget "X" (list (cons 0 "LINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "Start X" "Start Y" "Start Z"))))
      (if(= tog6  "1")(setq hdrStr (append hdrStr (list "End X" "End Y" "End Z"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "LineType"))))
      (if(= tog16 "1")(setq hdrStr (append hdrStr (list "Rotation"))))
      (if(= tog18 "1")(setq hdrStr (append hdrStr (list "Length"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                     
      ;;;--- Set up a counter
      (setq cntr 0)
                    
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                               
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (setq entEPt(cdr(assoc 11 enlist)))
        (setq entEPtStr(list (rtos(car entEPt)2 4) (rtos(cadr entEPt)2 4) (rtos(caddr entEPt)2 4)))
        (setq entRot(angtos (angle entPt entEPt)0 4))
        (setq entLen(rtos (distance entPt entEPt) 2 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )

        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )

        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                                  ; label       
            (polar entPt (angle entPt entEPt) (/ (distance entPt entEPt) 2.0))    ; arrow point 
            (polar                                                                ; text point  
              (polar entPt (angle entPt entEPt) (/ (distance entPt entEPt) 2.0))
              (+ (* pi 0.5)(angle entPt entEPt))
              (* 4.0 (getvar "textsize"))
            )
          )
        )                

        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))        
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog5  "1")(setq dStr (append dStr entPtSTr)))
        (if(= tog6  "1")(setq dStr (append dStr entEPtSTr)))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty))))
        (if(= tog16 "1")(setq dStr (append dStr (list entRot))))
        (if(= tog18 "1")(setq dStr (append dStr (list entLen))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr))) 
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)






;;;--- LWPOLYLINE DATA

(defun getLwpData(/ eset en enlist hdrStr dstr vStr entPt entPtStr entEPt entEPtStr
                    entLyr ptList entLty entClr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "LWPOLYLINE"))
    (setq eset(ssget "X" (list (cons 0 "LWPOLYLINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "Start X" "Start Y"))))
      (if(= tog6  "1")(setq hdrStr (append hdrStr (list "End X" "End Y"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "LineType"))))
      (if(= tog17 "1")(setq hdrStr (append hdrStr (list "Area"))))
      (if(= tog18 "1")(setq hdrStr (append hdrStr (list "Length"))))
      (if(= tog13 "1")(setq hdrStr (append hdrStr (list "Vertex X" "Vertex Y"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                      
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))

        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))

        ;;;--- Get the points in a list
        (setq ptList(list))
        (foreach a enlist(if(= (car a) 10)(setq ptList(append ptList (list (cdr a))))))

        (setq entPt(car ptList))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4)))
        (setq entEPt(car(reverse ptList)))
        (setq entEPtStr(list (rtos(car entEPt)2 4) (rtos(cadr entEPt)2 4)))
        (command "_area" "Object" en)
        (setq entAre(rtos (getvar "area") 2 4))
        (setq entLen(rtos (getvar "perimeter") 2 4))

        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )

        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (setq pt1 (car ptList) pt2 (cadr ptList))
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                      ; label       
            (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.0))    ; arrow point
            (polar                                                    ; text point  
              (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.0))
              (+ (* pi 0.5)(angle pt1 pt2))
              (* 4.0 (getvar "textsize"))
            )
          )
        )                

        (setq dStr(list) vStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum)))) vStr (append vStr (list " "))))       
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))    vStr (append vStr (list " "))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))    vStr (append vStr (list " "))))
        (if(= tog5  "1")(setq dStr (append dStr     entPtSTr)     vStr (append vStr (list " " " "))))
        (if(= tog6  "1")(setq dStr (append dStr    entEPtSTr)     vStr (append vStr (list " " " "))))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty))    vStr (append vStr (list " "))))
        (if(= tog17 "1")(setq dStr (append dStr (list entAre))    vStr (append vStr (list " "))))
        (if(= tog18 "1")(setq dStr (append dStr (list entLen))    vStr (append vStr (list " "))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))    vStr (append vStr (list " "))))
        (if(= tog13 "1")(setq dStr (append dStr (list "-" "-"))))
        (setq dataList(append dataList(list dStr)))

        (if(= tog13 "1")
          (progn
            (foreach a ptList
              (progn
                (setq nStr(append vStr (list (rtos(car a)2 4) (rtos(cadr a)2 4))))
                (setq dataList(append dataList(list nStr)))
              )
            )
          )
        )  
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)







;;;--- MLINE DATA

(defun getMliData(/ eset en enlist hdrStr dstr vStr entPt entPtStr entEPt entEPtStr
                    entLyr ptList entLty entClr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "MLINE"))
    (setq eset(ssget "X" (list (cons 0 "MLINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog9  "1")(setq hdrStr (append hdrStr (list "Style"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "Start X" "Start Y" "Start Z"))))
      (if(= tog6  "1")(setq hdrStr (append hdrStr (list "End X" "End Y" "Start Z"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "LineType"))))
      (if(= tog11 "1")(setq hdrStr (append hdrStr (list "Width"))))
      (if(= tog18 "1")(setq hdrStr (append hdrStr (list "Length"))))
      (if(= tog13 "1")(setq hdrStr (append hdrStr (list "Vertex X" "Vertex Y" "Vertex Z"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))

        ;;;--- Get the data from the group codes               
        (setq entSty(cdr(assoc 2 enlist)))

        ;;;--- Get the data from the group codes               
        (setq entWth(rtos(cdr(assoc 40 enlist))2 4))

        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))

        ;;;--- Get the points in a list
        (setq ptList(list))
        (foreach a enlist(if(= (car a) 11)(setq ptList(append ptList (list (cdr a))))))
        ;;;--- Get the length
        (setq entLen 0)
        (setq spt(car ptList))
        (foreach a (cdr ptList)
          (setq entLen(+ entLen (distance spt a)))
          (setq spt a)
        )
        (setq entLen(rtos entLen 2 4))
        (setq entEPt(car(reverse ptList)))
        (setq entEPtStr(list (rtos(car entEPt)2 4) (rtos(cadr entEPt)2 4) (rtos(caddr entEPt)2 4)))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (setq pt1 (car ptList) pt2 (cadr ptList))
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                      ; label       
            (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.0))    ; arrow point
            (polar                                                    ; text point  
              (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.0))
              (+ (* pi 0.5)(angle pt1 pt2))
              (* 4.0 (getvar "textsize"))
            )
          )
        )                
        
        (setq dStr(list) vStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum)))) vStr(append vStr (list " "))))
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr)) vStr (append vStr (list " "))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr)) vStr (append vStr (list " "))))
        (if(= tog9  "1")(setq dStr (append dStr (list entSty)) vStr (append vStr (list " "))))
        (if(= tog5  "1")(setq dStr (append dStr entPtSTr)      vStr (append vStr (list " " " " " "))))
        (if(= tog6  "1")(setq dStr (append dStr entEPtSTr)     vStr (append vStr (list " " " " " "))))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty)) vStr (append vStr (list " "))))
        (if(= tog11 "1")(setq dStr (append dStr (list entWth)) vStr (append vStr (list " "))))
        (if(= tog18 "1")(setq dStr (append dStr (list entLen)) vStr (append vStr (list " "))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (if(= tog13 "1")(setq dStr (append dStr (list " " " " " "))))
        (setq dataList(append dataList(list dStr)))
        (if(= tog13 "1")
           (progn
              (foreach a ptList
                (progn
                  (setq nStr(append vStr (list (rtos(car a)2 4) (rtos(cadr a)2 4) (rtos(caddr a)2 4))))
                  (setq dataList(append dataList(list nStr)))
                )
              )
           )
        )  
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)







;;;--- Function to break up an mtext entity into individual strings in a list

(defun mtx(mtxtVal)
  ;;;--- Strip trailing spaces
  (while(= (substr mtxtVal (strlen mtxtVal) 1) " ")
    (setq mtxtVal(substr mtxtVal 1 (-(strlen mtxtVal)1)))
  )
  (if(= (substr mtxtVal 1 2) "{\\")
    (progn
      (setq mtxtVal(substr mtxtVal 1 (-(strlen mtxtVal)1)))
      (setq mcnt 1)
      (while(/= (substr mtxtVal mcnt 1) ";")
        (setq mcnt(+ mcnt 1))
      )
      (setq mtxtVal(substr mtxtVal (+ mcnt 1)))
    )
  )    
  (setq mtxtLines(list))
  (setq mstpt 1 mcnt 1)
  (setq msrf "\\P")
  (while (< mcnt (+(-(strlen mtxtVal) (strlen msrf))1))
     (setq mtstStr(substr mtxtVal mcnt (strlen msrf)))
     (if(= mtstStr msrf)
       (progn
         (setq mtxtLines(append mtxtLines (list (substr mtxtVal mstpt (- mcnt mstpt)))))
         (setq mstpt (+ mcnt 2))
         (setq mcnt(+ mcnt 1))
       )
     )
     (setq mcnt(+ mcnt 1))
  )
  (if(< mstpt (strlen mtxtVal))
     (setq mtxtLines(append mtxtLines (list (substr mtxtVal mstpt))))
  )
  mtxtLines
)





;;;--- Function to strip control characters from mtext

(defun stripIt(sTxt / stripList sa sCntr sChk)
  (setq stripList (list "\\O" "\\o" "\\L" "\\l" "\\~" "\\A" "\\P"))
  (foreach sa stripList
    (setq sCntr 1)  
    (while(< sCntr (strlen sTxt))
      (setq sChk(substr sTxt sCntr 2))
      (if(= sChk sa)
        (progn
          (if(= sCntr 1)
            (setq sTxt (substr sTxt 3))
            (setq sTxt (strcat (substr sTxt 1 (- sCntr 1)) (substr sTxt (+ sCntr 2))))
          )
        )
      )
      (setq sCntr(+ sCntr 1))
    )
  )
  sTxt
)








;;;--- MTEXT DATA

(defun getMtxData(/ eset en enlist hdrStr entLyr entSty bigStr bigList
                    entPt entPtStr entVal entHgt entWth entRot dStr vStr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "MTEXT"))
    (setq eset(ssget "X" (list (cons 0 "MTEXT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog4  "1")(setq hdrStr (append hdrStr (list "Insertion X" "Insertion Y" "Insertion Z"))))
      (if(= tog8  "1")(setq hdrStr (append hdrStr (list "Text Value"))))
      (if(= tog9  "1")(setq hdrStr (append hdrStr (list "Style"))))
      (if(= tog11 "1")(setq hdrStr (append hdrStr (list "Height"))))
      (if(= tog12 "1")(setq hdrStr (append hdrStr (list "Width"))))
      (if(= tog16 "1")(setq hdrStr (append hdrStr (list "Rotation"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))

        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))

        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entSty(cdr(assoc 7 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (if(assoc 3 enlist)
           (progn
              (setq bigStr "")
              (foreach a enlist(if(= (car a) 3)(setq bigStr(strcat bigStr (cdr a))))) 
              (setq bigStr(strcat bigStr (cdr(assoc 1 enlist))))
           )
           (setq bigStr(cdr(assoc 1 enlist)))
        )

        ;;;--- Get rid of the font control data inside brackets {}
        (setq bigList(mtx bigStr))
        (setq entVal "")
        (foreach a bigList
           (setq entVal(strcat entVal " " a))
        )
        ;;;--- Get rid of the control characters
        (setq entVal(stripIt entVal))
        ;;;--- Strip prefixed spaces
        (while (= (substr entVal 1 1) " ")(setq entVal(substr entVal 2)))
        ;;;--- Strip suffixed spaces
        (while (= (substr entVal (strlen entVal) 1) " ")(setq entVal(substr entVal 1 (-(strlen entVal)1))))        
       
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (if(cdr(assoc 62 enlist))(setq entClr(cdr(assoc 62 enlist)))(setq entClr "BYLAYER"))
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))
        (setq entHgt(rtos(cdr(assoc 43 enlist))2 4))
        (setq entWth(rtos(cdr(assoc 42 enlist))2 4))                         
        (setq entRot(angtos (cdr(assoc 50 enlist)) 0 4))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                    ; label       
            entPt                                                   ; arrow point 
            (polar entPt (* pi 1.25) (* 4.0 (getvar "textsize")))   ; text point  
          )
        )                
        

        (setq dStr(list) vStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum)))) vStr (append vStr (list " "))))
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))  vStr (append vStr (list " "))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))  vStr (append vStr (list " "))))
        (if(= tog4  "1")(setq dStr (append dStr entPtSTr)       vStr (append vStr (list " " " "))))
        (if(= tog8  "1")(setq dStr (append dStr (list entVal))  vStr (append vStr (list " "))))
        (if(= tog9  "1")(setq dStr (append dStr (list entSty))))
        (if(= tog11 "1")(setq dStr (append dStr (list entHgt))))
        (if(= tog12 "1")(setq dStr (append dStr (list entWth))))
        (if(= tog16 "1")(setq dStr (append dStr (list entRot))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))        
      )
    )                    
  )
  dataList
)





;;;--- POINT DATA

(defun getPoiData(/ eset en enlist hdrStr entLyr entPt entPtStr entClr entRot dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "POINT"))
    (setq eset(ssget "X" (list (cons 0 "POINT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog4  "1")(setq hdrStr (append hdrStr (list "Insertion X" "Insertion Y" "Insertion Z"))))
      (if(= tog16 "1")(setq hdrStr (append hdrStr (list "Rotation"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (if(cdr(assoc 62 enlist))(setq entClr(cdr(assoc 62 enlist)))(setq entClr "BYLAYER"))
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))
        (if(cdr(assoc 50 enlist))(setq entRot(rtos(cdr(assoc 50 enlist))2 4))(setq entRot "0.0000"))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                    ; label       
            entPt                                                   ; arrow point 
            (polar entPt (* pi 1.25) (* 4.0 (getvar "textsize")))   ; text point  
          )
        )        
                         
        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog4  "1")(setq dStr (append dStr entPtSTr)))
        (if(= tog16 "1")(setq dStr (append dStr (list entRot))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))                                                               
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)






;;;--- POLYLINE DATA

(defun getPolData(/ eset en enlist hdrStr dstr vStr entPt entPtStr entEPt entEPtStr cntr entLyr ptList entLty entClr en2 enlist2)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "POLYLINE"))
    (setq eset(ssget "X" (list (cons 0 "POLYLINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "Start X" "Start Y" "Start Z"))))
      (if(= tog6  "1")(setq hdrStr (append hdrStr (list "End X" "End Y" "End Z"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "LineType"))))
      (if(= tog17 "1")(setq hdrStr (append hdrStr (list "Area"))))
      (if(= tog18 "1")(setq hdrStr (append hdrStr (list "Length"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (if(= tog13 "1")(setq hdrStr (append hdrStr (list "Vertex X" "Vertex Y" "Vertex Z"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))

        ;;;--- Get the points in a list
        (setq ptList(list))

        ;;;--- Get the sub entities name  
        (setq en2(entnext en)) 
                 
        ;;;--- Get the dxf group codes of the subentity
        (setq enlist2(entget en2))
                
        ;;;--- While the polyline has a next vertice
        (while (not (equal (cdr(assoc 0 (entget(entnext en2))))"SEQEND"))
                    
           ;;;--- Get the next subentity 
           (setq en2(entnext en2))
                    
           ;;;--- Get its dxf group codes
           (setq enlist2(entget en2))
                      
           ;;;--- Check to make sure it is not a spline reference point 
           (if(/= 16 (cdr(assoc 70 enlist2)))
                     
              ;;;--- It is a vertex, save the point in a list [ptlist]
              (setq ptList(append ptList (list (cdr(assoc 10 enlist2)))))
                     
           )
        )   

        (setq entPt(car ptList))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (setq entEPt(car(reverse ptList)))
        (setq entEPtStr(list (rtos(car entEPt)2 4) (rtos(cadr entEPt)2 4) (rtos(caddr entEPt)2 4)))
        (command "_area" "Object" en)
        (setq entAre(rtos (getvar "area") 2 4))
        (setq entLen(rtos (getvar "perimeter") 2 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (setq pt1 (car ptList) pt2 (cadr ptList))
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                      ; label       
            (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.0))    ; arrow point
            (polar                                                    ; text point  
              (polar pt1 (angle pt1 pt2) (/ (distance pt1 pt2) 2.0))
              (+ (* pi 0.5)(angle pt1 pt2))
              (* 4.0 (getvar "textsize"))
            )
          )
        )
                         
        (setq dStr(list) vStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum)))) vStr (append vStr (list " "))))        
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr)) vStr (append vStr (list " "))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr)) vStr (append vStr (list " "))))
        (if(= tog5  "1")(setq dStr (append dStr entPtSTr)      vStr (append vStr (list " " " " " "))))
        (if(= tog6  "1")(setq dStr (append dStr entEPtSTr)     vStr (append vStr (list " " " " " "))))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty)) vStr (append vStr (list " "))))
        (if(= tog17 "1")(setq dStr (append dStr (list entAre)) vStr (append vStr (list " "))))
        (if(= tog18 "1")(setq dStr (append dStr (list entLen)) vStr (append vStr (list " "))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan)) vstr (append vStr (list " "))))
        (if(= tog13 "1")(setq dStr (append dStr (list " " " " " "))))
        
        (setq dataList(append dataList(list dStr)))
        (if(= tog13 "1")
           (progn
              (foreach a ptList
                (progn
                  (setq nStr(append vStr (list (rtos(car a)2 4) (rtos(cadr a)2 4) (rtos(caddr a)2 4))))
                  (setq dataList(append dataList(list nStr)))
                )
              )
           )
        )  
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))        
      )
    )                    
  )
  dataList
)





;;;--- SOLID DATA

(defun getSolData(/ eset en enlist hdrStr entLyr entPt entPtStr entEPt entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "SOLID"))
    (setq eset(ssget "X" (list (cons 0 "SOLID"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "Line Type"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "1st X" "1st Y" "1st Z"))))
      (setq c(list "2nd X" "2nd Y" "2nd Z" "3rd X" "3rd Y" "3rd Z" "4th X" "4th Y" "4th Z"))  
      (if(= tog13 "1")(setq hdrStr (append hdrStr c)))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (setq entPt11(cdr(assoc 11 enlist)))
        (setq entPtStr11(list (rtos(car entPt11)2 4) (rtos(cadr entPt11)2 4) (rtos(caddr entPt11)2 4)))
        (setq entPt12(cdr(assoc 12 enlist)))
        (setq entPtStr12(list (rtos(car entPt12)2 4) (rtos(cadr entPt12)2 4) (rtos(caddr entPt12)2 4)))
        (setq entPt13(cdr(assoc 13 enlist)))
        (setq entPtStr13(list (rtos(car entPt13)2 4) (rtos(cadr entPt13)2 4) (rtos(caddr entPt13)2 4)))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                    ; label       
            entPt                                                   ; arrow point 
            (polar entPt (* pi 0.75) (* 4.0 (getvar "textsize")))   ; text point  
          )
        )        
                         
        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))        
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty))))
        (if(= tog5  "1")(setq dStr (append dStr entPtStr)))
        (if(= tog13  "1")(setq dStr (append dStr (append entPtStr11 (append entPtStr12 entPtStr13)))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))                                                               
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)





;;;--- TEXT DATA

(defun getTxtData(/ eset en enlist hdrStr cntr entLyr entSty bigStr bigList
                    entPt entPtStr entVal entHgt entWth entRot dStr vStr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "TEXT"))
    (setq eset(ssget "X" (list (cons 0 "TEXT"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "Start X" "Start Y" "Start Z"))))
      (if(= tog8  "1")(setq hdrStr (append hdrStr (list "Text Value"))))
      (if(= tog9  "1")(setq hdrStr (append hdrStr (list "Style"))))
      (if(= tog11 "1")(setq hdrStr (append hdrStr (list "Height"))))
      (if(= tog16 "1")(setq hdrStr (append hdrStr (list "Rotation"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entSty(cdr(assoc 7 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (setq entVal(cdr(assoc 1 enlist)))

        (if(cdr(assoc 62 enlist))(setq entClr(cdr(assoc 62 enlist)))(setq entClr "BYLAYER"))
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))
        (setq entHgt(rtos(cdr(assoc 40 enlist))2 4))
        (setq entRot(angtos(cdr(assoc 50 enlist))0 4))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                    ; label       
            entPt                                                   ; arrow point 
            (polar entPt (* pi 1.25) (* 4.0 (getvar "textsize")))   ; text point  
          )
        )        

        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))        
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog5  "1")(setq dStr (append dStr entPtSTr)))
        (if(= tog8  "1")(setq dStr (append dStr (list entVal))))
        (if(= tog9  "1")(setq dStr (append dStr (list entSty))))
        (if(= tog11 "1")(setq dStr (append dStr (list entHgt))))
        (if(= tog16 "1")(setq dStr (append dStr (list entRot))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))
      )
    )                    
  )
  dataList
)




;;;--- TRACE DATA

(defun getTraData(/ eset en enlist hdrStr entLyr entPt entPtStr entEPt entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "TRACE"))
    (setq eset(ssget "X" (list (cons 0 "TRACE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "Line Type"))))
      (if(= tog5  "1")(setq hdrStr (append hdrStr (list "1st X" "1st Y" "1st Z"))))
      (setq c(list "2nd X" "2nd Y" "2nd Z" "3rd X" "3rd Y" "3rd Z" "4th X" "4th Y" "4th Z"))  
      (if(= tog13 "1")(setq hdrStr (append hdrStr c)))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (setq entPt11(cdr(assoc 11 enlist)))
        (setq entPtStr11(list (rtos(car entPt11)2 4) (rtos(cadr entPt11)2 4) (rtos(caddr entPt11)2 4)))
        (setq entPt12(cdr(assoc 12 enlist)))
        (setq entPtStr12(list (rtos(car entPt12)2 4) (rtos(cadr entPt12)2 4) (rtos(caddr entPt12)2 4)))
        (setq entPt13(cdr(assoc 13 enlist)))
        (setq entPtStr13(list (rtos(car entPt13)2 4) (rtos(cadr entPt13)2 4) (rtos(caddr entPt13)2 4)))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )
        
        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                    ; label       
            entPt                                                   ; arrow point 
            (polar entPt (* pi 1.25) (* 4.0 (getvar "textsize")))   ; text point  
          )
        )        
                         
        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))       
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty))))
        (if(= tog5  "1")(setq dStr (append dStr entPtStr)))
        (if(= tog13  "1")(setq dStr (append dStr (append entPtStr11 (append entPtStr12 entPtStr13)))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)




;;;--- XLINE DATA

(defun getXliData(/ eset en enlist hdrStr entLyr entPt entPtStr entEPt entLty entClr dstr)

  ;;;--- Set up an empty list
  (setq dataList(list))

  ;;;--- If that type of entity exist in drawing
  (if (= tog20 "1")
    (setq eset(windowedSelection "XLINE"))
    (setq eset(ssget "X" (list (cons 0 "XLINE"))))
  )

  (if eset
    (progn

      ;;;--- Build a header
      (setq hdrStr(list))
      (if labelTog    (setq hdrStr (append hdrStr (list "Label"))))      
      (if(= tog2  "1")(setq hdrStr (append hdrStr (list "Layer"))))
      (if(= tog3  "1")(setq hdrStr (append hdrStr (list "Color"))))
      (if(= tog4  "1")(setq hdrStr (append hdrStr (list "Insertion X" "Insertion Y" "Insertion Z"))))
      (if(= tog10 "1")(setq hdrStr (append hdrStr (list "LineType"))))
      (if(= tog16 "1")(setq hdrStr (append hdrStr (list "Rotation"))))
      (if(= tog19 "1")(setq hdrStr (append hdrStr (list "Handle"))))
      (setq dataList(append dataList (list hdrStr)))
                       
      ;;;--- Set up a counter
      (setq cntr 0)
                     
      ;;;--- Loop through each entity
      (while (< cntr (sslength eset))
                                
        ;;;--- Get the entity's name
        (setq en(ssname eset cntr))
                         
        ;;;--- Get the DXF group codes of the entity
        (setq enlist(entget en))
                           
        ;;;--- Get the data from the group codes               
        (setq entLyr(cdr(assoc 8 enlist)))
        (setq entPt(cdr(assoc 10 enlist)))
        (setq entPtStr(list (rtos(car entPt)2 4) (rtos(cadr entPt)2 4) (rtos(caddr entPt)2 4)))
        (setq entRot(angtos (angle (list 0 0)(cdr(assoc 11 enlist)))0 4))
        (if(cdr(assoc 6 enlist))
          (setq entLty(cdr(assoc 6 enlist)))
          (setq entLty "BYLAYER")
        )
        (if(cdr(assoc 62 enlist))
           (setq entClr(cdr(assoc 62 enlist)))
           (setq entClr "BYLAYER")
        )
        (if(= 'INT (type entClr))(setq entClr (itoa entClr)))

        ;;;--- Get the handle
        (if (not (setq entHan(cdr(assoc 5 enlist))))
           (setq entHan "OFF")
        )

        ;;;--- Send a label and two points to the writeLabel function
        (if labelTog
          (writeLabel
            (strcat lblPrefix (itoa lblStrtNum))                    ; label       
            entPt                                                   ; arrow point 
            (polar entPt (* pi 1.25) (* 4.0 (getvar "textsize")))   ; text point  
          )
        )        
                         
        (setq dStr(list))
        (if labelTog    (setq dStr (append dStr (list (strcat lblPrefix (itoa lblStrtNum))))))        
        (if(= tog2  "1")(setq dStr (append dStr (list entLyr))))
        (if(= tog3  "1")(setq dStr (append dStr (list entClr))))
        (if(= tog4  "1")(setq dStr (append dStr entPtSTr)))
        (if(= tog10 "1")(setq dStr (append dStr (list entLty))))
        (if(= tog16 "1")(setq dStr (append dStr (list entRot))))
        (if(= tog19 "1")(setq dStr (append dStr (list entHan))))
        (setq dataList(append dataList(list dStr)))                                                               
        (setq cntr (+ cntr 1))
        (if labelTog(setq lblStrtNum(+ lblStrtNum 1)))          
      )
    )                    
  )
  dataList
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                 ;;;
;;;                                                                                 ;;;
;;;--- END OF Functions to get entity types                                         ;;;
;;;                                                                                 ;;;
;;;                                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;--- Function to determine which properties were selected

(defun setAble()
  (setq idx(atoi(get_tile "entityType")))
  (cond
     ( (= idx 0)  ;;; ARC
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius
           (mode_tile "tog12" 0)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "ARC-")
       )
     )
     ( (= idx 1)  ;;; ATTRIB
       (progn
           (mode_tile "tog1"  0)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  0)      ;;; Tag
           (mode_tile "tog8"  0)      ;;; Text Value
           (mode_tile "tog9"  0)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "ATT-")         
       )
     )
     ( (= idx 2)  ;;; CIRCLE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius
           (mode_tile "tog12" 0)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 0)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "CIR-")         
       )
     )
     ( (= idx 3)  ;;; ELLIPSE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 0)      ;;; Major Axis
           (mode_tile "tog15" 0)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 0)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "ELL-")         
       )
     )
     ( (= idx 4)  ;;; IMAGE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "IMG-")         
       )
     )
     ( (= idx 5)  ;;; INSERT
       (progn
           (mode_tile "tog1"  0)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "INS-")         
       )
     )
     ( (= idx 6)  ;;; LINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "LIN-")         
       )
     )
     ( (= idx 7)  ;;; LWPOLYLINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 0)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "LWP-")         
       )
     )
     ( (= idx 8)  ;;; MLINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  0)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius/Width
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "MLI-")         
       )
     )
     ( (= idx 9)  ;;; MTEXT
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  0)      ;;; Text Value
           (mode_tile "tog9"  0)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius/Height
           (mode_tile "tog12" 0)      ;;; Diameter/Width
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "MTX-")         
       )
     )
     ( (= idx 10)  ;;; POINT
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "PNT-")         
       )
     )
     ( (= idx 11)  ;;; POLYLINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 0)      ;;; Area
           (mode_tile "tog18" 0)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "PLY-")         
       )
     )
     ( (= idx 12)  ;;; SOLID
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex/Control Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "SOL-")         
       )
     )
     ( (= idx 13)  ;;; TEXT
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  0)      ;;; Text Value
           (mode_tile "tog9"  0)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius/Height
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "TXT-")         
       )
     )
     ( (= idx 14)  ;;; TRACE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  1)      ;;; Insertion Point
           (mode_tile "tog5"  0)      ;;; Start/Center Point 
           (mode_tile "tog6"  0)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 1)      ;;; Line Type
           (mode_tile "tog11" 0)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 0)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 1)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "TRC-")         
       )
     )
     ( (= idx 15)  ;;; XLINE
       (progn
           (mode_tile "tog1"  1)      ;;; Name
           (mode_tile "tog2"  0)      ;;; Layer Name
           (mode_tile "tog3"  0)      ;;; Color
           (mode_tile "tog4"  0)      ;;; Insertion Point
           (mode_tile "tog5"  1)      ;;; Start/Center Point 
           (mode_tile "tog6"  1)      ;;; End Point
           (mode_tile "tog7"  1)      ;;; Tag
           (mode_tile "tog8"  1)      ;;; Text Value
           (mode_tile "tog9"  1)      ;;; Style
           (mode_tile "tog10" 0)      ;;; Line Type
           (mode_tile "tog11" 1)      ;;; Radius
           (mode_tile "tog12" 1)      ;;; Diameter
           (mode_tile "tog13" 1)      ;;; Vertex Points
           (mode_tile "tog14" 1)      ;;; Major Axis
           (mode_tile "tog15" 1)      ;;; Minor Axis
           (mode_tile "tog16" 0)      ;;; Rotation
           (mode_tile "tog17" 1)      ;;; Area
           (mode_tile "tog18" 1)      ;;; Length/Perimeter
           (mode_tile "tog19" 0)      ;;; Handle
           (set_tile "lblprefix" "XLI-")         
       )
     )
  )
)


;;;--- Function to draw tables and fill in the cells 
;;;    Tables have a maximum limit set by variable:  
;;;    maximumObjects                                

(defun buildTable(title dList)

  ;;;--- Set the maximum number of items in each column   
  ;;;    If you increase this autocad's performance could 
  ;;;    suffer and eventually crash.                     
  (setq maximumObjects 50)

  ;;;--- Find the maximum number of characters in the columns 
  ;;;    to determine the column width                        
  (setq colWidth 0)
  (foreach a dList
    (foreach b a
      (if(> (strlen b) colWidth)(setq colWidth(strlen b)))
    )
  )

  (setq colText "")
  (repeat colWidth (setq colText (strcat colText "W")))

  ;;;--- Convert characters to inches to get column widths
  (setq tb(textbox (list(cons 0 "TEXT")(cons 1 colText)(cons 40 (getvar "textsize")))))
  (setq colWidth(- (car (cadr tb)) (car(car tb))))

  ;;;--- Get the header from the data list and remove it from the list
  (setq header(car dList))
  (setq dList(cdr dList))
  
  ;;;--- Get the model space object
  (setq acadSpace(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object))))

  ;;;--- Get the table insertion point
  (setq insPt(getpoint "\nTable Insertion Point: "))

  ;;;--- Divide the tables into 50 entities max
  (if(> (length dList) maximumObjects)
    (setq numTables(+ 1(fix(/ (length dList) (float maximumObjects)))))
    (setq numTables 1)
  )

  ;;;--- Set a list pointer
  (setq itemNo 0)

  ;;;--- Create tables with a set maximum objects
  (repeat numTables

    ;;;--- Get the length of this table
    (if(> (- (length dList) itemNo) (- maximumObjects 1))
      (setq tableLength maximumObjects)
      (setq tableLength (- (length dList) itemNo))
    )

    ;;;--- Set the number of columns
    (setq numColumns (length (car dList)))

    ;;;--- ReSet the start point if necessary
    (if(> itemNo 0)
      (setq insPt(polar insPt 0 (+ (* 1.5 (getvar "textsize"))(* colWidth numColumns))))
    )

    ;;;--- Set the number of rows to the table length plus two for title and header
    (setq numRows (+ tableLength 2))

    ;;;--- Draw the table
    (setq myTable
      (vla-AddTable 
        acadSpace                                            ;model space       
        (vlax-3d-point insPt)                                ;insertion point   
        numRows                                              ;number of rows    
        numColumns                                           ;number of columns 
        (* 1.5 (getvar "textsize"))                          ;row height        
        colWidth                                             ;column width      
      )
    )

    ;;;--- Set the text height for all cells
    (setq row -1)
    (repeat numRows
      (setq row(+ row 1) col -1)
      (repeat numColumns
        (setq col(+ col 1))
        (vla-setcelltextheight myTable row col (getvar "textsize"))
      )
    )

    ;;;--- Place the main title    vla-setText syntax -> tblName col row header
    (vla-setText mytable 0 0 title)

    ;;;--- Place the headers 
    (setq col 1 row 0)
    (foreach a header
      (vla-setText myTable col row a)
      (setq row(+ row 1))
    )

    ;;;--- Set up variables to add the cell values from the data list
    (setq row 0 col 2 cnt (length dList) cntr (* 1.0 itemNo) str nil)

    ;;;--- Cycle through the list
    (repeat tableLength

      ;;;--- Get the nth item in the list
      (setq a(nth itemNo dList))

      ;;;--- Increment to get the next item 
      (setq itemNo(+ itemNo 1))

      ;;;--- If this is not the first cycle...display a percentage complete
      (if(> cntr 0.0)
        (progn
          (if str(repeat(strlen str)(princ (chr 8))))
          (setq str(strcat (rtos(* 100(/ cntr cnt))2 0) "%"))
          (princ str)
          (princ)
        ) 
      )

      ;;;--- Increment the percentage counter
      (setq cntr(+ cntr 1.0))

      ;;;--- Place a row of cell values in the table
      (foreach b a
        (vla-setText myTable col row b)
        (setq row(+ row 1))
      )

      ;;;--- Increment to the next column
      (setq col(+ col 1))

      ;;;--- Reset the row
      (setq row 0)
    )

    ;;;--- Release the table object
    (vlax-release-object myTable)
    (if str(repeat(strlen str)(princ (chr 8))))
    (princ "100%")
  )

  ;;;--- Release the autocad object
  (vlax-release-object acadSpace)    

) 


;;;--- Function to enable or disable the layer list
(defun toggleLabel()
  (if(= (get_tile "labeltog") "1")
    (progn
      (mode_tile "layerlist" 0)
      (mode_tile "lblprefix" 0)
      (mode_tile "lblstrtnum" 0)
    )
    (progn
      (mode_tile "layerlist" 1)
      (mode_tile "lblprefix" 1)
      (mode_tile "lblstrtnum" 1)
    )
  )
)


;;;--- Function save the dialog box selections

(defun saveVars()
  (setq entType(atoi(get_tile "entityType")))
  (setq tog1 (get_tile "tog1"))
  (setq tog2 (get_tile "tog2"))
  (setq tog3 (get_tile "tog3"))
  (setq tog4 (get_tile "tog4"))
  (setq tog5 (get_tile "tog5"))
  (setq tog6 (get_tile "tog6"))
  (setq tog7 (get_tile "tog7"))
  (setq tog8 (get_tile "tog8"))
  (setq tog9 (get_tile "tog9"))
  (setq tog10(get_tile "tog10"))
  (setq tog11(get_tile "tog11"))
  (setq tog12(get_tile "tog12"))
  (setq tog13(get_tile "tog13"))
  (setq tog14(get_tile "tog14"))
  (setq tog15(get_tile "tog15"))
  (setq tog16(get_tile "tog16"))
  (setq tog17(get_tile "tog17"))
  (setq tog18(get_tile "tog18"))
  (setq tog19(get_tile "tog19"))
  (setq tog20(get_tile "tog20"))
  (setq labelTog(get_tile "labeltog"))
  (if(= labelTog "1")
    (progn
      (setq labelTog T)
      (setq layerIndex(atoi(get_tile "layerlist")))
      (setq layerChoice(nth layerIndex layerList))
    )
    (setq labelTog nil)
  )
  (setq lblPrefix(get_tile "lblprefix"))
  (setq lblStrtNum(fix(atof(get_tile "lblstrtnum"))))

  ;;;--- Save the settings to be used as defaults next time
  (setvar "useri1" entType)
  (setq togVal 0)
  (if(= tog1  "1")(setq togVal(+ togVal 1)))
  (if(= tog2  "1")(setq togVal(+ togVal 2)))
  (if(= tog3  "1")(setq togVal(+ togVal 4)))
  (if(= tog4  "1")(setq togVal(+ togVal 8)))
  (if(= tog5  "1")(setq togVal(+ togVal 16)))
  (if(= tog6  "1")(setq togVal(+ togVal 32)))
  (if(= tog7  "1")(setq togVal(+ togVal 64)))
  (if(= tog8  "1")(setq togVal(+ togVal 128)))
  (if(= tog9  "1")(setq togVal(+ togVal 256)))
  (if(= tog10 "1")(setq togVal(+ togVal 512)))
  (if(= tog11 "1")(setq togVal(+ togVal 1024)))
  (if(= tog12 "1")(setq togVal(+ togVal 2048)))
  (if(= tog13 "1")(setq togVal(+ togVal 4096)))
  (if(= tog14 "1")(setq togVal(+ togVal 8192)))
  (if(= tog15 "1")(setq togVal(+ togVal 16384)))
  (if(= tog16 "1")(setq togVal(+ togVal 32768)))
  (if(= tog17 "1")(setq togVal(+ togVal 65536)))
  (if(= tog18 "1")(setq togVal(+ togVal 131072)))
  (if(= tog19 "1")(setq togVal(+ togVal 262144)))
  (if(= tog20 "1")(setq togVal(+ togVal 524288)))
  (setvar "users1" (itoa togVal))
  (if labelTog
    (setvar "useri2" 1)
    (setvar "useri2" 0)
  )
  (if layerIndex
    (setvar "useri3" layerIndex)
  )
  (setvar "users2" lblPrefix)
  (setvar "useri4" lblStrtNum)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                 ;;;
;;;                                                                 ;;;
;;;                                                                 ;;;
;;;                                                                 ;;;
;;;               M A I N   A P P L I C A T I O N                   ;;;
;;;                                                                 ;;;
;;;                                                                 ;;;
;;;                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)

(defun EtableMain()

 ;;;--- Turn the command echo off 
 (setvar "cmdecho" 0)

  ;;;--- Save the text size for scaling purposes
  (setq txtSz(getvar "textsize"))

  ;;;--- Build a list to make a choice in the dialog box
  (setq entityType(list))
  (setq entityType
    (list
       "ARC" "ATTRIB" "CIRCLE" "ELLIPSE" "IMAGE" "INSERT (BLOCK)" "LINE" "LWPOLYLINE"
       "MLINE" "MTEXT" "POINT" "POLYLINE" "SOLID" "TEXT" "TRACE" "XLINE"
    )
  )

  ;;;--- Build a list to hold the layer names
  (setq layerList(list))
  (setq tbl(tblnext "LAYER" T))
  (while tbl
    (setq layerList(append layerList (list (cdr(assoc 2 tbl)))))
    (setq tbl(tblnext "LAYER"))
  )

  ;;;--- Load the dialog from file if found
  (setq dcl_id (load_dialog "ETABLE.dcl"))

  ;;;--- Load the dialog definition inside the DCL file
  (if (not (new_dialog "ETABLE" dcl_id))
    (progn
      (alert "The ETABLE.DCL file could not be found.\nPlease make sure this file is located within the autocad search path.")
      (exit)
    )
  )

  ;;;--- Add the entity list to the dialog box
  (start_list "entityType" 3)
  (mapcar 'add_list entityType)
  (end_list)

  ;;;--- Add the layer list to the dialog box
  (start_list "layerlist" 3)
  (mapcar 'add_list layerList)
  (end_list)

  


  ;;;--- Set the default properties for the first run
  (setAble)



  ;;;--- Set the default as the first item
  (set_tile "entityType" "0")

  ;;;--- Get the entity default if it exist
  (if(and (> (getvar "useri1") -1)(< (getvar "useri1") 16))
    (set_tile "entityType" (itoa (getvar "useri1")))
  )

  ;;;--- Get the toggle defaults if they exist
  (if(> (atoi(getvar "users1")) 0)
    (progn
      (setq togVal (atoi(getvar "users1")))
      (if(>= togVal 524288)(progn(setq togVal(- togVal 524288))(set_tile "tog20" "1")))
      (if(>= togVal 262144)(progn(setq togVal(- togVal 262144))(set_tile "tog19" "1")))
      (if(>= togVal 131072)(progn(setq togVal(- togVal 131072))(set_tile "tog18" "1")))
      (if(>= togVal  65536)(progn(setq togVal(- togVal  65536))(set_tile "tog17" "1")))
      (if(>= togVal  32768)(progn(setq togVal(- togVal  37768))(set_tile "tog16" "1")))
      (if(>= togVal  16384)(progn(setq togVal(- togVal  16384))(set_tile "tog15" "1")))
      (if(>= togVal   8192)(progn(setq togVal(- togVal   8192))(set_tile "tog14" "1")))
      (if(>= togVal   4096)(progn(setq togVal(- togVal   4096))(set_tile "tog13" "1")))
      (if(>= togVal   2048)(progn(setq togVal(- togVal   2048))(set_tile "tog12" "1")))
      (if(>= togVal   1024)(progn(setq togVal(- togVal   1024))(set_tile "tog11" "1")))
      (if(>= togVal    512)(progn(setq togVal(- togVal    512))(set_tile "tog10" "1")))
      (if(>= togVal    256)(progn(setq togVal(- togVal    256))(set_tile "tog9"  "1")))
      (if(>= togVal    128)(progn(setq togVal(- togVal    128))(set_tile "tog8"  "1")))
      (if(>= togVal     64)(progn(setq togVal(- togVal     64))(set_tile "tog7"  "1")))
      (if(>= togVal     32)(progn(setq togVal(- togVal     32))(set_tile "tog6"  "1")))
      (if(>= togVal     16)(progn(setq togVal(- togVal     16))(set_tile "tog5"  "1")))
      (if(>= togVal      8)(progn(setq togVal(- togVal      8))(set_tile "tog4"  "1")))
      (if(>= togVal      4)(progn(setq togVal(- togVal      4))(set_tile "tog3"  "1")))
      (if(>= togVal      2)(progn(setq togVal(- togVal      2))(set_tile "tog2"  "1")))
      (if(>= togVal      1)(progn(setq togVal(- togVal      1))(set_tile "tog1"  "1")))

    )
  )

  ;;;--- Get the label defaults if they exist
  (if(or(= (getvar "useri2") 0)(= (getvar "useri2") 1))
    (set_tile "labeltog" (itoa(getvar "useri2")))
  )
  (if(and (> (length layerList) (getvar "useri3"))(> (getvar "useri3") -1))
    (set_tile "layerlist" (itoa(getvar "useri3")))
    (set_tile "layerlist" "0")
  )
  (set_tile "lblprefix" (getvar "users2"))
  (set_tile "lblstrtnum" (itoa(getvar "useri4")))

  (if(= (getvar "useri2") 0)
    (progn  
      ;;;--- Disable the label tiles
      (mode_tile "layerlist" 1)
      (mode_tile "lblprefix" 1)
      (mode_tile "lblstrtnum" 1)
    )
  )




  ;;;--- If an action event occurs, do this function
  (action_tile "labeltog" "(toggleLabel)")
  (action_tile "entityType" "(setAble)")
  (action_tile "cancel" "(done_dialog 1)")
  (action_tile "accept" "(saveVars)(done_dialog 2)") 

  ;;;--- Display the dialog box
  (setq ddiag(start_dialog))

  ;;;--- Unload the dialog box from memory
  (unload_dialog dcl_id)

  ;;;--- If the cancel button was pressed - display message
  (if (= ddiag 1)
    (princ "\n \n ...ETABLE Cancelled. \n ")
  )

 ;;;--- If the "Create" button was pressed
 (if (= ddiag 2)
   (progn
         
     ;;;--- Check to see what type of entity it is 
     (cond
       ;;;--- ARC DATA
       ((= idx 0)(if(setq dataList(getArcData))(buildTable "ARC DATA" dataList)(alert "No ARCs found!")))
       ;;;--- ATTRIBUTE DATA
       ((= idx 1)(if(setq dataList(getAttData))(buildTable "ATTRIBUTE DATA" dataList)(alert "No ATTRIBUTEs found!")))
       ;;;--- CIRCLE DATA
       ((= idx 2)(if(setq dataList(getCirData)) (buildTable "CIRCLE DATA"  dataList)(alert "No CIRCLEs found!")))
       ;;;--- ELLIPSE DATA
       ((= idx 3)(if(setq dataList(getEllData)) (buildTable "ELLIPSE DATA" dataList)(alert "No ELLIPSEs found!")))                        
       ;;;--- IMAGE DATA
       ((= idx 4)(if(setq dataList(getImgData)) (buildTable "IMAGE DATA"   dataList)(alert "No IMAGEs found!")))
       ;;;--- BLOCK DATA
       ((= idx 5)(if(setq dataList(getInsData)) (buildTable "BLOCK DATA"   dataList)(alert "No BLOCKs found!")))                
       ;;;--- LINE DATA
       ((= idx 6)(if(setq dataList(getLinData)) (buildTable "LINE DATA"    dataList)(alert "No LINEs found!")))
       ;;;--- LWPOLYLINE DATA
       ((= idx 7)(if(setq dataList(getLwpData)) (buildTable "LWPOLYLINE DATA" dataList)(alert "No LWPOLYLINEs found!")))
       ;;;--- MLINE DATA
       ((= idx 8)(if(setq dataList(getMliData))(buildTable "MLINE DATA" dataList)(alert "No MLINEs found!")))
       ;;;--- MTEXT DATA
       ((= idx 9)(if(setq dataList(getMtxData))(buildTable "MTEXT DATA" dataList)(alert "No MTEXTs found!")))
       ;;;--- POINT DATA
       ((= idx 10)(if(setq dataList(getPoiData))(buildTable "POINT DATA" dataList)(alert "No POINTs found!")))
       ;;;--- POLYLINE DATA
       ((= idx 11)(if(setq dataList(getPolData))(buildTable "POLYLINE DATA" dataList)(alert "No POLYLINEs found!")))
       ;;;--- SOLID DATA
       ((= idx 12)(if(setq dataList(getSolData))(buildTable "SOLID DATA" dataList)(alert "No SOLIDs found!")))
       ;;;--- TEXT DATA
       ((= idx 13)(if(setq dataList(getTxtData))(buildTable "TEXT DATA" dataList)(alert "No TEXTs found!")))
       ;;;--- TRACE DATA
       ((= idx 14)(if(setq dataList(getTraData))(buildTable "TRACE DATA" dataList)(alert "No TRACEs found!")))
       ;;;--- XLINE DATA
       ((= idx 15)(if(setq dataList(getXliData))(buildTable "XLINE DATA" dataList)(alert "No XLINEs found!")))
     )                                                                       
   )
 )
 (princ "\n...ETABLE Complete.")
 (setvar "cmdecho" 1)
 (princ)
)
(defun C:ET()
  (etableMain)
)
(defun C:ETABLE()
  (etableMain)
)
(if(not(findfile "ETABLE.DCL"))
  (princ "\n The DCL file could not be found.  Please make sure it is in the autocad search path.")
)
(princ "ETABLE loaded!\n\nType ET or ETABLE to start the program.")(princ)