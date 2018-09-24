;;;--- SAFEX.lsp -  Explode a block and change attributes to text entities with attribute values.
;;;
;;;
;;;
;;;--- Created on 1/28/06
;;;
;;;
;;;
;;;--- Copyright 2006 by JefferyPSanders.com
;;;    All rights reserved
;;;
;;;
;;;
;;;    All questions and suggestions should be sent to jps@jefferypsanders.com
;;;
;;;
;;;
;;;--- FreeWare - Program distibuted as is without warranties or liabilities for accuracy.




;;;--- Function get attribute data from a block
;;;
;;;    Usage:  (setq myData(getAttData entityName))

(defun getAttData(en / dataList enlist blkType insPt attTag attVal group66)

  ;;;--- Set up an empty list
  (setq dataList(list))

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
            (setq insPt(assoc 10 enlist))

            ;;;--- Get the control point
            (setq cntrPt(assoc 11 enlist))

            ;;;--- Save the tag of the attribute
            (setq attTag(assoc 2 enlist))
                      
            ;;;--- Save the value of the attribute
            (setq attVal(assoc 1 enlist)) 

            ;;;--- Save the data in the main list
            (setq dataList(append dataList(list(list attval (cons 0 "ATTDEF") attTag insPt cntrPt))))

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
  dataList
)




;;;--- Main application

(defun C:SAFEX()

  ;;;--- Turn the command echo off
  (setvar "cmdecho" 0)

  ;;;--- Let the user select blocks
  (if(setq eset(ssget (list (cons 0 "INSERT"))))
    (progn
     
      ;;;--- Set up a counter
      (setq cntr 0)

      ;;;---- Cycle through each block selected
      (while(< cntr (sslength eset))

        ;;;--- Get the entity name of the nth entity in selection set
        (setq en(ssname eset cntr))

        ;;;--- If there are attributes in this entity...
        (if(setq dataList(getAttData en))
          (progn

            ;;;--- Set an undo mark for the user
            (command "undo" "mark")

            ;;;--- Explode the block
            (command "explode" en)

            ;;;--- Cycle through each attribute found in the block
            (foreach a dataList

              ;;;--- Inform the user
              (princ "\nFound Tag->")(princ(cdr(assoc 2 a)))

              ;;;--- If the attribute data matches the attribute definition data...
              (if
                (or
                  (setq newSet(ssget "X" (cdr a)))
                  (setq newSet(ssget "X" (list(cons 0 "ATTDEF")(cons 2 (cdr(assoc 2 a)))(cons 11(cdr(assoc 11 a))))))
                )
                (progn 

                  ;;;--- Get the entity name of the matching attribute definition 
                  (setq en2(ssname newSet 0))

                  ;;;--- Get the dxf group codes of the attribute definition
                  (setq enlist(entget en2))

                  ;;;--- Inform the user
                  (princ "...Found ATTDEF->")(princ(cdr(assoc 2 enlist)))

                  ;;;--- Build a text entity dxf group code list from the attribute definition plus
                  ;;;    the value from the original attribute
                  (setq newList(list (cons 0 "TEXT")))
                  (if(assoc 1  enlist)(setq newList(append newList (list (assoc 1  a)))))
                  (if(assoc 7  enlist)(setq newList(append newList (list (assoc 7  enlist)))))
                  (if(assoc 8  enlist)(setq newList(append newList (list (assoc 8  enlist)))))
                  (if(assoc 10 enlist)(setq newList(append newList (list (assoc 10 enlist)))))
                  (if(assoc 11 enlist)(setq newList(append newList (list (assoc 11 enlist)))))
                  (if(assoc 40 enlist)(setq newList(append newList (list (assoc 40 enlist)))))
                  (if(assoc 41 enlist)(setq newList(append newList (list (assoc 41 enlist)))))
                  (if(assoc 50 enlist)(setq newList(append newList (list (assoc 50 enlist)))))
                  (if(assoc 51 enlist)(setq newList(append newList (list (assoc 51 enlist)))))
                  (if(assoc 62 enlist)(setq newList(append newList (list (assoc 62 enlist)))))
                  (if(assoc 71 enlist)(setq newList(append newList (list (assoc 71 enlist)))))
                  (if(assoc 72 enlist)(setq newList(append newList (list (assoc 72 enlist)))))
                  (if(assoc 73 enlist)(setq newList(append newList (list (assoc 73 enlist)))))

                  ;;;--- Create the new text entity
                  (entmake newList)

                  ;;;--- Inform the user
                  (princ "...Replaced with Text->")(princ (cdr(assoc 1 a)))

                  ;;;--- Delete the attribute definition
                  (entdel en2)
                )
              )
            )
          )
        )

        ;;;--- Increment the counter to get the next block in the selection set
        (setq cntr(+ cntr 1))
      )
    )

    ;;;--- Else, send a message to the user
    (alert "Nothing selected!")
  )

  ;;;--- Reset the command echo
  (setvar "cmdecho" 1)

  ;;;--- Suppress the last echo for a clean exit
  (princ)
)
