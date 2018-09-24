;;;---  CSVTABLE.LSP  -  Draw a table from data inside a selected CSV file
;;;
;;;
;;;
;;;--- Copyright 2005 by JefferyPSanders.com
;;;    All rights reserved.



(defun C:CSVTABLE()

  ;;;--- Define a function to parse the comma delimited string
  ;;;    Parameter [ a ] = a comma delimited text string

  (defun parseCommas(a)

    ;;;--- Set up a counter, a data string storage var, and a list to hold the parsed data string
    (setq cnt 1 st "" pLine (list))

    ;;;--- Cycle through each character in the string  
    (while(<= cnt (strlen a))

      ;;;--- Get the first character
      (setq ch(substr a cnt 1))

      ;;;--- If the character is not a comma...
      (if(/= ch ",")

        ;;;--- Add the character to the data string [st]
        (setq st(strcat st ch) )

        ;;;--- Else, if it is a comma
        (progn

          ;;;--- Add the data string [st] to the list        
          (setq pLine(append pLine (list st)))

          ;;;--- And reset the storage var [st]
          (setq st "")
        )
      )

      ;;;--- Increment the counter to get the next character
      (setq cnt(+ cnt 1))
    )

    ;;;--- Now we need to check to see what is left at the end of the comma delimited text string
    (if(/= st ",")

      ;;;--- If there is not a comma left, add the data string [st] to the list
      (setq pLine(append pLine (list st)))

      ;;;--- Else the last char was a comma, add an empty string to the list
      (setq pLine(append pLine (list "")))
    )

    ;;;--- Return the list of data strings
    pLine
  )






  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;  Function to get the layers  ;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun getLayers(/ layList layr)

    ;;;--- Get the list of layers available in the drawing
    (setq layList(list))
    (setq layr(tblnext "LAYER" T))
    (setq layList(append layList (list (cdr(assoc 2 layr)))))
    (while (setq layr(tblnext "LAYER"))
      (setq layList(append layList (list (cdr(assoc 2 layr)))))
    )

    ;;;--- Sort the list
    (vl-sort layList '<)
  )



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;  Function to get the styles  ;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun getStyles(/ stylList styl)

    ;;;--- Get the list of styles available in the drawing
    (setq stylList(list))
    (setq styl(tblnext "STYLE" T))
    (setq stylList(append stylList (list (cdr(assoc 2 styl)))))
    (while (setq styl(tblnext "STYLE"))
      (setq stylList(append stylList (list (cdr(assoc 2 styl)))))
    )

    ;;;--- Sort the list
    (vl-sort stylList '<)
  )



  ;;;--- Function to save the dialog box selections

  (defun saveVars()
    (setq lineLay (atoi(get_tile "linelay")))
    (setq textLay (atoi(get_tile "textlay")))
    (setq textSty (atoi(get_tile "textsty")))  

    ;;;--- Get the names of the layers and style
    (setq lineLayer(nth lineLay layerList))
    (setq textLayer(nth textLay layerList))
    (setq txtStyle(nth textSty styleList))
  )





  ;;;--- Define a function to draw a chart based on a list of data

  (defun drawChart(pt1 datList)

    ;;;--- Get the proper text height
    (setq tHt(* (getvar "dimscale")(getvar "dimtxt")))

    ;;;--- Check to see if the style has a fixed height
    (if(setq tbl(tblsearch "STYLE" txtStyle)) 
      (progn
        (setq ht(cdr(assoc 40 tbl)))
        (if(= ht 0.0)
          (setq fixedHeight nil)
          (setq tHt ht fixedHeight T)
        )
      )
    )  

    ;;;--- Set up the row height [ text height * 1.75 ]
    (setq rHt(* 1.75 tHt))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;--- Find the column widths
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;--- Set up an empty list to hold the column widths
    (setq cwList(list))

    ;;;--- Cycle through each column
    (foreach a datList

      ;;;--- Set up a variable to hold the length of the text
      (setq oldLenOfText 0)

      ;;;--- Cycle through each row in the column
      (foreach b a

        ;;;--- Fix the crash when the string length is zero - Fixed on 3/31/05
        (if(= b "")(setq b " "))

        ;;;--- Use the text box function on the [ soon to be ] text entity
        (setq tb(textbox (list (cons 0 "TEXT")(cons 1 b)(cons 7 txtStyle)(cons 40 tHt)(cons 50 0.0))))

        ;;;--- Calculate the length and add the text height for a spacer
        (setq lenOfText(+ tHt(- (car(cadr tb)) (car(car tb)))))

        ;;;--- If this length is greater, reset the old length of text variable [ oldLenOfText ]
        (if(> lenOfText oldLenOfText)(setq oldLenOfText lenOfText)) 

      )

      ;;;--- Add the longest text string to the column width list [ cwlist ]
      (setq cwList(append cwList (list oldLenOfText)))
    )

    ;;;--- Update progress
    (princ "\n Calculating chart geometry...")
    (princ)

    ;;;--- Calculate the width of the chart
    (setq chW 0)
    (foreach a cwList
      (setq chW(+ chW a))
    )

    ;;;--- Calculate the height of the chart
    (setq chH 0)
    (foreach a (car datList)
      (setq chH(+ chH rHt))
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;--- Calculate the four corners of the chart
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;--- Get the top left corner
    (setq topL pt1)

    ;;;--- Get the top right corner of the chart
    (setq topR (polar topL 0 chW))

    ;;;--- Get the bottom left corner of the chart
    (setq botL (polar topL (* pi 1.5) chH))

    ;;;--- Get the bottom right corner of the chart
    (setq botR (polar topR (* pi 1.5) chH))

    ;;;--- Set the layer for lines
    (setvar "clayer" lineLayer)

    ;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;--- Draw the chart
    ;;;;;;;;;;;;;;;;;;;;;;;;;


    ;;;--- Update progress
    (princ "\n Drawing the chart...")
    (princ)

    ;;;--- Draw the outline of the chart
    (command "line" topR topL botL botR "")

    ;;;--- Draw the vertical lines
    (setq tempDis 0)
    (foreach a cwList
      (setq tempDis (+ tempDis a))
      (command "line"
        (polar topL 0 tempDis)
        (polar botL 0 tempDis)
        ""
      )
    )

    ;;;--- Draw the horizontal lines
    (setq tempDis 0)
    (repeat (length (car datList))
      (setq tempDis(+ tempDis rHt))
      (command "line"
        (polar topL (* pi 1.5) tempDis)
        (polar topR (* pi 1.5) tempDis)
        ""
      )
    )

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;--- Set the text layer and style
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (setvar "clayer" textLayer)
    (setvar "textstyle" txtStyle)


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;--- Create the text entities
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;--- Update progress
    (princ "\n Processing cell values...")

    ;;;--- Find the first row
    (setq tpt(polar topL (* pi 1.5) rHt))

    ;;;--- Make a copy of this point
    (setq spt tpt)

    ;;;--- Cycle through each column   
    (foreach a cwList

      ;;;--- Find the center of the column
      (setq x(+ (car tpt) (/ a 2.0)))

      ;;;--- Go up a little to clear the text from the line
      (setq y(+ (cadr tpt) (/ (- rHt tHt)2.0)))

      ;;;--- Cycle through each row in the column
      (foreach b (car datList)

        ;;;--- Insert the text
        (if fixedHeight
          (command "text" "C" (list x y) 0 b)
          (command "text" "C" (list x y) tHt 0 b)                         
        )

        ;;;--- Move down to the new row
        (setq y(- y rHt))
      )

      ;;;--- Move right...to the next column
      (setq tpt(polar spt 0 a))

      ;;;--- Save a copy of this point
      (setq spt tpt)

      ;;;--- Done with the first column so get rid of it
      (setq datList(cdr datList))
    )
  )






  ;;;--- Main application



  ;;;--- Turn the command echo off
  (setvar "cmdecho" 0)

  ;;;--- Save the current snap settings
  (setq oldOs(getvar "osmode"))

  ;;;--- Turn the osnaps off
  (setvar "osmode" 0)

  ;;;--- Let the user select the CSV file
  (setq dataFile(getfiled "Select a CSV file" "" "CSV" 0))

  ;;;--- Get the table start point
  (setq stpt(getpoint "\nPick the upper-left corner for the table: "))

  ;;;--- Get a list of all layers in the drawing
  (setq layerList(getLayers))

  ;;;--- Get a list of all the text styles in the drawing
  (setq styleList(getStyles))

  ;;;--- Load the dialog box
  (setq dcl_id (load_dialog "CSVTABLE.dcl"))

  ;;;--- Make sure it loaded
  (if (not (new_dialog "CSVTABLE" dcl_id))
    (progn
      (setq str "The CSVTABLE.DCL file was not found!")
      (setq str(strcat str "\n\nSee the XL help file on website for more information."))
      (setq str(strcat str "\n\nhttp://www.jefferypsanders.com/autolisp_XL_Help.html"))
      (setq str(strcat str "\n\nSee error: XL03; error: quit / exit abort"))
      (alert str)
      (exit)
    )
  )

  ;;;--- Add the layer list to the dialog box
  (start_list "linelay" 3)
  (mapcar 'add_list layerList)
  (end_list)

  ;;;--- Add the layer list to the dialog box
  (start_list "textlay" 3)
  (mapcar 'add_list layerList)
  (end_list)

  ;;;--- Add the layer list to the dialog box
  (start_list "textsty" 3)
  (mapcar 'add_list styleList)
  (end_list)

  ;;;--- If an action event occurs, do this function
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "accept" "(setq ddiag 2)(saveVars)(done_dialog)") 

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog box
  (unload_dialog dcl_id)

  ;;;--- If the cancel button was pressed - display message
  (if (= ddiag 1)
    (alert "CSVTABLE was cancelled.")
  )

  ;;;--- If the "Okay" button was pressed
  (if (= ddiag 2)
    (progn

      ;;;--- Open the file to read
      (if(setq fil(open dataFile "r"))
        (progn

          ;;;--- Set up a list to hold the parsed data
          (setq pData(list))

          ;;;--- Read each comma delimited line of data...
          (while (setq dataLine(read-line fil))

            ;;;--- Parse the string into a list
            (setq listOfData(parsecommas dataLine))

            ;;;--- Save the list 
            (setq pData(append pData (list listOfData)))
          )

          ;;;--- Close the open file
          (close fil)
        )
        (alert "The CSV file could not be opened!")  
      )
  
      ;;;--- If data was found in the CSV file...
      (if pData
        (progn

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;--- We need to make sure each row has the same number of items
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;;;--- Set up a variable to hold the max items in a row
          (setq maxRowLen 0)

          ;;;--- Cycle through the list ...
          (foreach a pData

            ;;;--- If this row has more items, set the max row variable to it's length
            (if(> (length a) maxRowLen)(setq maxRowLen (length a)))
          )

          ;;;--- Now we know the max row length, we need to set all rows to this length
          (setq tmpData pData pdata(list))
          (foreach a tmpData
            (while(< (length a) maxRowLen)
              (setq a(append a (list "")))
            )
            (setq pData(append pData (list a)))
          )  

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;--- Now we need to redo the data to sort it into columns instead of rows
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          (setq tmpData pData pData(list) cnt 0)
          (repeat maxRowLen
            (setq tmpLine(list)) 
            (foreach a tmpData
              (setq tmpLine(append tmpLine (list(nth cnt a))))
            )
            (setq pData(append pData (list tmpLine)))
            (setq cnt(+ cnt 1))
          )





          ;;;--- Draw the chart
          (drawChart stpt pData)
        )
         
        ;;;--- Else no data was found, alert the user
        (alert "No data found in the CSV file!")
      )
    )
  )

  ;;;--- Reset the system variables
  (setvar "osmode" oldOs)
  (setvar "cmdecho" 1)

  ;;;--- Suppress the last echo for a clean exit
  (princ)
)

