;;;--- Program to demonstrate the usage for the getCellsFunction
;;;    Version 2.0
;;;    Last updated 5/1/2013 4:20PM CST
;;;    By JefferyPSanders.com 

(defun C:GetCells()
  ;;;--- Function to retrieve values for a cell or a range of cells
  ;;;
  ;;;    To retrieve a cells value:
  ;;;    Usage (getCellFunction "C:\\MYFILE.XLS" "Sheet1" "A3")
  ;;;
  ;;;    To retrieve values for a range of cells
  ;;;    Usage (getCellFunction "C:\\MYFILE.XLS" "Sheet1" "A2:A4")
  ;;;
  
  (vl-load-com)
  (defun getCellsFunction(fileName sheetName cellName / myXL myBook mySheet myRange cellValue)
    (setq myXL(vlax-get-or-create-object "Calc.Application"))
    (vla-put-visible myXL :vlax-false)
    (vlax-put-property myXL 'DisplayAlerts :vlax-false)
    (setq myBook (vl-catch-all-apply 'vla-open (list (vlax-get-property myXL "WorkBooks") fileName))) 
    (setq mySheet (vl-catch-all-apply 'vlax-get-property (list (vlax-get-property myBook "Sheets") "Item" sheetName))) 
    (vlax-invoke-method mySheet "Activate") 
    (setq myRange (vlax-get-property (vlax-get-property mySheet 'Cells) "Range" cellName)) 
    (setq cellValue(vlax-variant-value (vlax-get-property myRange 'Value2))) 
    (vl-catch-all-apply 'vlax-invoke-method (list myBook "Close")) 
    (vl-catch-all-apply 'vlax-invoke-method (list myXL "Quit")) 
    (if (not (vlax-object-released-p myRange))(progn(vlax-release-object myRange)(setq myRange nil))) 
    (if (not (vlax-object-released-p mySheet))(progn(vlax-release-object mySheet)(setq mySheet nil))) 
    (if (not (vlax-object-released-p myBook))(progn(vlax-release-object myBook)(setq myBook nil))) 
    (if (not (vlax-object-released-p myXL))(progn(vlax-release-object myXL)(setq myXL nil)))     
    (if(= 'safearray (type cellValue)) 
      (progn 
        (setq tempCellValue(vlax-safearray->list cellValue)) 
        (setq cellValue(list)) 
        (if(= (length tempCellValue) 1) 
          (progn
            (foreach a tempCellValue 
              (if(= (type a) 'LIST) 
                (progn 
                  (foreach b a 
                    (if(= (type b) 'LIST) 
                      (setq cellValue(append cellValue (list (vlax-variant-value (car b))))) 
                      (setq cellValue(append cellValue (list (vlax-variant-value b)))) 
                    ) 
                  ) 
                ) 
                (setq cellValue(append cellValue (list (vlax-variant-value a))))  
              ) 
            ) 
          ) 
          (progn
            (foreach a tempCellValue
              (setq tmpList(list))
              (foreach b a
                (setq tmp(vlax-variant-value b))
                (setq tmpList(append tmpList (list tmp)))
              )
              (setq cellValue(append cellValue tmpList))
            )
          )
        )
      )
    )
    cellValue
  )

  ;;;--- Get the excel file
  (setq fileName(getfiled "Select Excel/CALC File" "" "*" 16))

  ;;;--- Get the sheet name
  (setq sheetName(getstring T "\nName of sheet: "))

  ;;;--- Cycle while the user enters cell addresses
  (while(/= "" (setq cellName(getstring "\nCell address to retrieve? [ Examples: A2 or A1:B4 ]:")))

    ;;;--- Get the value of the cell or cells
    (setq cellValue(getCellsFunction fileName sheetName cellName))

    ;;;--- Display the value of the cell(s)
    (princ (strcat "\n The value of address " cellName " is: "))(princ cellValue)
  )
  (princ)
)