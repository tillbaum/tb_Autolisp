;;;--- SELECTDIR.lsp      Version 1.0     7/2/04
;;;
;;;
;;;--- Allows selection of any diirectory. 
;;;
;;;    Returns the path and name of the directory.
;;;
;;;
;;;
;;;--- Usage:
;;;
;;;    (selectdir filePath verifyDrives)
;;;
;;;    Parameters:
;;;
;;;      filePath     - Starting Path. Complete path as string.   Example: "C://ACAD"
;;;                     Use nil for current directory
;;;
;;;
;;;      verifyDrives - Integer value of 1 will verify all drives 
;;;                     before adding them to the dialog box drop
;;;                     down list.
;;;                     Any other value will skip drive verification
;;;                     and all drives will show up in the list.
;;;
;;;                     No error will occur if the user selects a drive
;;;                     that is unavailable.
;;;
;;;                     IMPORTANT NOTE: Drive verification can be slow
;;;                     if you have a lot of drives with files and folders.
;;;
;;;
;;;--- Other notes of interest:
;;;
;;;    Selecting the "."  in the directory list box will take you back to the root.
;;;    Selecting the ".." in the directory list box will take you up a directory. 
;;;
;;;
;;;
;;;
;;;--- This program is offered as is without warranty.  
;;;    Feel free to modify,copy,destroy,sell,buy,rent,make fun of, tease or set fire to.
;;;    If you modify the program please remove the version number and my email address.
;;;
;;;    Please send comments or suggestions to jps@jefferypsanders.com
;;;
;;;
;;;
;;;


(defun SelectDir(filePath verifyDrives)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Function to fill in the directories in the dialog box
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun UpdateDialog ()

  (dbg "U1")

  ;;;--- Wipe out the previous current directory and
  ;;;    inform the user of a possible delay
  (set_tile "currentdirectory" "Working...")

  (dbg "U2")

  ;;;--- Get the directory names in a list
  (setq ListOfDirs (VL-Directory-Files FilePath nil -1))

  (dbg "U3")
  
  ;;;--- Sort the directories
  (setq ListOfDirs (VL-Sort ListOfDirs (function (lambda (a b)(< (strcase a) (strcase b))))))

  (dbg "U4")

  ;;;--- Add the directories to the dialog list box
  (start_list "directories")
  (mapcar 'add_list ListOfDirs)
  (end_list)

  (dbg "U5")

  ;;;--- Get the number of directories
  (setq dirCount(length ListOfDirs))

  (dbg "U6")

  ;;;--- Don't count the "." and the ".."
  (if(member "."  ListOfDirs)(setq dirCount(- dirCount 1)))
  (if(member ".." ListOfDirs)(setq dirCount(- dirCount 1)))  

  (dbg "U7")

  ;;;--- Count the number of directories and display it for the user
  (set_tile "DIRCOUNT" (strcat (itoa dirCount) " directories found."))

  (dbg "U8")

  ;;;--- Update the current directory tile
  (set_tile "currentdirectory" (strcat "Current Directory: " FilePath))

  (dbg "U9")
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Function to update the dialog box when a directory is clicked
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun UpdateDrives()

  (dbg "UD1")

  ;;;--- Get the drive attribute
  (setq myDrive(get_tile "drives"))

  (dbg "UD2")

  ;;;--- Find the selected directory
  (setq myDrive (nth (atoi myDrive) driveList))

  (dbg "UD3")

  ;;;--- Reset the file path
  (setq FilePath (strcat myDrive "\\"))

  (dbg "UD4")

  ;;;--- Reset the current directories tile
  (set_tile "currentdirectory" FilePath)

  (dbg "UD5")
  
  ;;;--- Update the dialog box to show the new directories
  (UpdateDialog)

  (dbg "UD6")
)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Function to update the dialog box when a directory is clicked
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun UpdateDirectory()

  (dbg "UDR1")

  ;;;--- Get the directories attribute
  (setq myPath(get_tile "directories"))

  (dbg "UDR2")

  ;;;--- Find the selected directory
  (setq myPath (nth (atoi myPath) ListOfDirs))

  (dbg "UDR3")

  ;;;--- Use a conditional loop to decide what to do next
  (cond
    
    ;;;--- If "." was selected, strip the path back to the drive
    ((and (= myPath ".")(> (strlen FilePath)3))
      (progn


        ;;;--- Set the path to the first three characters
        (setq FilePath (substr FilePath 1 3))

        (dbg "UDR4")
      )
   
    ) 
    
    ;;;--- If ".." was selected...
    ((and (= myPath "..")(> (strlen FilePath) 3))
      (progn

        (dbg "UDR5")

        ;;;--- Loop through the string from back to front stripping
	;;;    off one character at a time looking for a "\\".  When
	;;;    you find it, save the file path from the start of the
	;;;    string to the "\\" found.  This will give us a new
	;;;    path one directory up.

	;;;--- Get the index of the last character in the file path string
	(setq lChar(- (strlen FilePath) 1))

        (dbg "UDR6")

	;;;--- Strip off the last character
	(setq FilePath (substr FilePath 1 lChar))

        (dbg "UDR7")

	;;;--- While the last character is not a "\\" character
        (while (/= (substr FilePath lChar 1) "\\")

	  ;;;--- Move left one character to test it
          (setq lChar (- lChar 1))
	)

        (dbg "UDR8")

	;;;--- Save the new file path
        (setq FilePath (substr FilePath 1 lChar))

        (dbg "UDR9")
      )  
    )

    ;;;--- Else the user must have selected a directory on the same level
    ;;;    so simply add the directory to the path
    (T  (setq FilePath (strcat FilePath myPath "\\")))
  )


  (dbg "UDR10")

  ;;;--- Update the dialog box to show the new directories
  (UpdateDialog)

  (dbg "UDR11")
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;--- Function to find all of the available drives
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun findDrives(dList)

  (dbg "FD1")

  ;;;--- Set up an empty list to hold the verified drives
  (setq verifiedList(list))

  (dbg "FD2")

  ;;;--- Cycle through every possible drive letter
  (foreach a dList

    (dbg "FD3")

    ;;;--- Inform the user of the progress
    (princ "\n Verifying Drive ")(princ a)(princ " ... ")

    (dbg "FD4")

    ;;;--- If this drive contains files or folders
    (if(vl-directory-files (strcat a "//") "*.*" 1)
       (progn

         (dbg "FD5")

	 ;;;--- Add the drive to the verified drive list
         (setq verifiedList(append verifiedList (list a)))

         (dbg "FD6")

	 ;;;--- Inform the user
	 (princ " - Verified!")

         (dbg "FD7")
       )

       ;;;--- Inform the user of failure
       (princ " - Drive Unavailable.")
    )
  )

  (dbg "FD8")

  ;;;--- Clear the command line
  (princ "\n.\n.")

  ;;;--- Inform the user of the drives verified
  (princ "\n Verified ")(princ (length verifiedList))(princ " Drives.")

  (dbg "FD9")

  ;;;--- Return the list of verified files
  verifiedList
)  

 



  ;;;--- Main function to select directory

  ;;;--- Debug flag ...T=on nil=off
  (setq dbgr T)

  ;;;--- SetUp a debug routine
  (defun dbg(a)
    (if dbgr (progn (princ "\n")(princ a)))
  )

  (dbg 1)

  ;;;--- Set the file path to the default
  (if(= filePath nil)(setq filePath (getvar "dwgprefix")))

  (dbg 2)

  ;;;--- Build a drive list
  (setq driveList
    (list "A:" "B:" "C:" "D:" "E:" "F:" "G:" "H:" "I:" "J:" "K:" "L:" "M:"
	  "N:" "O:" "P:" "Q:" "R:" "S:" "T:" "U:" "V:" "W:" "X:" "Y:" "Z:"
  ) )

  (dbg 3)

  ;;;--- Find all of the available drives if verifyDrives equals 1
  (if(= verifyDrives 1)
    (setq driveList(findDrives driveList))
  )


  (dbg 4)

  ;;;--- Force the command line to update.
  (terpri)

  (dbg 5)

  ;;;--- Find the current drive specified by the file path
  (setq currentDrive (strcase(substr filePath 1 2)))

  (dbg 6)

  ;;;--- Set the drive list box to have the current drive selected
  (setq driveIndex(member currentDrive driveList))
  (setq driveIndex(- (length driveList) (length driveIndex)))
  (setq currentDrive(nth driveIndex driveList))

  (dbg 7)

  ;;;--- Load the dialog box
  (setq dcl_id (load_dialog "SELECTDIR"))

  (dbg 8)  

  ;;;--- See if it loaded
  (if (not (new_dialog "SELECTDIR" dcl_id) ) (exit))

  (dbg 9)

  ;;;--- Add the drives to the dialog list box
  (start_list "drives")
  (mapcar 'add_list driveList)
  (end_list)

  (dbg 10)

  ;;;--- Set the current drive to be selected
  (set_tile "drives" (itoa driveIndex))  

  (dbg 11)

  ;;;--- Update the directory list box
  (UpdateDialog)

  (dbg 12)

  ;;;--- Set up the action sequences from the dialog box
  (action_tile "drives" "(UpDateDrives)")
  (action_tile "directories" "(UpDateDirectory)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "accept" "(setq ddiag 2)(done_dialog)") 

  ;;;--- Display the dialog box
  (start_dialog)

  (dbg 13)

  ;;;--- If the CANCEL button was pressed, exit quietly
  (if (= ddiag 1)
    (setq SelectedFiles nil)
  )

  (dbg 14)

  ;;;--- Returns the last selected path if the accept button was pressed
  (if (= ddiag 2) filePath nil)
)
