;;;--- LoadLsp.lsp - Type: LL, Displays lisp programs with a description.
;;;
;;;
;;;--- For AutoCAD Release 2000+
;;;
;;;
;;;--- Created on 9/2/04  Revision 2
;;;
;;;
;;;--- Copyright 2004 by JefferyPSanders.com
;;;    All rights reserved.
;;;
;;;    Getting Started:
;;;--- This program locates and displays lisp programs from a user supplied search
;;;    path list.  It stores the search path list in a file named LOADLSP_DIR.dat.
;;;    If the file is not found, the user is told to press the "ADD SEARCH PATH"
;;;    button to create a search path list.  
;;;
;;;    Description:
;;;    The description of the program is displayed in the dialog box.  This is found
;;;    by opening the autolisp program and reading in the first line.  It must be
;;;    the first line inside the autolisp file.  The description will be stripped
;;;    down to 65 characters max.
;;;
;;;    Revise Description:
;;;    If you select a program in the list, the REVISE DESCRIPTION button will enable.
;;;    Press this button to revise the first line inside the autolisp file, which is 
;;;    or should be, the description.  Let me revise that, this program does not replace
;;;    or remove anything from the autolisp file, it simply adds a new remark to the top
;;;    of the program.  Just to make sure the file does not get corrupted, the program makes 
;;;    a backup copy of the file.  If something should go wrong, you can rename the back-
;;;    up file to the original name.  The backup file name will be the original program
;;;    name plus a space followed by three numbers with an extension of ".BAK".  For
;;;    example, a file named "JUNK.lsp" would become "JUNK 001.bak".
;;;
;;;
;;;
;;;--- All questions and suggestions should be sent to jps@jefferypsanders.com
;;;
;;;
;;; 
;;;--- Revisions:
;;;
;;;    0 . Released on 9/3/04
;;;    1 . Revised "Revise Description" button to display the previous description in the edit box.  12/8/04
;;;    2 . Revised error in description.  Program was cutting off the first character of the description.  12/21/05
;;;
;;;
;;;
;;;

;;;--- Debug Switch set to off
;;;    nil = OFF
;;;    T   = ON
(setq debg nil)
;;;(setq debg T)

;;;--- Define a debug routine
(defun dbg(a)
  (princ"\n")
  (princ a)
)




(vl-load-com)

;;;--- Enclose all code inside to force everything local

(defun C:LL(/ proList shortList dcl_id)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Sort Function   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;--- Usage (srt list)
;;;    Sorts on the first item in a list of list.
;;;
;;;    Example  (sort (list (3 6) (4 5) (1 7) (2 8) ))
;;;    Returns ((1 7)(2 8)(3 6)(4 5))
;;;
(defun srt(alist / n)(setq lcup nil rcup nil)
 (defun cts(a b)
  (cond
   ((> (car a) (car b))t)
   ((= (car a) (car b) )t)
   (t nil)
 ))
 (foreach n alist
  (while (and rcup(cts n(car rcup)))(setq lcup(cons(car rcup)lcup)rcup(cdr rcup)))
   (while (and lcup(cts(car lcup)n))(setq rcup(cons(car lcup)rcup)lcup(cdr lcup)))
   (setq rcup(cons n rcup))
 )
 (append(reverse lcup)rcup)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;  End of Sort Function  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Function to save the dialog box settings  ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun saveVars()
  
  ;;;--- Get the selected item from the list
  (setq lspStr(get_tile "shortlist"))

  ;;;--- If the selected item is "" then nothing is selected, so make sure 
  (if(/= lspStr "")
    (progn

      ;;;--- Something is selected, so convert from string to integer
      (setq lspIndex(atoi lspStr))

      ;;;--- Get the selected item from the list
      (setq lspName(caddr(nth lspIndex proList)))
    )

    ;;;--- Else, nothing is selected
    (progn

      ;;;--- Set the index number to -1
      (setq lspIndex -1)

      ;;;--- And set the name of the lisp program to nil
      (setq lspName nil)
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;; End of saving settings from dialog box ;;;;;;;;;;;;;;;;;;;;;





;;;--- Function to load the search paths to check for LSP files

(defun loadDirs()

  (if debg (dbg "LD1"))

  ;;;--- Set up a list to hold the search paths
  (setq pathList(list))

  ;;;--- If we can locate the file that we store the search paths in....
  (if(findfile "LOADLSP_DIR.dat")
    (progn

      (if debg (dbg "LD2"))

      ;;;--- Then open the file...
      (if(setq fil(open (findfile "LOADLSP_DIR.dat") "r"))
        (progn

          (if debg (dbg "LD3"))

          ;;;--- While there is a line to read from the file...
          (while(setq a(read-line fil))

            ;;;--- If it is at least something that resembles a path 
            (if(and (/= a "")(= (substr a 2 1) ":"))
              (progn

                ;;;--- Save the path in the path list              
                (setq pathList(append pathList (list a)))

                ;;;--- Inform the user of our progress
                (princ "\nLoading Path: ")(princ a)
              )
            )
          )

          ;;;--- Close the file
          (close fil)
        )

        ;;;--- Else the file could not be opened, inform the user
        (alert "Error attempting to open the LOADLSP_DIR.dat file.\n\nMake sure the file is not already open by another application.")
      )
    )

    ;;;--- Else the file was not found, inform the user
    (alert "You have not set a directory to search for LISP files.\n\nUse the REVISE SEARCH PATHS button to add a search directory.")
  )

  ;;;--- Return the path list
  pathList
)



;;;--- Function to cycle through search paths and load all autolisp programs found

(defun loadProgs(paths)

  (if debg (dbg "LP1"))

  ;;;--- Inform the user  
  (princ "\n Loading AutoLisp files...")

  ;;;--- Set up a list to hold all of the programs
  (setq proList(list))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                              ;;;
  ;;; proList = (                                                  ;;;
  ;;;             (program_name  description  path_and_file_name)  ;;;
  ;;;             (program_name  description  path_and_file_name)  ;;;
  ;;;             (program_name  description  path_and_file_name)  ;;;
  ;;;           )                                                  ;;;
  ;;;                                                              ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;--- Cycle through each of the search paths looking for lisp files
  (foreach a paths

    (if debg (dbg "LP2"))

    ;;;--- Attempt to get the file names from the first search path
    (setq files(vl-directory-files a "*.lsp" 1))

    ;;;--- If files were found
    (if files

       ;;;--- Cycle through each file found
       (foreach b files

         (if debg (dbg "LP3"))

         ;;;--- Get the name of the file, make it uppercase, and 15 characters long
         (setq nam (strcase(substr (strcat (substr b 1 (- (strlen b) 4))"                  ") 1 15)))

         ;;;--- Inform the user of the file we are loading
         ;(princ "\n Loading ")
         ;(princ nam)

         ;;;--- Build the path_and_file_name by stringing the search path and file name together
         (setq loc (strcat a b))

         ;;;--- Open the file for reading
         (if(setq fil(open loc "r"))
           (progn

             ;;;--- Read in the first line of the file which should be the program's description 
             (setq lin(read-line fil))

             ;;;--- Since that is all we need, go ahead and close the file
             (close fil)

             ;;;--- Strip off any unnecessary preceding characters
             (while
               (or
                 (and
                   (/= (ascii(substr lin 1 1)) 0)      
                   (< (ascii(substr lin 1 1)) 48)
                 )
                 (and
                   (> (ascii(substr lin 1 1)) 57)
                   (< (ascii(substr lin 1 1)) 65)
                 )
                 (and 
                   (> (ascii(substr lin 1 1)) 90)
                   (< (ascii(substr lin 1 1)) 97)
                 )
                 (> (ascii(substr lin 1 1)) 122)
               )
               (setq lin(substr lin 2))
             )

             ;;;--- Make the description string 65 characters long
             (setq lin(substr (strcat lin "                                                                  ") 1 65))
           )

           ;;;--- If the file could not be opened....set a default description
           (setq lin "No information available.")
         ) 

         ;;;--- Add the data to the program list 
         (setq proList(append proList (list (list nam lin loc))))
       )
    )
  )

  ;;;--- Inform the user of our progress
  (princ "\n.\n.\n Finished loading files.\n")

  ;;;--- Force the update on the command line
  (terpri)

  ;;;--- If autolisp programs were found...
  (if proList
    (progn

      (if debg (dbg "LP2"))

      ;;;--- Sort the programs 
      (setq proList(srt proList))

      ;;;--- Set up another list to hold just the program name and description
      ;;;    This is the list we will display in the dialog box, not the proList
      ;;;    with the path_and_file_name.
      (setq shortList(list))

      ;;;--- Cycle through each item in the program list
      (foreach a proList

        ;;;--- Add the item [ minus the long file name ] to the short list
        (setq shortList(append shortList (list (strcat (car a)(cadr a)))))
      )
    )
  )
)



;;;--- Function to toggle the remPath button

(defun toggleRemPath()

  ;;;--- If no item is selected in the list...
  (if(= "" (get_tile "pathlist"))

    ;;;--- Disable the Remove Path button
    (mode_tile "rempath" 1)

    ;;;--- Else, something is selected, so enable the Remove Path button
    (mode_tile "rempath" 0)
  )

  ;;;--- If there are no items in the path list
  (if(= 0 (length pathList))

    ;;;--- Disable the Remove Path button
    (mode_tile "rempath" 1)
  )
)



;;;--- Function to edit the search paths

(defun revSearch()

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;                                                                                ;;;
  ;;;  We are going to let the user add and remove paths to the path list all day    ;;;
  ;;;  long, but we will not save or update changes until he/she presses the "Save   ;;;
  ;;;  and CLOSE" button.  So, just in case the user makes a bunch of changes and    ;;;
  ;;;  then presses the CANCEL button, we had better save a copy of the path list    ;;;
  ;;;  before the user starts making changes.                                        ;;;
  ;;;                                                                                ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;--- Save the list before editing happens
  (setq oldPathList pathList)

  ;;;--- Load the DCL file
  (setq dcl_id3 (load_dialog "LOADLSP.dcl"))
  
  ;;;--- If the correct dialog definition is not loaded...
  (if (not (new_dialog "REVPATH" dcl_id3)) 
    (progn
      (alert "The LOADLSP.DCL file was not found.\n\nMake sure it is located in the AutoCAD search path.")
      (exit)
    )
  )

  ;;;--- Add the programs names and description to the dialog box
  (start_list "pathlist" 3)
  (mapcar 'add_list pathList)
  (end_list)

  ;;;--- Disable buttons that shouldn't be available
  (toggleRemPath)

  ;;;--- If an action event occurs, do this function
  (action_tile "pathlist" "(toggleRemPath)")
  (action_tile "addpath" "(addPath)")
  (action_tile "rempath" "(remPath)")
  (action_tile "cancel" "(setq ddiag 3)(done_dialog)")
  (action_tile "saveclose" "(setq ddiag 4)(done_dialog)") 

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog
  (unload_dialog dcl_id3)

  ;;;--- If the cancel button was pressed
  (if (= ddiag 3)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;                                            ;;;
    ;;;   Aha!  The user made changes and pressed  ;;;
    ;;;   the CANCEL button. Better reset things.  ;;;
    ;;;   I knew this would happen!                ;;;
    ;;;                                            ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;;;--- Reset the list to it's previous state
    (setq pathList oldPathList)
  )
  
  ;;;--- If the "Save and Close" button was pressed
  (if (= ddiag 4)
    (progn

       ;;;--- Inform the user pf progress...
       (princ "\n... Saving Search Paths...")

       ;;;--- If the search path list file was found...
       (if(findfile "LOADLSP_DIR.dat")

         ;;;--- Then open the existing file to write
         (setq fil(open (findfile "LOADLSP_DIR.dat") "w"))

         ;;;--- Else, open a new file to write
         (setq fil(open "LoadLSP_DIR.dat" "w"))
       )

       ;;;--- Set up a counter              
       (setq acnt 0)

       ;;;--- Cycle through each search path in the list 
       (foreach a pathList

         ;;;--- If this is the first line in the file...
         (if(= acnt 0)

           ;;;--- Write without a new line character 
           (princ a fil)

           ;;;--- Else, create a new line before writing
           (princ (strcat "\n" a) fil)
         )

         ;;;--- Increment the counter
         (setq acnt (+ acnt 1))
       )

       ;;;--- Close the file
       (close fil)

       ;;;--- Inform the user of progress
       (princ (strcat "\n... " (itoa acnt) " Search Paths saved."))
       
       ;;;--- Reload the programs using the new search path list
       (loadProgs pathList)

       ;;;--- Add the programs names and description to the dialog box
       (start_list "shortlist" 3)
       (mapcar 'add_list shortList)
       (end_list)

       ;;;--- Disable any buttons that aren't valid at this time
       (toggleLoad)
    )
  )    
)


;;;--- Function to add a path to the search path list

(defun addPath()

  ;;;--- If the SELECTDIR program is not loaded...
  (if(not (= 'SUBR (type SELECTDIR)))

    ;;;--- Go ahead and load it
    (load (findfile "SelectDir.lsp"))
  )

  ;;;--- Use the SELECTDIR routine to get a directory selection
  (if(setq newPath(selectDir "" 0))
    (progn

      ;;;--- A path was selected, make sure it isn't already in the list...
      (if(not(member newPath pathList))
        (progn

          ;;;--- It is not in the list, so add it to the list
          (setq pathList(append pathList (list newPath)))

          ;;;--- Update the list to show the addition of the new search path
          (start_list "pathlist" 3)
          (mapcar 'add_list pathList)
          (end_list)           
        )
      )
    )
  )

  ;;;--- Disable buttons that shouldn't be available
  (toggleRemPath)
)



;;;--- Function to remove a path from the search path list

(defun remPath()

  ;;;--- Get the index of the item selected in the search path list box
  (setq pathIndex(atoi(get_tile "pathlist")))

  ;;;--- Get the path from the list
  (setq path2Rem(nth pathIndex pathList))

  ;;;--- Save a copy of the old search path list
  (setq oldPathList pathList)

  ;;;--- Empty the original list so we can recreate it
  (setq pathList(list))

  ;;;--- Cycle through each path in the copied search path list
  (foreach a oldPathList

    ;;;--- If this is not the selected path...
    (if(/= a path2Rem)

      ;;;--- Then add it to the list
      (setq pathList(append pathList (list a)))
    )
  )

  ;;;--- Update the search path list to show the removal of the selected path
  (start_list "pathlist" 3)
  (mapcar 'add_list pathList)
  (end_list)           

  ;;;--- Disable buttons that shouldn't be available
  (toggleRemPath)
)




;;;--- Function to save the new description from the REVDESC dialog box

(defun saveDesc()

  ;;;--- Save the new program description from the dialog box
  (setq newDesc(get_tile "newdesc"))
)




;;;--- Function to revise the description of a lisp program

(defun revDescription()

  ;;;--- Get the index of the selected item
  (setq oldDescIndex(atoi(get_tile "shortlist")))

  ;;;--- Get the description of the selected item
  (setq oldDes(cadr(nth oldDescIndex proList)))

  ;;;--- Get the name of the selected item [program name]
  (setq oldNam(car(nth oldDescIndex proList)))

  ;;;--- Load the DCL file
  (setq dcl_id2 (load_dialog "LOADLSP.dcl"))

  ;;;--- If the correct dialog definition is not loaded...
  (if (not (new_dialog "REVDESC" dcl_id2))
    (progn

      ;;;--- Inform the user
      (alert "The LOADLSP.DCL file was not found.\n\nMake sure it is in the AutoCAD search path.")
      (exit)
    )
  )

  ;;;--- Add the old description to the dialog box so the user can see it
  (set_tile "olddesc" (strcat "Old Description: " oldDes))

  ;;;--- Add the old description to the edit box so the user can edit it
  (set_tile "newdesc" oldDes)

  ;;;--- If an action event occurs, do this function
  (action_tile "cancel2" "(setq ddiag 5)(done_dialog)")
  (action_tile "accept2" "(setq ddiag 6)(saveDesc)(done_dialog)") 

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog
  (unload_dialog dcl_id2)

  ;;;--- If the "Save and Close" button was pressed
  (if (= ddiag 6)
    (progn

      ;;;--- Make sure the user typed something!
      (if(/= newDesc "")
        (progn
 
          ;;;--- Save the old program list in a temporary list
          (setq tmpList proList)

          ;;;--- Clear out the original program list so we can rebuild it
          (setq proList(list))

          ;;;--- Cycle through each program in the temporary list
          (foreach a tmpList

            ;;;--- If we find the name of the selected item...
            (if(= (car a) oldNam)

              ;;;--- Replace the old description with the new one
              (setq proList(append proList(list (list (car a) newDesc (caddr a)))))

              ;;;--- Else, simply add it to the list with no changes
              (setq proList(append proList(list a)))
            )
          )

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;;;                                                    ;;;
          ;;;  We now need to open the autolisp program and      ;;;
          ;;;  add the new description to the file.  First       ;;;
          ;;;  we will make a backup copy of the original.       ;;;
          ;;;                                                    ;;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          ;;;--- Get the index of the selected item
          (setq shortIndex(atoi(get_tile "shortlist")))

          ;;;--- Get the long file name from the program list
          (setq short2Rev(caddr(nth shortIndex proList)))

          ;;;--- Get the short name of the program
          (setq tempName(car(nth shortIndex proList)))  

          ;;;--- Make sure we can find the file
          (if(findfile short2Rev)
            (progn

              ;;;--- Create a new file name for the back up file.
              (setq newfilName
                (vl-filename-mktemp 
                  tempName 
                  (vl-filename-directory (caddr (nth shortIndex proList)))
                  ".bak"
                )
              )

              ;;;--- If the backup copy was successful...
              (if (vl-file-rename short2Rev newFilName)
                (progn

                  ;;;--- Open the back up copy to read
                  (if(setq fil(open (findfile newFilName) "r"))
                    (progn

                      ;;;--- Open the original file name to write
                      (if (setq fil2(open short2Rev "w"))
                        (progn

                          ;;;--- Add the new description as the top line preceded with a semi-colon  
                          (princ (strcat "; " newDesc) fil2)

                          ;;;--- Read every line from the backup file
                          (while(setq a(read-line fil))

                            ;;;--- Write it to the new file
                            (princ (strcat "\n" a) fil2)
                          )

                          ;;;--- Close the new file
                          (close fil2) 

                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          ;;;                                                ;;;
                          ;;;  We now need to update the list. We will then  ;;;
                          ;;;  redislplay the dialog list box to show the    ;;;
                          ;;;  new description in the dialog box.  Finally   ;;;
                          ;;;  we will highlight the selected program so     ;;;
                          ;;;  the user can see immediate results.           ;;;
                          ;;;                                                ;;;
                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                          ;;;--- Make a back up copy of the programs list
                          (setq tmpList proList)

                          ;;;--- Clear out the programs list so we can rebuild it
                          (setq proList(list))

                          ;;;--- Cycle through each program in the temporary list
                          (foreach a tmpList

                            ;;;--- If it is not the selected item...
                            (if(/= (car a) oldNam)

                              ;;;--- Add it to the program list with no changes
                              (setq proList(append proList (list a)))

                              ;;;--- Else, Add it to the program list but first revise the description
                              (setq proList(append proList (list (list (car a) newDesc (caddr a)))))
                            )
                          )


                          ;;;--- Rebuild the shortened list for the dialog box
                          (setq shortList(list))
                          (foreach a proList
                            (setq shortList(append shortList (list (strcat (car a)(cadr a)))))
                          )

                  
                          ;;;--- Add the programs names and description to the dialog box
                          (start_list "shortlist" 3)
                          (mapcar 'add_list shortList)
                          (end_list)

                          ;;;--- Reset the selected item to the previously selected item
                          (set_tile "shortlist" (itoa shortIndex))

                          ;;;--- Disable buttons that are usuable at this time
                          (toggleLoad)
                        )
                        (alert (strcat "Could not open the new file.\n\nYou made need to rename "\n newFilName "\n to \n" short2Rev))
                      )

                      ;;;--- Close the file
                      (close fil)
                    )
                    (alert (strcat "Could not find the temporary file " newFilName))
                  )
                )
                (alert (strcat "Could not create the temporary file.\nMake sure the file " short2Rev " is not open by another application."))
              )
            )
            (alert (strcat "Could not find the file " short2Rev "\nMake sure it has not been moved."))
          )
        )
      )
    )
  )  
)



;;;--- Function to turn the LOAD button on and off depending on whether something is selected or not

(defun toggleLoad()

  ;;;--- If the index of the selected item equals "" then nothing is selected, so...
  (if(= "" (get_tile "shortlist"))
    (progn
       
      ;;;--- Disable the Load and Revise Description buttons
      (mode_tile "accept" 1)
      (mode_tile "revdes" 1)
    )
    (progn

      ;;;--- Else, enable the Load and Revise Description buttons
      (mode_tile "accept" 0)
      (mode_tile "revdes" 0)
    )
  )

  ;;;--- If there are no items in the list...
  (if(= 0 (length shortList))
    (progn

      ;;;--- Disable the Load and Revise Description buttons
      (mode_tile "accept" 1)
      (mode_tile "revdes" 1)
    )
  )
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                          ;;;
;;;     888       888          888           8888888      8888   888         ;;;
;;;     8888     8888         88888            888        88888  888         ;;;
;;;     88888   88888        888 888           888        888888 888         ;;;
;;;     888888 888888       888   888          888        888 888888         ;;;
;;;     888 88888 888      88888888888         888        888  88888         ;;;
;;;     888  888  888     888       888      8888888      888   8888         ;;;
;;;                                                                          ;;;
;;;                                                                          ;;;
;;;                888            888888888        888888888                 ;;;
;;;               88888           888   888        888   888                 ;;;
;;;              888 888          888   888        888   888                 ;;;
;;;             888   888         888888888        888888888                 ;;;
;;;            88888888888        888              888                       ;;;
;;;           888       888       888              888                       ;;;
;;;                                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ;;;--- Turn off the command echo
  (setvar "cmdecho" 0)

  ;;;--- Load the search paths
  (setq pathList(loadDirs))

  ;;;--- Find the available autolisp programs in the search paths
  (loadProgs pathList)       

  ;;;--- Load the DCL file
  (setq dcl_id (load_dialog "LOADLSP.dcl"))
 
  ;;;--- If the correct dialog definition was not loaded...
  (if (not (new_dialog "LOADLSP" dcl_id)) 
    (progn

      ;;;--- Inform the user
      (alert "Could not find the LOADLSP.DCL file.\n\nMake sure it is in the AutoCAD search path.")
      (exit)
    )
  )

  ;;;--- Add the programs names and description to the dialog box
  (start_list "shortlist" 3)
  (mapcar 'add_list shortList)
  (end_list)

  ;;;--- Disable buttons that are unusable at this time
  (toggleLoad)

  ;;;--- If an action event occurs, do this function
  (action_tile "shortlist" "(toggleLoad)")
  (action_tile "revdes" "(revDescription)")
  (action_tile "revsrch" "(revSearch)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")
  (action_tile "accept" "(setq ddiag 2)(saveVars)(done_dialog)") 

  ;;;--- Display the dialog box
  (start_dialog)

  ;;;--- Unload the dialog
  (unload_dialog dcl_id)

  ;;;--- If the cancel button was pressed
  (if (= ddiag 1)

    ;;;--- Display a message
    (princ "\n.\n LOADLSP Cancelled. \n ")
  )

  ;;;--- reset the command echo
  (setvar "cmdecho" 1)
  
  ;;;--- If the "Okay" button was pressed and a program was selected...
  (if (and lspName (= ddiag 2))
    (progn

      ;;;--- Clear the command line
      (princ "\n.")  

      ;;;--- Load the selected autolisp program
      (load lspName)
    )

    ;;;--- Else, a program was not selected, so...
    (progn

      ;;;--- Inform the user
      (princ "\n Nothing Loaded")
      (princ)
    )
  )
)  
