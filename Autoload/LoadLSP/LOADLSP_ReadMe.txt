 LoadLsp.lsp - Displays autolisp programs with a description.

 For AutoCAD Release 2000+

 Created on 9/2/04  Revision 2

 Copyright 2004 by JefferyPSanders.com
 All rights reserved.

 Required files:
    To use this program you must have the SelectDir.lsp and SelectDir.dcl files.
    I've included these files in the LoadLSP.zip file.
  

 Getting Started:
    This program locates and displays lisp programs from a user supplied search
    path list.  It stores the search path list in a file named LOADLSP_DIR.dat.
    If the file is not found, the user is told to press the "ADD SEARCH PATH"
    button to create a search path list.  This list can be edited at any time.

 Description gotten from:
    The description of the program is displayed in the dialog box.  This is found
    by opening the autolisp program and reading in the first line.  It must be
    the first line inside the autolisp file.  The description will be stripped
    down to 65 characters max.

 Revise Description button:
    If you select a program in the list, the REVISE DESCRIPTION button will enable.
    Press this button to revise the first line inside the autolisp file, which is 
    or should be, the description.  Let me revise that, this program does not replace
    or remove anything from the autolisp file, it simply adds a new remark to the top
    of the program.  Just to make sure the file does not get corrupted, the program makes 
    a backup copy of the file.  If something should go wrong, you can rename the back
    up file to the original name.  The backup file name will be the original program
    name plus a space followed by three numbers with an extension of ".BAK".  For
    example, a file named "JUNK.lsp" would become "JUNK 001.bak".



    All questions and suggestions should be sent to jps@jefferypsanders.com


 
    Revisions:

    0 . Released on 9/3/04
    1 . Revised the "Revise Description" button to display the previous description in the edit box. 12/8/04
    2 . Revised error in description.  Program was cutting off the first character of the description.  12/21/05

JPS
