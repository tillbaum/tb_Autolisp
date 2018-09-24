; ----------------------------------------------------------------------
; (Wblocks all local block definitions to target path)
;            Copyright (C) 2000 DotSoft, All Rights Reserved
;                   Website: http://www.dotsoft.com
; ----------------------------------------------------------------------
; DISCLAIMER:  DotSoft Disclaims any and all liability for any damages
; arising out of the use or operation, or inability to use the software.
; FURTHERMORE, User agrees to hold DotSoft harmless from such claims.
; DotSoft makes no warranty, either expressed or implied, as to the
; fitness of this product for a particular purpose.  All materials are
; to be considered ‘as-is’, and use of this software should be
; considered as AT YOUR OWN RISK.
; ----------------------------------------------------------------------

(defun c:wblockm ()
  (setq cmdecho (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  ;
  (if (not dos_getdir)
    (setq path (getstring "\nDS> Target Folder: " T))
    (setq path (dos_getdir "Target Folder" (getvar "DWGPREFIX")))
  )
  (if (/= path nil)
    (progn
      (if (= (substr path (strlen path) 1) "\\")
        (setq path (substr path 1 (1- (strlen path))))
      ) 
      (princ "\nDS> Building List of Blocks ... ")
      (setq lst nil)
      (setq itm (tblnext "BLOCK" T))
      (while (/= itm nil)
        (setq nam (cdr (assoc 2 itm)))
        (setq pass T)
        (if (/= (cdr (assoc 1 itm)) nil)
          (setq pass nil)
          (progn
            (setq ctr 1)
            (repeat (strlen nam)
              (setq chk (substr nam ctr 1))
              (if (or (= chk "*")(= chk "|"))
                (setq pass nil)
              )
              (setq ctr (1+ ctr))
            )
          )
        )
        (if (= pass T)
          (setq lst (cons nam lst))
        )
        (setq itm (tblnext "BLOCK"))
      )
      (setq lst (acad_strlsort lst))
      (princ "Done.")
      ;
      (foreach blk lst
        (setq fn (strcat path (chr 92) blk))
        (if (findfile (strcat fn ".dwg"))
          (command "_.WBLOCK" fn "_Y" blk)
          (command "_.WBLOCK" fn blk)
        )
      )
    )
  )
  ;
  (setvar "CMDECHO" cmdecho)
  (princ)
)
