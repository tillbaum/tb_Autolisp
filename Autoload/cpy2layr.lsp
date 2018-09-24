;                Copy selected object to a layer)

;            Copyright (C) 1998 DotSoft, All Rights Reserved
;                      Website: www.dotsoft.com
; ----------------------------------------------------------------------
; DISCLAIMER:  DotSoft Disclaims any and all liability for any damages
; arising out of the use or operation, or inability to use the software.
; FURTHERMORE, User agrees to hold DotSoft harmless from such claims.
; DotSoft makes no warranty, either expressed or implied, as to the
; fitness of this product for a particular purpose.  All materials are
; to be considered ‘as-is’, and use of this software should be
; considered as AT YOUR OWN RISK.
; ----------------------------------------------------------------------

(defun c:cpy2layr ()
  (setq sset (ssget))
  (if sset
    (progn
      (setvar "CMDECHO" 0)
      (setq pt (list 0.0 0.0))
      (setq nl (getstring "\nTarget Layer: "))
      (if (/= nl "")
        (progn
          (command "_.UNDO" "_G")
          (command "_.LAYER" "_N" nl "")
          (command "_.COPY" "_P" "" pt pt)
          (command "_.CHPROP" "_P" "" "_LA" nl "")
          (command "_.UNDO" "_E")
        )
      )
      (setvar "CMDECHO" 1)
    )
  )
  (setq sset nil)
  (princ)
)
