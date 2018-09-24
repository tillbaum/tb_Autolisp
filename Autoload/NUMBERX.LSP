;NUMBERX -  This programs makes numbers out of vertical text starting with number 1
; being the highest vertical text location and working downward.
;
;Jeffery P Sanders
;
(defun C:NX(/ eset cnt cntr tset en y yt enlist)
 (setvar "cmdecho" 0)
 (setq cntr(getint "\n Number to Start With: "))
 (princ "\n Select Text To Number: ")
 (if(setq eset(ssget) cnt 0)
  (progn
   (setq tset (ssadd))
   (repeat(sslength eset)
    (progn
     (setq en(ssname eset cnt))
     (if(= "TEXT" (cdr(assoc 0 (entget en))))(ssadd en tset))
     (setq cnt(+ cnt 1))
    )
   )
   (setq y 0)
   (princ (strcat "\nWorking on Number:  " (itoa cntr)" "))
   (while (> (sslength tset) 0)
    (progn
     (setq y 0 cnt 0)
     (repeat(sslength tset)
      (progn
       (setq yt(cadr(cdr(assoc 10 (entget(ssname tset cnt))))))
       (if (> yt y)(progn(setq y yt en (ssname tset cnt))))
       (setq cnt(+ cnt 1))
      )
     )
     (if en
      (progn
       (setq enlist (entget en))
       (setq enlist(subst (cons 1 (itoa cntr))(assoc 1 enlist)enlist))
       (entmod enlist)
      )
     )
     (princ (chr 8))(princ (chr 8))(princ (chr 8))(princ (chr 8))
     (princ (substr (strcat " " (itoa cntr) "   ") 1 4))
     (setq cntr(+ cntr 1))
     (ssdel en tset)
    )
   )
  )
 )
 (princ "  ....NUMBERX.lsp Complete.  \n ")
 (setvar "cmdecho" 1)
 (princ)
)
