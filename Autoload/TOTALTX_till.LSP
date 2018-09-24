;TOTALTX - Adds text entities. berechnet Summe und schreibt es in dtext, changed by till
;
;Jeffery P Sanders
;
(defun C:TTX(/ sum eset enlist oldht len ln total pt )
 (princ "\n \n \nSelect dText: ")
 (if (setq eset (ssget)) ;wenn Bedingung True ist (true ist nicht angegeben), do this
  (progn
   (setq len(sslength eset) ln(- len 1) total 0)
   (while(>=(setq len(- len 1))0)
    (setq en(ssname eset len)enlist(entget en))
    (if(= "TEXT" (cdr (assoc 0 enlist)))
     (progn
      (if (= len ln)
		(princ "\n \n \n...Please Wait....Totaling Selection Set...")
		)
      (setq sum(atof(cdr(assoc 1 enlist))))
      (setq total(+ total sum))
	 );end progn
	 (setq ln(- ln 1))
	);end if
   );end while
   (princ "\n \nTotal = ")(princ total)
   (setq pt(getpoint "\nInsertion Point:"))
     
   (setq Sum(strcat (rtos total 2 3) " ,=Summe")) ;2 Strings zu einer Stringkette
   (command "text" pt "0" Sum)
   
   (princ "\n....TTX.lsp Complete. \n ")
   )
  )
 (princ)
)