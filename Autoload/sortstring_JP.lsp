;;;--- SRTAN.lsp  -  Sort a list of strings alpha-numerically
;;;
;;;
;;;    Notes:
;;;    Strings can contain numbers or letters.  
;;;    Sorts numbers by value and letters alphabetically but not case sensitive.


(defun srtAN(alist / CTS sort)
  
  ;;;--- Function to compare strings and values of numbers
  (defun CTS(Lstr Rstr / Lstk Rstk fnd dis popl popr Numstr getchL getchR )
            
    ;;;--- Function to create a list of characters from a string  
    (defun dis(astr / b)
             
      ;;;--- While a character is found
      (while(/= astr "")
        (setq b (append b(list(substr astr 1 1)))
           astr (substr astr 2) 
        ) 
        b 
      ) 
    ) 

    ;;;--- Function to pop from left stack 
    (defun popl(/ top)(setq top(car lstk)lstk(cdr lstk))top) 

    ;;;--- Function to pop from right stack 
    (defun popr(/ top)(setq top(car rstk)rstk(cdr rstk))top)

    ;;;--- Checks to see if a character should be treated as a number 
    (defun Numstr(achar)
      (member achar'("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "."))
    )

    ;;;--- Returns character or characters to use in next comparison from left stack
    (defun getchL(/ ans2)
      (setq ans2 "")
      (cond
        ((and Lstk(not(Numstr(car Lstk))))(popL))
        ((Numstr(car Lstk))
          (while(and Lstk(Numstr(car Lstk)))
            (setq ans2(strcat ans2(popL)))
          ) 
        )
        (T "")
      )
    )

    ;;;--- Return character or characters to use in next comparison from right stack
    (defun getchR( / ans2)
      (setq ans2 "")
      (cond
        ((and Rstk(not(Numstr(car Rstk)))) (popR))
        ((Numstr(car Rstk))
          (while(and Rstk(Numstr(car Rstk)))
            (setq ans2(strcat ans2(popR)))
          ) 
        )
        (T "")
      )
    )

    ;;;--- Put strings into stacks
    (setq Lstk(dis(strcase Lstr))Rstk(dis(strcase Rstr)))

    ;;;--- Set up a deciding comparison variable
    (setq fnd 0)

    ;;;--- Loop through stach unti decide comparison
    (while(and Lstk Rstk(= fnd 0))
      
      ;;;--- Get characters to compare
      (setq Lcomp(getchL))
      (setq Rcomp(getchR))

      ;;;--- If character is a string that should be a number, convert to a number for left stack
      (if(and(/= Lcomp ".")(Numstr(substr Lcomp 1 1)))
        (setq Lcomp(atof Lcomp))
        (setq Lcomp Lcomp)
      )

      ;;;--- If character is a string that should be a number, convert to a number for right stack
      (if(and(/= Rcomp ".")(Numstr(substr Rcomp 1 1)))
        (setq Rcomp(atof Rcomp))
        (setq Rcomp Rcomp)
      ) 
      (setq ans
        (cond
          ((and(numberp Lcomp)(numberp Rcomp)(= Lcomp Rcomp));2 equal numbers
            (setq fnd 0);continue comparing stacks
            nil
          )
          ((and(numberp Lcomp)(numberp Rcomp));2 unequal numbers so return comparison
            (setq fnd 1)
            (> Lcomp Rcomp)
          )
          ((numberp Lcomp);Rcomp is string
            (setq fnd 1)
            nil;number is less than string
          )
          ((numberp Rcomp);Lcomp is string
            (setq fnd 1)
            T;string is greater than number
          ) 
          ((and(not(numberp Lcomp))(not(numberp Rcomp)));both are strings
            (if(= Lcomp Rcomp)(setq fnd 0)(setq fnd 1))
            (> Lcomp Rcomp)
          )
          (T nil)
        )
      )
    )

    ;;;--- If no comparison then compare length of string 	
    (cond	
      ((and lstk(= fnd 0));strings equal up to length of rstk but lstk longer so it has greater value	
        (setq ans T)		
      )		
      ((and rstk(= fnd 0));strings equal up to length of lstk but rstk longer so it has greater value		
        (setq ans nil)		
      )		
      ((= fnd 0);strings are equal		
        (setq ans nil)		
      )		
      (T nil)		
    )		
    ans
  ) 		
  (defun sort(alist / n)		
    (setq lcup nil rcup nil)		
    (foreach n alist		
      (while (and rcup(CTS n(car rcup)))(setq lcup(cons(car rcup)lcup)rcup(cdr rcup)))		
      (while (and lcup(CTS(car lcup)n))(setq rcup(cons(car lcup)rcup)lcup(cdr lcup)))		
      (setq rcup(cons n rcup))		
    )			
    (append(reverse lcup)rcup)		
  )		
  (sort alist)
)
