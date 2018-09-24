; von Joern Bosse

(defun text2zahl (String / LISTE RESTLIST TEMP ZAHLENLIST )
 
 
  (setq restlist  (vl-string->list string)
        zahlenlist (vl-string->list "1234567890+-.,"))

  (while restlist
   
    (while (not(member(car restlist)zahlenlist))
      (setq restlist (cdr restlist)))
   
    (while (member (car restlist)zahlenlist)
      (setq temp (cons (car restlist)temp)
    restlist (cdr restlist)))
   
    (if temp (setq liste (cons (vl-list->string(reverse temp))liste)
  temp nil)))
  (reverse liste))

