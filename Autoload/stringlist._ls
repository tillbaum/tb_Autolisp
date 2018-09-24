; str-list, str-ltrimset, str-rtrimset 
;
; aus Kochbuch autolist kap 3.1 STRING Bearbeitung!!
;
;

(defun str-list(str / ls i)
	(setq i 1)
	(repeat(strlen str)
		(setq ls(cons(substr str i 1)ls))
		(setq i(1+ i))
	)
	(reverse ls)
)

;Anwendungsbeispiel:
;(str-list "String") => ("S" "t" "r" "i" "n" "g")
;Jetzt können wir unsere universell verwendbaren Trimm-Funktionen schreiben:

(defun str-ltrimset(str cset / i)
	(if(>(strlen str)0)
		(progn
		(setq cset(str-list cset))
		(setq i 1)
			(while(member(substr str i 1)cset)
			(setq i(1+ i))
			)
		(substr str i)
		)
	)
)

(defun str-rtrimset(str cset / len)
	(if(>(strlen str)0)
		(progn
		(setq cset(str-list cset))
		(setq len(strlen str))
			(while(member(substr str len 1)cset)
			(setq len(1- len))
			)
		(substr str 1 len)
		)
	)
)


