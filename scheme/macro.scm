
(defmacro (for params . body)
  (mcase params
	 (`(`var `start `end)
	  (let ((LP 'f)) ;; 
	    (list 'letrec
		  (list
		   (list
		    LP
		    (list 'lambda
			  (list var)
			  (list 'if
				(list '< var end)
				(cons 'begin
				      (append body
					      (list (list LP
							  (list 'inc var)))))))))
		  (list LP start))))))

