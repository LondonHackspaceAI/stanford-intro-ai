
(defmacro (for params . body)
  (mcase params
	 (`(`var `start `end)
	  (let ((LP 'f)) ;; 
	    `(letrec
		 ((,LP
		   (lambda (,var)
		     (if (< ,var ,end)
			 (begin
			   ,@body
			   (,LP (inc ,var)))))))
	       (,LP ,start))))))

