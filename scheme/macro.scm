
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


;; (define-syntax (for (i start end) ...)
;;   (letrec
;;       ((LP
;; 	(lambda (var)
;; 	  (if (< var end)
;; 	      (begin
;; 		...
;; 		(LP (inc var)))))))
;;     (LP start)))

