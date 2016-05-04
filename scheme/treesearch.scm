(require easy
	 typed-list
	 (predicates nonnegative-real? length->=)
	 test)


;; we use symbols to represent cities
(def city? symbol?)

;; a citylink is a path segment between two cities
(class citylink
       (struct #(city? from)
	       #(city? to)
	       #(nonnegative-real? distance))

       ;; swap start and end
       (method (reverse p)
	       (citylink (.to p)
			 (.from p)
			 (.distance p))))


;; a path is a list of citylinks; define an object holding it
(class path
       (struct constructor-name: _path
	       #((typed-list-of citylink?) links)
	       #(nonnegative-real? total-distance))

       ;; n-ary custom constructor function that takes the links
       ;; making up a path:
       (def (path . links)
	    (_path (list.typed-list citylink? (reverse links))
		   (fold + 0 (map .distance links))))

       (method (add p link)
	       (_path (.cons (.links p) link)
		      (+ (.total-distance p) (.distance link))))

       ;; "first" link when looking backwards:
       (method (first p)
	       (.first (.links p)))

       (method (show p)
	       (list
		(.total-distance p)
		(let ((l (map .from
			      (.reverse-list
			       (.links p)
			       ;; append fake link for end city to
			       ;; make it show up:
			       (let ((c (.to (.first (.links p)))))
				 (list (citylink c c 0)))))))
		  ;; if the first and second city are the same,
		  ;; then that's because of the stupid initial
		  ;; frontier value from treesearch; drop the
		  ;; duplicate then.
		  (if (and (length->= l 2)
			   (eq? (first l) (second l)))
		      (rest l)
		      l)))))

(TEST
 > (.show (path (citylink 'A 'B 10)))
 (10 (A B))
 > (.show (path (citylink 'A 'B 10) (citylink 'B 'C 4.5)))
 (14.5 (A B C)))


;; The "frontier" (as called in the video) is a boxed list of paths
;; (boxing is necessary to allow for mutation--the used data
;; structures are immutable). It's the set of paths already taken.

;; remove-choice!  choses one of the paths, removes it from the
;; frontier and returns it
(def (remove-choice! frontier)
     (let ((l (sort (unbox frontier)
		    (on .total-distance <))))
       (set-box! frontier (rest l))
       (first l)))

(def (add! path frontier)
     (println "adding: " (.to (.first path)))
     (let ((l (unbox frontier)))
       (set-box! frontier (cons path l))))

(TEST
 > (def f (box (list (path (citylink 'A 'B 3))
		     (path (citylink 'A 'C 2))
		     (path (citylink 'A 'D 2.5)))))
 > (.show (remove-choice! f))
 (2 (A C))
 > (map .show (unbox f))
 ((2.5 (A D)) (3 (A B))))



(def (treesearch #((list-of citylink?) links)
		 #(city? start)
		 #(city? end))

     (def links* (append links
			 (map .reverse links)))
     
     ;; all links away from a given city:
     (def (links-for #(city? city))
	  (filter (lambda (cl)
		    (eq? (.from cl) city))
		  links*))

     (def frontier (box (list
			 (path (citylink start start 0)))))

     (let loop ()
       ;;(step)
       (if (null? (unbox frontier))
	   'FAIL
	   (let* ((path (remove-choice! frontier))
		  (s (.first path))
		  (city (.to s)))
	     (if (eq? city end)
		 path
		 (begin
		   (println city)
		   (for-each (lambda (a)
			       (add! (.add path a) frontier))
			     (links-for city))
		   (loop)))))))


(def treesearch* (comp .show treesearch))


;; (see test(s) in main.scm)

