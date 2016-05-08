(require easy
	 typed-list
	 (list-util-2 segregate*)
	 (predicates nonnegative-real? length->= box-of)
	 test
	 cj-seen
	 wbcollection
	 (cj-functional-2 chain*))

(defmacro (IF-DEBUG arg)
  (if #t
      arg
      `(void)))


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

       (method (view p)
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
		      l))))

       ;; compare two paths, returning lt eq gt
       (method distance-cmp (on .total-distance number-cmp)))

(TEST
 > (.view (path (citylink 'A 'B 10)))
 (10 (A B))
 > (.view (path (citylink 'A 'B 10) (citylink 'B 'C 4.5)))
 (14.5 (A B C)))


;; The "frontier" (as called in the video) is the collection of paths
;; already taken.

(class frontier
       (struct constructor-name: _frontier
	       #(wbcollection? pathcollection))
       
       (def (frontier . paths)
	    (_frontier (list.wbcollection .distance-cmp paths)))

       ;; remove-choice choses one of the paths, removes it from the
       ;; frontier and returns it
       (method (remove-choice f)
	       (letv ((min rest) (.min&rest (.pathcollection f)))
		     (values min (_frontier rest))))

       (method (add f path)
	       (IF-DEBUG (println "adding: " (.to (.first path))))
	       (.pathcollection-update f (chain* (.add path))))

       ;; delegates
       (method list (comp .list .pathcollection))
       (method empty? (comp .empty? .pathcollection)))

(TEST
 > (def f (frontier (path (citylink 'A 'B 3))
		    (path (citylink 'A 'C 2))
		    (path (citylink 'A 'D 2.5))))
 > (defvalues (p f*) (.remove-choice f))
 > (.view p)
 (2 (A C))
 > (map .view (.list f*))
 ((2.5 (A D)) (3 (A B))))


(def. (symbol.view v)
  v)



(def (treesearch #((list-of citylink?) links)
		 #(city? start)
		 #(city? end))

     ;; all links away from a given city:
     (def links-for
	  (let* ((links* (append links
				 (map .reverse links)))
		 (t (list->table (segregate* links* .from symbol<?))))
	    (lambda (#(city? c))
	      (table-ref t c '()))))

     (let loop ((frontier (frontier (path (citylink start start 0))))
		(visited (empty-wbcollection symbol-cmp)))
       ;;(step)
       (if (.empty? frontier)
	   'FAIL
	   (letv ((path frontier) (.remove-choice frontier))
		 (let* ((s (.first path))
			(city (.to s)))
		   (if (eq? city end)
		       path
		       (begin
			 (IF-DEBUG (println city))
			 (loop
			  (fold (lambda (a frontier)
				  (if (.contains? visited (.to a))
				      frontier
				      (.add frontier (.add path a))))
				frontier
				(links-for city))
			  (.add visited city)))))))))


(def treesearch* (comp .view treesearch))


(def (lists.citylinks l)
     (map (applying citylink) l))


(def (treesearch** ls a b)
     (treesearch* (lists.citylinks ls) a b))

(TEST
 > (treesearch** '() 'A 'B)
 FAIL
 > (treesearch** '((A C 3)) 'A 'B)
 FAIL ;; looped before seen check
 > (treesearch** '((C B 3)) 'A 'B)
 FAIL
 > (treesearch** '((X Y 3)) 'A 'B)
 FAIL
 > (treesearch** '((A A 3)) 'A 'B)
 FAIL ;; looped before seen check
 > (treesearch** '((A B 3)) 'A 'B)
 (3 (A B))
 > (treesearch** '((B A 3)) 'A 'B)
 (3 (A B))

 ;; multiple direct paths
 > (treesearch** '((B A 3) (B A 2)) 'A 'B)
 (2 (A B))
 > (treesearch** '((B A 3) (A B 2)) 'A 'B)
 (2 (A B))
 > (treesearch** '((A B 3) (B A 2)) 'A 'B)
 (2 (A B))
 > (treesearch** '((A B 3) (A B 2)) 'A 'B)
 (2 (A B))

 ;; unused paths
 > (treesearch** '((A B 3) (B C 2)) 'A 'B)
 (3 (A B))
 > (treesearch** '((A B 3) (B C 2)) 'B 'A)
 (3 (B A))

 ;; multi-segment
 > (treesearch** '((A B 3) (B C 2)) 'A 'C)
 (5 (A B C))
 > (treesearch** '((A B 3) (B C 2)) 'C 'A)
 (5 (C B A))

 ;; with alternatives: same length
 > (treesearch** '((A B 3) (B C 2) (A C 5)) 'A 'C)
 (5 (A B C))
 > (treesearch** '((A B 3) (B C 2) (A C 5)) 'C 'A)
 (5 (C B A))

 > (treesearch** '((A C 5) (A B 3) (B C 2)) 'A 'C)
 (5 (A B C)) ;; it prefers to take the shorter path first. right?

 ;; with alternative of shorter length
 > (treesearch** '((A C 4) (A B 3) (B C 2)) 'A 'C)
 (4 (A C))
 > (treesearch** '((A C 4) (A B 3) (B C 2)) 'C 'A)
 (4 (C A))

 ;; Now for all of these (that already have them): add random
 ;; unrelated, or singly connected, or even cross connected between
 ;; them, links that only link (directly or indirectly?) to the source
 ;; or target city (i.e. only link to one of the two, except for the
 ;; existing links I set out above).  TODO.

 ;; Also, add longer multi-segment paths (more segments, as well as
 ;; longer distances).
 )


;; (also see test(s) in main.scm)
