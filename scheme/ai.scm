(require easy
	 typed-list
	 (predicates nonnegative-real?))


;; we use symbols to represent cities
(def city? symbol?)

;; a citylink is a path segment between two cities
(class citylink
       (struct #(city? from)
	       #(city? to)
	       #(nonnegative-real? distance)))

;; a path is a list of citylinks; define an object holding it
(class path
       (struct constructor-name: _path
	       #((typed-list-of citylink?) links))
       ;; n-ary custom constructor function that takes the links
       ;; making up a path:
       (def (path . links)
	    (_path (list.typed-list citylink? links)))
       (method (add p link)
	       (_path (.cons (.links p) link)))
       ;; "first" link when looking backwards:
       (method (first p)
	       (.first (.links p)))
       (method (show p)
	       (cons (.to (.first (.links p)))
		     (map .from (reverse (cdr (reverse (.list (.links p)))))))))


;; to safe typing effort, enter values as a list of bare lists:
(def links '((Arad Zerind 75)
	     (Arad Sibiu 140)
	     (Arad Timisoara 118)
	     (Zerind Dradea 71)
	     (Dradea Sibiu 151)
	     (Sibiu Fagaras 99)
	     (Sibiu RimnicuVilcea 80)
	     (Fagaras Bucharest 211)
	     (RimnicuVilcea Pitesti 97)
	     (RimnicuVilcea Craiova 146)
	     (Pitesti Craiova 138)
	     (Pitesti Bucharest 101)
	     (Timisoara Lugoj 111)
	     (Lugoj Mehadia 70)
	     (Mehadia Drobeta 75)
	     (Drobeta Craiova 120)
	     (Bucharest Giurgiu 90)
	     (Bucharest Urziceni 85)
	     (Urziceni Hirsova 98)
	     (Hirsova Eforie 86)
	     (Urziceni Vaslui 142)
	     (Vaslui Iasi 92)
	     (Iasi Neamt 87)))

;; turn the bare lists into citylink objects:
(def l1 (map (applying citylink) links))
;; and again with from and to fields reversed:
(def l2 (map (applying
	      (lambda (a b c)
		(citylink b a c))) links))

(def links* (append l1 l2))

;; all links away from a given city:
(def (links-for #(city? city))
     (filter (lambda (cl)
	       (eq? (.from cl) city))
	     links*))


;; The "frontier" (as called in the video) is a boxed list of paths
;; (boxing is necessary to allow for mutation--the used data
;; structures are immutable). It's the set of paths already taken.

;; remove-choice!  choses one of the paths, removes it from the
;; frontier and returns it
(def (remove-choice! frontier)
     (let ((l (reverse (unbox frontier))))
       (set-box! frontier (reverse (rest l)))
       (first l)))

(def (add! path frontier)
     (let ((l (unbox frontier)))
       (set-box! frontier (cons path l))))


(def (treesearch #(city? start)
		 #(city? end))

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
