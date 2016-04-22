(require easy)

(class citylink (struct #(symbol? from)
			#(symbol? to)
			#(positive-real? distance)))

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

(def links*
     (append (map (applying citylink) links)
	     (map (applying
		   (lambda (a b c)
		     (citylink b a c))) links)))

(def (links-for city)
     (filter (lambda (cl)
	       (eq? city (.from cl)))
	     links*))


;; (def start 'Arad)
;; (def end 'Bucharest)

;; paths

(def (remove-choice! frontier)
     (let ((l (unbox frontier)))
       (set-box! frontier (cdr l))
       (first l)))

(def (add! path frontier)
     (let ((l (unbox frontier)))
       (set-box! frontier (cons path l))))

(def (treesearch #(symbol? start)
		 #(symbol? end))

     (def frontier (box (list
			 ;; path:
			 (list (citylink start start 0)))))

     (let loop ()
       (if (null? frontier)
	   'FAIL
	   (let* ((path (remove-choice! frontier))
		  (s (first path)))
	     (if (eq? (.to s) end)
		 path
		 (for-each (lambda (a)
			     (add! (cons a path) frontier))
			   (links-for s)))))))

