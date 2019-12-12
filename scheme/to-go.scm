(require easy
	 treesearch
	 test)

(def. (number.go-string s)
  (number->string s))

(def. (string.go-string s)
  (object->string s))

(def. (Node.go-string s)
  (string.go-string (Node.name s)))

(def. (Edge.go-string l)
  (let. ((from to distance) l)
	(string-append "Edge{Node{"
		       (.go-string from)
		       "}, Node{"
		       (.go-string to)
		       "}, "
		       (.go-string distance)
		       "}")))

(def. (Path.go-string p)
  (string-append "Path{Edge[]{"
		 (strings-join (map .go-string (.list (.links p)))
			       ", ")
		 "}}"))

(TEST
 > (.go-string (Path (Edge 'A 'B 10)))
 "Path{Edge[]{Edge{Node{\"A\"}, Node{\"B\"}, 10}}}")


(def (links->go links)
     (string-append "a = ["
		    (strings-join (map .go-string links)
				  ",\n     ")
		    "]"))

