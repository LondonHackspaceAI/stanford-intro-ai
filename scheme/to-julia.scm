(require easy
	 treesearch)

(def. (number.julia-string s)
  (number->string s))

(def. (string.julia-string s)
  (object->string s))

(def. (symbol.julia-string s)
  (string.julia-string (symbol.string s)))

(def. (citylink.julia-string l)
  (let. ((from to distance) l)
	(string-append "Segment(Node("
		       (.julia-string from)
		       "), Node("
		       (.julia-string to)
		       "), "
		       (.julia-string distance)
		       ")")))

(def (links->julia links)
     (string-append "a = ["
		    (strings-join (map .julia-string links)
				  ",\n     ")
		    "]"))

