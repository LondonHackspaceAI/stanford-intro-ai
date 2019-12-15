(require easy
         treesearch
         test)

(def. (number.go-string s)
  (number->string s))

(def. (string.go-string s)
  (object->string s))

(def. (Node.go-string s)
  (string.go-string (Node.name s)))

(def.* (Edge.go-string l)
  (string-append "Edge{Node{"
                 (.go-string from)
                 "}, Node{"
                 (.go-string to)
                 "}, "
                 (.go-string distance)
                 "}"))

(def.* (Path.go-string p)
  (string-append "Path{Edge[]{"
                 (strings-join (map .go-string (.list edges))
                               ", ")
                 "}}"))

(TEST
 > (.go-string (path (Edge 'A 'B 10)))
 "Path{Edge[]{Edge{Node{\"A\"}, Node{\"B\"}, 10}}}")


(def Edges? (nonempty-list-of Edge?))

(def. (Edges.go-string edges)
  (string-append "a = ["
                 (strings-join (map .go-string edges)
                               ",\n     ")
                 "]"))

(TEST
 > (.go-string (list (Edge 'A 'B 10) (Edge 'C 'D 5)))
 "a = [Edge{Node{\"A\"}, Node{\"B\"}, 10},\n     Edge{Node{\"C\"}, Node{\"D\"}, 5}]")

