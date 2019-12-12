(require easy
	 typed-list
	 (list-util-2 segregate*)
	 (predicates nonnegative-real? length->= box-of)
	 test
	 cj-seen
	 wbcollection)


(defmacro (DEBUG dbg . body)
  (if #t
      `(begin ,dbg ,@body)
      `(begin ,@body)))


;; we use symbols to represent nodes
(def Node? symbol?)
(def. Node.name symbol.string)
(def (Node= [Node? a] [Node? b])
     (eq? a b))


(defclass (Edge [Node? from]
                [Node? to]
                [nonnegative-real? distance])
  "a path segment between two nodes"

  (defmethod (reverse s)
    "swap start and end"
    (Edge to from distance)))


(defclass (Path [(typed-list-of Edge?) links]
                [nonnegative-real? total-distance])
  "a list of path segments (edges)"

  (def empty-Path (Path (typed-list-null Edge?) 0))
  
  ;; n-ary custom constructor function that takes the links
  ;; making up a Path:
  (def (path . links)
       (fold (flip Path.add) empty-Path links))

  (defmethod (add s link)
    (Path (.cons links link)
          (+ total-distance (.distance link))))

  (defmethod (first s)
    "'first' link as when looking backwards"
    (.first links))

  (defmethod (view s)
    (list
     total-distance
     (let ((l (map .from
		   (.reverse-list
		    links
		    ;; append fake link for end node to
		    ;; make it show up:
		    (let ((c (.to (.first links))))
		      (list (Edge c c 0)))))))
       ;; if the first and second node are the same,
       ;; then that's because of the stupid initial
       ;; frontier value from treesearch; drop the
       ;; duplicate then.
       (if (and (length->= l 2)
		(Node= (first l) (second l)))
	   (rest l)
	   l))))

  ;; compare two paths, returning lt eq gt
  (defmethod distance-cmp
    (on .total-distance number-cmp)))


(TEST
 > (.show (path (Edge 'A 'B 10)))
 (Path (typed-list Edge? (Edge 'A 'B 10)) 10)

 ;; Had .view first, could drop it now... XX
 > (.view (path (Edge 'A 'B 10)))
 (10 (A B))
 > (.view (path (Edge 'A 'B 10) (Edge 'B 'C 4.5)))
 (14.5 (A B C)))



(defclass (Frontier [wbcollection? pathcollection])
  "the collection of paths already taken."

  (def (frontier . paths)
       (Frontier (list.wbcollection Path.distance-cmp paths)))

  (defmethod (remove-choice s)
    "choses one of the paths, removes it from the frontier and returns it"
    (letv ((min rest) (.min&rest pathcollection))
	  (values min (Frontier rest))))

  (defmethod (add s path)
    (DEBUG (println "adding: " (=> path .first .to))
	   (Frontier.pathcollection-set s (.set pathcollection path))))

  ;; delegates
  (defmethod list (comp .list .pathcollection))
  (defmethod empty? (comp .empty? .pathcollection)))


(TEST
 > (def f (frontier (path (Edge 'A 'B 3))
		    (path (Edge 'A 'C 2))
		    (path (Edge 'A 'D 2.5))))
 > (defvalues (p f*) (.remove-choice f))
 > (.view p)
 (2 (A C))
 > (map .view (.list f*))
 ((2.5 (A D)) (3 (A B))))


(def. (false.view v)
  v)



(def (frontier-update [Frontier? front]
                      [wbcollection? visited] ;; nodes
                      [Path? path]
                      [(list-of Edge?) links])
     -> Frontier?

     (fold (lambda (a front)
             (if (.contains? visited (.to a))
                 front
                 (Frontier.add front
                               (.add path a))))
           front
           links))


(def (treesearch [(list-of Edge?) links]
		 [Node? start]
		 [Node? end])
     -> (maybe Path?)

     ;; all links away from a given node:
     (def links-for
	  (let* ((links* (append links
				 (map .reverse links)))
		 (t (list->table (segregate* links* .from symbol<?))))
	    (lambda ([Node? c]) -> (list-of Edge?)
               (table-ref t c '()))))

     (let loop ((front (frontier (path (Edge start start 0))))
		(visited (empty-wbcollection symbol-cmp)))
       (if (.empty? front)
	   #f
	   (letv ((path front) (.remove-choice front))
		 (let (node (.to (.first path)))
		   (if (Node= node end)
		       path
		       (DEBUG (println node)
			      (loop (frontier-update front
                                                     visited
                                                     path
                                                     (links-for node))
                                    (.set visited node)))))))))


(def treesearch* (comp// 3 .view treesearch))


(def (lists->Edges l)
     (map (applying Edge) l))


(def (treesearch** ls a b)
     (treesearch* (lists->Edges ls) a b))

(TEST
 > (treesearch** '() 'A 'B)
 #f
 > (treesearch** '((A C 3)) 'A 'B)
 #f ;; looped before seen check
 > (treesearch** '((C B 3)) 'A 'B)
 #f
 > (treesearch** '((X Y 3)) 'A 'B)
 #f
 > (treesearch** '((A A 3)) 'A 'B)
 #f ;; looped before seen check
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
 ;; or target node (i.e. only link to one of the two, except for the
 ;; existing links I set out above).  TODO.

 ;; Also, add longer multi-segment paths (more segments, as well as
 ;; longer distances).
 )


;; (also see test(s) in main.scm)
