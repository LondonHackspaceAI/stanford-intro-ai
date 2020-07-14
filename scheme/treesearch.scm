(require easy
         (list-util-2 segregate*)
         (predicates nonnegative-real? length->= box-of)
         wbcollection
         debug
         maybe
         oo-util-lazy
         test)


;; Node: we use symbols to represent nodes

(def Node? symbol?)

(def. Node.name symbol.string)

(def (Node= [Node? a] [Node? b])
     (eq? a b))

(def Node< symbol<?)

(def Node-cmp symbol-cmp)


(defclass (Edge [Node? from]
                [Node? to]
                [nonnegative-real? distance])
  "a path segment between two nodes"

  (defmethod (reverse s)
    "swap start and end"
    (Edge to from distance)))


(defclass (Path [(ilist-of Edge?) edges]
                [nonnegative-real? total-distance])
  "a list of path segments (edges)"

  (def empty-Path (Path '() 0))
  
  ;; n-ary custom constructor function that takes the edges
  ;; making up a Path:
  (def (path . edges)
       (fold (flip Path.add) empty-Path edges))

  (defmethod (add s edge)
    (Path (.cons edges edge)
          (+ total-distance (.distance edge))))

  (defmethod (first s)
    "'first' edge as when looking backwards"
    (.first edges))

  (defmethod (view s)
    (list total-distance
          (let ((l (map .from
                        (reverse/tail
                         edges
                         ;; append fake edge for end node to
                         ;; make it show up:
                         (let ((c (=> edges .first .to)))
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
    (on .total-distance real-cmp)))


(TEST
 > (.show (path (Edge 'A 'B 10)))
 (Path (list (Edge 'A 'B 10)) 10)

 ;; Had .view first, and it's still producing shorter output
 > (.view (path (Edge 'A 'B 10)))
 (10 (A B))
 > (.view (path (Edge 'A 'B 10) (Edge 'B 'C 4.5)))
 (14.5 (A B C)))



(defclass (Frontier [wbcollection? pathcollection])
  "the collection of paths already taken."

  (def (frontier . paths)
       (Frontier (list.wbcollection Path.distance-cmp paths)))

  (defmethod (maybe-remove-choice s) -> (maybe (values-of Path? Frontier?))
    "choses one of the paths, removes it from the frontier and returns it"
    (>>= (.maybe-min&rest pathcollection)
         (lambda-values ((min rest))
                   (return (values min (Frontier rest))))))

  (defmethod (add s path)
    (DEBUG "adding:" (=> path .first .to))
    (Frontier.pathcollection-set s (.set pathcollection path)))

  ;; delegates
  (defmethod list (comp .list .pathcollection))
  (defmethod empty? (comp .empty? .pathcollection)))


(TEST
 > (def f (frontier (path (Edge 'A 'B 3))
                    (path (Edge 'A 'C 2))
                    (path (Edge 'A 'D 2.5))))
 > (def-values (p f*) (.maybe-remove-choice f))
 > (.show p)
 (Path (list (Edge 'A 'C 2)) 2)
 > (.show f*)
 (Frontier (list.wbcollection
            Path.distance-cmp
            (list (Path (list (Edge 'A 'D 2.5)) 2.5)
                  (Path (list (Edge 'A 'B 3)) 3))))
 > (.view p)
 (2 (A C))
 > (map .view (.list f*))
 ((2.5 (A D)) (3 (A B))))


(def. (false.view v)
  v)



(def (frontier-update [Frontier? front]
                      [wbcollection? visited] ;; nodes
                      [Path? path]
                      [(ilist-of Edge?) edges])
     -> Frontier?

     (fold (lambda (a front)
             (if (.contains? visited (.to a))
                 front
                 (Frontier.add front
                               (.add path a))))
           front
           edges))


(def (treesearch [(ilist-of Edge?) edges]
                 [Node? start]
                 [Node? end])
     -> (maybe Path?)

     ;; all edges away from a given node:
     (def edges-for
          (let (t (list->table (segregate* (append edges
                                                   (map .reverse edges))
                                           .from
                                           Node<)))
            (lambda ([Node? c]) -> (ilist-of Edge?)
               (table-ref t c '()))))

     (let search ((front (frontier (path (Edge start start 0))))
                  (visited (empty-wbcollection Node-cmp)))
       (>>= (.maybe-remove-choice front)
            (lambda-values
             ((path front)) 
             (let (node (.to (.first path)))
               (if (Node= node end)
                   (return path)
                   (begin (DEBUG node)
                          (search (frontier-update front
                                                   visited
                                                   path
                                                   (edges-for node))
                                  (.set visited node)))))))))


(def treesearch* (=>*/arity 3 treesearch .view))


(def (lists->Edges l)
     (map (applying Edge) l))


(def (ts ls a b)
     (treesearch* (lists->Edges ls) a b))

(TEST
 > (ts '() 'A 'B)
 #f
 > (ts '((A C 3)) 'A 'B)
 #f ;; looped before seen check
 > (ts '((C B 3)) 'A 'B)
 #f
 > (ts '((X Y 3)) 'A 'B)
 #f
 > (ts '((A A 3)) 'A 'B)
 #f ;; looped before seen check
 > (ts '((A B 3)) 'A 'B)
 (3 (A B))
 > (ts '((B A 3)) 'A 'B)
 (3 (A B))

 ;; multiple direct paths
 > (ts '((B A 3) (B A 2)) 'A 'B)
 (2 (A B))
 > (ts '((B A 3) (A B 2)) 'A 'B)
 (2 (A B))
 > (ts '((A B 3) (B A 2)) 'A 'B)
 (2 (A B))
 > (ts '((A B 3) (A B 2)) 'A 'B)
 (2 (A B))

 ;; unused paths
 > (ts '((A B 3) (B C 2)) 'A 'B)
 (3 (A B))
 > (ts '((A B 3) (B C 2)) 'B 'A)
 (3 (B A))

 ;; multi-segment
 > (ts '((A B 3) (B C 2)) 'A 'C)
 (5 (A B C))
 > (ts '((A B 3) (B C 2)) 'C 'A)
 (5 (C B A))

 ;; with alternatives: same length
 > (ts '((A B 3) (B C 2) (A C 5)) 'A 'C)
 (5 (A B C))
 > (ts '((A B 3) (B C 2) (A C 5)) 'C 'A)
 (5 (C B A))

 > (ts '((A C 5) (A B 3) (B C 2)) 'A 'C)
 (5 (A B C)) ;; it prefers to take the shorter path first. right?

 ;; with alternative of shorter length
 > (ts '((A C 4) (A B 3) (B C 2)) 'A 'C)
 (4 (A C))
 > (ts '((A C 4) (A B 3) (B C 2)) 'C 'A)
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
