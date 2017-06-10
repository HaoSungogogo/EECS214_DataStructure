#lang dssl
;;;Representation
;; Vertex is Natural
;; Weight is Number

;;Maybe Weight is one of:
;; Weight
;; #false

;;A WUGraph is repersented as adjacency matrix, clearly it is a vector of vector.

(define GRAPH1 (vector (vector 0 2 #false 5)
                        (vector 2 0 3 #false)
                        (vector #false 3 0 4)
                        (vector 5 #false 4 0)))
(define GRAPH2 (vector (vector 0 5 #false #false #false #false)
                       (vector 5 0 1 3 #false #false)
                       (vector #false 1 0 #false 2 7)
                       (vector #false 3 #false 0 4 6)
                       (vector #false #false 2 4 0 #false)
                       (vector #false #false 7 6 #false 0)))

(define (graph-size graph)
  (vector-length graph))



(define (get-edge graph i j)
  (vector-ref (vector-ref graph i) j))



(define (make-graph size)
  (define matr
  (build-vector size (lambda(i) (make-vector size #false))))
  matr)

(define (set-edge! graph i j weight)
  (vector-set! (vector-ref graph i) j weight)
  (vector-set! (vector-ref graph j) i weight))



(define (get-adjacent graph i)
  (let loop ([j 0] [vec (vector-ref graph i)] [length (vector-length graph)] [lst (list )])
    (if (< j length)
        (if (or (false? (vector-ref vec j)) (zero? (vector-ref vec j)))
            (loop (+ 1 j) vec length lst)
            (loop (+ 1 j) vec length (cons j lst)))
        lst)))
        
        


(define (dfs graph start)
  (define (Visit v)
    (when (false? (vector-ref seen v))
        (begin
         (vector-set! seen v #true)
         (set! Lst (cons v Lst))
         (for [(element (get-adjacent graph v))]
           (Visit element)))))
  (define seen (make-vector (vector-length graph) #false))
  (define Lst (list ))
  (Visit start)
  Lst)

(define (sort lst)
  (cond
    [(or (empty? lst) (empty? (rest lst)))
     lst]
    [else
      (define pivot (first lst))
      (define non-pivot (rest lst))
      (define before (filter (lambda (x) (< x pivot)) non-pivot))
      (define after (filter (lambda (x) (>= x pivot)) non-pivot))
      (append (sort before) (cons pivot (sort after)))]))

;;;
;;; TESTING
;;;

;; You should test your code thoroughly. Here are some tests to get you
;; started, which you should uncomment when ready:


(check-expect (graph-size GRAPH1) 4)
(check-expect (graph-size GRAPH2) 6)

(check-expect (get-edge GRAPH1 0 1) 2)
(check-expect (get-edge GRAPH1 0 2) #false)

;(check-expect (get-adjacent GRAPH1 0)
;              (sort (list 3 1)))
;(check-expect (get-adjacent GRAPH2 3)
;              (sort (list 5 4 1)))

(check-expect (get-adjacent GRAPH1 0)
               (list 3 1))
(check-expect (get-adjacent GRAPH2 3)
              (list 5 4 1))


(check-expect
  (begin
    (define graph (make-graph 5))
    (set-edge! graph 1 3 10)
    (list (get-edge graph 1 3)
          (get-edge graph 1 3)
          (get-edge graph 1 4)))
  (list 10 10 #false))

;; DFS tests---see below for explanation of `sort`.

(check-expect
  (sort (dfs GRAPH1 0))
  '(0 1 2 3))

(check-expect
  (sort (dfs GRAPH2 0))
  '(0 1 2 3 4 5))

(check-expect
  (begin
    (define graph (make-graph 3))
    (set-edge! graph 0 2 10)
    (sort (dfs graph 0)))
  '(0 2))


;;;
;;; TESTING HELPERS
;;;

;; The following function may be convenient for creating graphs for
;; tests. It uses the graph API that you are defining above, so if you
;; define make-graph and set-edge! correctly then it will work.

;; build-graph : N [List-of (list Vertex Vertex Weight)] -> WUGraph
;; Returns a new graph of n vertices containing the given edges.
(define (build-graph n edges)
  (local [(define new-graph (make-graph n))]
    (begin
      (map (lambda (edge)
             (set-edge! new-graph (first edge) (second edge) (third edge)))
           edges)
      new-graph)))

;; Here's an example using build-graph to create a graph:
(define GRAPH3
  (build-graph 6
               '((0 1 5)
                 (0 2 7)
                 (0 3 2)
                 (1 4 9)
                 (1 5 6)
                 (3 5 0)
                 (3 4 1))))

;; DFS returns the list of nodes visited, but not in any particular
;; order, which makes it difficult to test. One way to test it is to
;; sort the resulting list and compare to the result we expect from
;; that. For example, suppose we have a graph where the DFS reaches
;; vertices 1, 2, and 3. Then the result of `dfs` could be (list 1 2 3)
;; or (list 2 1 3) or (list 3 1 2) etc. However, if we sort that then
;; the result of *that* should always be (list 1 2 3).

;; [List-of Number] -> [List-of Number]
;; Sorts a list of numbers.

(check-expect (sort '()) '())
(check-expect (sort '(4)) '(4))
(check-expect (sort '(5 3)) '(3 5))
(check-expect (sort '(3 5)) '(3 5))
(check-expect (sort '(4 4 4)) '(4 4 4))
(check-expect (sort '(2 8 5 3 0 2 1)) '(0 1 2 2 3 5 8))

(run-all-tests)
