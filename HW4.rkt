#lang dssl
(define-struct UnionFindEntry [id weight])

(define (create size)
  (define vec (build-vector size
                     (lambda (i) (UnionFindEntry i 1))))
  vec)

(define (size uf)
  (vector-length uf))

(check-expect (size (create 12)) 12)


(define (get-entry uf ix)
  (vector-ref uf ix))
(define (reparent! uf x1 x2)
  (begin
    (set-UnionFindEntry-id! (get-entry uf x1) x2)
    (set-UnionFindEntry-weight! (get-entry uf x2) (+ (UnionFindEntry-weight (get-entry uf x2)) (UnionFindEntry-weight (get-entry uf x1))))))

(define (find uf obj)
  (let loop [(i obj)]
    (cond
      [(= i (UnionFindEntry-id (get-entry uf i))) i]
      [else
       (begin
         (set-UnionFindEntry-id! (get-entry uf i)
                                 (UnionFindEntry-id (get-entry uf (UnionFindEntry-id (get-entry uf i)))))
         (loop (UnionFindEntry-id (get-entry uf i))))])))

(define (same-set? uf obj1 obj2)
  (if (= (find uf obj1) (find uf obj2)) #true #false))

(define (union! uf obj1 obj2)
  (when (false? (same-set? uf obj1 obj2))
      (if (> (UnionFindEntry-weight (vector-ref uf (find uf obj1)))
             (UnionFindEntry-weight (vector-ref uf (find uf obj2))))
          (reparent! uf (find uf obj2) (find uf obj1))
          (reparent! uf (find uf obj1) (find uf obj2)))))



;;;
;;; UNION-FIND TESTING
;;;

; The code below gives a clean way to test your union-find code. The
; idea is that you write a “script” consisting of “union” commands and
; “same” queries, and then running the script returns a list of the
; results of the 'same queries.

; A UnionFindCommand is one of:
; - (list 'union N N)
; - (list 'same N N)
; Interp.:
; - (list 'union m n) means to union the sets containing `m` and `n`
; - (list 'same m n) means to check whether `m` and `n` are in the same
;   set, producing a boolean in the script output

; A UnionFindScript is [List-of UnionFindCommand]

; run-script : N UnionFindScript -> [List-of Boolean]
; Runs the given script on a new UnionFind universe of size `n`
; and returns the list of query results.
(define (run-script n script)
  (interpret-script! (create n) script))

; interpret-script! : UnionFind UnionFindScript -> [List-of Boolean]
; Runs the given script on a the given UnionFind universe and returns the
; list of query results.
(define (interpret-script! uf script)
  (local
    [(define (interpret-command command)
       (if (symbol=? (first command) 'union)
           (begin
             (union! uf (second command) (third command))
             (interpret-script! uf (rest script)))
           (local
             [(define b (same-set? uf (second command) (third command)))]
             (cons b (interpret-script! uf (rest script))))))]
    (if (null? script) '()
        (interpret-command (first script)))))

; Now some example tests:

(check-expect
 (run-script 10 '())
 '())

(check-expect
 (run-script 10
   '((same 0 1)
     (same 0 2)
     (same 0 3)))
 '(#false #false #false))

(check-expect
 (run-script 10
   '((same 0 1)
     (union 0 1)
     (same 0 1)
     (union 1 2)
     (union 2 3)
     (same 0 3)
     (same 0 4)))
 '(#false #true #true #false))


;;define some information about graph
(define (graph-size graph)
  (vector-length graph))

(define (get-edge graph i j)
  (vector-ref (vector-ref graph i) j))

(define (make-graph size)
  (build-vector size (lambda(i) (make-vector size #false))))

(define (set-edge! graph i j weight)
  (begin
    (vector-set! (vector-ref graph i) j weight)
    (vector-set! (vector-ref graph j) i weight)))
(define (get-adjacent graph i)
  (let loop ([j 0] [vec (vector-ref graph i)] [length (vector-length graph)] [lst (list )])
    (if (< j length)
        (if (or (false? (vector-ref vec j)) (equal? i j))
            (loop (+ 1 j) vec length lst)
            (loop (+ 1 j) vec length (cons j lst)))
        lst)))



        



;; some information about binary heap


(define-struct heap [size It? data])

(define (createheap capacity It?)
  (heap 0 It? (make-vector capacity #false)))

(define (find-min heap)
  (vector-ref (heap-data heap) 0))

(define (ref heap i)
  (vector-ref (heap-data heap) i))
(define (set heap i x)
  (vector-set! (heap-data heap) i x))

(define (left i)
  (+ (* 2 i) 1))
(define (right i)
  (+ (* 2 i) 2))
(define (parent i)
  (cond
    [(zero? i) 0]
  [else (floor (/ (- i 1) 2))]))

(define (swap! heap i j)
  (define temp (vector-ref (heap-data heap) i))
  (vector-set! (heap-data heap) i (vector-ref (heap-data heap) j))
  (vector-set! (heap-data heap) j temp))

(define (Lessthan? heap i j graph)
  (if ((heap-It? heap) (get-edge graph (list-ref (ref heap i) 0) (list-ref (ref heap i) 1)) (get-edge graph (list-ref (ref heap j) 0) (list-ref (ref heap j) 1)))
      #true
      #false))



(define (bubble-up! heap i graph)
  (when (Lessthan? heap i (parent i) graph)
      (begin
        (define h (parent i))
        (swap! heap i (parent i))
        (bubble-up! heap h graph))))

(define (insert! heap x graph)
  (if (= (heap-size heap) (vector-length (heap-data heap)))
      (error "it is full")
      (begin
        (set heap (heap-size heap) x)
        (bubble-up! heap (heap-size heap) graph)
        (set-heap-size! heap (+ 1 (heap-size heap))))))

(define (find-smaller-child heap i graph)
  (define left1 (left i))
  (define right1 (right i))
  (cond
    [(>= left1 (heap-size heap)) i]
    [(>= right1 (heap-size heap))
     (if (Lessthan? heap left1 i graph)
         left1
          i)]
    [(and (Lessthan? heap left1 i graph) (Lessthan? heap right1 i graph))
     (if (Lessthan? heap left1 right1 graph)
         left1
         right1)]
    [(and (Lessthan? heap left1 i graph) (not (Lessthan? heap right1 i graph)))
     left1]
    [(and (Lessthan? heap right1 i graph) (not (Lessthan? heap left1 i graph)))
     right1]
    [else i]))

(define (percolate-down! heap i graph)
  (when (not (= (find-smaller-child heap i graph) i))
      (begin
        (define h (find-smaller-child heap i graph))
        (swap! heap i (find-smaller-child heap i graph))
        (percolate-down! heap h graph))))

(define (remove-min! heap graph)
  (if (= (heap-size heap) 0)
      (error "it is empty")
      (begin
        (set heap 0 (ref heap (- (heap-size heap) 1)))
        (set heap (- (heap-size heap) 1) #false)
        (set-heap-size! heap (- (heap-size heap) 1))
        (percolate-down! heap 0 graph)
        )))

;; the hw4 information

(define (get-all-edges graph)
  (let loop ([i 0] [j 1] [g graph] [length (vector-length graph)] [lst (list )])
    (if (< i length)
       (if (< j length)
           (if (not (false? (get-edge g i j)))
               (loop i (+ 1 j) g length (cons (list i j) lst))
               (loop i (+ 1 j) g length lst))
           (loop (+ 1 i) (+ 2 i) g length lst))
       lst)))

(define (heap-sort It? xs graph)
  (begin
  (define h (createheap (length xs) <))
  (let loop ([i 0] [length1 (length xs)])
    (if (< i length1)
        (begin
          (insert! h (list-ref xs i) graph)
          (loop (+ 1 i) length1))
    h))))

(define (pq-extract-min! hp graph)
 (begin
   (define ele (vector-ref (heap-data hp) 0))
   (remove-min! hp graph)
   ele))

(define (get-all-edges/increasing g)
  (begin
  (define ls (get-all-edges g))
  (define hp (heap-sort < ls g))
  (let loop ([i 0] [length (heap-size hp)] [lst (list )])
    (if (< i length)
        (loop (+ 1 i) length (cons (vector-ref (heap-data hp) i) lst))
        lst))))


(define (kruskal-mst g)
  (begin
    (define unionfind (create (graph-size g)))
    (define gph (make-graph (graph-size g)))
    (define lst (get-all-edges g))
    (define hp (heap-sort < lst g))
    (let loop ([i 0] [length (heap-size hp)])
      (if (< i length)
      (begin
        (define ele (pq-extract-min! hp g))
      (when (false? (same-set? unionfind (list-ref ele 0) (list-ref ele 1)))
        (begin
          (union! unionfind (list-ref ele 0) (list-ref ele 1))
          (set-edge! gph (list-ref ele 0) (list-ref ele 1) (get-edge g (list-ref ele 0) (list-ref ele 1)))))
          (loop (+ 1 i) length))
      gph))))

;; [List-of Number] -> [List-of Number]
;; Sorts a list of numbers.
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
;;

(define (build-graph n edges)
  (local [(define new-graph (make-graph n))]
    (begin
      (map (lambda (edge)
             (set-edge! new-graph (first edge) (second edge) (third edge)))
           edges)
      new-graph)))

(define EXAMPLE-GRAPH-1
  (build-graph 7
               '((0 1 3)
                 (1 2 3)
                 (2 3 1)
                 (3 4 3)
                 (4 5 3)
                 (6 0 2)
                 (6 1 2)
                 (6 2 2)
                 (6 3 3)
                 (6 4 2)
                 (6 5 2))))

(define EXAMPLE-MST-1 (kruskal-mst EXAMPLE-GRAPH-1))

(check-expect (get-adjacent EXAMPLE-MST-1 0) '(6))
(check-expect (get-adjacent EXAMPLE-MST-1 1) '(6))
(check-expect (sort (get-adjacent EXAMPLE-MST-1 2)) '(3 6))
(check-expect (get-adjacent EXAMPLE-MST-1 3) '(2))
(check-expect (get-adjacent EXAMPLE-MST-1 4) '(6))
(check-expect (get-adjacent EXAMPLE-MST-1 5) '(6))
(check-expect (sort (get-adjacent EXAMPLE-MST-1 6)) '(0 1 2 4 5))