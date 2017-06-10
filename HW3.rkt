#lang dssl
(define-struct heap [size It? data])

(define (create capacity It?)
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

(define (Lessthan? heap i j)
  (if ((heap-It? heap) (ref heap i) (ref heap j))
      #true
      #false))

(define (bubble-up! heap i)
  (when (Lessthan? heap i (parent i))
      (begin
        (swap! heap i (parent i))
        (bubble-up! heap (parent i)))))

(define (heap-copy heap2)                                                               ;; the function that enlarge as twice as previous vector and return the revised heap
  (define new-vector (make-vector (* 2 (vector-length (heap-data heap2))) #false))
  (let loop ([i 0] [h heap2] [len (vector-length (heap-data heap2))])
   (if (< i len)
       (begin
         (vector-set! new-vector i (vector-ref (heap-data h) i))
         (loop (+ i 1) h len))
       (begin
         (set-heap-data! heap2 new-vector)
         heap2))))

(define (insert! heap x)
  (if (= (heap-size heap) (vector-length (heap-data heap)))
      (begin
        (insert! (heap-copy heap) x))       ;; call the heap-copy function when the vector is full
      (begin
        (set heap (heap-size heap) x)
        (bubble-up! heap (heap-size heap))
        (set-heap-size! heap (+ 1 (heap-size heap))))))

(define (find-smaller-child heap i)
  (define left1 (left i))
  (define right1 (right i))
  (cond
    [(>= left1 (heap-size heap)) i]
    [(>= right1 (heap-size heap))
     (if (Lessthan? heap left1 i)
         left1
          i)]
    [(and (Lessthan? heap left1 i) (Lessthan? heap right1 i))
     (if (Lessthan? heap left1 right1)
         left1
         right1)]
    [(and (Lessthan? heap left1 i) (not (Lessthan? heap right1 i)))
     left1]
    [(and (Lessthan? heap right1 i) (not (Lessthan? heap left1 i)))
     right1]
    [else i]))

(define (percolate-down! heap i)
  (when (not (= (find-smaller-child heap i) i))
      (begin
        (swap! heap i (find-smaller-child heap i))
        (percolate-down! heap (find-smaller-child heap i)))))

(define (remove-min! heap)
  (if (= (heap-size heap) 0)
      (error "it is empty")
      (begin
        (set heap 0 (ref heap (- (heap-size heap) 1)))
        (set heap (- (heap-size heap) 1) #false)
        (set-heap-size! heap (- (heap-size heap) 1))
        (percolate-down! heap 0)
        )))

(define h (create 7 <))
(insert! h 7)
(insert! h 5)
(insert! h 6)
(insert! h 8)
(insert! h 3)
(insert! h 2)
(insert! h 9)
(insert! h 9)


