#lang dssl
(define-struct account [id owner balance])
(define ACCOUNT0 (account 0 "Alan Turing" 16384))
(define ACCOUNT1 (account 1 "Grace Hopper" 32768))
(define ACCOUNT2 (account 2 "Ada Lovelace" 32))
(define ACCOUNT3 (account 3 "David Parnas" 2048))
(define ACCOUNT4 (account 4 "Barbara Liskov" 8192))

(define (account-copy old-account)
  (account (account-id old-account) (account-owner old-account) (account-balance old-account)))
(check-expect (equal? (account-copy ACCOUNT2) ACCOUNT2) #true)
(check-expect (eq? (account-copy ACCOUNT2) ACCOUNT2) #false)

(define (account-credit! amount account)
  (set-account-balance! account amount))
(check-expect
 (begin
   (define countess-lovelace (account-copy ACCOUNT2))
   (account-credit! 200 countess-lovelace)
   countess-lovelace)
 (account 2 "Ada Lovelace" 232))

(define (account-transfer! amount from to)
  (set-account-balance! from (- (account-balance from) amount))
  (set-account-balance! to (+ (account-balance to) amount)))
(check-expect
 (begin
   (define parnas (account-copy ACCOUNT3))
   (define liskov (account-copy ACCOUNT4))
   (account-transfer! 1000 parnas liskov)
   (vector parnas liskov))
   (vector (account 3 "David Parnas" 1048)
           (account 4 "Barbara Liskov" 9192)))

(define (vector-swap! vec i j)
  (define temp (vector-ref vec i))
  (vector-set! vec i (vector-ref vec j))
  (vector-set! vec j temp))
(check-expect
 (let ([vec (vector 2 3 4 5 6)])
   (vector-swap! vec 1 3)
   vec)
 (vector 2 5 4 3 6))

(define (vector-copy vec)
  (build-vector (vector-length vec)
                (lambda (i) (vector-ref vec i))))
(check-expect
 (let* ([v (vector 2 3 4)] [w (vector-copy v)])
   (vector-set! w 0 100)
   w)
 (vector 2 3 4))

(define (vector-copy-resize size vec)
  (build-vector size (lambda (q)
                       (if (<= q (-(vector-length vec) 1))
                       (vector-ref vec q)
                       #false))))
(check-expect (vector-copy-resize 5 (vector 2 3 4 5 6 7 8))
              (vector 2 3 4 5 6))
(check-expect (vector-copy-resize 5 (vector 2 3 4))
              (vector 2 3 4 #false #false))

(define (find-largest-account accounts)
   (define largest 0)
   (define return-account (account 0 null 0))
   (for [(element accounts)]
     (if (< largest (account-balance element))
         (begin
           (set! largest (account-balance element))
           (set! return-account element))
         (set! return-account return-account))) return-account)

(check-expect
  (find-largest-account (vector ACCOUNT0 ACCOUNT1 ACCOUNT2 ACCOUNT3 ACCOUNT4))
  ACCOUNT1)
(check-expect
  (find-largest-account (vector ACCOUNT4 ACCOUNT3 ACCOUNT2 ACCOUNT1 ACCOUNT0))
  ACCOUNT1)
