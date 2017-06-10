#lang dssl
(define-struct account [id owner balance])
(define ACCOUNT0 (account 0  "Alan Turing"    16384))
(define ACCOUNT1 (account 8  "Grace Hopper"   32768))
(define ACCOUNT2 (account 16 "Ada Lovelace"   32))
(define ACCOUNT3 (account 24 "David Parnas"   2048))
(define ACCOUNT4 (account 32 "Barbara Liskov" 8192))
(define ACCOUNT5 (account 40 "Donald Knuth"   1024))

(define (account-copy old-account)
  (account (account-id old-account) (account-owner old-account) (account-balance old-account)))

(define (account-credit! amount account)
  (set-account-balance! account amount))

(define (account-transfer! amount from to)
  (set-account-balance! from (- (account-balance from) amount))
  (set-account-balance! to (+ (account-balance to) amount)))

(define-struct nil())
(define-struct node (element link))
(define LIST-LEDGER
  (node ACCOUNT0
        (node ACCOUNT1
              (node ACCOUNT2
                    (node ACCOUNT3
                          (node ACCOUNT4
                                (node ACCOUNT5
                                      (nil))))))))
(define (list-lookup id ledger)
  (cond
    [(nil? ledger) #false]
    [(= id (account-id (node-element ledger))) (node-element ledger)]
    [else (list-lookup id (node-link ledger))]))
(check-expect (list-lookup 0 LIST-LEDGER)
              ACCOUNT0)
(check-expect (list-lookup 8 LIST-LEDGER)
              ACCOUNT1)
(check-expect (list-lookup 16 LIST-LEDGER)
              ACCOUNT2)
(check-expect (list-lookup 20 LIST-LEDGER)
              #false)
(check-expect (list-lookup 24 LIST-LEDGER)
              ACCOUNT3)
(check-expect (list-lookup 32 LIST-LEDGER)
              ACCOUNT4)
(check-expect (list-lookup 40 LIST-LEDGER)
              ACCOUNT5)
(check-expect (list-lookup 48 LIST-LEDGER)
              #false)

(define-struct leaf ())
(define-struct branch (left element right))
(define BST-LEDGER
  (branch (branch (leaf)
                  ACCOUNT0
                  (branch (leaf) ACCOUNT1 (leaf)))
          ACCOUNT2
          (branch (branch (leaf) ACCOUNT3 (leaf))
                  ACCOUNT4
                  (branch (leaf) ACCOUNT5 (leaf)))))

(define (bst-lookup id ledger)
  (cond
    [(leaf? ledger) #false]
    [(< id (account-id (branch-element ledger))) (bst-lookup id (branch-left ledger))]
    [(> id (account-id (branch-element ledger))) (bst-lookup id (branch-right ledger))]
    [else (branch-element ledger)]))

(check-expect (bst-lookup 0 BST-LEDGER)
              ACCOUNT0)
(check-expect (bst-lookup 8 BST-LEDGER)
              ACCOUNT1)
(check-expect (bst-lookup 16 BST-LEDGER)
              ACCOUNT2)
(check-expect (bst-lookup 20 BST-LEDGER)
              #false)
(check-expect (bst-lookup 24 BST-LEDGER)
              ACCOUNT3)
(check-expect (bst-lookup 32 BST-LEDGER)
              ACCOUNT4)
(check-expect (bst-lookup 40 BST-LEDGER)
              ACCOUNT5)
(check-expect (bst-lookup 48 BST-LEDGER)
              #false)

(define VEC-LEDGER (vector ACCOUNT0 ACCOUNT1 ACCOUNT2 ACCOUNT3 ACCOUNT4 ACCOUNT5))

(define (vec-lookup id  ledger)
  (let loop [(i 0) (led ledger) (id1 id)]
    (cond
    [(>= i (vector-length led)) #false]
    [(= id1 (account-id (vector-ref led i))) (vector-ref led i)]
    [else (loop (+ 1 i) led id1)])))

(check-expect (vec-lookup 0 VEC-LEDGER)
              ACCOUNT0)
(check-expect (vec-lookup 8 VEC-LEDGER)
              ACCOUNT1)
(check-expect (vec-lookup 16 VEC-LEDGER)
              ACCOUNT2)
(check-expect (vec-lookup 20 VEC-LEDGER)
              #false)
(check-expect (vec-lookup 24 VEC-LEDGER)
              ACCOUNT3)
(check-expect (vec-lookup 32 VEC-LEDGER)
              ACCOUNT4)
(check-expect (vec-lookup 40 VEC-LEDGER)
              ACCOUNT5)
(check-expect (vec-lookup 48 VEC-LEDGER)
              #false)

(define (transfer! amount from-id to-id ledger)
  (cond
    [(false? (vec-lookup from-id ledger)) (error "Account not found")]
    [(false? (vec-lookup to-id ledger)) (error "Account not found")]
    [else (account-transfer! amount (vec-lookup from-id ledger) (vec-lookup to-id ledger))]))

(define (ledger-copy ledger)
  (build-vector (vector-length ledger)
                (lambda (i) (account-copy (vector-ref ledger i)))))

(check-expect
  (begin
    (define ledger (ledger-copy VEC-LEDGER))
    (transfer! 2000 32 24 ledger)
    ledger)
  (vector ACCOUNT0
          ACCOUNT1
          ACCOUNT2
          (account 24 "David Parnas" 4048)
          (account 32 "Barbara Liskov" 6192)
          ACCOUNT5))

(check-error
  (begin
    (define ledger (ledger-copy VEC-LEDGER))
    (transfer! 2000 31 24 ledger))
  "Account not found")