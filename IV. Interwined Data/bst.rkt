;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-info [])
(define NONE (make-no-info))
 
(define-struct node [ssn name left right])
; A BT (short for BinaryTree) is one of:
; – NONE
; – (make-node Number Symbol BT BT)

(define bt1 (make-node
             15
             'd
             NONE
             (make-node
              24 'i NONE NONE)))

; Exercise 322
; BT Number -> Boolean
(check-expect (contains-bt? bt1 24) #t)

(define (contains-bt? bt n)
  (cond
    [(no-info? bt) #f]
    [else (or (= (node-ssn bt) n)
              (contains-bt? (node-left bt) n)
              (contains-bt? (node-right bt) n))]))


; Exercise 323
; BT Number -> String | Boolean
(check-expect (search-bt bt1 24)
              'i)
(define (search-bt bt n)
  (if (contains-bt? bt n)
      (cond
        [(= (node-ssn bt) n) (node-name bt)]
        [(contains-bt? (node-right bt) n) (search-bt (node-right bt) n)]
        [(contains-bt? (node-left bt) n) (search-bt (node-left bt) n)])
      #false))

; Exercise 324
; BT -> List-of-numbers
(check-expect (inorder bt1) '(15 24))
(define (inorder bt)
  (cond
    [(no-info? bt) '()]
    [else (cons (node-ssn bt)
                (append (inorder (node-left bt))
                        (inorder (node-right bt))))]))

(define bst-1 (make-node
               63
               'c
               (make-node
                29
                'x
                (make-node
                 15
                 'z
                 (make-node
                  10 'i NONE NONE)
                 (make-node
                  24 'i NONE NONE))
                NONE)
               (make-node
                89
                'v
                (make-node
                 77
                 'b
                 NONE
                 NONE)
                (make-node
                 95
                 'n
                 NONE
                 (make-node
                  99
                  'n
                  NONE
                  NONE)))))

; Exercise 325
; Number BST -> String | NONE
(check-expect (search-bst 63 bst-1)
              'c)
(check-expect (search-bst 24 bst-1)
              'i)
(check-expect (search-bst 95 bst-1)
              'n)
(define (search-bst n bst)
  (cond
    [(no-info? bst) NONE]
    [(= n (node-ssn bst)) (node-name bst)]
    [else (if (> n (node-ssn bst))
              (search-bst n (node-right bst))
              (search-bst n (node-left bst))
              )]))

; Exercise 326
; !!!

; Exercise 327
; !!!