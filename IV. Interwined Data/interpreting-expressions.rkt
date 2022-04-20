;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpreting-expressions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct add [left right])
(define-struct mul [left right])
;; Expression is one of:
;; - Number
;; - (Number Expression)

;; Exercise 347. Design eval-expression.
;; The function consumes a representation of a BSL expression and computes its value.
;; Expression -> Number
(check-expect (eval-expression (make-add 10 -10)) 0)
(check-expect (eval-expression (make-add (make-mul 3 3)
                                         (make-mul 4 4))) 25)
(check-expect (eval-expression (make-add (make-add 5 20)
                                         (make-mul (make-add 5 20) (make-add 2 2)))) 125)
(define (eval-expression exp)
  (cond
    [(number? exp) exp]
    [(mul? exp) (* (eval-expression (mul-left exp))
                   (eval-expression (mul-right exp)))]
    [(add? exp) (+ (eval-expression (add-left exp))
                   (eval-expression (add-right exp)))]))

(define-struct bool-and [left right])
(define-struct bool-or [left right])
(define-struct bool-not [left])
;; Boolean expression is one of:
;; - Boolean
;; - (Boolean BooleanExpression)

;; Exercise 348. Design eval-bool-expression
(check-expect (eval-bool-expression (make-bool-and #t #t)) #t)
(check-expect (eval-bool-expression (make-bool-and #t #f)) #f)
(check-expect (eval-bool-expression (make-bool-and #f #f)) #f)
(check-expect (eval-bool-expression (make-bool-or #t #t)) #t)
(check-expect (eval-bool-expression (make-bool-or #f #f)) #f)
(check-expect (eval-bool-expression (make-bool-or #f #t)) #t)
(check-expect (eval-bool-expression (make-bool-not #t)) #f)
(check-expect (eval-bool-expression (make-bool-not #f)) #t)
(check-expect (eval-bool-expression (make-bool-and #t (make-bool-not #f))) #t)

(define (eval-bool-expression exp)
  (cond
    [(boolean? exp) exp]
    [(bool-not? exp) (not (bool-not-left exp))]
    [(bool-and? exp) (and (eval-bool-expression (bool-and-left exp))
                          (eval-bool-expression (bool-and-right exp)))]
    [(bool-or? exp) (or (eval-bool-expression (bool-or-left exp))
                        (eval-bool-expression (bool-or-right exp)))]))

 
