;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpreting-variables) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==================== Interpreting Variables ====================

(define-struct add [left right])
(define-struct mul [left right])
;; BSL-var-expr is one of:
;; - Number
;; - Symbol
;; - (make-add Number BSL-var-expr)
;; - (make-mul Number BSL-var-expr)

;; BSL-var-expr -> Number
;; implemented in exercise 347
(define (eval-expression exp)
  (cond
    [(number? exp) exp]
    [(mul? exp) (* (eval-expression (mul-left exp))
                   (eval-expression (mul-right exp)))]
    [(add? exp) (+ (eval-expression (add-left exp))
                   (eval-expression (add-right exp)))]))

;; Exercise 352. Design subst.
;; The function consumes a BSL-var-expr ex, a Symbol x, and a Number v.
;; It produces a BSL-var-expr like ex with all occurrences of x replaced by v.

;; BSL-var-expr Symbol Number -> BSL-var-expr
(check-expect (subst 1 'x 2) 1)
(check-expect (subst 'y 'x 1) 'y)
(check-expect (subst 'x 'x 1) 1)
(check-expect (subst (make-add 'x 2) 'x 1)
              (make-add 1 2))
(check-expect (subst (make-add (make-add 'x 2) 2) 'x 1)
              (make-add (make-add 1 2) 2))
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (eq? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]
    [else ex]))

;; Exercise 353. Design the numeric? function. It determines whether a BSL-var-expr is also a BSL-expr. 
(check-expect (numeric? 1) #true)
(check-expect (numeric? 'x) #false)
(check-expect (numeric? (make-add 'x 1)) #false) 
(check-expect (numeric? (make-add (make-add 1 2) 3)) #true)

;; Expression -> Boolean
(define (numeric? ex)
  (cond
    [(symbol? ex) #false]
    [(number? ex) #true]
    [(add? ex) (and (numeric? (add-left ex))
                    (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex))
                    (numeric? (mul-right ex)))]
    [else #false]))

;; Exercise 354. Design eval-variable.
(check-expect (eval-variable 1) 1)
(check-error (eval-variable 'x) "Not a numeric value")
(check-error (eval-variable (make-add 'x 1)) "Not a numeric value")
(check-expect (eval-variable (make-add (make-add 1 1) 2)) 4)

;; Expression
(define (eval-variable ex)
  (cond
    [(numeric? ex) (eval-expression ex)]
    [else (error "Not a numeric value")]))

;; An AL (short for association list) is [List-of Association].
;; An Association is a list of two items:
;;   (cons Symbol (cons Number '())).

(define AL '((x 1) (y 2) (z 3)))

;; BSL-var-expr AL -> Number
(check-expect (eval-variable* (make-add 'x 'y) AL) 3)
(check-expect (eval-variable* (make-add 'x 'y) AL) 3)
(check-expect (eval-variable* (make-add (make-add 'x 'x) 'y) AL) 4)
(check-expect (eval-variable* (make-mul (make-add 'x 'x) 'y) AL) 4)
(check-error (eval-variable* (make-mul (make-add 'a 'x) 'y) AL) "Not a numeric value")
(define (eval-variable* ex da)
  (local ((define SUBSTITUTED-EXPRESSION (subst-all ex da)))
    (cond
      [(numeric? SUBSTITUTED-EXPRESSION) (eval-expression SUBSTITUTED-EXPRESSION)]
      [else (error "Not a numeric value")])))

(check-expect (subst-all (make-add 'x 'y) AL) (make-add 1 2))
(check-expect (subst-all (make-add (make-add 'x 'x) 'y) AL) (make-add (make-add 1 1) 2))
(check-expect (subst-all (make-mul (make-add 'x 'x) 'y) AL) (make-mul (make-add 1 1) 2))
(define (subst-all ex da)
  (cond
    [(empty? da) ex]
    [else (subst-all (subst ex (first (first da)) (second (first da)))
                     (rest da))]))

;; Exercise 355. Design eval-var-lookup.
;; BSL-var-expr AL -> Number
(check-expect (eval-var-lookup (make-add 'x 'y) AL) 3)
(check-expect (eval-var-lookup (make-add (make-add 'x 'x) 'y) AL) 4)
(check-expect (eval-var-lookup (make-mul (make-add 'x 'x) 'y) AL) 4)
(check-error (eval-var-lookup (make-mul (make-add 'a 'x) 'y) AL) "Undefined variable")
(define (eval-var-lookup ex da)
  (local ((define SUBSTITUTED-EXPRESSION (replace-variables ex da)))
    (cond
      [(numeric? SUBSTITUTED-EXPRESSION) (eval-expression SUBSTITUTED-EXPRESSION)]
      [else (error SUBSTITUTED-EXPRESSION)])))

;; BSL-var-expr AL -> BSL-var-expr
;; replace all variables in a BSL-var-expr
(check-expect (replace-variables (make-add 'x 'y) AL) (make-add 1 2))
(check-expect (replace-variables (make-add (make-add 'x 'x) 'y) AL) (make-add (make-add 1 1) 2))
(define (replace-variables ex da)
  (cond
    [(empty? ex) '()]
    [(symbol? ex) (replace-variable ex)]
    [(add? ex) (make-add (replace-variable (add-left ex) da)
                         (replace-variable (add-right ex) da))]
    [(mul? ex) (make-mul (replace-variable (mul-left ex) da)
                         (replace-variable (mul-right ex) da))]
    [else ex]))

; BSL-var-expr AL -> BSL-var-expr
(define (replace-variable ex da)
  (cond
    [(symbol? ex) (if (boolean? (assq ex da))
                      (error "Undefined variable")
                      (second (assq ex da)))]
    [else (replace-variables ex da)]))