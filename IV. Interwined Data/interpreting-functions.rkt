;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpreting-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==================== Interpreting Functions ====================

(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name arg])
;; BSL-fun-expr is one of:
;; - Number
;; - Symbol
;; - (make-add Number BSL-fun-expr)
;; - (make-mul Number BSL-fun-expr)
;; - (make-fun Symbol BSL-fun-expr)

(define UNDEFINED-FUN "Function not defined")
(define UNDEFINED-SYMBOL "Symbol not found")

;; BSL-var-expr Symbol Number -> BSL-fun-expr
(check-expect (subst (make-add 'x 2) 'x 2)
              (make-add 2 2))
(check-expect (subst (make-fun 'f (make-mul 2 'x)) 'x 4)
              (make-fun 'f (make-mul 2 4)))
(define (subst ex x v)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (if (eq? ex x) v ex)]
    [(add? ex) (make-add (subst (add-left ex) x v)
                         (subst (add-right ex) x v))]
    [(mul? ex) (make-mul (subst (mul-left ex) x v)
                         (subst (mul-right ex) x v))]
    [(fun? ex) (make-fun (fun-name ex) (subst (fun-arg ex) x v))]
    [else ex]))

;; Exercise 357. Design eval-definition1
;; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
;; interpret. f represents a function name
;;            x represents a function parameter
;;            b represents a function body
(check-expect (eval-definition1 (make-fun 'f 1) 'f 'x (make-add 1 'x)) 2)
(define (eval-definition1 ex f x b)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error UNDEFINED-SYMBOL)]
    [(add? ex) (+ (eval-definition1 (add-left ex) f x b)
                  (eval-definition1 (add-right ex) f x b))]
    [(mul? ex) (* (eval-definition1 (mul-left ex) f x b)
                  (eval-definition1 (mul-right ex) f x b))]
    [(fun? ex)
     (if (symbol=? f (fun-name ex))
         (local ((define value (eval-definition1 (fun-arg ex) f x b))
                 (define plugd (subst b x value)))
           (eval-definition1 plugd f x b))
         (error UNDEFINED-FUN))]))

;; Exercise 358. Provide a structure type and a data definition for function definitions

(define-struct fun-def [name param body])
;; BSL-fun-def is:
;; (make-fun-def Symbol Symbol BSL-fun-expr)

(define f (make-fun-def 'f 'x (make-add 3 'x)))
(define g (make-fun-def 'g 'y (make-fun 'f (make-mul 2 'y))))
(define h (make-fun-def 'h 'v (make-add (make-fun 'f 'v) (make-fun 'g 'v))))

;; BSL-fun-def* is [List-of BSL-fun-def]
(define da-fgh (list f g h))

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error (lookup-def da-fgh 'x) UNDEFINED-FUN)
(define (lookup-def da f)
  (cond
    [(empty? da) (error UNDEFINED-FUN)]
    [else (if (eq? (fun-def-name (first da)) f)
              (first da)
              (lookup-def (rest da) f))]))

;; Exercise 359. Design eval-function*
;; BSL-fun-expr BSL-fun-def* -> Number
(check-expect (eval-function* (make-fun 'f 4) da-fgh) 7)
(check-expect (eval-function* (make-fun 'g 1) da-fgh) 5)
(check-expect (eval-function* (make-fun 'h 1) da-fgh) 9)
(define (eval-function* ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (error UNDEFINED-SYMBOL)]
    [(add? ex) (+ (eval-function* (add-left ex) da)
                  (eval-function* (add-right ex) da))]
    [(mul? ex) (* (eval-function* (mul-left ex) da)
                  (eval-function* (mul-right ex) da))]
    [(fun? ex)
      (local ((define function (lookup-def da (fun-name ex))))
       (if (not (empty? function))
           (local ((define value (eval-function* (fun-arg ex) da))
                   (define plugd (subst (fun-def-body function)
                                        (fun-def-param function)
                                        (eval-function* (fun-arg ex) da))))
             (eval-function* plugd da))
           (error UNDEFINED-FUN)))]))