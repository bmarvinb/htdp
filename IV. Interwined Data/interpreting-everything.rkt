;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpreting-everything) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==================== Interpreting Everything ====================

(define-struct add [left right])
(define-struct mul [left right])
(define-struct fun [name arg])
;; BSL-fun-expr is one of:
;; - Number
;; - Symbol
;; - (make-add Number BSL-fun-expr)
;; - (make-mul Number BSL-fun-expr)
;; - (make-fun Symbol BSL-fun-expr)

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

;; Exercise 360. Formulate a data definition for the representation of DrRacket’s definitions area

(define-struct con [name value])
;; BSL-con is:
;; (make-con Symbol Number)

(define-struct fun-def [name param body])
;; BSL-fun-def is:
;; (make-fun-def Symbol Symbol BSL-fun-expr)

;; BSL-da is:
;; (make-con Symbol Number)
;; (make-fun-def Symbol Symbol BSL-fun-expr)

(define x (make-con 'x 1))
(define y (make-con 'y 2))

(define f (make-fun-def 'f 'x (make-add 1 'x))) ; 1 -> 2
(define g (make-fun-def 'g 'x (make-mul 2 (make-add 2 'x)))) ; 1 -> 6
(define h (make-fun-def 'h 'x (make-add (make-fun 'f 'x) (make-fun 'g 'x)))) ; 1 -> 8

(define UNDEFINED-CON "Constant definition not found")
(define UNDEFINED-FUN "Function definition not found")

;; BSL-da-all is [List-of BSL-da]
(define da-all (list x y f g h))
  
;; BSL-da-all Symbol -> BSL-con
(check-expect (lookup-con-def da-all 'x) x)
(check-expect (lookup-con-def da-all 'y) y)
(check-error (lookup-con-def da-all 'z) UNDEFINED-CON)
(define (lookup-con-def da x)
  (cond
    [(empty? da) (error UNDEFINED-CON)]
    [(and (con? (first da))
          (eq? (con-name (first da)) x)) (first da)]
    [else (lookup-con-def (rest da) x)]))

;; BSL-da-all Symbol -> BSL-fun-def
(check-expect (lookup-fun-def da-all 'f) f)
(check-expect (lookup-fun-def da-all 'g) g)
(check-error (lookup-fun-def da-all 'z) UNDEFINED-FUN)
(define (lookup-fun-def da f)
  (cond
    [(empty? da) (error UNDEFINED-FUN)]
    [(and (fun-def? (first da))
          (eq? (fun-def-name (first da)) f)) (first da)]
    [else (lookup-fun-def (rest da) f)]))

;; Exercise 361. Design eval-all
(check-expect (eval-all 'x da-all) 1)
(check-expect (eval-all 'y da-all) 2)
(check-error (eval-all 'z da-all) UNDEFINED-CON)
(check-expect (eval-all (make-fun 'f 1) da-all) 2)
(check-expect (eval-all (make-fun 'h 'x) da-all) 8)
(check-error (eval-all (make-fun 'z 'x) da-all) UNDEFINED-FUN)
(define (eval-all ex da)
  (cond
    [(number? ex) ex]
    [(symbol? ex) (con-value (lookup-con-def da ex))]
    [(add? ex) (+ (eval-all (add-left ex) da)
                  (eval-all (add-right ex) da))]
    [(mul? ex) (* (eval-all (mul-left ex) da)
                  (eval-all (mul-right ex) da))]
    [(fun? ex)
     (local ((define function (lookup-fun-def da (fun-name ex))))
       (if (empty? function)
           (error UNDEFINED-FUN)
           (local ((define value (eval-all (fun-arg ex) da))
                   (define plugd (subst (fun-def-body function)
                                        (fun-def-param function)
                                        (eval-all (fun-arg ex) da))))
             (eval-all plugd da))))]))

;; Exercise 362. Design a function interpreter

;; S-expr Sl -> Number
;(define (interpreter s-expr sl)
;  ())