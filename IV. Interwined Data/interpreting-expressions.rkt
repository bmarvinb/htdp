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

; Exercise 349. Create tests for parse
; S-expr -> Boolean
(define (atom? s)
  (or (number? s)
      (string? s)
      (symbol? s)))

(define WRONG "Invalid BSL-expr")

; S-expr -> BSL-expr
(check-expect (parse '1) '1)
(check-expect (parse '(+ 10 -10)) (make-add 10 -10))
(check-expect (parse '(+ (* 5 5) 25)) (make-add (make-mul 5 5) 25))
(check-error (parse '(sqr 4)) WRONG)
(check-error (parse 'x) WRONG)
(check-error (parse "hello world") WRONG)
(check-error (parse '(1 2 3)) WRONG)
(check-error (parse '(string-append "hello" "world")) WRONG)
(define (parse s)
  (local (
          ; Atom -> BSL-expr 
          (define (parse-atom s)
            (cond
              [(number? s) s]
              [(string? s) (error WRONG)]
              [(symbol? s) (error WRONG)]))

          ; SL -> Boolean
          (define (consists-of-3 s)
            (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
                 (empty? (rest (rest (rest s))))))
          
          ; SL -> BSL-expr
          (define (parse-sl s)
            (cond
              [(and (consists-of-3 s) (symbol? (first s)))
               (cond
                 [(symbol=? (first s) '+)
                  (make-add (parse (second s)) (parse (third s)))]
                 [(symbol=? (first s) '*)
                  (make-mul (parse (second s)) (parse (third s)))]
                 [else (error WRONG)])]
              [else (error WRONG)])))
    (cond
      [(atom? s) (parse-atom s)]
      [else (parse-sl s)])))


; S-expr -> Number
(check-expect (interpreter-expr '(+ (* 5 5) 25)) 50)
(define (interpreter-expr s)
  (cond
    [(string? (parse s)) (parse s)]
    [else (eval-expression (parse s))]))

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; Exercise 352. Design subst.
; The function consumes a BSL-var-expr ex, a Symbol x, and a Number v.
; It produces a BSL-var-expr like ex with all occurrences of x replaced by v.

; BSL-var-expr Symbol Number -> BSL-var-expr
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

; Exercise 353. Design the numeric? function. It determines whether a BSL-var-expr is also a BSL-expr. 
(check-expect (numeric? 1) #true)
(check-expect (numeric? 'x) #false)
(check-expect (numeric? (make-add 'x 1)) #false) 
(check-expect (numeric? (make-add (make-add 1 2) 3)) #true)

(define (numeric? ex)
  (cond
    [(symbol? ex) #false]
    [(number? ex) #true]
    [(add? ex) (and (numeric? (add-left ex))
                    (numeric? (add-right ex)))]
    [(mul? ex) (and (numeric? (mul-left ex))
                    (numeric? (mul-right ex)))]
    [else #false]))

(define ERROR-MESSAGE "Not a numeric value")

; Exercise 354. Design eval-variable.
(check-expect (eval-variable 1) 1)
(check-error (eval-variable 'x) ERROR-MESSAGE)
(check-error (eval-variable (make-add 'x 1)) ERROR-MESSAGE)
(check-expect (eval-variable (make-add (make-add 1 1) 2)) 4)

(define (eval-variable ex)
  (cond
    [(numeric? ex) (eval-expression ex)]
    [else (error "Not a numeric value")]))


; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

(define AL '((x 1) (y 2) (z 3)))

(check-expect (eval-variable* (make-add 'x 'y) AL) 3)

; BSL-var-expr AL -> Number
(check-expect (eval-variable* (make-add 'x 'y) AL) 3)
(check-expect (eval-variable* (make-add (make-add 'x 'x) 'y) AL) 4)
(check-expect (eval-variable* (make-mul (make-add 'x 'x) 'y) AL) 4)
(check-error (eval-variable* (make-mul (make-add 'a 'x) 'y) AL) ERROR-MESSAGE)

(define (eval-variable* ex da)
  (local ((define SUBSTITUTED-EXPRESSION (subst-all ex da)))
    (cond
      [(numeric? SUBSTITUTED-EXPRESSION) (eval-expression SUBSTITUTED-EXPRESSION)]
      [else (error ERROR-MESSAGE)])))

(check-expect (subst-all (make-add 'x 'y) AL) (make-add 1 2))
(check-expect (subst-all (make-add (make-add 'x 'x) 'y) AL) (make-add (make-add 1 1) 2))
(check-expect (subst-all (make-mul (make-add 'x 'x) 'y) AL) (make-mul (make-add 1 1) 2))

(define (subst-all ex da)
  (cond
    [(empty? da) ex]
    [else (subst-all (subst ex (first (first da)) (second (first da)))
                     (rest da))]))

(check-expect (eval-var-lookup (make-add 'x 'y) AL) 3)
(check-expect (eval-var-lookup (make-add (make-add 'x 'x) 'y) AL) 4)
(check-expect (eval-var-lookup (make-mul (make-add 'x 'x) 'y) AL) 4)
(check-error (eval-var-lookup (make-mul (make-add 'a 'x) 'y) AL) UNDEFINED-ERROR)

(define (eval-var-lookup ex da)
  (local ((define SUBSTITUTED-EXPRESSION (replace-variables ex da)))
    (cond
      [(numeric? SUBSTITUTED-EXPRESSION) (eval-expression SUBSTITUTED-EXPRESSION)]
      [else (error SUBSTITUTED-EXPRESSION)])))

(define UNDEFINED-ERROR "Variable undefined")

; BSL-var-expr AL -> BSL-var-expr
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
  
(define (replace-variable ex da)
  (cond
    [(symbol? ex) (if (boolean? (assq ex da))
                      (error UNDEFINED-ERROR)
                      (second (assq ex da)))]
    [else (replace-variables ex da)]))



