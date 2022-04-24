;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interpreters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==================== Interpreting Expressions ====================

(define-struct add [left right])
(define-struct mul [left right])
;; BSL-var-expr is one of:
;; - Number
;; - Symbol
;; - (make-add Number BSL-var-expr)
;; - (make-mul Number BSL-var-expr)


;; Exercise 347. Design eval-expression.
;; The function consumes a representation of a BSL expression and computes its value.
(check-expect (eval-expression (make-add 10 -10)) 0)
(check-expect (eval-expression (make-add (make-mul 3 3)
                                         (make-mul 4 4))) 25)
(check-expect (eval-expression (make-add (make-add 5 20)
                                         (make-mul (make-add 5 20) (make-add 2 2)))) 125)

;; Expression -> Number
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

;; Expression -> Boolean
(define (eval-bool-expression exp)
  (cond
    [(boolean? exp) exp]
    [(bool-not? exp) (not (bool-not-left exp))]
    [(bool-and? exp) (and (eval-bool-expression (bool-and-left exp))
                          (eval-bool-expression (bool-and-right exp)))]
    [(bool-or? exp) (or (eval-bool-expression (bool-or-left exp))
                        (eval-bool-expression (bool-or-right exp)))]))

;; Exercise 349. Create tests for parse
;; S-expr -> Boolean
(define (atom? s)
  (or (number? s)
      (string? s)
      (symbol? s)))

;; S-expr -> BSL-expr
(check-expect (parse '1) '1)
(check-expect (parse '(+ 10 -10)) (make-add 10 -10))
(check-expect (parse '(+ (* 5 5) 25)) (make-add (make-mul 5 5) 25))
(check-error (parse '(sqr 4)) "Invalid BSL-expr")
(check-error (parse 'x) "Invalid BSL-expr")
(check-error (parse "hello world") "Invalid BSL-expr")
(check-error (parse '(1 2 3)) "Invalid BSL-expr")
(check-error (parse '(string-append "hello" "world")) "Invalid BSL-expr")
(define (parse s)
  (local (
          ;; Atom -> BSL-expr 
          (define (parse-atom s)
            (cond
              [(number? s) s]
              [(string? s) (error "Invalid BSL-expr")]
              [(symbol? s) (error "Invalid BSL-expr")]))

          ;; SL -> Boolean
          (define (consists-of-3 s)
            (and (cons? s) (cons? (rest s)) (cons? (rest (rest s)))
                 (empty? (rest (rest (rest s))))))
          
          ;; SL -> BSL-expr
          (define (parse-sl s)
            (cond
              [(and (consists-of-3 s) (symbol? (first s)))
               (cond
                 [(symbol=? (first s) '+)
                  (make-add (parse (second s)) (parse (third s)))]
                 [(symbol=? (first s) '*)
                  (make-mul (parse (second s)) (parse (third s)))]
                 [else (error "Invalid BSL-expr")])]
              [else (error "Invalid BSL-expr")])))
    (cond
      [(atom? s) (parse-atom s)]
      [else (parse-sl s)])))


;; S-expr -> Number
(check-expect (interpreter-expr '(+ (* 5 5) 25)) 50)
(define (interpreter-expr s)
  (cond
    [(string? (parse s)) (parse s)]
    [else (eval-expression (parse s))]))

