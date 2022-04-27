;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s-expressions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; An S-expr is one of: 
; – Atom
; – SL
 
; An SL is one of: 
; – '()
; – (cons S-expr SL)
          

; An Atom is one of: 
; – Number
; – String
; – Symbol 

; S-expr -> Boolean
(define (atom? s)
  (or (number? s)
      (string? s)
      (symbol? s)))

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(define (count sexp sy)
  (local (
          ; SL Symbol -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl)))]))
 
          ; Atom Symbol -> N
          ; counts all occurrences of sy in at 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)])))
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)])))

; Exercise 318. Design depth.
; The function consumes an S-expression and determines its depth.
; An Atom has a depth of 1. The depth of a list of S-expressions is the maximum depth of its items plus 1.
; S-expr -> Number
(check-expect (depth 10) 1)
(check-expect (depth '( "a" 1)) 2)
(check-expect (depth '((1 2))) 3)
(define (depth sexp)
  (local (
          ; SL Symbol -> Number
          (define (sl-depth sl)
            (cond
              [(empty? sl) 1]
              [else (max (depth (first sl))
                         (sl-depth (rest sl)))])))
    (cond
      [(atom? sexp) 1]
      [else (add1 (sl-depth sexp))])))

; Exercise 319. Design substitute.
; It consumes an S-expression s and two symbols, old and new.
; The result is like s with all occurrences of old replaced by new.
; S-expr Symbol Symbol -> S-expr
(check-expect (substitute 'hello 'hello 'goodbye) 'goodbye)
(check-expect (substitute 'hello 'world 'goodbye) 'hello)
(check-expect (substitute '(hello world) 'hello 'goodbye) '(goodbye world))
(check-expect (substitute '(hello (awesome world)) 'awesome 'awful) '(hello (awful world)))
(define (substitute sexp old new)
  (local (
          ; SL -> SL
          (define (map-sl sl)
            (cond
              [(empty? sl) '()]
              [else (cons (substitute (first sl) old new)
                          (map-sl (rest sl)))])))
    (cond
      [(atom? sexp) (if (eq? sexp old) new sexp)]
      [else (map-sl sexp)])))