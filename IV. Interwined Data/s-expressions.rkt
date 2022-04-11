;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname s-expressions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hello) hello) 'hello) 2)
(define (count sexp sy)
  (local (
          ; S-expr -> Boolean
          (define (atom? sexp)
            (cond
              [(number? sexp) #t]
              [(string? sexp) #t]
              [(symbol? sexp) #t]
              [else #f]))

          ; SL -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl)))]))
 
          ; Atom -> N 
          ; counts all occurrences of sy in at 
          (define (count-atom at)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)]))
          )
    (cond
      [(atom? sexp) (count-atom sexp)]
      [else (count-sl sexp)]))
  )
  