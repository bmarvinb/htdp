;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-138) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)
(check-expect (sum (cons 5 (cons 10 (cons 20 '())))) 35)
(check-expect (sum (cons 0 '())) 0)
(check-expect (sum (cons -5 (cons 10 '()))) 5)
(define (sum l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (+ (first l) (sum (rest l)))]))
