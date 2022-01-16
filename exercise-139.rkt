;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-139) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-amounts is one of: 
; – '()
; – (cons PositiveNumber List-of-amounts)
(check-expect (pos? (cons 5 (cons 10 (cons 20 '())))) #true)
(check-expect (pos? (cons 0 '())) #true)
(check-expect (pos? (cons -5 (cons 10 '()))) #false)
(define (pos? l)
  (cond
    [(< (first l) 0) #false]
    [(empty? (rest l)) (>= (first l) 0)]
    [else (pos? (rest l))]))
