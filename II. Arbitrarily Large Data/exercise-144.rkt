;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-144) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-temperatures -> Boolean
; Returns #true if the temperatures are sorted in descending order
(check-expect (sorted>? (cons 3 (cons 2 (cons 1 '())))) #true)
(check-expect (sorted>? (cons 3 (cons 4 (cons 1 '())))) #false)
(check-expect (sorted>? (cons 1 (cons 2 '()))) #false)
(define (sorted>? alot)
  (cond
    [(empty? (rest alot)) #true]
    [(cons? alot)
     (if (>= (first (rest alot)) (first alot))
         #false
         (sorted>? (rest alot)))]))