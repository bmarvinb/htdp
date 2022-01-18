;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname exercise-145) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; NEList-of-temperatures -> Boolean
(check-expect (sorted>? (list 3 2 1)) #true)
(check-expect (sorted>? (list 1 2 3)) #false)
(check-expect (sorted>? (list 3 1 2)) #false)
(define (sorted>? l)
  (cond
    [(empty? (rest l)) #true]
    [(> (first (rest l)) (first l)) #false]
    [else (sorted>? (rest l))]))