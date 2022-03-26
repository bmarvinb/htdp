;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-238) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nelon -> Number
; determines the smallest number on l
(define (inf l)
  (find < l))

; Nelon -> Number
; determines the largest number on l
(define (sup l)
    (find > l))

(define (find f l)
  (cond
    [(empty? (rest l)) (first l)]
    [else
     (if (f (first l) (find f (rest l)))
         (first l)
         (find f (rest l)))]))

(check-expect (inf-2 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1))
              1) 
(define (inf-2 l)
  (find-2 min l))

(define (sup-2 l)
  (find-2 max l))

(define (find-2 f l)
  (cond
    [(empty? (rest l)) (first l)]
    [else (f (first l) (find-2 f (rest l)))]))