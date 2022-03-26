;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-251) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (fold1 + l))


; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (fold1 * l))
   
; [Number -> Number] [List-of Number] -> Number
(define (fold1 f l)
  (cond
    [(empty? l) 0]
    [else
     (f (first l)
        (fold1 f (rest l)))]))