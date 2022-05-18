;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-151) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; N Number -> Number
(check-expect (multiply 2 4) 8)
(check-expect (multiply 2 0) 0)
(define (multiply n x)
  (cond
    [(zero? x) 0]
    [else (+ n (multiply n (sub1 x)))]))