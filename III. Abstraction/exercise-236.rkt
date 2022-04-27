;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-236) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Lon -> Lon
; adds 1 to each item on l
(check-expect (add1* (list 1 2 3))
              (list 2 3 4))
(define (add1* l)
  (increment-lon l 1))


; Lon -> Lon
; adds 5 to each item on l
(check-expect (plus5 (list 1 2 3))
              (list 6 7 8))
(define (plus5 l)
  (increment-lon l 5))


; Number List-of-number
(define (increment-lon l n)
  (cond
    [(empty? l) '()]
    [else
     (cons
      (+ (first l) n)
      (increment-lon (rest l) n))]))
