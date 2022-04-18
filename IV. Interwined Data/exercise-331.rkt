;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-331) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Dir.v1 (short for directory) is one of:
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)

; A File.v1 is a String.

(define text (list "part 1" "part 2" "part 3"))
(define code (list "hang" "draw"))
(define docs (list "read!"))
(define libs (list code docs))
(define ts (list text "read!" libs))
(define ts.v2 (list text "read!"))

; Exercise 331. Design the function how-many, which determines how many files a given Dir.v1 contains.
; Dir.v1 -> Number
(check-expect (how-many ts) 7)
(check-expect (how-many ts.v2) 4)
(define (how-many dir)
  (cond
    [(empty? dir) 0]
    [(string? dir) 1]
    [else (+ (how-many (first dir))
             (how-many (rest dir)))]))


