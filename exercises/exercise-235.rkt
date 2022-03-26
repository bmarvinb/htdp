;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-235) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 235. Use the contains? function to define functions that search
; for "atom", "basic", and "zoo", respectively.

; String Los -> Boolean
; determines whether l contains the string s
(define (contains? s l)
  (cond
    [(empty? l) #false]
    [else (or (string=? (first l) s)
              (contains? s (rest l)))]))

; Los -> Boolean
; does l contain "atom"
(define (contains-atom? l)
  (contains? "atom" l))

; Los -> Boolean
; does l contain "basic"
(define (contains-atom? l)
  (contains? "basic" l))

; Los -> Boolean
; does l contain "zoo"
(define (contains-atom? l)
  (contains? "zoo" l))

