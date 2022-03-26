;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-255) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; map-s, which consumes a list of strings and a function from strings to strings and produces a list of strings.

; [List-of T] [T -> R]
; -> [List-of R]
(define (map l f))