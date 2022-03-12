;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-37) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> String
; return a given string without first character
; given:
;  "Hello"
; expected:
;  "ello"
(define (string-rest str)
    (if (string? str)
      (substring str 1)
      (error "Not a string")))
