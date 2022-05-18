;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-35) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> String
; return last character from given string
; given:
;  "Hello" for str
; expected:
;  "o"
(define (string-last str)
  (if (string? str)
      (substring str (- (string-length str) 1) (string-length str))
      (error "Not string")))
