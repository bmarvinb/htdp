;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname exercise-198) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 198. Design words-by-first-letter.
; The function consumes a Dictionary and produces a list of Dictionarys, one per Letter.

; Dictionary -> List-of-dictionarys 
(define (words-by-first-letter dict