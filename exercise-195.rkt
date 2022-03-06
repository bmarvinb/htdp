;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname exercise-195) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a" ... "z"
; or, equivalently, a member? of this list: 
(define ALPHABET
  (explode "abcdefghijklmnopqrstuvwxyz"))

; String -> String
; return first character of the given string
(check-expect (string-first "Abc") "A")
(check-error (check-expect (string-first 1) "Not string"))
(define (string-first str)
   (if (string? str)
       (substring str 0 1)
       (error "Not string")))

(check-expect (start-with# "a" (list "a" "ab" "abc" "b" "c" "d")) 3)
(check-expect (start-with# "z" (list "a" "ab" "abc" "b" "c" "d")) 0)
(define (start-with# letter dic)
  (cond
    [(empty? dic) 0]
    [else (+ (if (string=? letter (string-first (first dic))) 1 0)
             (start-with# letter (rest dic)))]))

(start-with# "t" AS-LIST)