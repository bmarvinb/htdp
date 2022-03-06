;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname exercise-197) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 197. Design most-frequent. The function consumes a Dictionary.
; It produces the Letter-Count for the letter that occurs most often as the first one in the given Dictionary.
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings
(define AS-LIST (read-lines LOCATION))
(define ALPHABET
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter-Counts is a piece of data that combines letters and counts
(define-struct letter-count [letter count])

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

; Alternative implementation
; List-of-letters Dictionary -> Dictionary
(check-expect (count-by-letter-v2 (list "a" "b" "c") (list "a" "ab" "abc" "b" "bc" "c"))
              (list (make-letter-count "a" 3)
                    (make-letter-count "b" 2)
                    (make-letter-count "c" 1)))
(define (count-by-letter-v2 lol dict)
  (cond
    [(empty? lol) '()]
    [else (cons (make-letter-count (first lol) (start-with# (first lol) dict))
                (count-by-letter-v2 (rest lol) dict))]))

; Dictionary -> Dictionary
(check-expect (most-frequent-v1
               (list (make-letter-count "a" 3)
                     (make-letter-count "b" 2)
                     (make-letter-count "c" 1)))
              (make-letter-count "a" 3))
(define (most-frequent-v1 dict)
  (most-frequent dict (first dict)))

; Dictionary Letter-Count -> Letter-Count
(check-expect (most-frequent
               (list (make-letter-count "a" 3)
                     (make-letter-count "b" 5)
                     (make-letter-count "c" 1))
               (first (list (make-letter-count "a" 3)
                            (make-letter-count "b" 5)
                            (make-letter-count "c" 1))))
              (make-letter-count "b" 5))
(define (most-frequent dict lc)
  (cond
    [(empty? dict) lc]
    [else (most-frequent (rest dict)
                         (if (>= (letter-count-count (first dict)) (letter-count-count lc))
                             (first dict) lc))]))