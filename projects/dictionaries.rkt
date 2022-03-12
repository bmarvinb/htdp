;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname dictionaries) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings
(define AS-LIST (read-lines LOCATION))

; A Letter is one of the following 1Strings: 
; – "a" ... "z"
; or, equivalently, a member? of this list: 
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

; Exercise 195. Design the function starts-with#,
; which consumes a Letter and Dictionary and then counts how many words in the given Dictionary start with the given Letter.
(check-expect (start-with# "a" (list "a" "ab" "abc" "b" "c" "d")) 3)
(check-expect (start-with# "z" (list "a" "ab" "abc" "b" "c" "d")) 0)
(define (start-with# letter dic)
  (cond
    [(empty? dic) 0]
    [else (+ (if (string=? letter (string-first (first dic))) 1 0)
             (start-with# letter (rest dic)))]))

(start-with# "t" AS-LIST)

; Exercise 196. Design count-by-letter.
; The function consumes a Dictionary and counts how often each letter is used as the first one of a word in the given dictionary.
; Its result is a list of Letter-Counts, a piece of data that combines letters and counts.
(check-expect (count-by-letter-v2 (list "a" "b" "c") (list "a" "ab" "abc" "b" "bc" "c"))
              (list (make-letter-count "a" 3)
                    (make-letter-count "b" 2)
                    (make-letter-count "c" 1)))
(define (count-by-letter-v2 lol dict)
  (cond
    [(empty? lol) '()]
    [else (cons (make-letter-count (first lol) (start-with# (first lol) dict))
                (count-by-letter-v2 (rest lol) dict))]))

; Exercise 197. Design most-frequent. The function consumes a Dictionary.
; It produces the Letter-Count for the letter that occurs most often as the first one in the given Dictionary.
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