;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname exercise-198) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 198. Design words-by-first-letter.
; The function consumes a Dictionary and produces a list of Dictionarys, one per Letter.
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings
(define AS-LIST (read-lines LOCATION))
(define ALPHABET
  (explode "abcdefghijklmnopqrstuvwxyz"))

; Letter-count is a piece of data that combines letters and counts
(define-struct letter-count [letter count])

; Dict-of-words presents a map, where key is a letter, value is a dictionary
(define-struct dict-of-words [letter dict])

; String -> String
; return first character of the given string
(check-expect (string-first "Abc") "A")
(check-error (check-expect (string-first 1) "Not string"))
(define (string-first str)
   (if (string? str)
       (substring str 0 1)
       (error "Not string")))

; Dictionary String -> Dictionary
(check-expect (dictionary-by-letter (list "a" "ab" "abc" "b" "bc" "c")
                                    "a")
              (list "a" "ab" "abc"))
(check-expect (dictionary-by-letter (list "a" "ab" "abc" "b" "bc" "c")
                                    "b")
              (list "b" "bc"))
(define (dictionary-by-letter dict letter)
  (cond
    [(empty? dict) '()]
    [(string=? (string-first (first dict)) letter) (cons (first dict) (dictionary-by-letter (rest dict) letter))]
    [else (dictionary-by-letter (rest dict) letter)]))

; The function consumes a Dictionary and produces a list of Dictionarys, one per Letter.
; Dictionary -> List-of-Dict-of-words
(check-expect (words-by-first-letter (list "a" "ab" "abc" "b" "bc" "c"))
              (list (make-dict-of-words "a" (list "a" "ab" "abc"))
                    (make-dict-of-words "b" (list "b" "bc"))
                    (make-dict-of-words "c" (list "c"))))
(define (words-by-first-letter dict)
  (cond
    [(empty? dict) '()]
    [else (remove-empty-lists (words-by-first-letter-helper dict ALPHABET))]))

; List-of-Dict-of-words -> List-of-Dict-of-words
(check-expect (remove-empty-lists (list (make-dict-of-words "a" (list "a" "ab" "abc"))
                                        (make-dict-of-words "b" '())
                                        (make-dict-of-words "c" '())))
              (list (make-dict-of-words "a" (list "a" "ab" "abc"))))
(define (remove-empty-lists list)
  (cond
    [(empty? list) '()]
    [(cons? (dict-of-words-dict (first list))) (cons (first list) (remove-empty-lists (rest list)))]
    [else (remove-empty-lists (rest list))]))

; Dictionary Dictionary -> List-of-Dict-of-words
(check-expect (words-by-first-letter-helper (list "a" "ab" "abc" "b" "bc" "c")
                                            (list "a" "b" "c"))
              (list (make-dict-of-words "a" (list "a" "ab" "abc"))
                    (make-dict-of-words "b" (list "b" "bc"))
                    (make-dict-of-words "c" (list "c"))))
(define (words-by-first-letter-helper dict alphabet)
  (cond
    [(empty? alphabet) '()]
    [else (cons (make-dict-of-words (first alphabet)
                                    (dictionary-by-letter dict (first alphabet)))
                (words-by-first-letter-helper dict (rest alphabet)))]))

