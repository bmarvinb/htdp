;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname word-games) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings
(define AS-LIST (read-lines LOCATION))

; A 1String is a String of length 1

; A Word is one of:
; – '() or
; – (cons 1String Word)
; interpretation a Word is a list of 1Strings (letters)

; Exercise 209. The above leaves us with two additional wishes:
; a function that consumes a String
; and produces its corresponding Word, and a function for the opposite direction.


; String -> Word
; converts s to the chosen word representation
(check-expect (string->word "test")
              (list "t" "e" "s" "t"))
(check-expect (string->word "")
              '())
(define (string->word s)
  (explode s))
 
; Word -> String
; converts w to a string
(check-expect (word->string (list "t" "e" "s" "t"))
              "test")
(check-expect (word->string '())
              "")
(define (word->string w)
  (implode w))

; Exercise 210. Complete the design of the words->strings function
; List-of-words -> List-of-strings
; turns all Words in low into Strings
(check-expect (words->strings (list (list "a" "b" "c")
                                    (list "d" "e" "f")))
              (list "abc" "def"))
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low))
                (words->strings (rest low)))]))

; Exercise 211. Complete the design of in-dictionary
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(check-expect (in-dictionary (list "zombie" "zxc"))
              (list "zombie"))
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(member? (first los) AS-LIST) (cons (first los) (in-dictionary (rest los)))]
    [else (in-dictionary (rest los))]))




