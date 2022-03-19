;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname word-games) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings
(define AS-LIST (read-lines LOCATION))

; List-of-strings -> List-of-strings
; creates a copy of the list without repetitions
(define (create-set los)
  (cond
    [(empty? los) '()]
    [(cons? los)
     (if (member? (first los) (create-set (rest los)))
         (create-set (rest los))
         (cons (first los) (create-set (rest los))))]))

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
(define (string->word s)
  (explode s))

(check-expect (string->word "test")
              (list "t" "e" "s" "t"))
(check-expect (string->word "")
              '())
 
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
(define (words->strings low)
  (cond
    [(empty? low) '()]
    [else (cons (word->string (first low))
                (words->strings (rest low)))]))

(check-expect (words->strings (list (list "a" "b" "c")
                                    (list "d" "e" "f")))
              (list "abc" "def"))

; Exercise 211. Complete the design of in-dictionary
; List-of-strings -> List-of-strings
; picks out all those Strings that occur in the dictionary
(define (in-dictionary los)
  (cond
    [(empty? los) '()]
    [(member? (first los) AS-LIST) (cons (first los) (in-dictionary (rest los)))]
    [else (in-dictionary (rest los))]))

(check-expect (in-dictionary (list "zombie" "zxc"))
              (list "zombie"))

; Exercise 212. Write down the data definition for List-of-words.
; Make up examples of Words and List-of-words.
; Finally, formulate the functional example from above with check-expect.
(define LIST-OF-WORDS-EXAMPLE (list (string->word "de") (string->word "ed")))

; Word -> List-of-words
; creates all rearrangements of the letters in w
(define (arrangements w)
  (cond
    [(empty? w) (list '())]
    [else (insert-everywhere/in-all-words (first w)
                                          (arrangements (rest w)))]))

; Exercise 213. Design insert-everywhere/in-all-words.
; 1String List-of-words -> List-of-words
(define (insert-everywhere/in-all-words str low)
  (cond
    [(empty? low) '()]
    [else (append (insert-everywhere str (first low))
                  (insert-everywhere/in-all-words str (rest low)))]))

(check-expect (insert-everywhere/in-all-words "x" (list (string->word "ab")))
              (list (list "x" "a" "b")
                    (list "a" "x" "b")
                    (list "a" "b" "x")))


; 1String Word -> List-of-words
; inserts the character x at every position between the characters of the word w
(define (insert-everywhere str w)
  (cond
    [(empty? w) (list (list str))]
    [else (cons (prepend str w)
                (prepend-to-each (first w)
                                 (insert-everywhere str (rest w))))]))

(check-expect (insert-everywhere "d" '())
              (list (list "d")))

(check-expect (insert-everywhere "d" (list "e"))
              (list (list "d" "e") (list "e" "d")))

(check-expect (insert-everywhere "d" (list "e" "r"))
              (list (list "d" "e" "r") (list "e" "d" "r") (list "e" "r" "d")))

; 1String List-of-words -> List-of-words
; inserts the character x at the beginning of each word in the given list
(define (prepend-to-each x low)
  (cond
    [(empty? low) '()]
    [else (cons (prepend x (first low))
                (prepend-to-each x (rest low)))]))

(check-expect (prepend-to-each "d" '()) '())
(check-expect (prepend-to-each "d" (list '())) (list (list "d")))
(check-expect (prepend-to-each "d" (list (list "e") (list "r")))
              (list (explode "de") (explode "dr")))
(check-expect (prepend-to-each "d" (list (explode "er") (explode "re")))
              (list (explode "der") (explode "dre")))

; 1String Word -> Word
; inserts the character x at the beginning of the given word
(define (prepend x w)
  (cons x w)) 

; Exercise 214. Integrate arrangements
; with the partial program from Word Games, Composition Illustrated

; String -> List-of-strings
; finds all words that the letters of some given word spell
(define (alternative-words s)
  (create-set (in-dictionary (words->strings (arrangements (string->word s))))))
