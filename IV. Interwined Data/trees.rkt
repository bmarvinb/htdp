;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct no-parent [])
(define-struct child [father mother name date eyes])
; An FT (short for family tree) is one of: 
; – (make-no-parent)
; – (make-child FT FT String N String)

(define NP (make-no-parent))
; An FT is one of: 
; – NP
; – (make-child FT FT String N String)

; Oldest Generation:
(define Carl (make-child NP NP "Carl" 1926 "green"))
(define Bettina (make-child NP NP "Bettina" 1926 "green"))
 
; Middle Generation:
(define Adam (make-child Carl Bettina "Adam" 1950 "hazel"))
(define Dave (make-child Carl Bettina "Dave" 1955 "black"))
(define Eva (make-child Carl Bettina "Eva" 1965 "blue"))
(define Fred (make-child NP NP "Fred" 1966 "pink"))
 
; Youngest Generation: 
(define Gustav (make-child Fred Eva "Gustav" 1988 "brown"))

; Exercise 310. Develop count-persons. The function consumes a family tree and counts the child structures in the tree. 
; FT -> Number
; counts children

(check-expect (count-persons Gustav)
              5)

(define (count-persons ft)
  (cond
    [(no-parent? ft) 0]
    [(child? ft) (+ 1
                    (count-persons (child-father ft))
                    (count-persons (child-mother ft)))]))

; Exercise 311. Develop the function average-age.
; It consumes a family tree and the current year.
; It produces the average age of all child structures in the family tree.
; FT Date -> Number
; count an average age

(check-expect (average-age Gustav 2022)
              68)

(define (average-age ft year)
  (local (
          (define (years ft year)
            (cond
              [(no-parent? ft) '()]
              [else (cons (- year (child-date ft))
                          (append (years (child-mother ft) year)
                                  (years (child-father ft) year)))]))

          (define YEARS (years ft year))
          (define TOTAL (foldl (lambda (acc curr) (+ acc curr)) 0 YEARS)))
    (round (/ TOTAL
              (length YEARS)))))

; Exercise 312. Develop the function eye-colors,
; which consumes a family tree and produces a list of all eye colors in the tree.
; FT -> List-of-strings

(check-expect (eye-colors Gustav)
              (list "brown" "blue" "green" "green" "pink"))

(define (eye-colors ft)
  (cond
    [(no-parent? ft) '()]
    [else (cons (child-eyes ft)
                (append (eye-colors (child-mother ft))
                        (eye-colors (child-father ft))))]))


; Exercise 313. Suppose we need the function blue-eyed-ancestor?,
; which is like blue-eyed-child? but responds with #true only when a proper ancestor, not the given child itself, has blue eyes.


; FT -> Boolean
(check-expect (blue-eyed-ancestor? Gustav)
              #t)

(check-expect (blue-eyed-ancestor? Eva)
              #f)

(define (blue-eyed-ancestor? ft)
  (local (
          (define (blue-eyes? ft)
            (cond
              [(no-parent? ft) #f]
              [else (string=? (child-eyes ft) "blue")]))
          )

    (cond
      [(no-parent? ft) #false]
      [(blue-eyes? (child-father ft)) #t]
      [(blue-eyes? (child-mother ft)) #t]
      [else
       (or
        (blue-eyed-ancestor? (child-father ft))
        (blue-eyed-ancestor? (child-mother ft)))])
    ))



