;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname exercise-186) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; NEList-of-temperatures -> Boolean
(define (sorted>? l)
  (cond
    [(empty? (rest l)) #true]
    [(> (first (rest l)) (first l)) #false]
    [else (sorted>? (rest l))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
; (check-satisfied (sort> (list 1 2 3)) sorted>?)
(check-expect (sort> (list 1 2 3)) (list 3 2 1))
(define (sort> l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort> (rest l)))]))
 
; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l 
(define (insert n l)
  (cond
    [(empty? l) (cons n '())]
    [else (if (>= n (first l))
              (cons n l)
              (cons (first l) (insert n (rest l))))]))
