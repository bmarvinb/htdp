;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-140) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(define (all-true l)
  (cond
    [(not (first l)) #false]
    [(empty? (rest l)) (first l)]
    [else (all-true (rest l))]))

(check-expect (all-true (cons #true (cons #true '()))) #true)
(check-expect (all-true (cons #false '())) #false)
(check-expect (all-true (cons #true (cons #false '()))) #false)
(define (one-true l)
  (cond
    [(first l) #true]
    [(empty? (rest l)) (first l)]
    [else (one-true (rest l))]))