;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-134) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String List-of-names -> Boolean
(check-expect (contains? "Flatt" (cons "a" (cons "Flatt" '()))) #true)
(check-expect (contains? "Flatt" (cons "a" (cons "b" '()))) #false)
(define (contains? name alon)
  (cond
    [(empty? alon) #false]
    [(cons? alon)
     (or (string=? (first alon) name)
         (contains? name (rest alon)))]))