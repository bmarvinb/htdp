;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname exercise-189) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Number List-of-numbers -> Boolean
(check-expect (search 1 (list 2 4 3 1)) #true)
(check-expect (search 5 (list 2 4 3 1)) #false)
(check-expect (search-sorted 5 '()) #false)
(define (search n alon)
  (cond
    [(empty? alon) #false]
    [else (or (= (first alon) n)
              (search n (rest alon)))]))

; Number List-of-numbers -> Boolean
(check-expect (search-sorted 3 (list 1 2 3 4)) #true)
(check-expect (search-sorted 5 (list 1 2 3 4)) #false)
(check-expect (search-sorted 5 '()) #false)
(define (search-sorted n alon)
 (cond
    [(or (empty? alon) (> (first alon) n)) #f]
    [(= n (first alon)) #t]
    [else (search-sorted n (rest alon))]))