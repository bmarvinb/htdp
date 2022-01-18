;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname exercise-187) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercuse 187. Design a program that sorts lists of game players by score

(define-struct gp [name score])
; A GamePlayer is a structure: 
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who 
; scored a maximum of s points

(check-expect (insert (make-gp "Player 1" 10)
                      (list (make-gp "Player 2" 20) (make-gp "Player 3" 30)))
              (list (make-gp "Player 2" 20) (make-gp "Player 3" 30) (make-gp "Player 1" 10)))

(define (insert gp l)
  (cond
    [(empty? l) (cons gp '())]
    [else (if (>= (gp-score gp) (gp-score (first l)))
              (cons gp l)
              (cons (first l) (insert gp (rest l))))]))

; GamePlayer -> GamePlayer
; return players sorted by score
(check-expect (sort (list (make-gp "Player 1" 10) (make-gp "Player 2" 20) (make-gp "Player 3" 30)))
              (list (make-gp "Player 3" 30) (make-gp "Player 2" 20) (make-gp "Player 1" 10)))
(define (sort l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort (rest l)))]))