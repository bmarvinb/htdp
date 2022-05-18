;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname exercise-188) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 188. Design a program that sorts lists of emails by date

(define-struct email [from date message])
; An Email Message is a structure: 
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m 
; sent by f, d seconds after the beginning of time

(define (insert email l)
  (cond
    [(empty? l) (cons email '())]
    [else (if (>= (email-date email) (email-date (first l)))
              (cons email l)
              (cons (first l) (insert email (rest l))))]))

(check-expect (sort (list
                     (make-email "Author 1" 2015 "Text 1")
                     (make-email "Author 2" 2017 "Text 2")
                     (make-email "Author 3" 2016 "Text 3")))
              (list
               (make-email "Author 2" 2017 "Text 2")
               (make-email "Author 3" 2016 "Text 3")
               (make-email "Author 1" 2015 "Text 1")))
(define (sort l)
  (cond
    [(empty? l) '()]
    [(cons? l) (insert (first l) (sort (rest l)))]))
