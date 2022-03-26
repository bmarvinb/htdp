;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-257) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design build-l*st, which works just like build-list.
(check-expect (build-l*st 3)
              (list 1 2 3))
(define (build-l*st n)
  (cond
    [(= n 1) (cons 1 '())]
    [else (reverse (cons n
                         (reverse (build-l*st (- n 1)))))]))