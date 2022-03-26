;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-262) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Exercise 262. Design the function identityM,
; which creates diagonal squares of 0s and 1s

(check-expect (identityM 1)
              (list (list 1)))

(check-expect (identityM 2)
              (list (list 1 0) (list 0 1)))

(check-expect (identityM 3)
              (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

; Number -> LList-of-numbers
(define (identityM n)
  (local (;; the length of columns
          (define cols n)
          
          ;; N -> [List-of [List-of N]]
          (define (generate-matrix n)
            (cond
              [(zero? n) '()]
              [else (cons (generate-row n cols)
                         (generate-matrix (- n 1)))]))

          ;; N N -> [List-of N]
          (define (generate-row n len)
            (cond
              [(zero? len) '()]
              [else (cons (if (= n len) 1 0)
                          (generate-row n (- len 1)))])))
    (generate-matrix n)))

