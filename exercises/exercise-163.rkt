;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-163) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of-Fahrenheit -> List-of-Celsius
(check-expect (convertFC (cons 0 (cons 10 '()))) (cons -17.7 (cons -12.2 '()))) 
(define (convertFC lof)
  (cond
    [(empty? lof) '()]
    [else (cons (* (- (first lof) 32) (/ 5 9)) (convertFC (rest lof)))]))