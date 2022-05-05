;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname list-processing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 387

;; Pair is a list with Symbol and Number

;; Symbol [List-of Numbers] -> [List-of Pair]
;; Get pairs for concrete symbol
(check-expect (cross-one 'a '(1 2)) '((a 1) (a 2)))
(check-expect (cross-one 'b '(1 2 3)) '((b 1) (b 2) (b 3)))
(define (cross-one s lon)
  (cond
    [(empty? lon) '()]
    [else (cons (list s (first lon))
                (cross-one s (rest lon)))]))

;; [List-of Symbols] [List-of Numbers] -> [List-of Pair]
(check-expect (cross '(a b c) '(1 2)) '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(check-expect (cross '(a b) '(1 2 3)) '((a 1) (a 2) (a 3) (b 1) (b 2) (b 3)))
(define (cross los lon)
  (cond
    [(empty? los) '()]
    [else (append (cross-one (first los) lon)
                  (cross (rest los) lon))]))

;; Exercise 389
(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)

;; [List-of String] [List-of String] -> [List-of PhoneRecord]
(check-expect (zip '("Bob" "Alise") '("333-555-4444" "500-000-250"))
              (list (make-phone-record "Bob" "333-555-4444") (make-phone-record "Alise" "500-000-250")))
(define (zip names phones)
  (cond
    [(empty? names) '()]
    [else (cons (make-phone-record (first names) (first phones))
                (zip (rest names) (rest phones)))]))





