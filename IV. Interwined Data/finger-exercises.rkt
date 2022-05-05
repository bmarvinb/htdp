;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname finger-exercises) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

;; Exercise 393
;; Set Set -> Set
(check-expect (union '() '()) '())
(check-expect (union '(1) '()) '(1))
(check-expect (union '() '(2)) '(2))
(check-expect (union '(1) '(2)) '(1 2))
(check-expect (union '(1 2) '(2)) '(1 2))
(check-expect (union '(1) '(1 2)) '(1 2))
(check-expect (union '(1 2 3) '(0 3 4)) '(1 2 3 0 4))
(define (union s1 s2)
  (foldl (lambda (value acc)
           (if (not (member value acc))
               (append acc (list value))
               acc))
         '()
         (append s1 s2)))

(check-expect (intersect '() '()) '())
(check-expect (intersect '(1) '()) '())
(check-expect (intersect '() '(2)) '())
(check-expect (intersect '(1) '(2)) '())
(check-expect (intersect '(1) '(1)) '(1))
(check-expect (intersect '(1 2) '(2)) '(2))
(check-expect (intersect '(1) '(1 2)) '(1))
(check-expect (intersect '(1 2 3) '(0 3 4)) '(3))
(define (intersect s1 s2)
  (foldl (lambda (value acc)
           (if (and (member value s1) (member value s2) (not (member value acc)))
               (append acc (cons value '()))
               acc))
         '()
         (append s1 s2)))

;; Exercise 394
;; [List-of Number] [List-of Number] -> [List-of Number]
(check-expect (merge '() '()) '())
(check-expect (merge '(1) '()) '(1))
(check-expect (merge '() '(2)) '(2))
(check-expect (merge '(1) '(2)) '(1 2))
(check-expect (merge '(2) '(1)) '(1 2))
(check-expect (merge '(1) '(2)) '(1 2))
(check-expect (merge '(1 2) '(2 3)) '(1 2 2 3))
(check-expect (merge '(3 4 6) '(1 2 5 6)) '(1 2 3 4 5 6 6))
(define (merge l1 l2)
  (cond
    [(and (empty? l1) (empty? l2)) '()]
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else
     (if (<= (first l2) (first l1))
         (cons (first l2) (merge l1 (rest l2)))
         (cons (first l1) (merge (rest l1) l2)))]))

;; Exercise 395
;; [List-of Number] Number -> [List-of Number]
(check-expect (take '() 1) '())
(check-expect (take '(5) 0) '())
(check-expect (take '(5) 1) '(5))
(check-expect (take '(5 10 6 3) 2) '(5 10))
(check-expect (take '(1 2) 10) '(1 2))
(define (take lon n)
  (cond
    [(empty? lon) '()]
    [(<= n 0) '()]
    [else (cons (first lon)
                (take (rest lon) (- n 1)))]))

;; [List-of Number] Number -> [List-of Nubmber]
(check-expect (drop '() 1) '())
(check-expect (drop '(5) 0) '(5))
(check-expect (drop '(5) 1) '())
(check-expect (drop '(5) 2) '())
(check-expect (drop '(5 10) 1) '(10))
(check-expect (drop '(5 10 6 3) 0) '(5 10 6 3))
(check-expect (drop '(5 10 6 3) 2) '(6 3))
(check-expect (drop '(5 10 6 3) 20) '())
(define (drop lon n)
  (cond
    [(empty? lon) '()]
    [(<= n 0) lon]
    [else (drop (rest lon) (- n 1))]))

;; Exercise 396
(define LOCATION "/usr/share/dict/words") ; on OS X
(define AS-LIST (read-lines LOCATION))
(define SIZE (length AS-LIST))
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

;; An HM-Word is a [List-of Letter or "_"]
;; interpretation "_" represents a letter to be guessed
;; HM-Word N -> String
;; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ;; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ;; String [List-of String] KeyEvent -> HM-Word
          (define (compare-word word status ke)
            (map (lambda (c)
                   (if (or (string=? ke c)
                           (member? c status)) c "_"))
                 word))
          ;; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
       [to-draw render-word]
       [on-tick do-nothing 1 time-limit]
       [on-key  checked-compare]))))
 
; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

(play (list-ref AS-LIST (random SIZE)) 10)

;; Exercise 397
;; !!!


