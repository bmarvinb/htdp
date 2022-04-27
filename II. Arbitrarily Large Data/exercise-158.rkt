;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-158) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
 
(define HEIGHT 220)
(define WIDTH 30)
(define XSHOTS (- (/ WIDTH 2) 5))

; graphical constants
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (rectangle 3 3 "solid" "green"))

; ShotWorld -> ShotWorld
; moves each shot up by one pixel
(check-expect (tock (cons 5 (cons 4 (cons 3 '())))) (cons 4 (cons 3 (cons 2 '()))))
(check-expect (tock (cons 2 (cons 1 (cons 0 '())))) (cons 1 '()))
(define (tock w)
  (cond
    [(empty? w) '()]
    [(<= (sub1 (first w)) 0) '()]
    [else (cons (sub1 (first w)) (tock (rest w)))]))

; ShotWorld KeyEvent -> ShotWorld
; adds a shot to the world if the space bar is hit
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))

; ShotWorld -> Image
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
(check-expect (to-image (cons 9 (cons 11 '())))
              (place-image SHOT XSHOTS 9 (place-image SHOT XSHOTS 11 BACKGROUND)))
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

; ShotWorld -> ShotWorld
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))