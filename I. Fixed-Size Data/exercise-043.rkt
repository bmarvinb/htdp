;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-043) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH-OF-WORLD 500)
(define HEIGHT-OF-WORLD 30)
(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 5))

(define BACKGROUND
  (rectangle WIDTH-OF-WORLD
             HEIGHT-OF-WORLD
             "outline"
             "transparent"))

(define WHEEL
  (circle WHEEL-RADIUS
          "solid"
          "black"))

(define SPACE
  (rectangle WHEEL-DISTANCE
             WHEEL-RADIUS
             "solid"
             "transparent"))

(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

(define CAR-BODY (overlay/align "middle" "bottom"
                                (rectangle
                                 (* 6 WHEEL-RADIUS)
                                 (* 3 WHEEL-RADIUS)
                                 "solid" "red")
                                (rectangle
                                 (+ (image-width BOTH-WHEELS) (* WHEEL-RADIUS 1.5))
                                 (* 1.75 WHEEL-RADIUS)
                                 "solid" "red")))

(define CAR
  (underlay/xy CAR-BODY
               (* WHEEL-RADIUS 0.75)
               (* WHEEL-RADIUS 2.25)
               BOTH-WHEELS))

(define Y-CAR
  (- HEIGHT-OF-WORLD (/ (image-height CAR) 2)))

(define START-POSITION
  (/ (image-width CAR) 2))

; WorldState -> Image
; places the image of the car x pixels from 
; the left margin of the BACKGROUND image 
(define (render x)
  (place-image CAR x Y-CAR BACKGROUND))
 
; WorldState -> WorldState
; adds 3 to x to move the car right 
(define (tock ws)
  (+ ws 3))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down" 
(define (hyper x-position-of-car x-mouse y-mouse me)
  (if (equal? me "button-down") x-mouse x-position-of-car))

; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
  (big-bang ws
    [on-tick tock]
    [on-mouse hyper]
    [to-draw render]))