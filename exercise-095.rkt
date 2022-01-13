;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-95) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 200)
(define HEIGHT 200)
(define BACKGROUND
  (rectangle WIDTH HEIGHT "outline" "black"))
(define UFO
  (rectangle 20 10 "solid" "green"))
(define TANK
  (rectangle 30 10 "solid" "blue"))
(define MISSILE
  (triangle 10 "solid" "red"))
(define TANK-Y (- HEIGHT (image-height TANK)))

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
; A UFO is a Posn.
; interpretation (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn.
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a space invader game

; Ufo Image -> Image
(define (ufo-render ufo image)
  (place-image UFO (posn-x ufo) (posn-y ufo) image))

; Tank Image -> Image
(define (tank-render tank image)
  (place-image TANK (tank-loc tank) TANK-Y image))

; Missile Image -> Image
(define (missile-render missile image)
  (place-image MISSILE (posn-x missile) (posn-y missile) image))

; SIGS -> Image
(define (aim-render s)
  (ufo-render (aim-ufo s) 
              (tank-render (aim-tank s) BACKGROUND)))

; SIGS -> Image
(define (fired-render s)
  (ufo-render (fired-ufo s)
              (tank-render (fired-tank s)
                           (missile-render (fired-missile s) BACKGROUND))))

; SIGS -> Image
; adds TANK, UFO, and possibly MISSILE to 
; the BACKGROUND scene
(define (si-render s)
  (cond
    [(aim? s) (aim-render s)]
    [(fired? s) (fired-render s)]))

;(si-render (make-aim
;            (make-posn 13 20)
;            (make-tank 28 -3)))

;(si-render (make-fired
;  (make-posn 20 100)
;  (make-tank 100 3)
;  (make-posn 22 103)))

(si-render (make-fired
  (make-posn 10 20)
  (make-tank 28 -3)
  (make-posn 32 (- HEIGHT (image-height TANK) 10))))