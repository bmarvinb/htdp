;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-094) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WINDOW-WIDTH 800)
(define WINDOW-HEIGHT 600)

; (make-ufo Image Number Number)
(define-struct ufo [image pos-x pos-y])

; (make-tank Image Number Number)
(define-struct tank [image pos-x pos-y])

; (make-game-state Ufo Tank)
(define-struct game-state [ufo tank])

(define UFO-IMAGE (rectangle 50 50 "solid" "red"))
(define TANK-IMAGE (rectangle 100 50 "solid" "green"))

; GameState -> GameState
(define (render game-state)
  (place-image
   (tank-image (game-state-tank game-state))
   (tank-pos-x (game-state-tank game-state))
   (tank-pos-y (game-state-tank game-state))
  (place-image
   (ufo-image (game-state-ufo game-state))
   (ufo-pos-x (game-state-ufo game-state))
   (ufo-pos-y (game-state-ufo game-state))
   (rectangle WINDOW-WIDTH  WINDOW-HEIGHT "outline" "transparent"))))

(define (main game-state)
   (big-bang game-state
     [to-draw render]))