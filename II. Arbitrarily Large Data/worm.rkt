;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Worm — also known as Snake — is one of the oldest computer games

;; =================
;; Constants:

(define WORLD-WIDTH 200)
(define WORLD-HEIGHT 200)

(define MTS (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define RADIUS 5)
(define WORM-IMG (circle RADIUS "solid" "red"))
(define FOOD-IMG (circle RADIUS "solid" "green"))
  
;; =================
;; Data definitions:

(define-struct worm-part (x-pos y-pos))
;; WormPart is (make-worm Number Number)
;; interp. the worm part at position x, y

(define WORM-PART-1 (make-worm-part 0 0))
(define WORM-PART-2 (make-worm-part 10 10))
(define WORM-PART-3 (make-worm-part (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2)))

(define-struct food (x-pos y-pos))
;; Food is (make-food Number Number)
;; interp. the food at position x, y

(define FOOD-1 (make-food 0 0))
(define FOOD-2 (make-food 10 10))
(define FOOD-3 (make-food (/ WORLD-WIDTH 2) (/ WORLD-HEIGHT 2)))

(define-struct worm (worm-parts food direction))
;; Worm is (make-worm List-of-WormPart Food String)
;; interp. the worm store worm parts, food and worm direction

(define WORM-1 (make-worm (cons WORM-PART-1 empty) FOOD-2 "right"))

;; =================
;; Functions:

;; Number -> Number
;; add fixed padding to position coordinate
(define (add-padding pos)
  (+ pos RADIUS))

;; Worm -> Worm
;; produce the next ...
;; !!!
(define (tock w) w)

;; Worm -> Image
;; render the worm parts and food 
;; !!!
(define (render-worm w)
  (place-image WORM-IMG
               (add-padding 0)
               (add-padding 0)
               (place-image WORM-IMG
                            (add-padding 10)
                            (add-padding 0)
                            MTS)))

;; Worm KeyEvent -> Worm
;; !!!
(define (handle-key w ke)
  (cond [(key=? ke " ") (... w)]
        [else 
         (... w)]))

;; Worm -> Boolean
;; !!!
(define (stop-game? w)
  #f)

;; Worm -> Worm
;; ...
(define (main w)
  (big-bang w                ; Worm 
    (on-tick tock)           ; Worm -> Worm
    (to-draw render-worm)    ; Worm -> Image
    (on-key handle-key)      ; Worm KeyEvent -> Worm
    (stop-when stop-game?))) ; Worm -> Boolean

(main WORM-1)