;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-51) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define GREEN "green")
(define YELLOW "yellow")
(define RED "red")
(define DELAY 3)
(define DURATION 15)

(define BACKGROUND
  (rectangle 100
             100
             "outline"
             "transparent"))

; String -> Image
(define (render str)
  (place-image (circle 50 "solid" str)
               (/ (image-width BACKGROUND) 2)
               (/ (image-height BACKGROUND) 2)
               BACKGROUND))

; String -> String
(define (traffic-light-next traffic-light)
  (cond
    [(string=? traffic-light RED) GREEN]
    [(string=? traffic-light GREEN) YELLOW]
    [(string=? traffic-light YELLOW) RED]))

(define (main traffic-light delay)
   (big-bang traffic-light
     [on-tick traffic-light-next delay (/ DURATION DELAY)]
     [to-draw render]))

(main YELLOW DELAY)