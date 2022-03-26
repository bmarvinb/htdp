;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-47) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WINDOW-WIDTH 800)
(define WINDOW-HEIGHT 200)
(define GAUGE-CONTAINER-WIDTH 500)
(define GAUGE-CONTAINER-HEIGHT 100)

(define BACKGROUND
  (rectangle WINDOW-WIDTH
             WINDOW-HEIGHT
             "outline"
             "transparent"))

(define GAUGE-CONTAINER
  (rectangle GAUGE-CONTAINER-WIDTH
             GAUGE-CONTAINER-HEIGHT
             "outline"
             "black"))

(define (gauge-width percents)
  (if (<= percents 0) 0 (/ (* GAUGE-CONTAINER-WIDTH percents) 100)))

(define (gauge-value percents)
  (rectangle (gauge-width (- 100 percents))
             GAUGE-CONTAINER-HEIGHT
             "solid"
             "red"))

(define (gauge percents)
  (overlay/xy GAUGE-CONTAINER
              0
              0
              (gauge-value percents)))

(define (render ws)
  (place-image (gauge ws)
               300
               100
               BACKGROUND))

(define (tock ws)
  (+ ws 0.1))

(define (change-gauge ws a-key)
  (cond
    [(key=? a-key "up") (- ws (* ws 1/5))]
    [(key=? a-key "down") (+ ws (* ws 1/3))]
    [else ws]))

(define (main ws)
   (big-bang ws
     [on-tick tock]
     [on-key change-gauge]
     [to-draw render]))

(main 0)