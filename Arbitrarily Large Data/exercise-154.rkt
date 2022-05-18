;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-154) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [color doll])
; An RD (short for Russian doll) is one of:
; -- String
; -- (make-layer String RD)

; RD -> String
(check-expect (colors (make-layer "yellow" (make-layer "green" "red"))) "yellow, green, red")

(define (colors an-rd)
  (cond
    [(string? (layer-doll an-rd)) (string-append (layer-color an-rd) ", " (layer-doll an-rd))]
    [else (string-append (layer-color an-rd) ", " (colors (layer-doll an-rd)))]))