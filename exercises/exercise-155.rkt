;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-155) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct layer [color doll])
; An RD (short for Russian doll) is one of:
; -- String
; -- (make-layer String RD)

; RD -> String
(check-expect (inner (make-layer "yellow" (make-layer "green" "red"))) "red")

(define (inner an-rd)
  (cond
    [(string? (layer-doll an-rd)) (layer-doll an-rd)]
    [else (inner (layer-doll an-rd))]))