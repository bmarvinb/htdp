;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-83) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

(define CURSOR (rectangle 1 20 "solid" "red"))
(define (display-text content)
  (text content 16 "black"))

; Editor -> Image
(define (render editor)
  (overlay/align "left" "center"
                 (beside/align "bottom" (display-text (editor-pre editor)) CURSOR (display-text (editor-post editor)))
                 (empty-scene 200 20)))

(define EDITOR (make-editor "hello" "world"))

(render EDITOR)