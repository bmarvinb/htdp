;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-084) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; Editor KeyEvent -> Editor
(check-expect (edit (make-editor "hello" "world") "left") (make-editor "hell" "oworld"))
(check-expect (edit (make-editor "hello" "world") "right") (make-editor "hellow" "orld"))
(check-expect (edit (make-editor "hell" "world") "o") (make-editor "hello" "world"))
(define (edit ed ke)
  (cond
    [(string=? ke "left") (cursor-left ed)]
    [(string=? ke "right") (cursor-right ed)]
    [(string=? ke "\b") (backspace ed)]
    [(string=? ke "\t") ed]
    [(string=? ke "\r") ed]
    [(= 1 (string-length ke)) (make-editor (append-char (editor-pre ed) ke) (editor-post ed))]
    [else ed]))

; Editor -> Editor
(check-expect (cursor-left (make-editor "Hello" "world")) (make-editor "Hell" "oworld"))
(define (cursor-left ed)
  (make-editor (chop-last (editor-pre ed))
               (prepend-char (editor-post ed) (last-char (editor-pre ed)))))

; Editor -> Editor
(check-expect (cursor-right (make-editor "Hello" "world")) (make-editor "Hellow" "orld"))
(define (cursor-right ed)
  (make-editor (append-char (editor-pre ed) (first-char (editor-post ed)))
               (chop-first (editor-post ed))))

; String -> String
(check-expect (last-char "Hello") "o")
(define (last-char str)
  (substring str (- (string-length str) 1) (string-length str)))

; String -> String
(check-expect (first-char "Hello") "H")
(define (first-char str)
  (substring str 0 1))

; String String -> String
(check-expect (prepend-char "ello" "H") "Hello")
(define (prepend-char str char)
  (string-append char str))

; String String -> String
(check-expect (append-char "Hell" "o") "Hello")
(define (append-char str char)
  (string-append str char))

; String -> String
(check-expect (chop-last "Hello") "Hell")
(define (chop-last str)
  (substring str 0 (- (string-length str) 1)))

; String -> String
(check-expect (chop-first "Hello") "ello")
(define (chop-first str)
  (substring str 1))

; String -> String
(check-expect (backspace "Hello") "Hell")
(define (backspace str)
  (chop-last str))
     

(define EDITOR (make-editor "hello" "world"))