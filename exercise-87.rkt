;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-87) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [text index])
; An Editor is a structure:
;   (make-editor String Number)

(define CURSOR (rectangle 1 20 "solid" "red"))
(define BACKGROUND (empty-scene 200 20))

; Editor -> Image
(define (draw-text ed)
  (beside/align "bottom"
                (text (pre ed) 16 "black")
                CURSOR
                (text (post ed) 16 "black")))

; Editor -> Image
(define (render ed)
  (overlay/align "left" "center" (draw-text ed) BACKGROUND))

; Editor KeyEvent -> Boolean
(define (add-character? ed ke)
  (and (= 1 (string-length ke))
       (< (image-width (draw-text ed)) (image-width BACKGROUND))))

; Editor KeyEvent -> Editor
(define (edit ed ke)
  (cond
    [(string=? ke "left") (cursor-left ed)]
    [(string=? ke "right") (cursor-right ed)]
    [(string=? ke "\b") (backspace ed)]
    [(string=? ke "\t") ed]
    [(string=? ke "\r") ed]
    [(add-character? ed ke) (add ed ke)]
    [else ed]))

; Editor -> Editor
(check-expect (cursor-left (make-editor "Helloworld" 5)) (make-editor "Helloworld" 4))
(check-expect (cursor-left (make-editor "Helloworld" 0)) (make-editor "Helloworld" 0))
(define (cursor-left ed)
  (make-editor (editor-text ed)
               (if (<= (editor-index ed) 0)
                   0
                   (- (editor-index ed) 1))))

; Editor -> Editor
(check-expect (cursor-right (make-editor "Helloworld" 5)) (make-editor "Helloworld" 6))
(check-expect (cursor-right (make-editor "Helloworld" 10)) (make-editor "Helloworld" 10))
(define (cursor-right ed)
  (make-editor (editor-text ed)
               (if (>= (editor-index ed) (string-length (editor-text ed)))
                   (editor-index ed)
                   (+ (editor-index ed) 1))))

; Editor -> Editor
(check-expect (backspace (make-editor "Hello" 5)) (make-editor "Hell" 4))
(check-expect (backspace (make-editor "Hello" 1)) (make-editor "ello" 0))
(define (backspace ed)
  (if (<= (string-length (editor-text ed)) 0)
      ed
      (make-editor (string-append (chop-last (pre ed)) (post ed))
                   (- (editor-index ed) 1))))

; Editor KeyEvent -> Editor
(check-expect (add (make-editor "Hell" 4) "o") (make-editor "Hello" 5))
(define (add ed ke)
  (make-editor (string-append (editor-text ed) ke)
               (+ (editor-index ed) 1)))

; Editor -> String
(check-expect (pre (make-editor "Helloworld" 5)) "Hello")
(define (pre ed)
  (substring (editor-text ed)
             0
             (editor-index ed)))

; Editor -> String
(check-expect (post (make-editor "Helloworld" 5)) "world")
(define (post ed)
  (substring (editor-text ed)
             (editor-index ed)
             (string-length (editor-text ed))))

; String -> String
(check-expect (chop-last "Hello") "Hell")
(define (chop-last str)
  (substring str 0 (- (string-length str) 1)))

(define (run str)
  (big-bang (make-editor str 0)
            [to-draw render]
            [on-key edit]))
