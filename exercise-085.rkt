;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-85) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

(define CURSOR (rectangle 1 20 "solid" "red"))
(define BACKGROUND (empty-scene 200 20))

; String -> Image
(check-expect (draw-text-part "Hello") (text "Hello" 16 "black"))
(define (draw-text-part content)
  (text content 16 "black"))

; Editor -> Image
(define (draw-text ed)
  (beside/align "bottom"
                (draw-text-part (editor-pre ed))
                CURSOR
                (draw-text-part (editor-post ed))))

; Editor -> Image
(define (render ed)
  (overlay/align "left"
                 "center"
                 (draw-text ed)
                 BACKGROUND))

; Editor KeyEvent -> Boolean
(define (add-character? ed ke)
  (and (= 1 (string-length ke))
       (< (image-width (draw-text ed)) (image-width BACKGROUND))))

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
    [(add-character? ed ke) (add ed ke)]
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

; Editor -> Editor
(check-expect (backspace (make-editor "Hello" "world")) (make-editor "Hell" "world"))
(define (backspace ed)
  (if (<= (string-length (editor-pre ed)) 0)
      ed
      (make-editor (chop-last (editor-pre ed))
                   (editor-post ed))))

; Editor KeyEvent -> Editor
(check-expect (add (make-editor "Hell" "world") "o") (make-editor "Hello" "world"))
(define (add ed ke)
  (make-editor (append-char (editor-pre ed) ke)
               (editor-post ed)))

; String -> String
(check-expect (last-char "Hello") "o")
(define (last-char str)
  (substring str
             (- (string-length str) 1)
             (string-length str)))

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

(define (run str)
  (big-bang (make-editor str "")
            [to-draw render]
            [on-key edit]))
