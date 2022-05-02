;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rendering-xml-enumerations) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==================== Rendering XML Enumerations ====================
(require 2htdp/abstraction)
(require 2htdp/image)

;; An XWord is '(word ((text String))).

;; Exercise 370
(define w1 '(word ((text "Hello"))))
(define w2 '(word ((text "Hello World"))))
(define w3 '(word ((text ""))))

;; Any -> Boolean
(check-expect (word? w1) #true)
(check-expect (word? w2) #true)
(check-expect (word? 'word) #false)
(define (word? v)
  (match v
    [(list 'word (list (list 'text (? string?)))) #true]
    [else #false]))

; Xword -> String
(check-expect (word-text w1) "Hello")
(check-expect (word-text w2) "Hello World")
(define (word-text w)
  (match w
    [(list 'word (list (list 'text txt))) txt]))

;; Exercise 371

;; An Attribute is a list of two items:
;;   (cons Symbol (cons String '()))

;; A body is one of:
;; - [List-of Xexpr]
;; - XWord

;; An Xexpr is a list:
;; - (cons Symbol Body)
;; - (cons Symbol (cons [List-of Attribute] Body))

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

(define e0
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))

(define BT (overlay (circle 2 "solid" 'black) (circle 3 "solid" 'white)))

(define e0-rendered
  (above/align
   'left
   (beside/align 'center BT (text "one" 12 'black))
   (beside/align 'center BT (text "two" 12 'black))))

;; Xexpr -> Body
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe))
          (define (list-of-attributes? x)
            (cond
              [(empty? x) #true]
              [else
               (local ((define possible-attribute (first x)))
                 (cons? possible-attribute))])))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (if (list-of-attributes? (first optional-loa+content))
           (rest optional-loa+content)
           optional-loa+content)])))

;; XItem -> Image
;; Renders an item as a "word" prefixed by a bullet.
(check-expect (render-item `(li ,w1))
              (beside/align 'center BT (text "Hello" 12 'black)))
(check-expect (render-item `(li ,w2))
              (beside/align 'center BT (text "Hello World" 12 'black)))
(check-expect (render-item `(li ((id "second")) ,w2))
              (beside/align 'center BT (text "Hello World" 12 'black)))
(define (render-item i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center BT item)))
