;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname dsl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ==================== Rendering XML Enumerations ====================
(require 2htdp/abstraction)
(require 2htdp/image)
(require 2htdp/universe)

;; An FSM is a [List-of 1Transition]
;; A 1Transition is a list of two items:
;;   (cons FSM-State (cons FSM-State '()))
;; An FSM-State is a String that specifies a color
 
;; data examples 
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))
 
;; FSM-State FSM -> FSM-State 
;; matches the keys pressed by a player with the given FSM 
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
    [to-draw
      (lambda (current)
        (overlay (text current 16 "black") (square 100 "solid" current)))]
    [on-key
      (lambda (current key-event)
        (find transitions current))]))

;; Attrs-or-Xexpr -> Boolean
;; Determines whether x is an element of [List-of Attribute].
;; Otherwise produces #false.
(define (list-of-attributes? x)
  (cond
    [(empty? x) #true]
    [else
     (local ((define possible-attribute (first x)))
       (cons? possible-attribute))]))
  
;; [X Y] [List-of [List X Y]] X -> Y
;; finds the matching Y for the given X in alist
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-expect (find fsm-traffic "yellow") "red")
(check-error (find fsm-traffic "black") "not found")
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm) (second fm) (error "not found"))))

;; [List-of Attribute] Symbol -> [Maybe String]
;; Retrieves a string associated with s from loa.
(define (find-attr loa s)
  (local ((define found (assq s loa)))
    (if (false? found)
        #false
        (second found))))

;; Xexpr -> [List-of Xexpr]
;; Retrieves the list of content elements.
(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (if (list-of-attributes? (first optional-loa+content))
           (rest optional-loa+content)
           optional-loa+content)])))

;; Xexpr -> [List-of Attribute]
;; Retrieves the list of attributes of xe.
(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond
      [(empty? optional-loa+content) '()]
      [else
       (local ((define loa-or-x
                 (first optional-loa+content)))
         (if (list-of-attributes? loa-or-x)
             loa-or-x
             '()))])))

;; XMachine -> FSM-State 
;; interprets the given configuration as a state machine 
(define (simulate-xmachine xm)
  (simulate (xm-state0 xm) (xm->transitions xm)))

(define xm0
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))

;; XMachine -> FSM-State 
;; extracts and translates the transition table from xm0 
(check-expect (xm-state0 xm0) "red")
(define (xm-state0 xm0)
  (find-attr (xexpr-attr xm0) 'initial))
 
;; XMachine -> [List-of 1Transition]
;; extracts the transition table from xm
(check-expect (xm->transitions xm0) fsm-traffic)
(define (xm->transitions xm)
  (local (; X1T -> 1Transition
          (define (xaction->action xa)
            (list (find-attr (xexpr-attr xa) 'state)
                  (find-attr (xexpr-attr xa) 'next))))
    (map xaction->action (xexpr-content xm))))