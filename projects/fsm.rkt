;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; An FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
 
; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to keystrokes

; Exercise 226. Design state=?, an equality predicate for states.
; Example
(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

; FSM-State FSM-State -> Boolean
(check-expect (state=? "red" "red") #t)
(check-expect (state=? "red" "green") #f)
(define (state=? s1 s2)
  (equal? s1 s2))

; Exercise 227. The BW Machine is an FSM that flips from black to white and back to black for every key event.
; Formulate a data representation for the BW Machine.
(define fsm-bw
  (list (make-transition "black" "black")
        (make-transition "white" "black")))

; FSM-State -> FSM-State
(check-expect (next-bw-state (make-transition "white" '())) "black")
(check-expect (next-bw-state (make-transition "black" '())) "white")
(define (next-bw-state s)
  (if (state=? "white" (transition-current s)) "black" "white"))

; Exercise 228. Complete the design of find
; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "purple")
             "not found: purple")
(define (find transitions current)
  (cond
    [(empty? transitions) (error (string-append "not found: " current))]
    [else (if (state=? current
                       (transition-current (first transitions)))
              (transition-next (first transitions))
              (find (rest transitions) current))]))


  
