;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-27) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define TICKET_PRICE 5)
(define PEOPLE 120)
(define AVERAGE_ATTENDANCE 15)
(define CHANGE 0.10)
(define COST 180)
(define COST_FOR_ATTENDEE 0.04)

(define (attendees ticket-price)
  (- PEOPLE (* (- ticket-price TICKET_PRICE) (/ AVERAGE_ATTENDANCE CHANGE))))

(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(define (cost ticket-price)
  (+ COST (* COST_FOR_ATTENDEE (attendees ticket-price))))

(define (profit ticket-price)
  (- (revenue ticket-price)
     (cost ticket-price)))
