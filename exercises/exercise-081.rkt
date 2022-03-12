;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-81) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct time [hours minutes seconds])

(define (time->seconds t)
  (+
   (* 60 60 (time-hours t))
   (* 60 (time-minutes t))
   (time-seconds t)))

(check-expect (time->seconds (make-time 12 30 2)) 45002)