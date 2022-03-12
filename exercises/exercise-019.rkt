;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname exercise-19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (join s1 s2)(string-append s1 "_" s2))


(define (string-insert str i)(join (substring str 0 i) (substring str i)))

(string-insert "Helloworld" 5)
