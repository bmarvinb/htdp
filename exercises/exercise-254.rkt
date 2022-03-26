;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-254) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Formulate signatures for the following functions:
; sort-n, which consumes a list of numbers and a function that consumes two numbers (from the list) and produces a Boolean;
; sort-n produces a sorted list of numbers.

; sort-s, which consumes a list of strings and a function that consumes two strings (from the list) and produces a Boolean;
; sort-s produces a sorted list of strings.

; [List-of T] [T T -> Boolean]
; -> [List-of T]
(define (sort l f))