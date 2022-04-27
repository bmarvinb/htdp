;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-267) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; List-of Dollar -> List-of Euro
; converts a list of US$ amounts into a list of € amounts based on an exchange rate of US$1.06 per €

(define (convert-euro lod)
  (local (
          (define (dollar->euro dollar)
            (/ dollar 1.06)))
    (map dollar->euro lod)))