;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname database-challenges) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct db [schema content])
; A DB is a structure: (make-db Schema Content)
 
; A Schema is a [List-of Spec]
; A Spec is a [List Label Predicate]
; A Label is a String
; A Predicate is a [Any -> Boolean]
 
; A (piece of) Content is a [List-of Row]
; A Row is a [List-of Cell]
; A Cell is Any
; constraint cells do not contain functions 
 
; integrity constraint In (make-db sch con), 
; for every row in con,
; (I1) its length is the same as sch's, and
; (I2) its ith Cell satisfies the ith Predicate in sch

;; School database
(define school-schema
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))
 
(define school-content
  `(("Alice" 35 #true)
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
 
(define school-db
  (make-db school-schema
           school-content))

;; Figure 141
;; DB [List-of Label] -> DB
;; The result is a db only with selecting labels
(define (project db labels)
  (local ((define schema  (db-schema db))
          (define content (db-content db))
 
          ; Spec -> Boolean
          ; does this column belong to the new schema
          (define (keep? c)
            (member? (first c) labels))
 
          ; Row -> Row 
          ; retains those columns whose name is in labels
          (define (row-project row)
            (foldr (lambda (cell m c) (if m (cons cell c) c))
                   '()
                   row
                   mask))
          (define mask (map keep? schema)))
    (make-db (filter keep? schema)
             (map row-project content))))

;; Exercise 408. Design the function select
;; DB [List-of Label] Predicate -> [List-of Row]
;; The result is a list of rows that satisfy the given predicate,
;; projected down to the given set of labels
(check-expect (select school-db '("Name" "Present") (lambda (r) (> (second r) 40)))
              `())
(check-expect (select school-db '("Name" "Age") (lambda (r) (> (third r) #false)))
              `(("Bob" 25)
                ("Dave" 32)))
(check-expect (select school-db '("Name" "Present") (lambda (r) (> (second r) 30)))
              `(("Alice" #true)
                ("Dave"  #false)))
(check-expect (select school-db '("Name") (lambda (r) (> (string-length (first r)) 0)))
              `(("Alice")
                ("Bob")
                ("Carol")
                ("Dave")))
(define (select db lol pred)
  '())

;; Exercise 409. Design reorder
;; DB [List-of Labels] -> DB
;(define (reoder db lol)
;  ())

;; Exercise 410. Design the fuction db-union
;; DB DB -> DB
;(define (db-union db1 db2)
;  ())

;; Exercise 411. Design join
;; DB DB -> DB
;(define (join db1 db2)
;  ())
