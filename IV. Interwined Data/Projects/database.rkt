;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname database) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; Presence database
(define presence-schema
  `(("Present"     ,boolean?)
    ("Description" ,string?)))

(define presence-content
  `((#true  "presence")
    (#false "absence")))

(define presence-db
  (make-db presence-schema
           presence-content))

;; Exercise 403
(define-struct spec [label predicate])
;; Spec is a structure: (make-spec Label Predicate)

(define school-schema.v2
  `((make-spec "Name"    string?)
    (make-spec "Age"     integer?)
    (make-spec "Present" boolean?)))

(define presence-schema.v2
  `((make-spec "Present"     boolean?)
    (make-spec "Description" string?)))

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
(check-expect (integrity-check school-db) #true)
(check-expect (integrity-check presence-db) #true) 
(define (integrity-check db)
  (local (; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (= (length row) (length (db-schema db)))))
    (andmap row-integrity-check (db-content db))))

;; Exercise 404
(check-expect (andmap2 number? '(1 2 3) '(4 5 6)) #true)
(check-expect (andmap2 number? '(1 "Two" 3) '(4 5 6)) #false)
(check-expect (andmap2 number? '(1 2 3) '(4 5 "Six")) #false)
(check-expect (andmap2 number? '("a" "b") '("c" "d" "e")) #false)
(define (andmap2 f l1 l2)
  (and (andmap f l1) (andmap f l2)))

;; School database
(define school-schema.v3
  `(("Name"    ,string?)
    ("Age"     ,integer?)
    ("Present" ,boolean?)))
 
(define school-content.v3
  `(("Alice" 35 "true")
    ("Bob"   25 #false)
    ("Carol" 30 #true)
    ("Dave"  32 #false)))
 
(define school-db.v3
  (make-db school-schema.v3
           school-content.v3))

; DB -> Boolean
; do all rows in db satisfy (I1) and (I2)
(check-expect (integrity-check.v2 school-db.v3) #false)
(define (integrity-check.v2 db)
  (local (; Row -> Boolean 
          ; does row satisfy (I1) and (I2) 
          (define (row-integrity-check row)
            (and (= (length row)
                    (length (db-schema db)))
                 (andmap (lambda (s c) [(second s) c])
                         (db-schema db)
                         row))))
    (andmap row-integrity-check (db-content db))))

(define projected-content
  `(("Alice" #true)
    ("Bob"   #false)
    ("Carol" #true)
    ("Dave"  #false)))
 
(define projected-schema
  `(("Name" ,string?) ("Present" ,boolean?)))
 
(define projected-db
  (make-db projected-schema projected-content))

;; Figure 141
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