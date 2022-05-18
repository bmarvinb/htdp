;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname iterative-refinement) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ========== Model 1 ==========

; A Dir.v1 (short for directory) is one of:
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)

; A File.v1 is a String.

(define text (list "part 1" "part 2" "part 3"))
(define code (list "hang" "draw"))
(define docs (list "read!"))
(define libs (list code docs))
(define ts (list text "read!" libs))

; Exercise 331. Design the function how-many, which determines how many files a given Dir.v1 contains.
; Dir.v1 -> Number
(check-expect (how-many ts) 7)
(define (how-many dir)
  (cond
    [(string? dir) 1]
    [(empty? dir) 0]
    [else (+ (how-many (first dir))
             (how-many (rest dir)))]))

; ========== Model 2 ==========

(define-struct dir [name content])

; A Dir.v2 is a structure:
;   (make-dir String LOFD)
 
; An LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.

(define text.v2 (make-dir "Text" (list "part 1" "part 2" "part 3")))
(define code.v2 (make-dir "Code" (list "hang" "draw")))
(define docs.v2 (make-dir "Docs" (list "read!")))
(define libs.v2 (make-dir "Libs" (list code docs)))
(define ts.v2 (make-dir "TS" (list text "read!" libs)))

; Exercise 333. Design the function how-many, which determines how many files a given Dir.v2 contains.
; Dir.v2 -> Number
(check-expect (how-many.v2 ts.v2) 7)
(define (how-many.v2 dir)
  (cond
    [(string? dir) 1]
    [(empty? (dir-content dir)) 0]
    [else (+ (how-many (first (dir-content dir)))
             (how-many (rest (dir-content dir))))]))

; ========== Model 3 ==========
(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)

(define-struct dir.v3 [name dirs files])

; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

(define TEXT-DIR (make-dir.v3 "Text" '() (list (make-file "part 1" 99 "") (make-file "part 2" 52 "") (make-file "part 3" 17 ""))))
(define CODE-DIR (make-dir.v3 "Code" '() (list (make-file "hang" 8 "") (make-file "draw" 2 ""))))
(define DOCS-DIR (make-dir.v3 "Docs" '() (list (make-file "read!" 19 ""))))
(define LIBS-DIR (make-dir.v3 "Libs" (list CODE-DIR DOCS-DIR) '()))
(define TS-DIR (make-dir.v3 "TS" (list TEXT-DIR LIBS-DIR) (list (make-file "read!" 10 ""))))

; Exercise 336. Design the function how-many, which determines how many files a given Dir.v3 contains.
(check-expect (how-many.v3 TS-DIR) 7)
(define (how-many.v3 dir)
  (+ (length (dir.v3-files dir))
     (foldl (lambda (val acc) (+ acc (how-many.v3 val)))
            0                                
            (dir.v3-dirs dir))))