;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname refining-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; String -> Dir.v3
; creates a representation of the a-path directory 
; (define (create-dir a-path) ...)

; (define O (create-dir "/Users/dmitrymoiseev/Documents/racket"))
(define O (make-dir
           "racket"
           (list (make-dir "another-dir" '() '()) (make-dir "example-dir" (list (make-dir "secret-dir" '() '())) (list (make-file "secret.txt" 7 (make-date 2022 4 18 19 56 29) ""))))
           (list (make-file "test.txt" 28 (make-date 2022 4 18 19 28 53) ""))))

(define (how-many dir)
  (+ (length (dir-files dir))
     (foldl (lambda (val acc) (+ acc (how-many val)))
            0                                
            (dir-dirs dir))))

;; Exercise 339. Design find?
;; The function consumes a Dir and a file name and determines whether or not a file with this name occurs in the directory tree.

;; [List-of Dir] String -> Boolean
;; check if file founded in some dir
(define (found-in-dirs? lod name) 
  (ormap (lambda (d) (or (found-in-files? (dir-files d) name)
                         (found-in-dirs? (dir-dirs d) name))) lod))

;; [List-of File] String -> Boolean
;; check if some file name are equal to searched file name
(check-expect (found-in-files? (dir-files O) "test.txt") #t)
(define (found-in-files? lof name)
  (ormap (lambda (f) (string=? (file-name f) name)) lof))

;; Dir String -> Boolean
(check-expect (find? O "test.txt") #t)
(check-expect (find? O "secret.txt") #t)
(check-expect (find? O "data.json") #f)
(define (find? dir name)
  (or (found-in-dirs? (dir-dirs dir) name) 
      (found-in-files? (dir-files dir) name)))

;; Exercise 340. Design the function ls, which lists the names of all files and directories in a given Dir.

;; [List-of Dir] -> List-of-String
(define (directories lod)
  (foldl (lambda (val acc)
           (append acc
                   (files (dir-files val))
                   (cons (string-append (dir-name val) "/")
                         (directories (dir-dirs val)))))
         '()
         lod)) 

;; [List-of File] -> List-of-String
(define (files lof)
  (map (lambda (f) (file-name f)) lof))

;; Dir -> List-of-String
(check-expect (ls O)
              (list "another-dir/" "secret.txt" "example-dir/" "secret-dir/" "test.txt"))
(define (ls dir)
  (append (directories (dir-dirs dir))
          (files (dir-files dir))))

;; Exercise 341. Design du, a function that consumes a Dir and computes the total size of all the files in the entire directory tree.
;; Assume that storing a directory in a Dir structure costs 1 file storage unit.

;; [List-of File] -> Number
(define (file-sizes lof)
  (foldl (lambda (f acc)
           (+ acc (file-size f)))
         0
         lof))

;; [List-of Dir] -> Number
(define (dir-sizes lod)
  (foldl (lambda (val acc)
           (+ acc
              1
              (file-sizes (dir-files val))
              (dir-sizes (dir-dirs val))))
         0
         lod))

;; Dir -> Number
;; calculate total size of files/directories in current directory
(check-expect (du O) 38)
(define (du dir)
  (+ (dir-sizes (dir-dirs dir))
     (file-sizes (dir-files dir))))