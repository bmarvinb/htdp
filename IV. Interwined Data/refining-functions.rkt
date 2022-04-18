;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname refining-functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require htdp/dir)

; String -> Dir.v3
; creates a representation of the a-path directory 
; (define (create-dir a-path) ...)

(define O (create-dir "/Users/dmitrymoiseev/Documents/"))

(define (how-many dir)
  (+ (length (dir-files dir))
     (foldl (lambda (val acc) (+ acc (how-many val)))
            0                                
            (dir-dirs dir))))

;; Exercise 339. Design find?
;; The function consumes a Dir and a file name and determines whether or not a file with this name occurs in the directory tree.
;; Dir String -> Boolean
(check-expect (find? O "test.txt") #t)
(check-expect (find? O "secret.txt") #t)
(check-expect (find? O "data.json") #f)
(define (find? dir name)
  (or (found-in-dirs? (dir-dirs dir) name) 
      (found-in-files? (dir-files dir) name)))

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