;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname itunes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

; An LTracks is one of:
; – '()
; – (cons Track LTracks)

; (define-struct track [name artist album time track# added play# played])
; A Track is a structure:
;   (make-music-track String String String N N Date N Date)
; interpretation An instance records in order: the track's 
; title, its producing artist, to which album it belongs, 
; its playing time in milliseconds, its position within the 
; album, the date it was added, how often it has been 
; played, and the date when it was last played
 
; (define-struct date [year month day hour minute second])
; A Date is a structure:
;   (make-date N N N N N N)
; interpretation An instance records six pieces of information:
; the date's year, month (between 1 and 12 inclusive), 
; day (between 1 and 31), hour (between 0 
; and 23), minute (between 0 and 59), and 
; second (also between 0 and 59).

(define ITUNES-LOCATION "itunes.xml")

; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

; Exercise: 200. Design the function total-time, which consumes an element of LTracks and produces the total amount of play time. 
; LTracks -> Number
(check-expect (total-time (list (create-track "Chop Suey!" "SOAD" "Toxicity" 180000 1 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 11 13 21 12 35))
                                (create-track "Bounce" "SOAD" "Toxicity" 240000 2 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 11 13 21 16 59))
                                (create-track "Toxicity" "SOAD" "Toxicity" 240000 3 (create-date 2001 9 1 14 31 25) 6 (create-date 2014 11 13 21 20 11))))
              660000)
(define (total-time tracks)
  (cond
    [(empty? tracks) 0]
    [else (+ (track-time (first tracks)) (total-time (rest tracks)))]))


; Exercise 201. Design select-all-album-titles.
; The function consumes an LTracks and produces the list of album titles as a List-of-strings.
; LTracks -> List-of-strings
(check-expect (select-all-album-titles (list (create-track "Chop Suey!" "SOAD" "Toxicity" 180000 1 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 11 13 21 12 35))
                                             (create-track "Bounce" "SOAD" "Toxicity" 240000 2 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 11 13 21 16 59))
                                             (create-track "Toxicity" "SOAD" "Toxicity" 240000 3 (create-date 2001 9 1 14 31 25) 6 (create-date 2014 11 13 21 20 11))))
              (list "Chop Suey!" "Bounce" "Toxicity"))
(define (select-all-album-titles tracks)
  (cond
    [(empty? tracks) '()]
    [else (cons (track-name (first tracks)) (select-all-album-titles (rest tracks)))]))

; Design the function create-set.
; It consumes a List-of-strings and constructs one that contains every String from the given list exactly once.

; List-of-strings String -> List-of-strings
(check-expect (filter-by-name (list "a" "a" "b" "c") "a")
              (list "a" "a"))
(check-expect (filter-by-name (list "a" "a" "b" "c") "b")
              (list "b"))
(define (filter-by-name los name)
  (cond
    [(empty? los) '()]
    [(string=? name (first los)) (cons name (filter-by-name (rest los) name))]
    [else (filter-by-name (rest los) name)]))

; String -> Boolean
(check-expect (unique-name? (list "a" "a" "b" "c") "a") #false)
(check-expect (unique-name? (list "a" "a" "b" "c") "b") #true)
(define (unique-name? los name)
  (= 1 (length (filter-by-name los name))))

; List-of-strings -> List-of-strings
(check-expect (create-set (list "Chop Suey!" "Chop Suey!" "Bounce" "Toxicity" "Toxicity"))
              (list "Chop Suey!" "Bounce" "Toxicity"))
(define (create-set los)
  (cond
    [(empty? los) '()]
    [(unique-name? los (first los)) (cons (first los) (create-set (rest los)))]
    [else (create-set (rest los))]))

; Exercise 202. Design select-album.
; The function consumes the title of an album and an LTracks. It extracts from the latter the list of tracks that belong to the given album.