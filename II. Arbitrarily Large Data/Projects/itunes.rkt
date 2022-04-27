;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname itunes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/itunes)

; Part I
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

; Example data for testing
(define TRACKS-STUB (list (create-track "Chop Suey!" "SOAD" "Toxicity" 180000 1 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 11 13 21 12 35))
                          (create-track "Bounce" "SOAD" "Toxicity" 240000 2 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 12 20 21 16 59))
                          (create-track "Toxicity" "SOAD" "Toxicity" 240000 3 (create-date 2001 9 1 14 31 25) 6 (create-date 2014 12 20 21 20 11))
                          (create-track "Attack" "SOAD" "Hypnotize" 180000 3 (create-date 2004 22 11 14 31 25) 6 (create-date 2014 12 1 21 20 11))))

; Exercise: 200. Design the function total-time, which consumes an element of LTracks and produces the total amount of play time. 
; LTracks -> Number
(check-expect (total-time TRACKS-STUB) 840000)
(define (total-time tracks)
  (cond
    [(empty? tracks) 0]
    [else (+ (track-time (first tracks)) (total-time (rest tracks)))]))


; Exercise 201. Design select-all-album-titles.
; The function consumes an LTracks and produces the list of album titles as a List-of-strings.
; LTracks -> List-of-strings
(check-expect (select-all-album-titles TRACKS-STUB)
              (list "Toxicity" "Toxicity" "Toxicity" "Hypnotize"))
(define (select-all-album-titles tracks)
  (cond
    [(empty? tracks) '()]
    [else (cons (track-album (first tracks)) (select-all-album-titles (rest tracks)))]))

; Design the function create-set.
; It consumes a List-of-strings and constructs one that contains every String from the given list exactly once.

; List-of-strings String -> List-of-Strings
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

; List-of-strings -> List-of-Strings
(check-expect (create-set (list "Chop Suey!" "Chop Suey!" "Bounce" "Toxicity" "Toxicity"))
              (list "Chop Suey!" "Bounce" "Toxicity"))
(define (create-set los)
  (cond
    [(empty? los) '()]
    [(unique-name? los (first los)) (cons (first los) (create-set (rest los)))]
    [else (create-set (rest los))]))

; Exercise 202. Design select-album.
; The function consumes the title of an album and an LTracks.
; It extracts from the latter the list of tracks that belong to the given album.

; String LTracks -> List-of-Strings
(check-expect (select-album "Toxicity" TRACKS-STUB) (list "Chop Suey!" "Bounce" "Toxicity"))
(define (select-album title tracks)
  (cond
    [(empty? tracks) '()]
    [(string=? title (track-album (first tracks))) (cons (track-name (first tracks))
                                                         (select-album title (rest tracks)))]
    [else (select-album title (rest tracks))]))

; Exercise 203. Design select-album-date. The function consumes the title of an album, a date, and an LTracks.
; It extracts from the latter the list of tracks that belong to the given album and have been played after the given date.

(define SECONDS-IN-YEAR 31556952)
(define SECONDS-IN-MONTH 2629746)
(define SECONDS-IN-DAY 86400)
(define SECONDS-IN-HOUR 3600)
(define SECONDS-IN-MINUTE 60)

; Date -> Number
(define (date-to-sec date)
  (+ (* (date-year date) SECONDS-IN-YEAR)
     (* (date-month date) SECONDS-IN-MONTH)
     (* (date-day date) SECONDS-IN-DAY)
     (* (date-hour date) SECONDS-IN-HOUR)
     (* (date-minute date) SECONDS-IN-MINUTE)))

; Date Date -> Boolean
(check-expect (newer? (create-date 2014 11 2 0 0 0) (create-date 2014 12 1 0 0 0))
              #false)
(check-expect (newer? (create-date 2014 12 2 21 12 35) (create-date 2014 12 1 0 0 0))
              #true)
(define (newer? d1 d2)
  (> (date-to-sec d1) (date-to-sec d2)))

; String Date LTracks -> List-of-Strings
(check-expect (select-album-date "Toxicity" (create-date 2014 12 1 0 0 0) TRACKS-STUB)
              (list "Bounce" "Toxicity"))
(define (select-album-date title date tracks)
  (cond
    [(empty? tracks) '()]
    [(and (newer? (track-played (first tracks)) date)
          (string=? title
                    (track-album (first tracks)))) (cons (track-name (first tracks))
                                                         (select-album-date title date (rest tracks)))]
    [else (select-album-date title date (rest tracks))]))
 
; Exercise 204. Design select-albums. The function consumes an element of LTracks.
; It produces a list of LTracks, one per album.
; Each album is uniquely identified by its title and shows up in the result only once.

; LTracks -> List-of-Strings
; Function produces a list of albums
(check-expect (album-titles TRACKS-STUB)
              (list "Toxicity" "Hypnotize"))
(define (album-titles tracks)
  (create-set (select-all-album-titles tracks)))

; LTracks Strings -> LTracks
(check-expect (select-album-tracks "Toxicity" TRACKS-STUB)
              (list (create-track "Chop Suey!" "SOAD" "Toxicity" 180000 1 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 11 13 21 12 35))
                    (create-track "Bounce" "SOAD" "Toxicity" 240000 2 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 12 20 21 16 59))
                    (create-track "Toxicity" "SOAD" "Toxicity" 240000 3 (create-date 2001 9 1 14 31 25) 6 (create-date 2014 12 20 21 20 11))))
(check-expect (select-album-tracks "Hypnotize" TRACKS-STUB)
              (list (create-track "Attack" "SOAD" "Hypnotize" 180000 3 (create-date 2004 22 11 14 31 25) 6 (create-date 2014 12 1 21 20 11))))
(define (select-album-tracks title tracks)
  (cond
    [(empty? tracks) '()]
    [(string=? title
               (track-album (first tracks))) (cons (first tracks)
                                                   (select-album-tracks title (rest tracks)))]
    [else (select-album-tracks title (rest tracks))]))

; LTracks List-of-Strings -> List-of-LTracks
(check-expect (album-tracks TRACKS-STUB (album-titles TRACKS-STUB))
              (list (cons "Toxicity" (list (create-track "Chop Suey!" "SOAD" "Toxicity" 180000 1 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 11 13 21 12 35))
                                           (create-track "Bounce" "SOAD" "Toxicity" 240000 2 (create-date 2001 9 4 14 31 25) 6 (create-date 2014 12 20 21 16 59))
                                           (create-track "Toxicity" "SOAD" "Toxicity" 240000 3 (create-date 2001 9 1 14 31 25) 6 (create-date 2014 12 20 21 20 11))))
                    (cons "Hypnotize" (list (create-track "Attack" "SOAD" "Hypnotize" 180000 3 (create-date 2004 22 11 14 31 25) 6 (create-date 2014 12 1 21 20 11))))))
(define (album-tracks tracks albums)
  (cond
    [(empty? albums) '()]
    [else (cons (cons (first albums)
                      (select-album-tracks (first albums) tracks))
                (album-tracks tracks (rest albums)))]))

; LTracks -> List-of-LTracks
(define (select-albums tracks)
  (album-tracks tracks (album-titles tracks)))

; Part II

; An LLists is one of:
; – '()
; – (cons LAssoc LLists)
 
; An LAssoc is one of: 
; – '()
; – (cons Association LAssoc)
; 
; An Association is a list of two items: 
;   (cons String (cons BSDN '()))
 
; A BSDN is one of: 
; – Boolean
; – Number
; – String
; – Date

; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))

; Exercise 205. Develop examples of LAssoc and LLists, that is, the list representation of tracks and lists of such tracks.
; Association
(define racket-article-1 (cons "Title" (cons "Why I learned Racket?" '())))
(define racket-article-2 (cons "Title" (cons "Racket prose and cons" '())))
(define ts-article-1 (cons "Title" (cons "TypeScript in 2022" '())))
(define ts-article-2 (cons "Title" (cons "TypeScript prose and cons" '())))
 
; LAssoc
(define racket-articles (cons racket-article-1 (cons racket-article-2 '())))
(define ts-articles (cons ts-article-1 (cons ts-article-2 '())))

; LLists
(define blog (cons racket-articles (cons ts-articles '())))

(define LLIST-STUB (list (list
                          (list "Album" "Toxicity")
                          (list "Artist" "SOAD")
                          (list "Name" "Toxicity")
                          (list "Total Time" 240000))
                         (list
                          (list "Album" "Toxicity")
                          (list "Artist" "SOAD")
                          (list "Name" "Bounce")
                          (list "Total Time" 240000))
                         ))

; Exercise 206. Design the function find-association.
; It consumes three arguments: a String called key, an LAssoc, and an element of Any called default.
; It produces the first Association whose first item is equal to key, or default if there is no such Association.
; String LAssoc Any -> Association | Any
(check-expect (find-association "Title" racket-articles "Not found")
              racket-article-1)
(check-expect (find-association "Unknown" racket-articles "Not found")
              "Not found")
(define (find-association key la default)
  (cond
    [(empty? la) default]
    [(string=? key (first (first la))) (first la)]
    [else (find-association key (rest la) default)]))

; Exercise 207. Design total-time/list, which consumes an LLists and produces the total amount of play time.

; LLists -> Number
(check-expect (total-time/list LLIST-STUB)
              480000)
(check-expect (total-time/list '())
              0)
(define (total-time/list ll)
  (cond
    [(empty? ll) 0]
    [else (+
           (second (assoc "Total Time" (first ll)))
           (total-time/list (rest ll)))]))

; Exercise 208. Design boolean-attributes.
; The function consumes an LLists and produces the Strings that are associated with a Boolean attribute.
; LLists -> List-of-strings
(check-expect (boolean-attributes (list (list (list "active" #t)
                                              (list "title" "Task"))))
              (list "active"))
(define (boolean-attributes ll)
  (cond
    [(empty? ll) '()]
    [else (create-set (append (lassoc-bool-attrs (first ll))
                              (boolean-attributes (rest ll))))]))

; LAssoc -> List-of-strings
(define (lassoc-bool-attrs lassoc)
  (cond
    [(empty? lassoc) '()]
    [(boolean? (second (first lassoc))) (cons (first (first lassoc))
                                              (lassoc-bool-attrs (rest lassoc)))]
    [else (lassoc-bool-attrs (rest lassoc))]))