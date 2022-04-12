;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname worm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Worm — also known as Snake — is one of the oldest computer games

;; =================
;; Constants:

(define WORLD-WIDTH 200)
(define WORLD-HEIGHT 200)
(define SPEED-IN-SECONDS 0.5)

(define MTS (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define RADIUS 5)
(define PADDING 5)
(define WORM-IMG (circle RADIUS "solid" "red"))
(define FOOD-IMG (circle RADIUS "solid" "green"))
  
;; =================
;; Data definitions:

(define-struct worm-part (x-pos y-pos))
;; WormPart is (make-worm Number Number)
;; interp. the worm part at position x, y

(define-struct food (x-pos y-pos))
;; Food is (make-food Number Number)
;; interp. the food at position x, y

(define-struct worm (parts food direction))
;; Worm is (make-worm List-of-WormPart Food String)
;; interp. the worm store worm parts, food and worm direction

;; =================
;; Functions:

;; Number -> Number
;; add fixed padding to position coordinate
(define (add-padding pos)
  (+ pos PADDING))

;; List-of-WormPart -> WormPart
(check-expect (last-worm-part (cons (make-worm-part 0 0) (cons (make-worm-part 0 10) empty)))
              (make-worm-part 0 10))
(define (last-worm-part low)
  (cond
    [(empty? (rest low)) (first low)]
    [else (last-worm-part (rest low))]))

;; List-of-WormPart direction -> List-of-WormPart
;; update worm parts position coordinates base on direction
(define (move-worm-parts low direction)
  (cond
    [(empty? low) empty]
    [(string=? direction "up") (append (rest low) (cons (make-worm-part (worm-part-x-pos (last-worm-part low))
                                                                         (- (worm-part-y-pos (last-worm-part low)) 10)) empty))]
    [(string=? direction "down") (append (rest low) (cons (make-worm-part (worm-part-x-pos (last-worm-part low))
                                                                          (+ (worm-part-y-pos (last-worm-part low)) 10)) empty))]
    [(string=? direction "right") (append (rest low) (cons (make-worm-part (+ (worm-part-x-pos (last-worm-part low)) 10)
                                                                           (worm-part-y-pos (last-worm-part low))) empty))]
    [(string=? direction "left") (append (rest low) (cons (make-worm-part (- (worm-part-x-pos (last-worm-part low)) 10)
                                                                          (worm-part-y-pos (last-worm-part low))) empty))]
    [else low]))

;; Worm -> Worm
;; update worm parts and food position
(define (tock w)
  (cond
    [(and (= (food-x-pos (worm-food w)) (worm-part-x-pos (last-worm-part (worm-parts w))))
          (= (food-y-pos (worm-food w)) (worm-part-y-pos (last-worm-part (worm-parts w)))))
     (make-worm (move-worm-parts (append (cons (make-worm-part (food-x-pos (worm-food w))
                                                               (food-y-pos (worm-food w))) empty)
                                         (worm-parts w))
                                 (worm-direction w))
                (make-food 70 70)
                (worm-direction w))]
    [else (make-worm (move-worm-parts (worm-parts w) (worm-direction w))
                     (worm-food w)
                     (worm-direction w))]))

;; Worm -> Image
;; render the worm parts and food 
(define (render w)
  (place-image FOOD-IMG
               (add-padding (food-x-pos (worm-food w)))
               (add-padding (food-y-pos (worm-food w)))
               (render-worm (worm-parts w))))

;; List-of-WormPart -> Image
;; return worm parts images
(check-expect (render-worm empty) MTS)
(check-expect (render-worm (cons (make-worm-part 0 0) (cons (make-worm-part 0 10) empty)))
              (place-image WORM-IMG 5 5 (place-image WORM-IMG 5 15 MTS)))
(define (render-worm low)
  (cond
    [(empty? low) MTS]
    [else (place-image WORM-IMG
                       (add-padding (worm-part-x-pos (first low)))
                       (add-padding (worm-part-y-pos (first low)))
                       (render-worm (rest low)))]))

;; Worm KeyEvent -> Worm
;; !!!
(define (handle-key w ke)
  (cond
    [(or (string=? (worm-direction w) "up") (string=? (worm-direction w) "down"))
     (cond
       [(key=? ke "right") (make-worm (worm-parts w) (worm-food w) "right")]
       [(key=? ke "left") (make-worm (worm-parts w) (worm-food w) "left")]
       [else w])
     ]
    [(or (string=? (worm-direction w) "left") (string=? (worm-direction w) "right"))
     (cond
       [(key=? ke "up") (make-worm (worm-parts w) (worm-food w) "up")]
       [(key=? ke "down") (make-worm (worm-parts w) (worm-food w) "down")]
       [else w])
     ]
    [else w]))

;; Worm -> Boolean
;; !!!
(define (stop-game? w)
  (or (> (worm-part-y-pos (last-worm-part (worm-parts w))) WORLD-HEIGHT)
      (< (worm-part-y-pos (last-worm-part (worm-parts w))) 0)
      (> (worm-part-x-pos (last-worm-part (worm-parts w))) WORLD-WIDTH)
      (< (worm-part-x-pos (last-worm-part (worm-parts w))) 0)))

;; Worm -> Worm
(define (main w)
  (big-bang w
    (on-tick tock SPEED-IN-SECONDS)
    (to-draw render)
    (on-key handle-key)
    (stop-when stop-game?)))

(define WORM-1 (make-worm (cons (make-worm-part 0 0) (cons (make-worm-part 0 10) (cons (make-worm-part 0 20) empty))) (make-food 50 50) "down"))
(main WORM-1)