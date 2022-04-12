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

(define MTS (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define WORM-IMG (square 10 "solid" "red"))
(define FOOD-IMG (square 10 "solid" "green"))
  
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

;; List-of-WormPart -> WormPart
(check-expect (last-worm-part (cons (make-worm-part 0 0) (cons (make-worm-part 0 10) empty)))
              (make-worm-part 0 10))
(define (last-worm-part low)
  (cond
    [(empty? (rest low)) (first low)]
    [else (last-worm-part (rest low))]))

;; List-of-WormPart -> Food
;; !!!
(define (random-food low)
  (make-food 70 70))

;; List-of-WormPart direction -> List-of-WormPart
;; update worm parts position coordinates base on direction
(check-expect (move-worm (cons (make-worm-part 0 0) (cons (make-worm-part 0 10) (cons (make-worm-part 0 20) empty))) "right")
              (cons (make-worm-part 0 10) (cons (make-worm-part 0 20) (cons (make-worm-part 10 20) empty))))

(check-expect (move-worm (cons (make-worm-part 10 0) (cons (make-worm-part 10 10) (cons (make-worm-part 10 20) empty))) "left")
              (cons (make-worm-part 10 10) (cons (make-worm-part 10 20) (cons (make-worm-part 0 20) empty))))

(check-expect (move-worm (cons (make-worm-part 0 0) (cons (make-worm-part 10 0) (cons (make-worm-part 20 0) empty))) "down")
              (cons (make-worm-part 10 0) (cons (make-worm-part 20 0) (cons (make-worm-part 20 10) empty))))

(define (move-worm low direction)
  (local (
          ;; WormPart -> WormPart
          (define (change-y-pos wp dx)
            (make-worm-part (worm-part-x-pos wp) (dx (worm-part-y-pos wp) 10)))

          ;; WormPart -> WormPart
          (define (change-x-pos wp dx)
            (make-worm-part (dx (worm-part-x-pos wp) 10) (worm-part-y-pos wp)))

          ; String List-of-WormPart -> List-of-WormPart
          (define (updated-worms direction low)

            (local ((define WORM-HEAD (last-worm-part low)))
              (cond
                [(string=? direction "up") (change-y-pos WORM-HEAD -)]
                [(string=? direction "down") (change-y-pos WORM-HEAD +)]
                [(string=? direction "right") (change-x-pos WORM-HEAD +)]
                [(string=? direction "left") (change-x-pos WORM-HEAD -)]
              [else low]))))
    (cond
      [(empty? low) empty]
      [else (append (rest low)
                    (cons (updated-worms direction low) empty))])))

;; Food List-of-WormPart -> Boolean
;; return true if worm is eat food
(define (can-eat? food low)
  (and (= (food-x-pos food) (worm-part-x-pos (last-worm-part low)))
       (= (food-y-pos food) (worm-part-y-pos (last-worm-part low)))))

;; Food -> WormPart
;; return WormPart that transformed from Food
(define (eat food)
  (make-worm-part (food-x-pos food) (food-y-pos food)))

;; Worm -> Worm
;; update worm parts and food position
(define (tock w)
  (cond
    [(can-eat? (worm-food w) (worm-parts w))
     (make-worm (move-worm (append (cons (eat (worm-food w)) empty) (worm-parts w)) (worm-direction w))
                (random-food (worm-parts w))
                (worm-direction w))]
    [else
     (make-worm (move-worm (worm-parts w) (worm-direction w))
                (worm-food w)
                (worm-direction w))]))

;; Worm -> Image
;; render the worm parts and food 
(define (render w)
  (place-image FOOD-IMG
               (+ 5 (food-x-pos (worm-food w)))
               (+ 5 (food-y-pos (worm-food w)))
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
                       (+ 5 (worm-part-x-pos (first low)))
                       (+ 5 (worm-part-y-pos (first low)))
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
    (on-tick tock 0.1)
    (on-key handle-key)
    (to-draw render)
    )) ; (stop-when stop-game?)

(define WORM-1 (make-worm (cons (make-worm-part 0 0) (cons (make-worm-part 0 10) empty)) (make-food 50 50) "down"))
(main WORM-1)