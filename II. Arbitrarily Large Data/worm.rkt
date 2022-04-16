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
(define CELL-SIZE 10)

(define MTS (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define WORM-IMG (square CELL-SIZE "solid" "red"))
(define FOOD-IMG (square CELL-SIZE "solid" "green"))

;; =================
;; Data definitions:

(define-struct worm-part (x-pos y-pos))
;; WormPart is (make-worm Number Number)
;; interp. the worm part at position x, y

(define-struct food (x-pos y-pos))
;; Food is (make-food Number Number)
;; interp. the food at position x, y

(define-struct cell (x-pos y-pos))
;; EmptyCell is (make-cell Number Number)
;; interp. the empty cell mean cell that doesn't contain form part or food

(define-struct worm (parts food direction))
;; Worm is (make-worm List-of-WormPart Food String)
;; interp. the worm store worm parts, food and worm direction

;; =================
;; Functions:

;; List-of-Any -> Any | #f
(check-expect (index-of '(1 2 3 4) 0) 1)
(check-expect (index-of '(1 2 3 4) 2) 3)
(check-expect (index-of '(1 2 3 4) 5) #f)
(define (index-of lst target)
  (local (
          (define (fn-for-index-of lst index)
            (cond
              [(empty? lst) #f]
              [(= index target) (first lst)]
              [else (fn-for-index-of (rest lst) (+ index 1))]))
          )
    (fn-for-index-of lst 0)))

;; List-of-WormPart -> WormPart
(check-expect (worm-head (cons (make-worm-part 0 0) (cons (make-worm-part 0 10) empty)))
              (make-worm-part 0 10))
(define (worm-head lowp)
  (cond
    [(empty? (rest lowp)) (first lowp)]
    [else (worm-head (rest lowp))]))

;; List-of-WormPart -> List-of-WormPart
(check-expect (worm-tail (cons (make-worm-part 0 0) empty)) empty)

(check-expect (worm-tail (cons (make-worm-part 0 0)
                               (cons (make-worm-part 0 10)
                                     (cons (make-worm-part 0 20) empty))))
              (cons (make-worm-part 0 0)
                    (cons (make-worm-part 0 10) empty)))

(define (worm-tail lowp)
  (if (empty? (rest lowp))
      empty
      (cons (first lowp) (worm-tail (rest lowp)))))

;; List-of-WormPart -> Food
;; generate new food in random position
(define (random-food lowp)
  (local (
          ;; List-of-WormPart Number Number -> Boolean
          ;; determine is this available position for food
          (define (empty-space? lowp x-pos y-pos)
            (cond
              [(empty? lowp) #t]
              [(and (= (worm-part-x-pos (first lowp)) x-pos)
                    (= (worm-part-y-pos (first lowp)) y-pos)) #f]
              [else (empty-space? (rest lowp) x-pos y-pos)]))

          ;; Number Number -> List-of-Numbers
          ;; generate sequence of positions
          (define (positions size step)
            (cond
              [(<= size 10) (cons 10 empty)]
              [else (cons size (positions (- size step) step))]))

          ;; List-of-Number -> Number
          (define (random-pos lop)
            (index-of lop (random (length lop))))

          (define STEP 10)
          (define RANDOM-X-POS (random-pos (positions (- WORLD-WIDTH STEP) STEP)))
          (define RANDOM-Y-POS (random-pos (positions (- WORLD-HEIGHT STEP) STEP))))

    (cond
      [(empty-space? lowp RANDOM-X-POS RANDOM-Y-POS) (make-food RANDOM-X-POS RANDOM-Y-POS)]
      [else (random-food lowp)])))

;; List-of-WormPart WormPart -> List-of-WormPart
;; concat new worm head and tail
(define (concat lowp wp)
  (append lowp (cons wp empty)))

;; List-of-WormPart direction -> List-of-WormPart
;; update worm parts position coordinates base on direction
(check-expect (move-worm (cons (make-worm-part 0 0) (cons (make-worm-part 0 10) (cons (make-worm-part 0 20) empty))) "right")
              (cons (make-worm-part 0 10) (cons (make-worm-part 0 20) (cons (make-worm-part 10 20) empty))))

(check-expect (move-worm (cons (make-worm-part 10 0) (cons (make-worm-part 10 10) (cons (make-worm-part 10 20) empty))) "left")
              (cons (make-worm-part 10 10) (cons (make-worm-part 10 20) (cons (make-worm-part 0 20) empty))))

(check-expect (move-worm (cons (make-worm-part 0 0) (cons (make-worm-part 10 0) (cons (make-worm-part 20 0) empty))) "down")
              (cons (make-worm-part 10 0) (cons (make-worm-part 20 0) (cons (make-worm-part 20 10) empty))))

(define (move-worm lowp direction)
  (local (
          ;; WormPart -> WormPart
          (define (change-y-pos wp dx)
            (make-worm-part (worm-part-x-pos wp) (dx (worm-part-y-pos wp) 10)))

          ;; WormPart -> WormPart
          (define (change-x-pos wp dx)
            (make-worm-part (dx (worm-part-x-pos wp) 10) (worm-part-y-pos wp)))

          ; List-of-WormPart KeyEvent -> List-of-WormPart
          (define (updated-worms lowp direction)

            (local ((define WORM-HEAD (worm-head lowp)))
              (cond
                [(string=? direction "up") (change-y-pos WORM-HEAD -)]
                [(string=? direction "down") (change-y-pos WORM-HEAD +)]
                [(string=? direction "right") (change-x-pos WORM-HEAD +)]
                [(string=? direction "left") (change-x-pos WORM-HEAD -)]
                [else lowp]))))
    (cond
      [(empty? lowp) empty]
      [else (concat (rest lowp) (updated-worms lowp direction))])))

;; Food WormPart -> Boolean
;; return true if worm is eat food
(define (can-eat? food wp)
  (and (= (food-x-pos food) (worm-part-x-pos wp))
       (= (food-y-pos food) (worm-part-y-pos wp))))

;; Food -> WormPart
;; return WormPart that transformed from Food
(define (eat food)
  (make-worm-part (food-x-pos food) (food-y-pos food)))

;; Worm -> Worm
;; update worm parts and food position
(define (tock w)
  (cond
    [(can-eat? (worm-food w) (worm-head (worm-parts w)))
     (make-worm (move-worm (concat (worm-parts w) (eat (worm-food w))) (worm-direction w))
                (random-food (concat (worm-parts w) (eat (worm-food w))))
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
;; render worm parts
(check-expect (render-worm empty) MTS)
(check-expect (render-worm (cons (make-worm-part 0 0) (cons (make-worm-part 0 10) empty)))
              (place-image WORM-IMG 5 5 (place-image WORM-IMG 5 15 MTS)))
(define (render-worm lowp)
  (cond
    [(empty? lowp) MTS]
    [else (place-image WORM-IMG
                       (+ 5 (worm-part-x-pos (first lowp)))
                       (+ 5 (worm-part-y-pos (first lowp)))
                       (render-worm (rest lowp)))]))

;; Worm KeyEvent -> Worm
(define (handle-key w ke)
  (cond
    ; by pressing "escape" the game will be started again
    [(key=? ke "escape") (make-worm (cons (make-worm-part 0 0) empty)
                                (random-food (cons (make-worm-part 0 0) empty))
                                "down")]
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
;; determine is head hit the wall
(define (hit-the-wall? w)
  (or (> (worm-part-y-pos (worm-head (worm-parts w))) (- WORLD-HEIGHT CELL-SIZE))
      (< (worm-part-y-pos (worm-head (worm-parts w))) 0)
      (> (worm-part-x-pos (worm-head (worm-parts w))) (- WORLD-WIDTH CELL-SIZE))
      (< (worm-part-x-pos (worm-head (worm-parts w))) 0)))

;; List-of-WormPart WormPart -> Boolean
;; determine is worm eat himself
(define (eat-himself? lowp wp)
  (local (
          (define EATEN-CELLS (filter (lambda (x) (and (= (worm-part-x-pos x) (worm-part-x-pos wp))
                                                      (= (worm-part-y-pos x) (worm-part-y-pos wp))))
                                     lowp))
          )
    (>= (length EATEN-CELLS) 1)))

;; Worm -> Boolean
;; the game should stop when head hit the wall, or eat himself
(define (stop-game? w)
  (cond
    [(hit-the-wall? w) #t]
    [(eat-himself? (worm-tail (worm-parts w)) (worm-head (worm-parts w))) #t]
    [else #f]))

(define (main w)
  (big-bang w
    (on-tick tock 0.1)
    (on-key handle-key)
    (to-draw render)
    (stop-when stop-game?)))

(main (make-worm (cons (make-worm-part 0 0) empty)
                 (random-food (cons (make-worm-part 0 0) empty))
                 "down"))