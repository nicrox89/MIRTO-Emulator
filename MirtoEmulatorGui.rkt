#lang racket/gui
(require math/matrix)
(require 2htdp/image)
(require images/flomap)
(require picturing-programs)

(define mouse_x 0)
(define mouse_y 0)

(define bumpDelta 21)


(define x 80)
(define y 300)
(define z 0)
(define cosz 0)
(define sinz 0)
(define delta 0)
(define power 0)

(define rightWheelPwr 150)
(define leftWheelPwr 150)

(define right #f)
(define left #f)

;sensor struct
(struct point (x y intx inty black)#:mutable)
(struct line (x1 y1 x2 y2)#:mutable)
(struct destination (x y)#:mutable)

;sensors
(define ir0 (point 0 0 0 0 #f))
(define ir1 (point 0 0 0 0 #f))
(define ir2 (point 0 0 0 0 #f))

;euclidean vector
(define direction (destination 0 0))

;wheels
(define leftWheel (line 0 0 0 0))
(define rightWheel (line 0 0 0 0))


(define bg_img (make-object bitmap% "bg.png"))

(define WIDTH (image-width bg_img))
(define HEIGHT (image-height bg_img))

; Convert image to list of colors
(define list_of_colors (image->color-list bg_img))

; Convert list of colors to list of true/false (true if black, only checking 1 colour)
(define simple_list (map (λ (x) (not (= (color-red x) 255))) list_of_colors))

; utility function that returns the list of positions that are #t (i.e., black)
(define (indexes-of-black l)
  (for/list ((i l)
             (n (in-naturals))
             #:when (equal? i #t))
    n))
; The list of positions that are black. Each number is row*width + column
(define blacks (indexes-of-black simple_list))


(define (position) 
  (set! delta (* 0.0001 (- rightWheelPwr leftWheelPwr)))
  (set! power (* 0.01 (/ ( + leftWheelPwr rightWheelPwr) 2)))
  ;(set! power (* 0.01 (max leftWheel rightWheel)))
  (set! z (+ z delta))
  (set! cosz (cos z))
  (set! sinz (sin z))
  (set! x (+ x (* cosz power)))
  (set! y (+ y (* -1 sinz power))) ; negative becuse the images have the y positive in down direction

  ;Infrared
  (set-point-x! ir0 (+ x (* 18 (cos (+ z 0.2)))))
  (set-point-y! ir0 (+ y (* -1 18 (sin (+ z 0.2)))))
  (set-point-intx! ir0 (exact-round (point-x ir0)))
  (set-point-inty! ir0 (exact-round (point-y ir0)))
  
  (set-point-x! ir1 (+ x (* 18 cosz)))
  (set-point-y! ir1 (+ y (* -1 18 sinz)))
  (set-point-intx! ir1 (exact-round (point-x ir1)))
  (set-point-inty! ir1 (exact-round (point-y ir1)))
  
  (set-point-x! ir2 (+ x (* 18 (cos (- z 0.2)))))
  (set-point-y! ir2 (+ y (* -1 18 (sin (- z 0.2)))))
  (set-point-intx! ir2 (exact-round (point-x ir2)))
  (set-point-inty! ir2 (exact-round (point-y ir2)))

  ;color extraction - too slow
  (set-point-black! ir0 (not (eq? #f (member (+ (* HEIGHT (point-inty ir0)) (point-intx ir0)) blacks))))
  (set-point-black! ir1 (not (eq? #f (member (+ (* HEIGHT (point-inty ir1)) (point-intx ir1)) blacks))))
  (set-point-black! ir2 (not (eq? #f  (member (+ (* HEIGHT (point-inty ir2)) (point-intx ir2)) blacks))))
  
;  (set-point-black! ir0 (color=? (get-pixel-color (point-intx ir0) (point-inty ir0) bg_img) 'Black))
;  (set-point-black! ir1 (color=? (get-pixel-color (point-intx ir1) (point-inty ir1) bg_img) 'Black))
;  (set-point-black! ir2 (color=? (get-pixel-color (point-intx ir2) (point-inty ir2) bg_img) 'Black))


  ;euclidean vector
  (set-destination-x! direction (+ x (* power 20 cosz)))
  (set-destination-y! direction (+ y (* -1 power 20 sinz)))

  ;left wheel
  (set-line-x1! leftWheel (+ x (* 15 (cos (+ z (/ pi 2) 0.2)))))
  (set-line-y1! leftWheel (+ y (* -1 15 (sin (+ z (/ pi 2) 0.2)))))
  (set-line-x2! leftWheel (+ x (* 15 (cos (+ z (/ pi 2) -0.2)))))
  (set-line-y2! leftWheel (+ y (* -1 15 (sin (+ z (/ pi 2) -0.2)))))
  ;right wheel
  (set-line-x1! rightWheel (+ x (* 15 (cos (- z (/ pi 2) 0.2)))))
  (set-line-y1! rightWheel (+ y (* -1 15 (sin (- z (/ pi 2) 0.2)))))
  (set-line-x2! rightWheel (+ x (* 15 (cos (- z (/ pi 2) -0.2)))))
  (set-line-y2! rightWheel (+ y (* -1 15 (sin (- z (/ pi 2) -0.2)))))
  

  ;print ir values
  ; (printf "IR0=~s IR1=~s IR2=~s\n" (point-black ir0) (point-black ir1) (point-black ir2))
)
                       


(define frame (new
               (class frame%
                 (super-new [label "Frame"] [width 500] [height 500])
                 (define/augment (on-close) (printf "closed window") (close-asip))
                 )
               )
  )




(define bot (new canvas%
                 [parent frame]
                 [paint-callback
                     (λ (c dc)
                       (send dc clear) ;erease
                       
                       (send dc draw-bitmap bg_img 0 0)
                   
                       
                       ;bumpers
                       (send dc set-pen "blue" 3 'solid)
                       (cond ( (equal? left #f)
                       (send dc draw-arc (- x 20) (- y 20) 40 40 (+ z 0.2) (+ z (/ pi 4))) ))
                       (cond ( (equal? right #f)
                       (send dc draw-arc (- x 20) (- y 20) 40 40 (- z (/ pi 4)) (- z 0.2)) ))
                       
                       ;base
                       (send dc set-pen "red" 36 'solid)
                       (send dc draw-point x y)

                       ;wheels
                       (send dc set-pen "black" 6 'solid)
                       (send dc draw-line (line-x1 leftWheel) (line-y1 leftWheel) (line-x2 leftWheel) (line-y2 leftWheel)) ; left wheel
                       (send dc draw-line (line-x1 rightWheel) (line-y1 rightWheel) (line-x2 rightWheel) (line-y2 rightWheel)) ; right wheel

                       ;sensors position
                       
                       ;IR
                       (send dc set-pen "black" 2 'solid)
                       (send dc draw-point (point-x ir0) (point-y ir0)) ; left
                       (send dc set-pen "orange" 2 'solid)
                       (send dc draw-point (point-x ir1) (point-y ir1)) ; center
                       (send dc set-pen "blue" 2 'solid)
                       (send dc draw-point (point-x ir2) (point-y ir2)) ; right
                       
                       ;direction euclidean vector
                       (send dc set-pen "black" 2 'solid)
                       (send dc draw-line x y (destination-x direction) (destination-y direction))

                       
                       ;external border
                       (send dc set-pen "black" 4 'solid)
                       (send dc draw-line 6 6 6 460)
                       (send dc draw-line 6 6 460 6)
                       (send dc draw-line 6 460 460 460)
                       (send dc draw-line 460 6 460 460)

                       )]
                 ))



(define (loop)
  
  (cond (
         (and
          (> x (+ 6 bumpDelta)) (> y (+ 6 bumpDelta)) (< x (- 461 bumpDelta)) (< y (- 461 bumpDelta))
          #t ;(not (and
           ;(and (> x (- 200 bumpDelta)) (> y (- 80 bumpDelta)) (< x (+ 230 bumpDelta)) (< y (+ 230 bumpDelta)))
           ;)
           ;)
          )
         ;(set! left #f) (set! right #f)
         (send bot on-paint)
         (position)

         ;(printf "z:~s  cos(z):~s  sin(z):~s  x:~s  y:~s pwr:~s\n" z cosz sinz x y power)
         )
        )
  
  
  ;(set! x (remainder (+ x 1) 500))
  (sleep/yield 0.001) ; ex 0.01
  (loop)
  )

(define open-asip
  (lambda ()
    (send frame show #t)
    ;(send bg on-paint)
    (loop)
    )
  )

(define close-asip
  (lambda ()
    (exit #t)
    )
  )