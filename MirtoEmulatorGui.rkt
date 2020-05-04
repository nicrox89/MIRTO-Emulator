#lang racket/gui

;; *******************************
;; **** RACKET MIRTO EMULATOR ****
;; *******************************

(provide open-asip
         close-asip
         digital-read
         analog-read
         
         playTone ;play wav

         ;; Myrtle-specific functions
         w1-stopMotor
         w2-stopMotor
         stopMotors
         setMotor
         setMotors
         getCount
         resetCount
         getIR
         leftBump?
         rightBump?
         enableIR
         enableBumpers
         enableCounters
         setLCDMessage
         clearLCD
         ;enableDistance
         ;getDistance
         )


(require picturing-programs)

;GUI Threads
(define gui-thread null)

;Mouse position - for bot position
(define mouse-x 0)
(define mouse-y 0)

;Initial potentiometer position
(define slider-x 30)

;distance with the centroid
(define bumpDelta 18)

;initial position and direction in radiants
(define x 80)
(define y 300)
(define z 0)

;rotation
(define cosz 0)
(define sinz 0)

;power
(define delta 0) ;balance left-right power
(define power 0) ;global direction power

(define rightWheelPwr 0) ;right motor power
(define leftWheelPwr 0) ;left motor power

;bumpers
(define bumpersInterval 0) ;0 means disabled
(define right #f)
(define left #f)


(struct point (x y intx inty black)#:mutable)
(struct line (x1 y1 x2 y2)#:mutable)
(struct destination (x y)#:mutable)

;IR sensors
(define ir0 (point 0 0 0 0 #f))
(define ir1 (point 0 0 0 0 #f))
(define ir2 (point 0 0 0 0 #f))

(define irInterval 0) ;0 means disabled

;onboard button
(define button #f)

;onboard potentiometer
(define potentiometer 0)

;Counters
(define leftCounter 0)
(define rightCounter 0)

(define countersInterval 0) ;0 means disabled

;euclidean test vector
(define direction (destination x y))

;wheels
(define leftWheel (line 0 0 0 0))
(define rightWheel (line 0 0 0 0))

;display text vector
(define displayLines (make-vector 5))

;background image
(define bg_img (make-object bitmap% "bg.png"))

(define WIDTH (image-width bg_img))
(define HEIGHT (image-height bg_img))
(define TOOLSWIDTH 200)

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


;; Main function that compute the bot position
(define (position) 
  (set! delta (* 0.0001 (- rightWheelPwr leftWheelPwr)))
  (set! power (* 0.01 (/ ( + leftWheelPwr rightWheelPwr) 2)))
  ;(set! power (* 0.01 (max leftWheel rightWheel)))
  (set! z (+ z delta))
  (set! cosz (cos z))
  (set! sinz (sin z))

  ;counters
  (cond ( (> countersInterval 0)
          (set! leftCounter (- leftCounter (* 10 leftWheelPwr)))
          (set! rightCounter (+ rightCounter (* 10 rightWheelPwr)))))

  (define tempX (+ x (* cosz power)))
  (define tempY (+ y (* -1 sinz power)))
  
  (cond (
         (and
         ;center of the bot inside the box
         (> tempX bumpDelta) (> tempY bumpDelta) (< tempX (- WIDTH bumpDelta)) (< tempY (- HEIGHT bumpDelta))
         ;internal direction of the bot
         
          )
         (set! x tempX)
         (set! y tempY)
         (set! right #f) (set! left #f)            
         )
        (else
         ;only if the direction is backward
         (set! right #t) (set! left #t)
         )
        )
  

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

  ;color extraction
  (set-point-black! ir0 (not (eq? #f (member (+ (* HEIGHT (point-inty ir0)) (point-intx ir0)) blacks))))
  (set-point-black! ir1 (not (eq? #f (member (+ (* HEIGHT (point-inty ir1)) (point-intx ir1)) blacks))))
  (set-point-black! ir2 (not (eq? #f  (member (+ (* HEIGHT (point-inty ir2)) (point-intx ir2)) blacks))))

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
  
)
                       
;; *******************************
;; ********** Windowing **********
;; *******************************

;;Bot frame
(define frame (let ([new-es (make-eventspace)])
                (parameterize ([current-eventspace new-es])
                  (new
                   (class frame%
                     (super-new [label "MIRTO Emulator"]
                                [style '(no-resize-border)]
                                [width (+ WIDTH TOOLSWIDTH)]
                                [height HEIGHT]
                                )
                     (define/augment (on-close) (println "closed window") (close-asip))
                     )
                   )
                  ))
  )


;;Panel container
(define mainPanel (new horizontal-panel%
                   [parent frame]
                   [min-width (+ WIDTH TOOLSWIDTH)]	 
                   [min-height HEIGHT]
                   )
  )

;;Bot moving area
(define leftPanel (new panel%
                   [parent mainPanel]
                   [min-width WIDTH]	 
                   [min-height HEIGHT]
                   )
  )

;;Right panel container
(define rightPanel (new vertical-panel%
                   [parent mainPanel]
                   [min-width TOOLSWIDTH]	 
                   [min-height HEIGHT]
                   )
  )

;;Controls panel
(define topRightPanel
  (new vertical-panel%
       [parent rightPanel]
       [min-width TOOLSWIDTH]	 
       [min-height (/ HEIGHT 2)]
       )
  )

;;Display panel
(define bottomRightPanel (new vertical-panel%
                   [parent rightPanel]
                   [min-width TOOLSWIDTH]	 
                   [min-height (/ HEIGHT 2)]
                   )
  )

;; ************* TEST NEW POTENTIOMETER *****************

;;Potentiometer
;(define slider (new slider% [label ""]
;                            [min-value 0]
;                            [max-value 1023]
;                            [parent topRightPanel]
;                            [init-value 0]
;                            [style '(horizontal)]
;                            [vert-margin 10]
;                            [horiz-margin 10]))


;;Onboard button
;(define onboardButton (new button% [parent topRightPanel]
;                                   [label "onboard button"]
;                                   [callback (λ (c dc) (set! button #t))]))

(define slider-cb (lambda (e)
                    (define x (send e get-x))
                    (cond [ (and (> x 30) (< x 170))
                            (set! slider-x x)
                            (set! potentiometer (exact-round (/ (* (- x 29) 1023) 141)))
                           ]
                          [ (< x 31)
                            (set! slider-x 30)
                            (set! potentiometer 0)
                            ]
                          [ (> x 169)
                            (set! slider-x 170)
                            (set! potentiometer 1023)]
                          )))

(define canvas-slider%
  (class canvas%
    (define/override (on-event e)
           (when (equal? (send e dragging?) #t)
             (slider-cb e)
             ))
    (super-new)))

;;Slider
(define sliderEmulator (new canvas-slider%
                 [parent topRightPanel]
                 [paint-callback
                  (λ (c dc)
                    (send dc erase)
                    (send dc set-pen "black" 2 'solid)
                    (send dc draw-rounded-rectangle 30 100 140 6 2)
                    (send dc set-pen "orange" 3 'solid)
                    (send dc draw-line 32 102 slider-x 102)
                    (send dc set-pen "blue" 10 'solid)
                    (send dc draw-point slider-x 102)
                    
                    )
                  
                  ]
                 [style '(transparent)]
                 )
  )



;; ************* TEST NEW BUTTON *****************

(define button-cb (λ () (cond [ (equal? button #t) (set! button #f)]
                              [ (equal? button #f) (set! button #t)])))

(define canvas-button%
  (class canvas%
    (define/override (on-event e)
           (when (equal? (send e get-event-type) 'left-down)
             (button-cb)
             ))
    (super-new)))

;;Button
(define buttonEmulator (new canvas-button%
                 [parent topRightPanel]
                 [paint-callback
                  (λ (c dc)
                    (send dc erase)
                    (cond [ (equal? button #t)
                            (send dc set-pen "blue" 5 'solid)
                            (send dc set-brush "blue" 'solid)
                            ]
                          [ (equal? button #f)
                            (send dc set-pen "red" 5 'solid)
                            (send dc set-brush "red" 'solid)
                            ])
                    (send dc draw-rounded-rectangle 30 50 140 30)
                    (send dc set-font (make-font #:size 18 #:family 'roman #:weight 'bold))
                    (send dc set-text-foreground "white")
                    (send dc draw-text "onboard button" 40 55)
                    )
                  
                  ]
                 [style '(transparent)]
                 )
  )




;;Bot Display
(define display (new canvas%
                 [parent bottomRightPanel]
                 [paint-callback
                  (λ (c dc)
                    (send dc erase)

                    (send dc set-font (make-font #:size 20 #:family 'roman
                                                 #:weight 'bold))
                    (send dc set-text-foreground "black")
                    (send dc draw-text "Display" 10 120)
                    
                    (send dc set-pen "blue" 3 'solid)
                    (send dc set-brush "blue" 'solid)
                    (send dc draw-rectangle
                          5 150   ; Top-left at (##, ##) inside the panel
                          190 90) ; ## pixels wide and ## pixels high
                    
                    (send dc set-pen "white" 1 'solid)
                    (send dc draw-rectangle
                          8 153   ; Top-left at (##, ##) inside the panel
                          183 84) ; ## pixels wide and ## pixels high
                    (send dc set-font (make-font #:size 16 #:family 'modern
                                                 #:weight 'bold))
                    (send dc set-text-foreground "white")
                    (send dc draw-text (vector-ref displayLines 0) 10 157) ;display line 0
                    (send dc draw-text (vector-ref displayLines 1) 10 172) ;display line 1
                    (send dc draw-text (vector-ref displayLines 2) 10 187) ;display line 2
                    (send dc draw-text (vector-ref displayLines 3) 10 202) ;display line 3
                    (send dc draw-text (vector-ref displayLines 4) 10 217) ;display line 4

                    )
                  ]
                 [style '(transparent)]
                 )
  )

;canvas for the bot
(define bot (new canvas%
                 [parent leftPanel]
                 [paint-callback
                     (λ (c dc)
                       (send dc clear) ;clear set the standard background, (alt erase)
                       
                       (send dc draw-bitmap bg_img 0 0) ; draw background with line
                   
                       ;draw bumpers
                       (send dc set-pen "red" 3 'solid)
                       ;(cond ( (equal? left #f) ; cond to emulate the bumper touch
                       (send dc draw-arc (- x 20) (- y 20) 40 40 (+ z 0.2) (+ z (/ pi 4)))
                       ;))
                       ;(cond ( (equal? right #f)
                       (send dc draw-arc (- x 20) (- y 20) 40 40 (- z (/ pi 4)) (- z 0.2))
                       ;))
                       
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


                       )]
              )
  )

;; the refresh function loop
;; compute positions and update the canvas
(define (loop)
  ;update status bar
  (send frame set-status-text
        (string-append "IR0: " (format "~a" (point-black ir0))
                       " IR1: " (format "~a" (point-black ir1))
                       " IR2: " (format "~a" (point-black ir2))
                       " leftBump: " (format "~a" left)
                       " rightBump: "(format "~a" right)
                       " LC: " (format "~a" leftCounter)
                       " RC: " (format "~a" rightCounter)
                       " button: " (format "~a" button)
                       " pot: " (format "~a" potentiometer)
                       ))
  (send bot refresh-now)
  (send display refresh-now)
  (send sliderEmulator refresh-now)
  (send buttonEmulator refresh-now)
  (position)
  (sleep/yield 0.05)
  (loop)
  )

;; loop function for thread
(define (read-hook)
  (printf "Read thread started ...")
  (loop))

;;racket-main function mapping

;open the GUI in a thread
(define open-asip
  (λ ()
    (clearLCD)
    (send frame create-status-line) 
    (send frame show #t)
    (set! gui-thread (thread (lambda ()  (read-hook))))
    )
  )

;close the GUI and the thread
(define close-asip
  (λ ()
    (when (not (null? gui-thread)) (println "Killing gui thread .... ") (kill-thread gui-thread))
    (exit #t)
    (println "closed")
    )
  )


;analog read - only pin 7 for potentiometer
(define analog-read
  (λ (pin)
    (cond ( (equal? pin 7) potentiometer ) (0))
    )
  )

;digital read - only pin 5 for button
(define digital-read
  (λ (pin)
    (cond ( (equal? pin 5) (cond [(equal? button #t) 1][else 0] )) (else 0))
    )
  )



;; Stopping the motor with utility functions
(define w1-stopMotor
  (λ () (setMotor 0 0))
  )
(define w2-stopMotor
  (λ () (setMotor 1 0))
  )
(define stopMotors
  (λ ()
    (setMotor 0 0)
    (setMotor 1 0)
    )
  )


;; Set both motors power at the same time
(define setMotors
  (λ (s1 s2)
    (setMotor 0 s1)
    (setMotor 1 s2)
    )
  )

;; Set the power of the specified motor
(define setMotor
  (λ (m s)
    (cond ( (equal? m 0) (set! leftWheelPwr s))
          ( (equal? m 1) (set! rightWheelPwr s)))
    )
  )


;; Boolean functions for bump sensors
(define rightBump?
  (λ () (cond ( (> bumpersInterval 0 ) right) (else #f)))
  )
(define leftBump?
  (λ () (cond ( (> bumpersInterval 0 ) left) (else #f)))
  )

;; Enabling the bumpers
(define enableBumpers
  (λ (interval)
    (set! bumpersInterval interval)
    )
  )

;; Enabling the infrared sensors
(define enableIR
  (λ (interval)
    (set! irInterval interval)
    )
  )

; Return 20 if on black, 900 if on white, 0 otherwise
(define getIR
  (λ (num)
    (cond [(> irInterval 0)
           (cond
             [(= num 0)
              (cond [(point-black ir0) 900]
                    [else 20]
                    )
              ]
             [(= num 1)
              (cond [(point-black ir1) 900]
                    [else 20]
                    )
              ]
             [(= num 2)
              (cond [(point-black ir2) 900]
                    [else 20]
                    )
              ]
             [else 0]
             )
           ]
          [else 0]
          )           
    )
  )

;enable counters
(define enableCounters
  (λ (interval)
    (set! countersInterval interval)
    )
  )

;reset motor's num counter
(define resetCount
  (λ (num)
    (cond ( (equal? num 0) (set! leftCounter 0))
          ( (equal? num 1) (set! rightCounter 0)))
    )
  )


;get the motor num counter
(define getCount
  (λ (num)
    (cond ( (equal? num 0) leftCounter)
          ( (equal? num 1) rightCounter))
    )
  )

;set LCD Lines
(define setLCDMessage
  (λ (m r) ;; m = message to be displayed; r = row (max 5 rows)
    (vector-set! displayLines r
                 (cond
                   ((> (string-length m) 18)
                    (substring m 0 17)
                   )
                   (else m))
     )
    )
  )

;clear LCD Lines
(define clearLCD
  (λ ()
    (for [(i 5)] (vector-set! displayLines i ""))
   )
 )


;playTone
(define playTone
  (λ (t d) ;; t in Hz, d in ms is the duration
    (play-sound "beep.wav" #f)
    (sleep (/ d 1000)) ;maybe not necessary
   )
 )