#lang racket

(require "MirtoEmulatorGui.rkt")

(define leftwheel 0)
(define rightwheel 1)

(define  findLine (lambda  ()
                       (cond
                         [(< (getIR 0) 200)
                          (sleep 0.05)
                          (findLine)
                          ]
                        )
))

(define rotateLeftToLine (λ ()
                           (cond
                             [(< (getIR 1) 200)
                              (sleep 0.05)
                              (rotateLeftToLine)
                              ]
                             )
                           )
  )

(define currentlyTurning #f)
(define followLine (λ ()
                     (cond
                       [(> (getIR 1) 200)
                        (cond [currentlyTurning (setMotors 150 150) (set currentlyTurning #f)]
                              )
                        ]
                       [(> (getIR 0) 200)
                        (set! currentlyTurning #t)
                        (setMotors -100 100)
                        ]
                       [(> (getIR 2) 200)
                        (set! currentlyTurning #t)
                        (setMotors 100 -100)
                        ]
                       )
                     (followLine)
                     )
  )
                        
                       
(define test (lambda ()
               (open-asip)
               (enableIR 100)
               (setMotor leftwheel 200)
               (setMotor rightwheel 200)               
               (findLine)

               ;; Found the line. Move forward a little bit and then rotate left
               ;; until IR0 is on the line
               (stopMotors)
               (setMotor leftwheel 100)
               (setMotor rightwheel 100)
               (sleep 1.5)
               (stopMotors)
               (setMotors -100 100)
               (rotateLeftToLine)
               (stopMotors)
               (setMotors 150 150)
               (followLine)    
               (close-asip)))
(test)