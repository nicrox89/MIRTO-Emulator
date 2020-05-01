#lang racket

(require "MirtoEmulatorGui.rkt")

(define leftwheel 0)
(define rightwheel 1)

(define  stopAtLine (lambda  ()
                       (cond
                         [(< (getIR 0) 200)
                          (sleep 0.05)
                          (stopAtLine)
                          (printf "~a ~a ~a\n" (getIR 0) (getIR 1) (getIR 2))]
                        )
))

                       
(define test (lambda ()
               (open-asip)
               (enableIR 100)
               (setMotor leftwheel 200)
               (setMotor rightwheel 200)               
               (stopAtLine)
               (stopMotors)
               (displayln "FOUND IT!!")
               (sleep 3)               
               (close-asip)))
(test)