#lang racket

;; Another simple test: this one simulates a Roomba robot.
;; When the robot hits the walls, it backtracks a bit and rotates, then it starts again

(require "../MirtoEmulatorGui.rkt")

(define leftwheel 0)
(define rightwheel 1)

(define  sensorsLoop (lambda  ()
                       (cond ( (leftBump?)  (setMotors -100 -100)
                                            (sleep 0.5)
                                            (setMotor leftwheel 100)
                                            (setMotor rightwheel -100)
                                            (sleep 2)
                                            (setMotors 100 100)
                                            )
                             ( (rightBump?)(setMotors -100 -100)
                                            (sleep 0.5)
                                            (setMotor leftwheel -100)
                                            (setMotor rightwheel 100)
                                            (sleep 2)
                                            (setMotors 100 100)
                                            )

                             )
                       (sleep 0.5)
                       (cond [
                              ; (not (digital-read 5))
                              (< (analog-read 7) 500)
                                 (sensorsLoop)]
                             )
                       ))

(define test (lambda ()
                         (open-asip)
                         (setMotor leftwheel 200)
                         (setMotor rightwheel 200)
                         (enableBumpers 100)
                         (sensorsLoop)
                         (close-asip)))
(test)
