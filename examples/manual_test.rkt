#lang racket

;; This is a simple test program to test the functionality of all the components

;;CMD CTRL UP

(require "../MirtoEmulatorGUI.rkt")

; display 18 x 5
(define stdDisplayText (lambda ()
     (clearLCD)
     (sleep 1)
     (setLCDMessage "ASIP Ver 1.1" 0)
     (setLCDMessage "Mirtle2017" 1)
     (setLCDMessage "Teesy3.1" 2)
     (setLCDMessage "" 3)                    
     (setLCDMessage "LCD NOT ORIGINAL" 4)
)
)

; function to test motors
; return true or false
(define motorTest (lambda () 
                    
  (enableCounters 1)
  (define left-steps 0) (define right-steps 0)
  (define left-ok #f) (define right-ok #f)
                    
  ;; 0 = left
  ;; 1 = right

  (printf "Left motor\n") (setMotor 0 50) (sleep 3) (stopMotors)
  ;record counters
  (set left-steps (getCount 0)) (set left-steps (getCount 1))
  (sleep 1)
         
  (printf "Right motor\n") (setMotor 1 50) (sleep 3) (stopMotors)
  ;record counters
  (set left-steps (getCount 0)) (set left-steps (getCount 1))                 
  (sleep 1)

  (printf "F F\n") (setMotors 50 50) (sleep 3) (stopMotors)
  ;record counters
  (set left-steps (getCount 0)) (set left-steps (getCount 1))              
  (sleep 1)

  ;; turn right
  (printf "F B\n") (setMotor 0 50) (setMotor 1 -50) (sleep 3) (stopMotors)
  ;record counters
  (set left-steps (getCount 0)) (set left-steps (getCount 1))

  ;; turn left
  (printf "B F\n") (setMotor 0 -50) (setMotor 1 50) (sleep 3) (stopMotors)
  ;record counters
  (set left-steps (getCount 0)) (set left-steps (getCount 1))

  (cond ( (and (> left-steps -20) (< left-steps 20) ) (set! left-ok #t)))
  (cond ( (and (> right-steps -20) (< right-steps 20) ) (set! right-ok #t)))
  (list left-ok right-ok)
 )
)

; function to test bumpers
; return true or false
(define sensorsLoopTen (lambda ()
 (enableBumpers 100)
                         
 (define left #f)
 (define right #f)
 (define both #f)
                         
 (for ([i 6])
   (cond ( (leftBump?) (printf "\nLEFT bump pressed\n") (set! left #t)))
   (cond ( (rightBump?) (printf "\nRIGHT bump pressed\n")  (set! right #t)))
   (sleep 1)
 )

  (cond ( (and left right) (printf "\nBumps works correctly\n\n") (set! both #t))
        ( (printf "\nBumps problem left: ~s right: ~s\n\n" left right) )
  )
both                         
)
)

; function to test ir
; return true or false
(define sensorsIR (lambda ()
 (enableIR 50)
 (define IRzero (list))
 (define IRone (list))
 (define IRtwo (list))
 (define IRres (list))
                    
 (printf "\nIR test\n")
                         
 (for ([i 10])
   (printf "IR 0: ~s IR 1: ~s IR 2: ~s\n" (getIR 0) (getIR 1) (getIR 2))
   (sleep 0.5)
   (set! IRzero (append IRzero (list (getIR 0))))
   (set! IRone (append IRone (list (getIR 1))))
   (set! IRtwo (append IRtwo (list (getIR 2))))
 )
 (define IRzeroSorted (sort IRzero <))
 (define IRoneSorted (sort IRone <))
 (define IRtwoSorted (sort IRtwo <))

 (cond ( (and (< (first IRzeroSorted) 100) (> (last IRzeroSorted) 800)) (set! IRres (append IRres (list #t)))) ( (set! IRres (append IRres (list #f))) ) )
 (cond ( (and (< (first IRoneSorted) 100) (> (last IRoneSorted) 800)) (set! IRres (append IRres (list #t)))) ( (set! IRres (append IRres (list #f))) ) )
 (cond ( (and (< (first IRtwoSorted) 100) (> (last IRtwoSorted) 800)) (set! IRres (append IRres (list #t)))) ( (set! IRres (append IRres (list #f))) ) )
 IRres
)
)

; function that test the potentiometer - read only
(define potentiometerTest (lambda ()
   ;add automatic check
   (define pot-low #f)
   (define pot-high #f)
   (define pot-ok #f)
                            
   (for ([i 40])
     (cond ( (< (analog-read 7) 10) (set! pot-low #t) ))
     (cond ( (> (analog-read 7) 1000) (set! pot-high #t) ))
     
     (printf "potentiometer: ~s\n" (analog-read 7) )
   (sleep 0.1)
 )
 (cond ( (and pot-low pot-high) (set! pot-ok #t)))
 pot-ok
))

; function to test the button on the board
; return true or false
(define buttonTest (lambda ()
                         
 (define pressed #f)
                         
 (for ([i 5])
   (cond ( (= (digital-read 5) 1) (printf "\nbutton pressed\n") (set! pressed #t)))
   (sleep 1)
 )

  (cond ( pressed (printf "\nButton works correctely\n\n") )
        ( (printf "\nButton problem\n\n") )
  )
pressed
)
)


(define bumpTest (lambda ()
  (open-asip)
  (sensorsLoopTen)
  (close-asip)
))


(define testSensors (lambda ()
  (open-asip)
  (sensorsIR)          
  (close-asip)
))


(define testSpeaker (lambda ()
  (open-asip)
  (playTone 440 50)          
  (close-asip)
))


; main sequence test function
; motors - bumps - ir - speaker - potentiometer - button
(define test (lambda ()
  (define motors #f)
  (define bumps #f)
  (define ir (list))
  (define potentiometer #f)
  (define button #f)

  (open-asip)
  (clearLCD) (sleep 0.2)
  (setLCDMessage "MIRTO TEST" 0)
               
  (printf "\nSPEAKER TEST\n\n" )
  (setLCDMessage "" 3) (setLCDMessage "Speaker test" 2)
  (playTone 440 100)
               
  (sleep 2)
               
  (printf "\nMOTORS TEST\n\n" )
  (setLCDMessage "" 3) (setLCDMessage "Motors test" 2)
  (set! motors (motorTest))
               
  (sleep 2)
               
  (printf "\nBUMP TEST\n\n" )
  (setLCDMessage "" 3) (setLCDMessage "Bumps test" 2)
  (set! bumps (sensorsLoopTen))
               
  (sleep 2)
               
  (printf "\nIR TEST\n\n" )
  (setLCDMessage "" 3) (setLCDMessage "IR test" 2)
  (set! ir (sensorsIR))
               
  (sleep 2)
               
  (printf "\nPOTENTIOMETER TEST\n\n" )
  (setLCDMessage "" 3) (setLCDMessage "Potentiometer test" 2)
  (set! potentiometer (potentiometerTest))
               
  (sleep 2)
               
  (printf "\nBUTTON TEST\n\n" )
  (setLCDMessage "" 3) (setLCDMessage "Button test" 2)
  (set! button (buttonTest))
  (close-asip)


  (printf "\nTest results:\nmotors:\t\t~s
bumps:\t\t~s
ir:\t\t~s
potentiometer:\t~s
button:\t\t~s" motors bumps ir potentiometer button)
               
))


; function that execute a single test function
(define singleTest (lambda (function)
  (open-asip)
  (define result (function))
  (close-asip)
  result
))


; function that read a character from input
; and perform an action
(define loopRead (lambda ()
    (define pwr 200)
    (define time 0.3)

    (define command (read))
    (cond ( (equal? command 'w) (setMotors pwr pwr) ) )
    (cond ( (equal? command 'd) (setMotors pwr (* -1 pwr)) ) )
    (cond ( (equal? command 'a) (setMotors (* -1 pwr) pwr) ) )
    (cond ( (equal? command 's) (setMotors (* -1 pwr) (* -1 pwr)) ) )
    (sleep time)
    (stopMotors)
    (sleep time)
    (cond ( (or (leftBump?) (rightBump?)) (setMotors (* -1 pwr) (* -1 pwr)) (sleep time) (stopMotors)) ) 
    (loopRead)
))




(test)
;(singleTest loopRead)
;(singleTest sensorsLoopTen)
;(singleTest motorTest)
;(singleTest stdDisplayText)
;(singleTest sensorsIR)
;(singleTest potentiometerTest)


