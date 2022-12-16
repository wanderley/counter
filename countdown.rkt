#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH  (/ 3360 3))
(define HEIGHT (/ 1890 3))
(define BACKGROUND
  (freeze
   (rectangle WIDTH HEIGHT 'solid 'black)))

(define (change w a-key)
  (cond
    [(key=? a-key "r")  0]
    [(key=? a-key "p") -1]
    [else w]))

(define (tick w)
  (if (>= w 0)
      (add1 w)
      w))

(define (render w)
  (overlay
   (text/font (cond
                [(or (zero? w) (positive? w))
                 (format "~a:~a"
                         (~r (quotient w 60) #:min-width 2 #:pad-string "0")
                         (~r (remainder w 60) #:min-width 2 #:pad-string "0"))]
                [else "00:00"])
              240 'white
              "Mono" 'default 'normal 'bold #f)
   BACKGROUND))

(big-bang 0
          [name "Countdown in Racket"]
          [on-tick tick 1]
          [on-key change]
          [to-draw render])
