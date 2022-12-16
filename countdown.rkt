#lang racket

(require 2htdp/image)
(require 2htdp/universe)

(struct state [time paused? finished?])

(define WIDTH  (/ 3360 3))
(define HEIGHT (/ 1890 3))
(define BACKGROUND
  (freeze
   (rectangle WIDTH HEIGHT 'solid 'black)))

(define (change s a-key)
  (cond
    [(key=? a-key "r") (struct-copy state s [time 0])]
    [(key=? a-key "p") (struct-copy state s [paused? (not (state-paused? s))])]
    [(key=? a-key "q") (struct-copy state s [finished? #t])]
    [else s]))

(define (tick s)
  (cond
    [(state-paused? s) s]
    [else (struct-copy state s [time (add1 (state-time s))])]))

(define (render s)
  (overlay
   (text/font (cond
                [(or (zero? (state-time s)) (positive? (state-time s)))
                 (format "~a:~a"
                         (~r (quotient (state-time s) 60) #:min-width 2 #:pad-string "0")
                         (~r (remainder (state-time s) 60) #:min-width 2 #:pad-string "0"))]
                [else "00:00"])
              240 'white
              "Mono" 'default 'normal 'bold #f)
   BACKGROUND))

(big-bang (state 0 #f #f)
          [name "Countdown"]
          [on-tick tick 1]
          [on-key change]
          [to-draw render]
          [stop-when state-finished?]
          [close-on-stop #t])
