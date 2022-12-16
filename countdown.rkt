#lang racket

(require (only-in racket/gui
                  get-display-size))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH  (/ (let-values ([(w _) (get-display-size)]) w) 3))
(define HEIGHT (/ (let-values ([(_ h) (get-display-size)]) h) 3))

(struct state [last-update time paused? finished?])

(define (state->seconds s) (quotient (state-time s) 60))
(define (state->minutes s) (remainder (state-time s) 60))

(define (state->minutes-string s)
  (format "~a:~a"
          (~r (state->seconds s) #:min-width 2 #:pad-string "0")
          (~r (state->minutes s) #:min-width 2 #:pad-string "0")))


(define (change s a-key)
  (cond
    [(key=? a-key "r") (struct-copy state s [time 0])]
    [(key=? a-key "p") (struct-copy state s [paused? (not (state-paused? s))])]
    [(key=? a-key "q") (struct-copy state s [finished? #t])]
    [else s]))

(define (tick s)
  (cond
    [(state-paused? s) s]
    [else
     (define seconds (current-seconds))
     (define diff (- seconds (state-last-update s)))
     (struct-copy state s
                  [time (+ (state-time s) diff)]
                  [last-update seconds])]))


(define (render s)
  (overlay
   (text/font (state->minutes-string s)
              240 'white
              "Mono" 'default 'normal 'bold #f)
   (render-background s)))

(define (render-background s)
  (rectangle WIDTH HEIGHT 'solid (if (state-paused? s) 'gray 'black)))


(big-bang (state (current-seconds) 0 #f #f)
          [name "Simple Counter"]
          [on-tick tick]
          [on-key change]
          [to-draw render]
          [stop-when state-finished?]
          [close-on-stop #t])
