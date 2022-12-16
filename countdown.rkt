#lang racket

(require (only-in racket/gui
                  get-display-size))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH  (/ (let-values ([(w _) (get-display-size)]) w) 3))
(define HEIGHT (/ (let-values ([(_ h) (get-display-size)]) h) 3))

(struct counter [time default-time up? paused? finished?])
(struct state [last-update counter finished?])

(define (counter-up mm ss) (counter (mm-ss mm ss) (mm-ss mm ss) #t #t #f))
(define (counter-down mm ss) (counter (mm-ss mm ss) (mm-ss mm ss) #f #t #f))

(define (counter->seconds c) (quotient (counter-time c) 60))
(define (counter->minutes c) (remainder (counter-time c) 60))
(define (counter->minutes-string c)
  (format "~a:~a"
          (~r (counter->seconds c) #:min-width 2 #:pad-string "0")
          (~r (counter->minutes c) #:min-width 2 #:pad-string "0")))
(define (mm-ss mm ss) (+ (* 60 mm) ss))


(define (change s a-key)
  (cond
    [(key=? a-key "r")
     (struct-copy state s
                  [counter (struct-copy counter (state-counter s)
                                        [time (counter-default-time (state-counter s))])])]
    [(key=? a-key "p")
     (struct-copy state s
                  [counter (struct-copy counter (state-counter s)
                                        [paused? (not (counter-paused? (state-counter s)))])])]
    [(key=? a-key "q") (struct-copy state s [finished? #t])]
    [else s]))

(define (tick s)
  (cond
    [(counter-paused? (state-counter s))
     (struct-copy state s
                  [last-update (current-seconds)])]
    [else
     (define seconds (current-seconds))
     (define diff (- seconds (state-last-update s)))
     (define op (if (counter-up? (state-counter s)) + -))
     (struct-copy state s
                  [counter (struct-copy counter (state-counter s)
                                        [time (max 0 (op (counter-time (state-counter s)) diff))])]
                  [last-update seconds])]))


(define (render s)
  (overlay
   (text/font (counter->minutes-string (state-counter s))
              240 'white
              "Mono" 'default 'normal 'bold #f)
   (render-background s)))

(define (render-background s)
  (rectangle WIDTH HEIGHT 'solid (if (counter-paused? (state-counter s)) 'gray 'black)))


(define (start! c)
  (big-bang (state (current-seconds) c #f)
            [name "Simple Counter"]
            [on-tick tick]
            [on-key change]
            [to-draw render]
            [stop-when state-finished?]
            [close-on-stop #t]))

(start! (counter-down 10 00))
