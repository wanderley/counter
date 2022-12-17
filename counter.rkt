#lang racket

(require (only-in racket/gui
                  get-display-size))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH  (/ (let-values ([(w _) (get-display-size)]) w) 3))
(define HEIGHT (/ (let-values ([(_ h) (get-display-size)]) h) 3))

(define (mm-ss mm ss) (+ (* 60 mm) ss))

(struct counter [time default-time up? paused?])
(struct state [last-update
               primary-counter
               secondary-counter
               stop?])

(define (counter-up mm ss) (counter 0 (mm-ss mm ss) #t #t))
(define (counter-down mm ss) (counter (mm-ss mm ss) (mm-ss mm ss) #f #t))
(define (counter-reset c) (struct-copy counter c [time (if (counter-up? c) 0 (counter-default-time c))]))
(define (counter-pause c) (struct-copy counter c [paused? (not (counter-paused? c))]))
(define (counter-tick c diff)
  (cond
    [(counter-paused? c) c]
    [else
     (define op (if (counter-up? c) + -))
     (struct-copy counter c
                  [time (max 0 (op (counter-time c) diff))])]))
(define (counter-finished? c)
  (or (and (counter-up? c)
           (> (counter-time c)
              (counter-default-time c)))
      (and (not (counter-up? c)) (zero? (counter-time c)))))
(define (counter->seconds c) (quotient (counter-time c) 60))
(define (counter->minutes c) (remainder (counter-time c) 60))
(define (counter->minutes-string c)
  (format "~a:~a"
          (~r (counter->seconds c) #:min-width 2 #:pad-string "0")
          (~r (counter->minutes c) #:min-width 2 #:pad-string "0")))


(define (change s a-key)
  (define pc (state-primary-counter s))
  (define sc (state-secondary-counter s))
  (cond
    [(key=? a-key "r")
     (struct-copy state s [primary-counter (counter-reset pc)])]
    [(key=? a-key "R")
     (struct-copy state s [secondary-counter (counter-reset sc)])]
    [(key=? a-key "p")
     (struct-copy state s [primary-counter (counter-pause pc)])]
    [(key=? a-key "P")
     (struct-copy state s [secondary-counter (counter-pause sc)])]
    [(key=? a-key "q") (struct-copy state s [stop? #t])]
    [else s]))

(define (tick s)
  (cond
    [(counter-paused? (state-primary-counter s))
     (struct-copy state s
                  [last-update (current-seconds)])]
    [else
     (define seconds (current-seconds))
     (define diff (- seconds (state-last-update s)))
     (struct-copy state s
                  [primary-counter (counter-tick (state-primary-counter s) diff)]
                  [secondary-counter (counter-tick (state-secondary-counter s) diff)]
                  [last-update seconds])]))

(define (render s)
  (cond
    [(false? (state-secondary-counter s))
     (render-counter (state-primary-counter s) #t #t)]
    [else
     (above
      (render-counter (state-primary-counter s) #t #f)
      (render-counter (state-secondary-counter s) #f #f))]))

(define (render-counter c primary? full-screen?)
  (overlay
   (text/font (counter->minutes-string c)
              (if primary? 240 180) 'white
              "Mono" 'default 'normal 'bold #f)
   (rectangle WIDTH (/ HEIGHT (if full-screen? 1 2))
              'solid (counter-background-color c primary?))))

(define (counter-background-color c primary?)
  (cond
    [(counter-paused? c) 'gray]
    [(counter-finished? c) (if primary? 'darkblue 'darkred)]
    [primary? 'black]
    [else 'darkgreen]))


(define (start! pc sc)
  (big-bang (state (current-seconds) pc sc #f)
            [name "Simple Counter"]
            [on-tick tick]
            [on-key change]
            [to-draw render]
            [stop-when state-stop?]
            [close-on-stop #t]))

(start! (counter-down 15 00)
        (counter-up   02 00))
