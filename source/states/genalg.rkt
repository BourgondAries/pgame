#lang racket

(provide genalg)

(require logger spipe "../../../genalg/main.rkt")

(define (loopfun s)
  (dbug s)
  (H~>
    s
    (rest io.main))
  )

(define (genalg state)
  (define s
    (dbug
      (variables state
         (io.main (list loopfun))
       )))
  ((loop~> io main) s)
  (exit)
  )
