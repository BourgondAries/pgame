#lang racket

(provide core)

(require spipe "states/initializing.rkt")

(define (core state)
  (H~>
    state
    (initializing)
    ((const (list core2)) io.main)
    ((loop~> io main))
    ))

(define (core2 s)
  (H~>
    s
    (add1 ae.tick.iteration)
    ((top~> io core))))
