#lang racket

(provide core)

(require spipe "states/initializing.rkt")

(define/H~> core
  (initializing)
  ((const (list core2)) io.main)
  ((loop~> io main))
  )

(define/H~> core2
  (add1 ae.tick.iteration)
  ((top~> io core)))
