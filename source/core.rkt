#lang racket

(provide core)

(require spipe logger "pure.rkt" "states/initialize.rkt" "states/shutdown.rkt")

(define/H~> core
  (initialize)
  ((const (list core2 shutdown)) io.main)
  ((top-loop~> io main))
  )

(define/H~> core2
  (add1 ae.tick.iteration)
  ((top~> io core))
  )
