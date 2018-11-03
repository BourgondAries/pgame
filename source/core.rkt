#lang racket

(provide core)

(require spipe logger glfw3 "impure.rkt" "pure.rkt" "states/initialize.rkt" "states/shutdown.rkt")
(require syntax/parse/define)

(define/H~> core
  (initialize)
  ((const '()) fsm)
  ((const (list core2 shutdown)) io.main)
  ((const #t)         ae.should-clear?)
  ((top-loop~>* io main))
  )

(define/H~> core2
  (add1            ae.tick.iteration)
  ((oscillate 60)  (ae.tick.iteration) (ae.tick.oscillate))
  ; (crash-if (ae.should-clear?))
  (clear-graphics    (ae.should-clear?))
  ((const #t)         ae.should-clear?)
  ((top~>* io core))
  (glfwSwapBuffers             (io.window))
  ((if-empty-pop '(io core)))
  )

(define (crash-if s)
  (when (not s)
    (exit 1)))
