#lang racket

(provide begin-state)

(require nested-hash logger glfw3 spipe pgame/utils)

(define/H~> begin-state
  run-init
  trce
  ((const '()) fsm)
  (construct-io-main (io.states) (io.main))
  ((const #t)         ae.should-clear?)
  ((top-loop~>* io main)))

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

(define (construct-io-main states)
  (list core2 (hash-ref states 'shutdown)))

(define (run-init state)
  ((nested-hash-ref state 'io 'states 'initialize) state))
