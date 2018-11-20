#lang racket

(require glfw3 spipe pgame/utils)


(define-recursive-load states "states" functions->hash)

(module+ main
  (void
    (H~>
      ((hash-ref states 'initialize) (hasheq))
      ((const '()) fsm)
      ((const (list core2 (hash-ref states 'shutdown))) io.main)
      ((const #t)         ae.should-clear?)
      ((top-loop~>* io main)))))

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
