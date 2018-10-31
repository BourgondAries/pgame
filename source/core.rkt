#lang racket

(provide core)

(require spipe logger glfw3 "impure.rkt" "pure.rkt" "states/initialize.rkt" "states/shutdown.rkt")
(require syntax/parse/define)

(define-syntax-parser top-loop~>*
  ([_ term:id ...+]
   #'(lambda (state)
       (H~>
         state
         ((curry cons '(term ...)) fsm)
         ((top-loop~> term ...))
         (rest fsm)))))
(define-syntax-parser top~>*
  ([_ term:id ...+]
   #'(lambda (state)
       (H~>
         state
         ((curry cons '(term ...)) fsm)
         ((top~> term ...))
         (rest fsm)))))
(define-syntax-parser loop~>*
  ([_ term:id ...+]
   #'(lambda (state)
       (H~>
         state
         ((curry cons '(term ...)) fsm)
         ((loop~> term ...))
         (rest fsm)))))

(define/H~> core
  (initialize)
  ((const '()) fsm)
  ((const (list core2 shutdown)) io.main)
  ((top-loop~>* io main))
  )

(define/H~> core2
  (add1 ae.tick.iteration)
  (clear-graphics           ())
  ((top~>* io core))
  (glfwSwapBuffers (io.window))
  ((if-empty-pop '(io core)))
  )
