#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe xml
         "../breakpoint.rkt" "../drawing.rkt" "../impure.rkt" "../initialization.rkt" "../pure.rkt" "../shutdown.rkt" "../state.rkt")

(provide (all-defined-out))

(define (x filename)
  (with-input-from-file filename
    (thunk
      (define document (read-xml))
      (trce document))))

(define (t)
  (x "data/town-2.tmx"))

(state decode-tmx
  (enter
    (t ())
   )
  (pre
   trce
   )
  (pure
    (pop-fsm fsm)
    (pop-fsm fsm))
  (post)
  (exit))

