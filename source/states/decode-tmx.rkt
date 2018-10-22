#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe xml
         "../drawing.rkt" "../impure.rkt" "../pure.rkt" "../shutdown.rkt" "../state.rkt")

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
    (clear-keys (io.window) ae.keys)
   )
  (pure
    (pop-fsm fsm)
    )
  (post)
  (exit))

