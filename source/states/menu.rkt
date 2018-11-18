#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         pgame/utils)

(require "../visualizer.rkt")

(provide (all-defined-out))

(define/H~> menu
  (show-visualization *) ; Blocks by using a subprogram
  ;; We need to reset the keys due to sticky keys,
  ;; if we didn't, we'd return to this function again.
  (clear-keys (io.window) ae.keys)
  ;; Finally, indicate that we no longer want to loop here,
  ;; return from whence we came.
  (pop-fsm    io.core)
  )
