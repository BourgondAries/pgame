#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cleanup and exit procedure
;;
;; The cleanup and exit procedure allows a clean exit of
;; the game. It should destroy all resources that would
;; otherwise be collected by the operating system.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide cleanup)

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt" "drawing.rkt" "impure.rkt" "initialization.rkt" "pure.rkt")

(define (cleanup state)
  (H~>
    state
    (glfwDestroyWindow io.window)
    ))
