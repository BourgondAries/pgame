#lang racket

(provide breakpoint core)

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt" "drawing.rkt" "impure.rkt" "initialization.rkt" "pure.rkt" "shutdown.rkt")
(require "states/core.rkt" "states/experimental.rkt" "states/menu.rkt" "states/top-map.rkt")
(require "visualizer.rkt" (for-syntax racket/base racket/syntax))
(require "state.rkt" "states/initializing.rkt")

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define/memoize (meval form) (eval form ns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the semantic entry point of the program
;;
;; The reason for having with-handlers is to be able
;; to catch exceptions using that state so it can
;; be replayed.
;;
;; This function also acts as the main state machine
;; handler. Each frame it checks the current game.fsm
;; and enters the top function. This function is stored
;; as a symbol and eval'd to get the actual function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (core state)
  (with-handlers* ([exn:break? (lambda (_) (break state cleanup))])
    (cond
      ([break-seen?]         (break state cleanup))
      ([empty? state]        (initializing state))
      ([should-exit? state]  (break state cleanup))
      (else                  ((meval (first (nested-hash-ref state 'game 'fsm))) state)))))
