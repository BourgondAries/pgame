#lang racket

(provide breakpoint core)

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt" "drawing.rkt" "impure.rkt" "initialization.rkt" "pure.rkt" "shutdown.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the semantic entry point of the program
;;
;; The reason for having with-handlers is to be able
;; to catch exceptions using that state so it can
;; be replayed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (core state)
  (with-handlers* ([exn:break? (lambda (_) (break state cleanup))])
    (cond
      ([break-seen?]         (break state cleanup))
      ([empty? state]        (initialize state))
      ([should-exit? state]  (break state cleanup))
      (else                  (core* state)))))

;; Acts as glue between pure an impure. Mainly
(define (core* state)
  (H~> state
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (impure system)  ;; Purely impure game logic              ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; What follows are system -> game transformations
    (glfwWindowShouldClose (system.window) (should-exit?))
    (last-key              (system.last-direction game.keys.w game.keys.a game.keys.s game.keys.d) (system.last-direction))
    (get-keys              (system.window) (game.keys))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (pure   game) ;; Pure game logic. All else is glue/impure ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; What follows are game -> system transformations
    (any-direction-keys?   (game.keys) (system.any-direction-keys?))
    (make-global-transform (game.transform) (system.transform))
    trce
  ))


;; (("data/madoka.png" 4 4) 13 0)
