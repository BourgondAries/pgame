#lang racket

(provide breakpoint core)

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt" "drawing.rkt" "impure.rkt" "initialization.rkt" "pure.rkt" "shutdown.rkt")

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
      ([empty? state]        (initialize state))
      ([should-exit? state]  (break state cleanup))
      (else                  ((meval (first (nested-hash-ref state 'game 'fsm))) state)))))

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
    (identity              (game.render.absolute) (system.render.absolute))
    trce
  ))

(require "visualizer.rkt")

(define (menu state)
  (info^ "Entering visualization state")
  (H~>
    state
    (show-visualization)  ; Blocks by using a subprogram

    ;; We need to reset the keys due to sticky keys,
    ;; if we didn't, we'd return to this function again.
    (get-keys   (system.window) (game.keys))
    (set-false  game.keys.escape)

    ;; Finally, indicate that we no longer want to loop here,
    ;; return from whence we came.
    (pop-fsm    game.fsm)
    ))
