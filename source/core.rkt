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

(define (core* state)
  (H~> state
    (clear-graphics   ())
    ((if* add1)       (game.any-direction-keys?
                       game.tick.direction-keys)
                      (game.tick.direction-keys))
    (render-absolute  ())
    (draw             (system.transform game.tick.direction-keys system.last-direction system.animation.madotsuki))
    (draw-relative    (system.transform system.render.relative))
    (drawtext               (game.tick.direction-keys))
    (glfwSwapBuffers        (system.window))
    (get-keys               (system.window) (game.keys))
    (glfwWindowShouldClose  (system.window) (game.should-exit?))
    (collect-wasd           (game.keys) (game.keys.wasd))
    (last-key               (system.last-direction game.keys.wasd) (system.last-direction))
    (pure   game)
    ((lambda (game)
       (H~> game
          (add1 tick.iteration)
          ))
     game)
    (check-door-collision       (game.transform.x game.transform.y)  (game.collides?))
    ((if* (push-fsm 'top-map))  (game.collides? game.fsm)            (game.fsm))
    (any-direction-keys?        (game.keys)                 (game.any-direction-keys?))
    (make-global-transform      (game.transform)            (system.transform))
  ))

(define (top-map state)
  (H~>
    state
    ((const -100) game.transform.y)
    ((set-fsm 'top-map2)  game.fsm)
    ))

(define (top-map2 state)
  (H~>
    state
    (clear-graphics   ())
    ((if* add1)       (game.any-direction-keys?
                       game.tick.direction-keys)
                      (game.tick.direction-keys))
    (render-absolute  ())
    (draw             (system.transform game.tick.direction-keys system.last-direction system.animation.madotsuki))
    (draw-relative    (system.transform system.render.relative))
    ; (drawtext               (game.tick.direction-keys))
    (glfwSwapBuffers        (system.window))
    (get-keys               (system.window) (game.keys))
    (glfwWindowShouldClose  (system.window) (game.should-exit?))
    (collect-wasd           (game.keys) (game.keys.wasd))
    (last-key               (system.last-direction game.keys.wasd) (system.last-direction))
    (pure   game)
    ((lambda (game)
       (H~> game
          (add1 tick.iteration)
          ))
     game)
    (check-door-collision       (game.transform.x game.transform.y)  (game.collides?))
    ((if* (push-fsm 'top-map))  (game.collides? game.fsm)            (game.fsm))
    (any-direction-keys?        (game.keys)                 (game.any-direction-keys?))
    (make-global-transform      (game.transform)            (system.transform))
    ))

(define (collect-wasd keys)
  (or
    (if (hash-ref keys 'a #f) 'a #f)
    (if (hash-ref keys 'd #f) 'd #f)
    (if (hash-ref keys 'w #f) 'w #f)
    (if (hash-ref keys 's #f) 's #f)))

(define (check-door-collision x y)
  (and (< -20 x 20) (> 130 y 100)))

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
