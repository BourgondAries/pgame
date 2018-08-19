#lang racket

(provide breakpoint core)

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt" "drawing.rkt" "impure.rkt" "initialization.rkt" "pure.rkt" "shutdown.rkt")
(require "states/experimental.rkt")
(require "visualizer.rkt" (for-syntax racket/base racket/syntax))

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

(define (draw-coin tick)
  (animate "data/coin48.png" '(0.1 0.1) '(0.2 0.2) 61 1 tick 5))

(define ((every tick-modulus action) tick value)
  (if (zero? (modulo tick tick-modulus))
    (begin (action value) value)
    value
    ))

(define (sub state)
  (parameterize ([*view* (matrix* (rotate (nested-hash-ref state 'game 'rotation)) (perspective (nested-hash-ref state 'io 'window-size 'width #:default #f)
                                                                                                (nested-hash-ref state 'io 'window-size 'height #:default #f))
                                               (*view*))])
    (H~>
      state
      (render-absolute   ())
      (draw              (io.transform game.tick.direction-keys io.last-direction io.animation.madotsuki))
      (draw-relative     (io.transform io.render.relative))
      (drawtext          (game.tick.direction-keys))
      (draw-coin         (game.tick.iteration)))))

(define (core* state)
  (H~> state
    (glfwGetWindowSize      (io.window) (io.window-size.width io.window-size.height))
    ; ((every 60 displayln)   (game.tick.iteration io.window-size) (io.window-size))
    (clear-graphics         ())
    ((if* add1)             (game.any-direction-keys?
                            game.tick.direction-keys)
                            (game.tick.direction-keys))
    (sub *)
    (glfwSwapBuffers        (io.window))
    (get-keys               (io.window) (game.keys))
    (glfwWindowShouldClose  (io.window) (game.should-exit?))
    (collect-wasd           (game.keys) (game.keys.wasd))
    (last-key               (io.last-direction game.keys.wasd) (io.last-direction))
    (pure   game)
    ((lambda (game)
       (H~> game
          (add1 tick.iteration)
          ))
     game)
    (check-door-collision       (game.transform.x game.transform.y)  (game.collides?))
    ((if* (push-fsm 'top-map))  (game.collides? game.fsm)            (game.fsm))
    (any-direction-keys?        (game.keys)                 (game.any-direction-keys?))
    (make-global-transform      (game.transform)            (io.transform))
  ))


(require "state.rkt")

;; This is a scene. Very simple, H~>-oriented
(state top-map
  (enter
    ((const -100)  game.transform.y)
    ((const  120)  game.tick.to-zero)
    (do-fade       (io.window io.window-size.width io.window-size.height))
    )
  (pre
    (clear-graphics   ())
    (render-absolute  ())
    (draw             (io.transform game.tick.direction-keys game.last-direction io.animation.madotsuki))
    (draw-relative    (io.transform io.render.relative))
    (fade/invert            (game.tick.to-zero))
    ; (trce game.tick.to-zero)
    (glfwSwapBuffers        (io.window))
    (get-keys               (io.window) (game.keys))
    (glfwWindowShouldClose  (io.window) (game.should-exit?))
    )
  (pure
    (collect-wasd           (keys) (keys.wasd))
    (last-key               (last-direction keys.wasd) (last-direction))
    ((step-to 0)            tick.to-zero)
    (pure   *)
    (add1                      tick.iteration)
    (check-door-collision      (transform.x transform.y)  (collides?))
    ((if* (set-fsm 'top-map))  (collides? fsm)            (fsm))
    ((set-fsm 'experimental)      fsm)
    (any-direction-keys?       (keys)                 (any-direction-keys?))
    ((if* add1)       (any-direction-keys?
                       tick.direction-keys)
                      (tick.direction-keys))
    )
  (post
    (make-global-transform     (game.transform)            (io.transform))
    )
  (exit)
  )



(define (menu state)
  (info^ "Entering visualization state")
  (H~>
    state
    (show-visualization)  ; Blocks by using a subprogram

    ;; We need to reset the keys due to sticky keys,
    ;; if we didn't, we'd return to this function again.
    (get-keys   (io.window) (game.keys))
    (set-false  game.keys.escape)

    ;; Finally, indicate that we no longer want to loop here,
    ;; return from whence we came.
    (pop-fsm    game.fsm)
    ))
