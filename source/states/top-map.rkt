#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "../breakpoint.rkt" "../drawing.rkt" "../impure.rkt" "../initialization.rkt" "../pure.rkt" "../shutdown.rkt" "../state.rkt")

(provide (all-defined-out))

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
