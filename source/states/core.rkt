#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "../breakpoint.rkt" "../drawing.rkt" "../impure.rkt" "../initialization.rkt" "../pure.rkt" "../shutdown.rkt" "../state.rkt")

(provide (all-defined-out))

(state core*
  (enter)
  (pre
    (get-window-size        (io.window) (io.window-size.width io.window-size.height))
    (clear-graphics         ())
    (sub *)
    (glfwSwapBuffers        (io.window))
    (get-keys               (io.window) (ae.keys))
    (glfwWindowShouldClose  (io.window) (ae.should-exit?))
    (make-global-transform      (ae.transform)            (io.transform))
  )
  (pure
    (collect-wasd           (keys) (keys.wasd))
    (last-key               (last-direction keys.wasd) (last-direction))
    (add1 tick.iteration)
    ((if* add1)             (any-direction-keys? tick.direction-keys) (tick.direction-keys))
    (pure   *)
    (check-door-collision       (transform.x transform.y)  (collides?))
    ((if* (push-fsm 'top-map))  (collides? fsm)            (fsm))
    (any-direction-keys?        (keys)                 (any-direction-keys?))
    )
  (post)
  (exit))

