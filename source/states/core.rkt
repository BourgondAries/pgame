#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "../breakpoint.rkt" "../drawing.rkt" "../impure.rkt" "../initialization.rkt" "../pure.rkt" "../shutdown.rkt" "../state.rkt")

(provide (all-defined-out))

(state core*
  (enter
    )
  (pre
    (context io.window
      (get-keys                 ()      (ae.keys))
      (glfwWindowShouldClose    ()      (ae.should-exit?)))
  )
  (pure
    (collect-wasd           (keys)                       (keys.wasd))
    (last-key               (last-direction keys.wasd)   (last-direction))
    (add1                   tick.iteration)
    ((if* add1)             (any-direction-keys? tick.direction-keys)   (tick.direction-keys))
    (check-C-W-exit         (keys.left-control keys.w)   (should-exit?))
    (add1-if-true           (keys.q)   transform.r)
    (sub1-if-true           (keys.e)   transform.r)
    (add1-if-true           (keys.w)   transform.y)
    (sub1-if-true           (keys.a)   transform.x)
    (sub1-if-true           (keys.s)   transform.y)
    (add1-if-true                (keys.d)   transform.x)
    (sub1-if-true                (keys.q)   rotation)
    (add1-if-true                (keys.e)   rotation)
    ((if* (push-fsm 'menu))      (keys.escape) fsm)
    (check-door-collision        (transform.x transform.y)  (collides?))
    ((if* (push-fsm 'top-map))   (collides? fsm)            (fsm))
    (any-direction-keys?         (keys)                     (any-direction-keys?))
    )
  (post
    (make-global-transform    (ae.transform)   (io.transform))
    (get-window-size          (io.window)      (io.window-size.width io.window-size.height))
    (clear-graphics           ())
    (sub                      *)
    (glfwSwapBuffers          (io.window))
    )
  (exit))

