#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "../breakpoint.rkt" "../drawing.rkt" "../impure.rkt" "../pure.rkt" "../shutdown.rkt" "../state.rkt")

(provide (all-defined-out))

(state core*
  (enter
    ((const (identity-matrix 4)) io.neutral)
    ((const '("data/basictiles.png" 1 1)) ae.tmp)
    ((const 0) ae.transform.x)
    ((const 0) ae.transform.y)
    )
  (pre
    (context (io.window)
      (get-window-size          ()      (io.window-size.width io.window-size.height))
      (get-keys                 ()      (ae.keys))
      (glfwWindowShouldClose    ()      (ae.should-exit?))
      )
    (update-perspective (ae.tick.iteration
                         io.window-size.width
                         io.window-size.height) io.perspective)
  )
  (pure
    (collect-wasd           (keys)                       (keys.wasd))
    (last-key               (last-direction keys.wasd)   (last-direction))
    (add1                   tick.iteration)
    (compute-walk-tick      (keys)   tick.direction-keys)
    (check-C-W-exit         (keys.left-control keys.w)   (should-exit?))
    (add1-if-true           (keys.q)   transform.r)
    (sub1-if-true           (keys.e)   transform.r)
    (count-walking          (keys) transform.x transform.y)
    (sub1-if-true                (keys.q)   rotation)
    (add1-if-true                      (keys.e)   rotation)
    ((if* (push-fsm 'menu))            (keys.escape) fsm)
    ((if* (push-fsm 'decode-tmx))      (keys.g) fsm)
    (check-door-collision              (transform.x transform.y)  (collides?))
    ((if* (push-fsm 'top-map))         (collides?)            fsm)
    ; (any-direction-keys?               (keys)                 (any-direction-keys?))
    )
  (post
    (make-global-transform    (ae.transform)   (io.transform))
    (clear-graphics           ())
    (render-absolute   ())
    (context (io.transform)
      (draw              (ae.tick.direction-keys ae.last-direction io.animation.madotsuki) ())

      (draw-relative     (io.render.relative)                                              ())
      )
    (draw-texture-2 (io.perspective ae.tmp))
    (drawtext2      (io.perspective ae.tick.iteration))
    ; (drawtext     (ae.tick.direction-keys))
    (draw-coin         (ae.tick.iteration))
    (draw-portal       (ae.tick.iteration))
    (glfwSwapBuffers   (io.window))
    )
  (exit))

