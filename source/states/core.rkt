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
    (glfwGetWindowSize      (io.window) (io.window-size.width io.window-size.height))
    (clear-graphics         ())
    ((if* add1)             (ae.any-direction-keys?
                            ae.tick.direction-keys)
                            (ae.tick.direction-keys))
    (sub *)
    (glfwSwapBuffers        (io.window))
    (get-keys               (io.window) (ae.keys))
    (glfwWindowShouldClose  (io.window) (ae.should-exit?))
    (collect-wasd           (ae.keys) (ae.keys.wasd))
    (last-key               (io.last-direction ae.keys.wasd) (io.last-direction))
    (pure   ae)
    ((lambda (ae)
       (H~> ae
          (add1 tick.iteration)
          ))
     ae)
    (check-door-collision       (ae.transform.x ae.transform.y)  (ae.collides?))
    ((if* (push-fsm 'top-map))  (ae.collides? ae.fsm)            (ae.fsm))
    (any-direction-keys?        (ae.keys)                 (ae.any-direction-keys?))
    (make-global-transform      (ae.transform)            (io.transform))
  )
  (pure)
  (post)
  (exit))

