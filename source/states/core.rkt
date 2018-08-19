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
  )
  (pure)
  (post)
  (exit))

