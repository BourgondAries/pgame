#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "../breakpoint.rkt" "../drawing.rkt" "../impure.rkt" "../initialization.rkt" "../pure.rkt" "../shutdown.rkt" "../state.rkt")

(provide (all-defined-out))

(state experimental
  (enter
    (do-fade       (io.window io.window-size.width io.window-size.height))
    )
  (pre
    (glfwGetWindowSize      (io.window) (io.window-size.width io.window-size.height))
    (perspective            (io.window-size.width io.window-size.height) (io.graphics.perspective))
    (clear-graphics    ())
    (dd                (io.graphics.perspective))
    (do-draw-tiles     (game.tick.iteration io.graphics.perspective))
    (render-absolute   ())
    (draw              (io.transform game.tick.direction-keys game.last-direction io.animation.madotsuki))
    (draw-relative     (io.transform io.render.relative))
    (fade/invert            (game.tick.to-zero))
    (glfwSwapBuffers        (io.window))
    (get-keys               (io.window) (game.keys))
    )
  (pure
    (collect-wasd           (keys) (keys.wasd))
    (last-key               (last-direction keys.wasd) (last-direction))
    ((step-to 0)  tick.to-zero)
    (pure *)
    (add1 tick.iteration)
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
