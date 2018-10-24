#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "../drawing.rkt" "../impure.rkt" "../pure.rkt" "../shutdown.rkt")

(provide (all-defined-out))

(define (experimental s)
  (trce 'TEST)
  (H~>
    s
    (do-fade       (io.window io.window-size.width io.window-size.height))
    ((const  120)  ae.tick.to-zero)
    ((set-fsm experimental*) io.main)
  ))
(define (experimental* s)
  (H~>
    s
    (glfwGetWindowSize      (io.window) (io.window-size.width io.window-size.height))
    (perspective            (io.window-size.width io.window-size.height) (io.graphics.perspective))
    (clear-graphics    ())
    (dd                (io.graphics.perspective))
    (do-draw-tiles     (ae.tick.iteration io.graphics.perspective))
    (render-absolute   ())
    (context (io.transform)
      (draw              (ae.tick.direction-keys ae.last-direction io.animation.madotsuki))
      (draw-relative     (io.render.relative))
      )
    (fade/invert            (ae.tick.to-zero))
    (context (io.window)
      (glfwSwapBuffers        ())
      (get-keys               () (ae.keys))
      )
    (H~> ae
      (collect-wasd           (keys) (keys.wasd))
      (last-key               (last-direction keys.wasd) (last-direction))
      ((step-to 0)  tick.to-zero)
      (pure *)
      (add1 tick.iteration)
      (any-direction-keys?       (keys)                 (any-direction-keys?))
      ((if* add1)       (any-direction-keys?) tick.direction-keys)
    )
    (make-global-transform     (ae.transform)            (io.transform))
  ))
