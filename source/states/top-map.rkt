#lang racket

(require racket/list syntax/parse/define
  (for-syntax racket/base racket/list racket/string
             threading)
  ffi/vector finalizer math/matrix opengl opengl/util threading
  glfw3 logger memo nested-hash spipe
  "../drawing.rkt" "../impure.rkt" "../pure.rkt" "../shutdown.rkt" "../state.rkt")

(provide (all-defined-out))


;; This is a scene. Very simple, H~>-oriented
(define/H~> top-map
  (H~> ae
    ((const '( (hitbox (-1 -1) (1 1) (trce "INSIDE")) )) triggers)
    ((const -100)  transform.y)
    ((const  120)  tick.to-zero))
  (do-fade       (io.window io.window-size.width io.window-size.height))
  ((set-fsm top-map*) io.main)
  )
(define/H~> top-map*
  top-map-pre
  (top-map-pure ae)
  top-map-post
  )
(define/H~> top-map-pre
  (clear-graphics   ())
  (render-absolute  ())
  (#:context (io.transform)
    (draw             (ae.tick.direction-keys ae.last-direction io.animation.madotsuki))
    (draw-relative    (io.render.relative))
    )
  (fade/invert            (ae.tick.to-zero))
  ; (trce ae.tick.to-zero)
  (#:context (io.window)
    (glfwSwapBuffers        ())
    (get-keys               () (ae.keys))
    (glfwWindowShouldClose  () (ae.should-exit?))
    )
  )
(define/H~> top-map-pure
  (collect-wasd           (keys) (keys.wasd))
  (last-key               (last-direction keys.wasd) (last-direction))
  ((step-to 0)            tick.to-zero)
  (pure   *)
  (check-door-collision      (transform.x transform.y)  (collides?))
  ; ((if* (set-fsm 'top-map))  (collides? fsm)            (fsm))
  ; ((set-fsm 'experimental)      fsm)
  (any-direction-keys?       (keys)                 (any-direction-keys?))
  ((if* add1)       (any-direction-keys?) tick.direction-keys)
  )
(define/H~> top-map-post
  (make-global-transform     (ae.transform)            (io.transform))
  )
