#lang racket

(provide (all-defined-out))

(require "../all.rkt" "shutdown.rkt")

(define/H~> core*
  ((const (identity-matrix 4)) io.neutral)
  ((const '("data/basictiles.png" 1 1)) ae.tmp)
  ((const 0) ae.transform.x)
  ((const 0) ae.transform.y)
  ((set-fsm cores) io.core)
  )
(define/H~> cores
  core*-pre
  (current-s *)
  pop ; I'm done
  (core*-pure* ae)
  core*-post
  )
(define/H~> core*-pre
  (dbug fsm)
  (#:context (io.window)
    (get-window-size          ()      (io.window-size.width io.window-size.height))
    (get-keys                 ()      (ae.keys))
    (glfwWindowShouldClose*   ()      ae.should-exit?)
    )
  (update-perspective (ae.tick.iteration
                       io.window-size.width
                       io.window-size.height) io.perspective)
  )

(define/H~> core*-pure*
  (check-C-W-exit         (keys.left-control keys.w)   should-exit?)
  (collect-wasd           (keys)                       (keys.wasd))
  (last-key               (last-direction keys.wasd)   (last-direction))
  (compute-walk-tick      (keys)                       tick.direction-keys)
  (add1-if-true           (keys.q)   transform.r)
  (sub1-if-true           (keys.e)   transform.r)
  (count-walking          (keys) transform.x transform.y)
  (check-door-collision              (transform.x transform.y)  (collides?))
  (check-collision                   (transform collidables) (action))
  ; (any-direction-keys?               (keys)                 (any-direction-keys?))
  )

(define/H~> core*-post
  (exit-c (ae.should-exit?) io.main)
  ((if* (push-fsm decode-tmx)) (ae.keys.g)  io.core)
  ((if* (set-fsm top-map))     (ae.collides?)   io.core)
  ((if* (push-fsm menu))       (ae.keys.escape) io.core)
  (make-global-transform*    (ae.transform)   (io.transform))
  (matrix* (io.transform io.perspective) (io.view))
  (clear-graphics           ())
  (render-absolute   ())
  (#:context (io.transform)
    (draw              (ae.tick.direction-keys ae.last-direction io.animation.madotsuki) ())
    ; (draw-relative     (io.render.relative)                                              ())
    )
  (draw-texture-2    (io.perspective ae.tmp))
  (drawtext2         (io.perspective ae.tick.iteration))
  (draw-coin         (ae.tick.iteration))
  (draw-portal       (ae.tick.iteration))
  (glfwSwapBuffers   (io.window))
  )
