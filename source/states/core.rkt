#lang racket

(provide (all-defined-out))

(require "../all.rkt" "native-menu.rkt" pgame/utils)

#| To gather knowledge |#
#| The greatest of pains exists |#
#| Ignorance is bliss |#

(define (core s) s)

(define/H~> core*
  ((const (identity-matrix 4)) io.neutral)
  ((const '("data/basictiles.png" 1 1)) ae.tilemap-data)
  ((const 0) ae.transform.x)
  ((const 0) ae.transform.y)
  ((set-fsm cores) io.core)
  )
(define/H~> cores
  core*-pre
  ; (current-s *)
  (core*-pure* ae)
  core*-post
  )
(define/H~> core*-pre
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
  (check-C-W-exit         (keys.left-control keys.w) should-exit?)
  walk
  (check-door-collision   (transform.x transform.y)  (collides?))
  (check-collision        (transform collidables)    (action))
  (check-box              (transform.x transform.y)  should-exit?)
  ; (dbug (transform.x transform.y))
  ; (any-direction-keys?               (keys)                 (any-direction-keys?))
  )

(define/H~> core*-post

  (exit-c                  (ae.should-exit?)  io.main)
  ((cond-push decode-tmx)  (ae.keys.g)        io.core)
  ((cond-set top-map)      (ae.collides?)     io.core)
  ((cond-push menu)        (ae.keys.escape)   io.core)
  ((cond-set native-menu)  (ae.keys.b)        io.core)

  (or* (ae.keys.g ae.collides? ae.keys.escape ae.keys.b)
       (ae.should-clear?))

  (not ae.should-clear?)
  (make-global-transform* (ae.transform)   (io.transform))
  (render-absolute        ())
  (matrix* (io.transform io.perspective) (io.view))
  (#:context (io.transform)
    (draw              (ae.tick.direction-keys ae.last-direction io.animation.madotsuki) ())
    ; (draw-relative     (io.render.relative)                                              ())
    )
  (draw-texture-2    (io.perspective ae.tilemap-data))
  (drawtext2         (io.perspective ae.tick.iteration))
  (draw-coin         (ae.tick.iteration))
  (draw-portal       (ae.tick.iteration))
  )

(module+ test
  (require errortrace rackunit)
  )

(module+ test
  (define fixture
    (variables (hasheq)
      (keys (hasheq))
      (transform.x 0)
      (transform.y 0)
      (tick.direction-keys (hasheq))
      ))
  (test-equal? "Ensure should-exit is not negated"
               (hash-ref (core*-pure* (H~> fixture ((const #t) should-exit?))) 'should-exit?)
               #t)
  )
