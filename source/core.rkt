#lang racket

(provide breakpoint core)

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt" "drawing.rkt" "impure.rkt" "initialization.rkt" "pure.rkt" "shutdown.rkt")

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define/memoize (meval form) (eval form ns))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the semantic entry point of the program
;;
;; The reason for having with-handlers is to be able
;; to catch exceptions using that state so it can
;; be replayed.
;;
;; This function also acts as the main state machine
;; handler. Each frame it checks the current game.fsm
;; and enters the top function. This function is stored
;; as a symbol and eval'd to get the actual function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (core state)
  (with-handlers* ([exn:break? (lambda (_) (break state cleanup))])
    (cond
      ([break-seen?]         (break state cleanup))
      ([empty? state]        (initialize state))
      ([should-exit? state]  (break state cleanup))
      (else                  ((meval (first (nested-hash-ref state 'game 'fsm))) state)))))

(define (draw-coin tick)
  (animate "data/coin48.png" '(0.1 0.1) '(0.2 0.2) 61 1 tick 5))

(define (perspective width height)
  (when (and width height)
    (glViewport 0 0 width height))
  (if (and width height)
    (matrix [[(/ height width) 0 0 0]
             [0                1 0 0]
             [0                0 1 0]
             [0                0 0 1]])
  (identity-matrix 4)))

(define (rotate r)
  (matrix [[(cos r) (- (sin r)) 0 0]
           [(sin r) (cos r)     0 0]
           [0       0           1 0]
           [0       0           0 1]]))

(define (sub state)
  (parameterize ([*view* (matrix* (rotate (nested-hash-ref state 'game 'rotation)) (perspective (nested-hash-ref state 'system 'window-size 'width)
                                                                                                (nested-hash-ref state 'system 'window-size 'height))
                                               (*view*))])
    (trce (*view*))
    (H~>
      state
      (render-absolute  ())
      (draw             (system.transform game.tick.direction-keys system.last-direction system.animation.madotsuki))
      (draw-relative    (system.transform system.render.relative))
      (drawtext               (game.tick.direction-keys))
      (draw-coin (game.tick.iteration)))))

(define (core* state)
  (H~> state
    (glfwGetWindowSize (system.window) (system.window-size.width system.window-size.height))
    (trce (system.window-size))
    (clear-graphics   ())
    ((if* add1)       (game.any-direction-keys?
                       game.tick.direction-keys)
                      (game.tick.direction-keys))
    (sub *)
    (glfwSwapBuffers        (system.window))
    (get-keys               (system.window) (game.keys))
    (glfwWindowShouldClose  (system.window) (game.should-exit?))
    (collect-wasd           (game.keys) (game.keys.wasd))
    (last-key               (system.last-direction game.keys.wasd) (system.last-direction))
    (pure   game)
    ((lambda (game)
       (H~> game
          (add1 tick.iteration)
          ))
     game)
    (check-door-collision       (game.transform.x game.transform.y)  (game.collides?))
    ((if* (push-fsm 'top-map))  (game.collides? game.fsm)            (game.fsm))
    (any-direction-keys?        (game.keys)                 (game.any-direction-keys?))
    (make-global-transform      (game.transform)            (system.transform))
  ))

(define (do-fade window)
  (define texture (get-previous-frame 800 600))
  (glClearColor 0. 0. 0. 0.)
  (for ([i 40])
    (glClear GL_COLOR_BUFFER_BIT)
    (draw-texture-fullscreen texture)
    (fade (/ i 30))
    (glfwSwapBuffers window))
  (glClearColor 0.3 0.8 0.3 0.))

(require "visualizer.rkt" (for-syntax racket/base racket/syntax))

(define-syntax-parser node
  ([_ name state
      ((~datum enter)
       enter-body ...)
      ((~datum pre)
       pre-body ...)
      ((~datum pure)
       pure-body ...)
      ((~datum post)
       post-body ...)
      ((~datum exit)
       exit-body ...)]
   #:with name-core (format-id #'name "~a-core" #'name)
   #'(begin
       (define (name state)
         (trce^ name)
         (H~>
           state
           enter-body ...
           ((set-fsm 'name-core) game.fsm)))
       (define (name-core state)
         (H~>
           state
           pre-body ...
           ((lambda (game)
            (H~> game pure-body ...)) game)
           post-body ...
           )))))

(define (dd)
  (draw-texture "data/text/main-text2.png" '(-1 -1) '(1 1)))

(node experiment state
  (enter
    (do-fade       (system.window))
    )
  (pre
    (clear-graphics   ())
    (dd ())
    (aa (game.tick.iteration))
    (render-absolute  ())
    (draw             (system.transform game.tick.direction-keys game.last-direction system.animation.madotsuki))
    (draw-relative    (system.transform system.render.relative))
    (fade/invert            (game.tick.to-zero))
    (glfwSwapBuffers        (system.window))
    (get-keys               (system.window) (game.keys))
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
    (make-global-transform     (game.transform)            (system.transform))
    )
  (exit)
  )

(define grass (make-list 20 (make-list 20 49)))

(define (aa tick)
  ; (trce tick)
  (define-values (x y) (ticker (quotient tick 60) 8 15))
  (trce (quotient tick 60))
  ((animate-texture "data/basictiles.png" '(-1 -1) '(-0.5 -0.5) 8 15)
   x y)
  (draw-tiles "data/basictiles.png" 8 15 grass (scale 5/100) 0)
  (draw-shape '((-1 1) (0.5 1) (0.5 0.5))
              '((0 0 0 0) (0 1 0 0) (1 0 0 1)))
  )

;; This is a scene. Very simple, H~>-oriented
(node top-map state
  (enter
    ((const -100)  game.transform.y)
    ((const  120)  game.tick.to-zero)
    (do-fade       (system.window))
    )
  (pre
    (clear-graphics   ())
    (render-absolute  ())
    (draw             (system.transform game.tick.direction-keys game.last-direction system.animation.madotsuki))
    (draw-relative    (system.transform system.render.relative))
    (fade/invert            (game.tick.to-zero))
    ; (trce game.tick.to-zero)
    (glfwSwapBuffers        (system.window))
    (get-keys               (system.window) (game.keys))
    (glfwWindowShouldClose  (system.window) (game.should-exit?))
    )
  (pure
    (collect-wasd           (keys) (keys.wasd))
    (last-key               (last-direction keys.wasd) (last-direction))
    ((step-to 0)            tick.to-zero)
    (pure   *)
    (add1                      tick.iteration)
    (check-door-collision      (transform.x transform.y)  (collides?))
    ((if* (set-fsm 'top-map))  (collides? fsm)            (fsm))
    ((set-fsm 'experiment)      fsm)
    (any-direction-keys?       (keys)                 (any-direction-keys?))
    ((if* add1)       (any-direction-keys?
                       tick.direction-keys)
                      (tick.direction-keys))
    )
  (post
    (make-global-transform     (game.transform)            (system.transform))
    )
  (exit)
  )


(define (collect-wasd keys)
  (or
    (if (hash-ref keys 'a #f) 'a #f)
    (if (hash-ref keys 'd #f) 'd #f)
    (if (hash-ref keys 'w #f) 'w #f)
    (if (hash-ref keys 's #f) 's #f)))

(define (check-door-collision x y)
  (and (< -20 x 20) (> 130 y 100)))


(define (menu state)
  (info^ "Entering visualization state")
  (H~>
    state
    (show-visualization)  ; Blocks by using a subprogram

    ;; We need to reset the keys due to sticky keys,
    ;; if we didn't, we'd return to this function again.
    (get-keys   (system.window) (game.keys))
    (set-false  game.keys.escape)

    ;; Finally, indicate that we no longer want to loop here,
    ;; return from whence we came.
    (pop-fsm    game.fsm)
    ))
