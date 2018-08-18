#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Procedures for initializing the entire state
;;
;; Ensures that the state is set up before running
;; the rest of the program.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt" "drawing.rkt" "pure.rkt")

(define (initialize-glfw)
  (when (= (glfwInit) GLFW_FALSE)
    (ftal^ "glfwInit returned non-success code")
    (exit 1))
  (define window (glfwCreateWindow 800 600 "Example Window" #f #f))
  (glfwMakeContextCurrent window)
  (glfwWindowHint GLFW_SAMPLES 4)
  (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 3)
  (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 3)
  (glfwWindowHint GLFW_OPENGL_FORWARD_COMPAT GL_TRUE)
  (glfwWindowHint GLFW_OPENGL_PROFILE GLFW_OPENGL_CORE_PROFILE)
  (glViewport 0 0 800 600)
  (glEnable GL_BLEND)
  (glEnable GL_MULTISAMPLE)
  (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
  (glDisable GL_DEPTH_TEST)
  (glClearColor 0.3 0.8 0.3 0.)
  (glfwSetInputMode window GLFW_STICKY_KEYS GL_TRUE)
  ; (glPolygonMode GL_FRONT_AND_BACK GL_LINE)
  (glPolygonMode GL_FRONT GL_FILL)
  ;;
  (collect-garbage 'incremental)
  ;;
  window)

(define (initialize state)
  (H~>
    (hash)
    (initialize-glfw    () (system.window))
    (add-sprites*       () (system.animation.madotsuki))
    ((const '())        game.fsm)
    ;((push-fsm 'core*)  game.fsm)
    ((push-fsm 'core*)  game.fsm)
    ((const 0)          game.tick.direction-keys)
    ((const 0)          game.tick.iteration)
  ))

(define (add-sprites*)
  (list
    ;; Walking down
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.00 0.75) '(0.25 1.00)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.25 0.75) '(0.50 1.00)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.50 0.75) '(0.75 1.00)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.75 0.75) '(1.00 1.00)))

    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.25 0.50) '(0.00 0.75)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.50 0.50) '(0.25 0.75)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.75 0.50) '(0.50 0.75)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(1.00 0.50) '(0.75 0.75)))

    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.25 0.25) '(0.00 0.50)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.50 0.25) '(0.25 0.50)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.75 0.25) '(0.50 0.50)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(1.00 0.25) '(0.75 0.50)))

    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.00 0.50) '(0.25 0.75)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.25 0.50) '(0.50 0.75)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.50 0.50) '(0.75 0.75)))
    (thunk (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.75 0.50) '(1.00 0.75)))
    ))
