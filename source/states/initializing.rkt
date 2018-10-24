#lang racket

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "core.rkt" "experimental.rkt"
         "../drawing.rkt" "../impure.rkt" "../pure.rkt" "../shutdown.rkt" "../state.rkt")

(provide (all-defined-out))

(define (initializing)
  (H~>
    (hasheq)
    info
    (initialize-glfw    () (io.window))
    (context (io.window)
      (get-window-size          ()      (io.window-size.width io.window-size.height))
      )
    (add-sprites*       () (io.animation.madotsuki))
    ((const '())        ae.fsm)
    ((const 0)          ae.tick.direction-keys)
    ((const 0)          ae.tick.iteration)
    ((const 0)          ae.rotation)
    ((const (list experimental))     io.core)
  ))

(define (add-tick s)
  (H~> s (add1 ae.tick.iteration)))

(define (initialize-glfw)
  (when (= (glfwInit) GLFW_FALSE)
    (ftal^ "glfwInit returned non-success code")
    (exit 1))
  (define window (glfwCreateWindow 800 600 "Example Window" #f #f))
  (glfwMakeContextCurrent window)
  (info (glGetString GL_VERSION))
  (info (s32vector->list (glGetIntegerv GL_MAJOR_VERSION)))
  (info (s32vector->list (glGetIntegerv GL_MINOR_VERSION)))
  (glfwWindowHint GLFW_SAMPLES 4)
  (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 4)
  (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 5)
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
  (collect-garbage 'incremental)
  window)

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
