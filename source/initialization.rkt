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
  (glClearColor 0.3 0.3 0.3 0.5)
  (glfwSetInputMode window GLFW_STICKY_KEYS GL_TRUE)
  ; (glPolygonMode GL_FRONT_AND_BACK GL_LINE)
  ;;
  (collect-garbage 'incremental)
  ;;
  window)

(define (initialize state)
  (H~>
    (hash)
    (initialize-glfw () (system.window))
    (add-sprites     () (system.sprite))
    (add-sprites*    () (system.animation.madotsuki))
    ((const 0)       system.iter)
  ))

(define (add-sprites)
  (list
    (draw-texture/uv "data/sprite2.png" '(0 0) '(1 1) '(0    0) '(0.25 1))
    (draw-texture/uv "data/sprite2.png" '(0 0) '(1 1) '(0.25 0) '(0.50 1))
    (draw-texture/uv "data/sprite2.png" '(0 0) '(1 1) '(0.50 0) '(0.75 1))
    (draw-texture/uv "data/sprite2.png" '(0 0) '(1 1) '(0.75 0) '(1    1))))

(define (animate-texture source bottom-left top-right horizontal-panes vertical-panes)
  (let ([runs
    (flatten
      (for/list ([i horizontal-panes])
        (for/list ([j vertical-panes])
          (draw-texture/uv source bottom-left top-right (list (/ i horizontal-panes)
                                                              (/ j vertical-panes))
                                                        (list (/ (add1 i) horizontal-panes)
                                                              (/ (add1 j) vertical-panes))))))])
    (lambda (i j #:transform [transform (identity-matrix 4)])
      ((list-ref runs (+ (modulo j vertical-panes) (* vertical-panes (modulo i horizontal-panes)))) #:transform transform)
      )))

(define (add-sprites*)
  (list
    ;; Walking down
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.00 0.75) '(0.25 1.00))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.25 0.75) '(0.50 1.00))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.50 0.75) '(0.75 1.00))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.75 0.75) '(1.00 1.00))

    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.25 0.50) '(0.00 0.75))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.50 0.50) '(0.25 0.75))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.75 0.50) '(0.50 0.75))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(1.00 0.50) '(0.75 0.75))

    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.25 0.25) '(0.00 0.50))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.50 0.25) '(0.25 0.50))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.75 0.25) '(0.50 0.50))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(1.00 0.25) '(0.75 0.50))

    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.00 0.50) '(0.25 0.75))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.25 0.50) '(0.50 0.75))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.50 0.50) '(0.75 0.75))
    (draw-texture/uv "data/Madotsuki.png" '(-0.7 -1) '(0.7 1) '(0.75 0.50) '(1.00 0.75))
    ))
