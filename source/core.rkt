#lang racket

(provide breakpoint core)

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list)
         finalizer
         glfw3 logger memo nested-hash opengl spipe
         "breakpoint.rkt")

(define (cleanup state)
  (H~>
    state
    (glfwDestroyWindow system.window)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (glDisable GL_DEPTH_TEST)
  (glClearColor 0.3 0.3 0.3 0.5)
  (glfwSetInputMode window GLFW_STICKY_KEYS GL_TRUE)
  window)

(define (initialize state)
  (H~>
    (hash)
    (initialize-glfw () (system.window))
  ))

(define (core state)
  (with-handlers* ([exn:break? (lambda (_) (break state cleanup))])
    (cond
      ([break-seen?]      (break state cleanup))
      ([empty? state]     (initialize state))
      (else               (core* state)))))

;; Acts as glue between pure an impure. Mainly 
(define (core* state)
  (H~> state
    (impure system)
    (pure   game)
  ))

;; Handles all impure state changes
(define (impure state)
  (H~> state
    (glfwSwapBuffers (window))
  ))

;; Handles all pure state changes
(define (pure state)
  state
  )
