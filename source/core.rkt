#lang racket

(provide breakpoint core)

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt" "drawing.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cleaning up state
;;
;; Used to clean up the state after a break exception has
;; been thrown. Intended to gracefully exit the application.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (glEnable GL_BLEND)
  (glEnable GL_MULTISAMPLE)
  (glBlendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA)
  (glDisable GL_DEPTH_TEST)
  (glClearColor 0.3 0.3 0.3 0.5)
  (glfwSetInputMode window GLFW_STICKY_KEYS GL_TRUE)
  window)

(define (initialize state)
  (H~>
    (hash)
    (initialize-glfw () (system.window))
    (add-sprites () (system.sprite))
  ))

(define (add-sprites)
  (list
    (draw-texture/uv "data/sprite2.png" '(0 0) '(1 1) '(0 0)    '(0.25 1))
    (draw-texture/uv "data/sprite2.png" '(0 0) '(1 1) '(0.25 0) '(0.50 1))
    (draw-texture/uv "data/sprite2.png" '(0 0) '(1 1) '(0.50 0) '(0.75 1))
    (draw-texture/uv "data/sprite2.png" '(0 0) '(1 1) '(0.75 0) '(1    1))))

(define (core state)
  (with-handlers* ([exn:break? (lambda (_) (break state cleanup))])
    (cond
      ([break-seen?]      (break state cleanup))
      ([empty? state]     (initialize state))
      ([nested-hash-ref state 'game 'should-exit?] (break state cleanup))
      (else               (core* state)))))

(define-syntax-parser map-glfw-keys
  ([_ key:id ...+]
   #:with (key* ...) (map (lambda (stx)
                            (~> stx
                                syntax-e
                                symbol->string
                                string-upcase
                                (string-replace "-" "_")
                                (string-append "GLFW_KEY_" _)
                                string->symbol
                                (datum->syntax stx _ stx)))
                          (attribute key))
   #'(lambda (window)
       (glfwPollEvents)
       (~>
         (hash)
         (hash-set 'key (not (zero? (glfwGetKey window key*)))) ...)
   )))

(define get-keys
  (map-glfw-keys left-control right-control w a s d up down left right))

;; Acts as glue between pure an impure. Mainly
(define (core* state)
  (H~> state
    (construct-matrix (game.translation) (system.translation))
    (impure   system)
    (get-keys (system.window) (game.keys))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (pure   game) ;; Pure game logic. All else is glue/impure ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; trce
  ))

(define (add1* n) (if n (add1 n) 0))

(define (construct-matrix trn)
  (dbug trn)
  (if trn
    (let ([x (hash-ref trn 'x 0)]
          [y (hash-ref trn 'y 0)])
      (matrix [[1 0 0 0]
               [0 1 0 0]
               [0 0 1 0]
               [(/ x 20) (/ y 20) 0 1]]))
    (identity-matrix 4)))

;; Handles all impure state changes
(define (impure state)
  (glClear GL_COLOR_BUFFER_BIT)
  (H~> state
    (add1* iter)
    (draw (translation sprite iter))
    (glfwSwapBuffers (window))
  ))

(define (draw global-trn sprites iter)
  ((list-ref sprites (floor (/ (modulo iter 40) 10))) 0)
  ((draw-texture "data/walking.png" '(-1 -1) '(0 0)) 1)
  ((draw-white-shape '((0.1 0.1 0.0)
                       (0.1 0.3 0.0)
                       (0.2 0.3 0.0)))
   0 0 #:translation global-trn)
  )

;; Handles all pure state changes
(define (pure state)
  (H~>
    state
    (check-C-W-exit (keys.left-control keys.w) (should-exit?))
    (add1-if-true (keys.w translation.y) (translation.y))
    (sub1-if-true (keys.a translation.x) (translation.x))
    (sub1-if-true (keys.s translation.y) (translation.y))
    (add1-if-true (keys.d translation.x) (translation.x))
    trce
  ))

(define (add1-if-true condition value)
  (if condition
    (if value
      (add1 value)
      0)
    (if value
      value
      0)))

(define (sub1-if-true condition value)
  (if condition
    (if value
      (sub1 value)
      0)
    (if value
      value
      0)))

(define (check-C-W-exit left-control w)
  (and left-control w))
