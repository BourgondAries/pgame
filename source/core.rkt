#lang racket

(provide breakpoint core)

(require racket/list syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt")

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

(define/memoize (load-shader* file shader-type) #:finalize (lambda (x) (erro x) (glDeleteShader x))
  (load-shader file shader-type))

(define/memoize (create-program* vertex fragment) #:finalize (lambda (x) (erro x) (glDeleteProgram x))
  (create-program vertex fragment))

(define/memoize (draw-white-shape points)
  (let* ([vertexarray   (glGenVertexArrays 1)]
         [vertexarray*  (u32vector-ref vertexarray 0)]
         [vertexbuffer  (glGenBuffers 1)]
         [vertexbuffer* (u32vector-ref vertexbuffer 0)]
         [program-id    (create-program* (load-shader* "source/shaders/shape.vertex.glsl"   GL_VERTEX_SHADER)
                                         (load-shader* "source/shaders/shape.fragment.glsl" GL_FRAGMENT_SHADER))]
         [move-loc      (glGetUniformLocation program-id "movement")]
         [point-length  (length points)]
         [points*       (list->f32vector (map real->single-flonum (flatten points)))])
    (glBindVertexArray vertexarray*)
    (glBindBuffer GL_ARRAY_BUFFER vertexbuffer*)
    (glBufferData GL_ARRAY_BUFFER
                  (* (length points) 3 4)
                  (f32vector->cpointer points*)
                  GL_STATIC_DRAW)
    (lambda (x y #:translation [translation (identity-matrix 4)])
      (glUseProgram (create-program* (load-shader* "source/shaders/shape.vertex.glsl"   GL_VERTEX_SHADER)
                                     (load-shader* "source/shaders/shape.fragment.glsl" GL_FRAGMENT_SHADER)))
      (glEnableVertexAttribArray 0)
      (glBindBuffer GL_ARRAY_BUFFER vertexbuffer*)
      (glVertexAttribPointer
        0
        3
        GL_FLOAT
        #f
        0
        #f)
      (glUniformMatrix4fv move-loc 1 #f
                          (list->f32vector
                            (map real->single-flonum
                              (matrix->list
                                (matrix* translation
                                         (matrix [[1.0 0 0 0]
                                                  [0 1.0 0 0]
                                                  [0 0 1.0 0]
                                                  [x y 0 1]]))))))
      (glDrawArrays GL_TRIANGLES 0 point-length)
      (glDisableVertexAttribArray 0)
      )))

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
    (draw (translation))
    (glfwSwapBuffers (window))
  ))

(define (draw global-trn)
  (info global-trn)
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
