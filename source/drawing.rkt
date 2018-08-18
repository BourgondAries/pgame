#lang racket/base

(provide (all-defined-out))

(require racket/list
         ffi/vector finalizer math/matrix opengl opengl/util
         "pure.rkt"
         logger memo)

(define size-gl-float 4)

(define/memoize (load-shader* file shader-type) #:finalize glDeleteShader
  (load-shader file shader-type))

(define/memoize (create-program* vertex fragment) #:finalize glDeleteProgram
  (create-program vertex fragment))

(define (fade/invert value)
  ((fade) (max 0 (/ value 120))))

(define/memoize-zero fade
  (let* ([vertexarray   (u32vector-ref   (glGenVertexArrays 1) 0)]
         [vertexbuffer  (u32vector-ref   (glGenBuffers      1) 0)]
         [program-id    (create-program* (load-shader* "source/shaders/fade.vertex.glsl"   GL_VERTEX_SHADER)
                                         (load-shader* "source/shaders/fade.fragment.glsl" GL_FRAGMENT_SHADER))]
         [opacity       (glGetUniformLocation program-id "opacity")]
         [points        '((-1 -1) (-1 1) (1 -1) (1 -1)  (-1 1) (1 1))]
         [point-length  (length points)]
         [points*       (list->f32vector (map real->single-flonum (flatten points)))])
    (register-finalizer vertexarray  (lambda (x) (glDeleteVertexArrays 1 (u32vector x))))
    (register-finalizer vertexbuffer (lambda (x) (glDeleteBuffers      1 (u32vector x))))
    ; (glBindVertexArray vertexarray)
    (glBindBuffer GL_ARRAY_BUFFER vertexbuffer)
    (glBufferData GL_ARRAY_BUFFER
                  (* (length points) 2 size-gl-float)
                  (f32vector->cpointer points*)
                  GL_STATIC_DRAW)
    (lambda (alpha)
      (glUseProgram (create-program* (load-shader* "source/shaders/fade.vertex.glsl"   GL_VERTEX_SHADER)
                                     (load-shader* "source/shaders/fade.fragment.glsl" GL_FRAGMENT_SHADER)))
      (glEnableVertexAttribArray   0)
      (glBindBuffer                GL_ARRAY_BUFFER vertexbuffer)
      (glVertexAttribPointer       0 2 GL_FLOAT #f 0 #f)
      (glUniform1f                 opacity (real->double-flonum alpha))
      (glDrawArrays                GL_TRIANGLES 0 point-length)
      (glDisableVertexAttribArray  0))))

;; Draws a white shape, takes in a set of triangles.
(define/memoize (draw-white-shape points)
  (let* ([vertexarray   (u32vector-ref (glGenVertexArrays 1) 0)]
         [vertexbuffer  (u32vector-ref (glGenBuffers      1) 0)]
         ;;
         [program-id    (create-program* (load-shader* "source/shaders/shape.vertex.glsl"   GL_VERTEX_SHADER)
                                         (load-shader* "source/shaders/shape.fragment.glsl" GL_FRAGMENT_SHADER))]
         ;;
         [move-loc      (glGetUniformLocation program-id "movement")]
         [point-length  (length points)]
         [points*       (list->f32vector (map real->single-flonum (flatten points)))])
    (register-finalizer vertexarray  (lambda (x) (glDeleteVertexArrays 1 (u32vector x))))
    (register-finalizer vertexbuffer (lambda (x) (glDeleteBuffers      1 (u32vector x))))
    (glBindVertexArray vertexarray)
    (glBindBuffer GL_ARRAY_BUFFER vertexbuffer)
    (glBufferData GL_ARRAY_BUFFER
                  (* (length points) 3 4)
                  (f32vector->cpointer points*)
                  GL_STATIC_DRAW)
    (lambda (x y #:translation [translation (identity-matrix 4)])
      (glUseProgram (create-program* (load-shader* "source/shaders/shape.vertex.glsl"   GL_VERTEX_SHADER)
                                     (load-shader* "source/shaders/shape.fragment.glsl" GL_FRAGMENT_SHADER)))
      (glEnableVertexAttribArray 0)
      (glBindBuffer GL_ARRAY_BUFFER vertexbuffer)
      (glVertexAttribPointer 0 3 GL_FLOAT #f 0 #f)
      (glUniformMatrix4fv move-loc 1 #f
                          (list->f32vector
                            (map real->single-flonum
                              (matrix->list
                                (matrix* translation
                                         (matrix [[1.0 0 0 0]
                                                  [0 1.0 0 0]
                                                  [0 0 1.0 0]
                                                  [x y 0 1]]))))))
      (glDrawArrays GL_TRIANGLES  0 point-length)
      (glDisableVertexAttribArray 0)
      )))

(define (draw-previous-frame x y)
  (define tex (create-blank-texture x y))
  (glReadBuffer GL_BACK)
  (glCopyTexSubImage2D GL_TEXTURE_2D 0 0 0 0 0 x y)
  (lambda ()
    (draw-texture-obj tex)))

(define (create-blank-texture w h)
  (define tex (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D tex)
  (glPixelStorei GL_UNPACK_ALIGNMENT 1)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  ; (glTexEnvf GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE 0)
  tex)

(define (draw-texture-obj tx)
  (define program-id (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                 (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
  (glUseProgram program-id)
  (define points* (list->f32vector (map real->single-flonum '(
                                            -1 -1    0  0
                                            -1  1    0  1
                                             1 -1    1  0
                                            -1  1    0  1
                                             1  1    1  1
                                             1 -1    1  0
                                            ))))
  (define tex-loc (glGetUniformLocation program-id "texture"))
  (define move-loc (glGetUniformLocation program-id "movement"))
  (define vertexbuffer  (u32vector-ref (glGenBuffers 1) 0))
  ; (glBindVertexArray vertexarray)
  (glBindBuffer GL_ARRAY_BUFFER vertexbuffer)
  (glBufferData GL_ARRAY_BUFFER
                (* (f32vector-length points*) 4)
                (f32vector->cpointer points*)
                GL_STATIC_DRAW)
  (glEnableVertexAttribArray 0)
  (glEnableVertexAttribArray 1)
  (glBindBuffer GL_ARRAY_BUFFER vertexbuffer)
  (glVertexAttribPointer 0 2 GL_FLOAT #f 16 #f)
  (glVertexAttribPointer 1 2 GL_FLOAT #f 16 8)

  (glActiveTexture GL_TEXTURE0)
  (glUniform1i tex-loc #|GL_TEXTURE|# 0)
  (glBindTexture GL_TEXTURE_2D tx)

  (glUniformMatrix4fv move-loc 1 #f
                      (list->f32vector
                        (map real->single-flonum
                          (matrix->list
                            (matrix* (matrix [[1.0 0 0 0]
                                              [0 1.0 0 0]
                                              [0 0 1.0 0]
                                              [0 0 0 1]]))))))
  (glDrawArrays GL_TRIANGLES 0 6)

  (glDisableVertexAttribArray 1)
  (glDisableVertexAttribArray 0))

(define/memoize (load-texture* file) #:finalize (lambda (x) (glDeleteTextures 1 (u32vector x)) (erro 'del) (exit))
  (load-texture file #:mipmap #t))

(define (rectangle->f32vector bottom-left top-right)
  (define lx (real->single-flonum (first bottom-left)))
  (define rx (real->single-flonum (first top-right)))
  (define ty (real->single-flonum (second top-right)))
  (define by (real->single-flonum (second bottom-left)))
    (f32vector lx ty 0f0 0f0
               rx ty 1f0 0f0
               lx by 0f0 1f0

               lx by 0f0 1f0
               rx ty 1f0 0f0
               rx by 1f0 1f0))

(define (rectangle->f32vector/uv bottom-left    top-right
                                 bottom-left-uv top-right-uv)
  (define lx (real->single-flonum (first bottom-left)))
  (define rx (real->single-flonum (first top-right)))
  (define ty (real->single-flonum (second top-right)))
  (define by (real->single-flonum (second bottom-left)))

  (define lx* (real->single-flonum (first bottom-left-uv)))
  (define rx* (real->single-flonum (first top-right-uv)))
  (define ty* (real->single-flonum (- 1 (second top-right-uv))))
  (define by* (real->single-flonum (- 1 (second bottom-left-uv))))
    (f32vector lx ty lx* ty*
               rx ty rx* ty*
               lx by lx* by*

               lx by lx* by*
               rx ty rx* ty*
               rx by rx* by*))

(define/memoize-partial draw-tiles (file horizontal-panes vertical-panes tiles transform) (x)
  ((define runs (animate-texture file '(-1 -1) '(1 1) horizontal-panes vertical-panes))
   (define fbo (u32vector-ref (glGenFramebuffers 1) 0))
   (glBindFramebuffer GL_FRAMEBUFFER fbo)

   (define tex (create-blank-texture 800 600))
   (glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 tex 0)
   (when (not (= GL_FRAMEBUFFER_COMPLETE (glCheckFramebufferStatus GL_FRAMEBUFFER)))
     (ftal^ "Unable to create framebuffer")
     (exit 3))
   (glBindFramebuffer GL_FRAMEBUFFER fbo)
   (glViewport 0 0 800 600)
   (for/fold ([y* 0])
             ([tile-line tiles])
     (for/fold ([x* 0])
               ([tile tile-line])
       (let-values ([(x y) (ticker tile horizontal-panes vertical-panes)])
         (runs x y #:transform (matrix-transpose (matrix* transform (matrix-transpose (translate x* y*)))))
         (+ 2 x*)))
     (+ 2 y*))
    (glBindFramebuffer GL_FRAMEBUFFER 0)
    (glViewport 0 0 800 600))
  (
   (draw-texture-obj tex)
   ))

(define (scale c)
  (let ([c (real->single-flonum c)])
    (matrix* (matrix [[c 0 0 0]
                      [0 c 0 0]
                      [0 0 c 0]
                      [0 0 0 1f0]]))))

(define/memoize (animate-texture source bottom-left top-right horizontal-panes vertical-panes)
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

(define/memoize-partial draw-texture (file bottom-left top-right) (x)
  ((define tex (load-texture* file))
   (define vertexarray   (u32vector-ref (glGenVertexArrays 1) 0))
   (define vertexbuffer  (u32vector-ref (glGenBuffers 1) 0))
   (define program-id    (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                   (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
   (define move-loc      (glGetUniformLocation program-id "movement"))
   (define tex-loc       (glGetUniformLocation program-id "texture"))
   (define points*       (rectangle->f32vector bottom-left top-right))
    (register-finalizer tex (lambda (x) (glDeleteBuffers 1 (u32vector x))))
    (register-finalizer vertexarray
                        (lambda (x)
                          (trce `(gldelet vertex))
                          (exit 99)
                          (glDeleteVertexArrays 1 (u32vector x))))
    (register-finalizer vertexbuffer
                        (lambda (x)
                          (trce `(gldelet))
                          (glDeleteBuffers 1 (u32vector x))))

    (glBindVertexArray vertexarray)
    (glBindBuffer GL_ARRAY_BUFFER vertexbuffer)
    (glBufferData GL_ARRAY_BUFFER
                  (* (f32vector-length points*) 4)
                  (f32vector->cpointer points*)
                  GL_STATIC_DRAW)

    (glBindTexture GL_TEXTURE_2D tex)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER)

    ;; Border color if clamp-to-border
    ; (glTexParameterfv GL_TEXTURE_2D GL_TEXTURE_BORDER_COLOR {1 0 0 1})

    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)

    (glGenerateMipmap GL_TEXTURE_2D))
   (
    (glUseProgram (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                     (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))

      (glEnableVertexAttribArray 0)
      (glEnableVertexAttribArray 1)
      (glBindBuffer GL_ARRAY_BUFFER vertexbuffer)
      (glVertexAttribPointer 0 2 GL_FLOAT #f 16 #f)
      (glVertexAttribPointer 1 2 GL_FLOAT #f 16 8)

      (glActiveTexture GL_TEXTURE0)
      (glUniform1i tex-loc #|GL_TEXTURE|# 0)
      (glBindTexture GL_TEXTURE_2D tex)

      (glUniformMatrix4fv move-loc 1 #f
                          (list->f32vector
                            (map real->single-flonum
                              (matrix->list
                                (matrix* (matrix [[1.0 0 0 0]
                                                  [0 1.0 0 0]
                                                  [0 0 1.0 0]
                                                  [0 0 0 1]]))))))
      (glDrawArrays GL_TRIANGLES 0 6)

      (glDisableVertexAttribArray 1)
      (glDisableVertexAttribArray 0)
      ))

(define (translate x y)
  (matrix [[1 0 0 0]
           [0 1 0 0]
           [0 0 1 0]
           [x y 0 1]]))

(define/memoize (draw-text text-sheet
                           bottom-left  top-right
                           horizontal   vertical)
  (let ([animation (animate-texture text-sheet bottom-left top-right horizontal vertical)]
        [x-width (- (first top-right) (first bottom-left))]
        [height (- (second top-right) (second bottom-left))])
    (lambda (text)
      (for/fold ([nl 0]
                 [x 0])
                ([ch text]
                 [n (in-naturals)])
        (if (char=? ch #\newline)
          (values (add1 nl) 0)
          (let* ([i (modulo (- (char->integer ch) 32) horizontal)]
                 [j (sub1 (- vertical (floor (/ (- (char->integer ch) 32) horizontal))))])
            (animation i j #:transform (translate (* x x-width) (* -1 nl height)))
            (values nl (add1 x))
             ))))))


(define/memoize (draw-texture/uv file bottom-left    top-right
                                      bottom-left-uv top-right-uv)
  (let* ([tex           (load-texture* file)]
         [vertexarray   (u32vector-ref (glGenVertexArrays 1) 0)]
         [vertexbuffer  (u32vector-ref (glGenBuffers      1) 0)]
         [program-id    (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                         (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER))]
         [move-loc      (glGetUniformLocation program-id "movement")]
         [tex-loc       (glGetUniformLocation program-id "texture")]
         [points*       (rectangle->f32vector/uv bottom-left    top-right
                                                 bottom-left-uv top-right-uv)])
    (register-finalizer tex (lambda (x) (glDeleteBuffers 1 (u32vector x))))
    (register-finalizer vertexarray
                        (lambda (x)
                          (trce `(gldelet vertex))
                          (exit 99)
                          (glDeleteVertexArrays 1 (u32vector x))))
    (register-finalizer vertexbuffer
                        (lambda (x)
                          (trce `(gldelet))
                          (glDeleteBuffers 1 (u32vector x))))

    (glBindVertexArray vertexarray)
    (glBindBuffer GL_ARRAY_BUFFER vertexbuffer)
    (glBufferData GL_ARRAY_BUFFER
                  (* (f32vector-length points*) 4)
                  (f32vector->cpointer points*)
                  GL_STATIC_DRAW)

    (glBindTexture GL_TEXTURE_2D tex)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER)

    ;; Border color if clamp-to-border
    ; (glTexParameterfv GL_TEXTURE_2D GL_TEXTURE_BORDER_COLOR {1 0 0 1})

    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)

    (glGenerateMipmap GL_TEXTURE_2D)
    (lambda (#:transform [transformation-matrix (identity-matrix 4)])
      (glUseProgram (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                     (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))

      (glEnableVertexAttribArray 0)
      (glEnableVertexAttribArray 1)
      (glBindBuffer GL_ARRAY_BUFFER vertexbuffer)
      (glVertexAttribPointer 0 2 GL_FLOAT #f 16 #f)
      (glVertexAttribPointer 1 2 GL_FLOAT #f 16 8)

      (glActiveTexture GL_TEXTURE0)
      (glUniform1i tex-loc #|GL_TEXTURE|# 0)
      (glBindTexture GL_TEXTURE_2D tex)

      (glUniformMatrix4fv move-loc 1 #f
                          (list->f32vector
                            (map real->single-flonum
                              (matrix->list
                                transformation-matrix))))
      (glDrawArrays GL_TRIANGLES 0 6)

      (glDisableVertexAttribArray 1)
      (glDisableVertexAttribArray 0)
      )))

