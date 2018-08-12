#lang racket/base

(provide draw-white-shape
         draw-texture
         draw-texture/uv)

(require racket/list
         ffi/vector finalizer math/matrix opengl opengl/util
         logger memo)

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
    (register-finalizer vertexarray*
                        (lambda (x)
                          (trce `(gldelet vertex))
                          (exit 99)
                          (glDeleteVertexArrays 1 (u32vector x))))
    (register-finalizer vertexbuffer*
                        (lambda (x)
                          (trce `(gldelet))
                          (glDeleteBuffers 1 (u32vector x))))
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

(define (load-texture* file)
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
               rx by 1f0 1f0)
    )

(define (rectangle->f32vector/uv bottom-left    top-right
                                 bottom-left-uv top-right-uv)
  (define lx (real->single-flonum (first bottom-left)))
  (define rx (real->single-flonum (first top-right)))
  (define ty (real->single-flonum (second top-right)))
  (define by (real->single-flonum (second bottom-left)))

  (define lx* (real->single-flonum (first bottom-left-uv)))
  (define rx* (real->single-flonum (first top-right-uv)))
  (define ty* (real->single-flonum (second top-right-uv)))
  (define by* (real->single-flonum (second bottom-left-uv)))
    (f32vector lx ty lx* by*
               rx ty rx* by*
               lx by lx* ty*

               lx by lx* ty*
               rx ty rx* by*
               rx by rx* ty*)
    )

(define/memoize (draw-texture file bottom-left top-right)
  (let* ([tex (load-texture* file)]
         [vertexarray   (u32vector-ref (glGenVertexArrays 1) 0)]
         [vertexbuffer  (u32vector-ref (glGenBuffers 1) 0)]
         [program-id    (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                         (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER))]
         [move-loc      (glGetUniformLocation program-id "movement")]
         [tex-loc       (glGetUniformLocation program-id "texture")]
         [points*       (rectangle->f32vector bottom-left top-right)])
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
    (lambda (x)
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
      )))

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
    (lambda (x)
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
      )))

