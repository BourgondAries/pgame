#lang racket/base

(provide (all-defined-out))

(require racket/list
         ffi/vector finalizer math/matrix opengl opengl/util
         "pure.rkt"
         "../../memo/main.rkt"
         logger )

(define size-gl-float 4)

;;; Utility functions for defining transformation matrices
(define (scale c)
  (let ([c (real->single-flonum c)])
    (matrix* (matrix [[c 0 0 0]
                      [0 c 0 0]
                      [0 0 c 0]
                      [0 0 0 1f0]]))))

(define (translate x y)
  (matrix [[1 0 0 0]
           [0 1 0 0]
           [0 0 1 0]
           [x y 0 1]]))

(define/memoize (load-shader* file shader-type) #:finalize glDeleteShader
  (load-shader file shader-type))

(define/memoize (create-program* vertex fragment) #:finalize glDeleteProgram
  (create-program vertex fragment))

(define (fade/invert value)
  (fade (max 0 (/ value 120))))

;; Global view that affects most drawing functions
;; The reason for using a global here is to avoid passing
;; a view so often, this makes it easier to simply do
;; a parameterization that stacks with the previous view
(define *view* (make-parameter (matrix [[1 0 0 0]
                                        [0 1 0 0]
                                        [0 0 1 0]
                                        [0 0 0 1]])))

;; Turning the view into a f32vector is remarkably slow
;; this speeds it up by 2 orders of magnitude
(define/memoize (matrix->f32vector view)
  (list->f32vector
    (map real->single-flonum
      (matrix->list view))))

(define (get-view)
  (matrix->f32vector (*view*)))

(define/memoize-partial fade () (alpha)
  ((define vertexarray   (u32vector-ref   (glGenVertexArrays 1) 0))
   (register-finalizer          vertexarray  (lambda (x) (glDeleteVertexArrays 1 (u32vector x))))
   (define vertexbuffer  (u32vector-ref   (glGenBuffers      1) 0))
   (define program-id    (create-program* (load-shader* "source/shaders/fade.vertex.glsl"   GL_VERTEX_SHADER)
                                          (load-shader* "source/shaders/fade.fragment.glsl" GL_FRAGMENT_SHADER)))
   (define opacity       (glGetUniformLocation program-id "opacity"))
   (define points               '((-1 -1) (-1 1) (1 -1) (1 -1)  (-1 1) (1 1)))
   (define point-length         (length points))
   (define points*              (list->f32vector (map real->single-flonum (flatten points))))
   (define vertex-location      (glGetAttribLocation program-id "vertex"))
   (define dim-vertex           2)
   (glBindVertexArray           vertexarray)
   (glBindBuffer                GL_ARRAY_BUFFER  vertexbuffer)
   (glVertexAttribPointer       vertex-location dim-vertex GL_FLOAT #f 0 #f)
   (glBufferData                GL_ARRAY_BUFFER
                                (* (length points) dim-vertex size-gl-float)
                                (f32vector->cpointer points*)
                                GL_STATIC_DRAW)
   (glEnableVertexAttribArray   vertex-location)
   (glBindVertexArray           0)  ; Unbind the VAO, so other draw calls do not affect this one
   ;; The buffer is referenced by the container object (VAO), so it is not deleted from memory, and this is safe (MUST be after unbinding the VAO)
   ;; See OpenGL 4.5 spec.: 5.1.2 (Automatic Unbinding of Deleted Objects) and 5.1.3 (Deleted Object and Object Name Lifetimes)
   (glDeleteBuffers             1 (u32vector vertexbuffer))
   )
  ;; TODO May be useful to cache the state of glUseProgram too, as to not switch shaders too often
  ((glUseProgram (create-program* (load-shader* "source/shaders/fade.vertex.glsl"   GL_VERTEX_SHADER)
                                  (load-shader* "source/shaders/fade.fragment.glsl" GL_FRAGMENT_SHADER)))
   (glBindVertexArray           vertexarray)
   ; (glEnableVertexAttribArray   0)  ; Not needed, is inside the VAO
   (glUniform1f                 opacity (real->double-flonum alpha))
   (glDrawArrays                GL_TRIANGLES 0 point-length)
   ; (glDisableVertexAttribArray  0)  ; Not needed, is inside the VAO
   (glBindVertexArray           0)    ; Unbinds the VAO
   ))

;; Draws a white shape, takes in a set of triangles and vec4 color
(define/memoize-partial draw-shape (points color) ()
  ((define vertexarray    (u32vector-ref (glGenVertexArrays 1) 0))
   (register-finalizer          vertexarray  (lambda (x) (glDeleteVertexArrays 1 (u32vector x))))
   ;; TODO perhaps use glGenBuffers 2 for both vertex and color data
   (define vertexbuffer   (u32vector-ref (glGenBuffers      1) 0))
   (define colorbuffer    (u32vector-ref (glGenBuffers      1) 0))
   (define program-id     (create-program* (load-shader* "source/shaders/shape.vertex.glsl"   GL_VERTEX_SHADER)
                                           (load-shader* "source/shaders/shape.fragment.glsl" GL_FRAGMENT_SHADER)))
   (define move-loc       (glGetUniformLocation program-id "movement"))
   (define point-length   (length points))
   (define points*        (list->f32vector (map real->single-flonum (flatten points))))
   (define color-length         (length color))
   (define color*               (list->f32vector (map real->single-flonum (flatten color))))
   (define vertex-location      (glGetAttribLocation program-id "vertex"))
   (define color-location       (glGetAttribLocation program-id "color"))
   (define dim-vertex           2)  ; Dimension of vertices
   (define dim-color            4)  ; Dimension of color
   (glBindVertexArray           vertexarray)
   (glBindBuffer                GL_ARRAY_BUFFER vertexbuffer)
   (glBufferData                GL_ARRAY_BUFFER
                                (* (length points) dim-vertex size-gl-float)
                                (f32vector->cpointer points*)
                                GL_STATIC_DRAW)
   (glVertexAttribPointer       vertex-location dim-vertex GL_FLOAT #f 0 #f)
   (glEnableVertexAttribArray   vertex-location)
   (glBindBuffer                GL_ARRAY_BUFFER  colorbuffer)
   (glBufferData                GL_ARRAY_BUFFER
                                (* (length color) dim-color size-gl-float)
                                (f32vector->cpointer color*)
                                GL_STATIC_DRAW)
   (glVertexAttribPointer       color-location dim-color GL_FLOAT #f 0 #f)
   (glEnableVertexAttribArray   color-location)
   (glBindVertexArray           0)
   (glDeleteBuffers             1 (u32vector vertexbuffer))
   (glDeleteBuffers             1 (u32vector colorbuffer))
   )
  ((glUseProgram (create-program* (load-shader* "source/shaders/shape.vertex.glsl"   GL_VERTEX_SHADER)
                                  (load-shader* "source/shaders/shape.fragment.glsl" GL_FRAGMENT_SHADER)))
   (glBindVertexArray    vertexarray)
   (glUniformMatrix4fv   move-loc 1 #f (get-view))
   (glDrawArrays         GL_TRIANGLES 0 point-length)
   (glBindVertexArray    0)
   ))

;; Used mainly during transitions
(define (get-previous-frame width height)
  (define texture        (create-blank-texture width height))
  (glReadBuffer          GL_BACK)
  (glCopyTexSubImage2D   GL_TEXTURE_2D 0 0 0 0 0 width height)
  texture)

;; The returned texture is registered in the GC and will be dealt with gracefully
(define (create-blank-texture width height)
  (define texture    (u32vector-ref (glGenTextures 1) 0))
  (register-finalizer texture (lambda (x) (glDeleteTextures 1 (u32vector x))))
  (glBindTexture     GL_TEXTURE_2D texture)
  (glPixelStorei     GL_UNPACK_ALIGNMENT 1)
  (glTexParameteri   GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (glTexParameteri   GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (glTexParameteri   GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri   GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
  (glTexImage2D      GL_TEXTURE_2D 0 GL_RGBA width height 0 GL_RGBA GL_UNSIGNED_BYTE 0)
  texture)

;; Useful routine for drawing the previous frame, which just takes over the screen
(define/memoize-partial draw-texture-fullscreen () (texture)
  ((define program-id (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                       (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
   (define points* (list->f32vector (map real->single-flonum (flatten '((-1 -1) (0 0)
                                                                        (-1  1) (0 1)
                                                                        ( 1 -1) (1 0)
                                                                        (-1  1) (0 1)
                                                                        ( 1  1) (1 1)
                                                                        ( 1 -1) (1 0))))))
   (define tex-loc        (glGetUniformLocation program-id "texture"))
   (define move-loc       (glGetUniformLocation program-id "movement"))
   (define vertex         (glGetAttribLocation program-id "vertex"))
   (define vertex-uv      (glGetAttribLocation program-id "vertex_uv"))
   (define vertexarray    (u32vector-ref (glGenVertexArrays 1) 0))
   (define vertexbuffer   (u32vector-ref (glGenBuffers 1) 0))
   (define dim            2)
   (glBindVertexArray           vertexarray)
   (glBindBuffer                GL_ARRAY_BUFFER vertexbuffer)
   (glBufferData                GL_ARRAY_BUFFER
                                (* (f32vector-length points*) size-gl-float)
                                (f32vector->cpointer points*)
                                GL_STATIC_DRAW)
   (glEnableVertexAttribArray   vertex)
   (glEnableVertexAttribArray   vertex-uv)
   (glVertexAttribPointer       vertex     dim GL_FLOAT #f 16 #f) ; TODO use calculations to get to 16 and 8
   (glVertexAttribPointer       vertex-uv  dim GL_FLOAT #f 16  8)
   (glBindVertexArray           0)
   (glDeleteBuffers             1 (u32vector vertexbuffer))
  )
  ((glUseProgram (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                  (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
   (glBindVertexArray    vertexarray)
   (glActiveTexture      GL_TEXTURE0)
   (glUniform1i          tex-loc #|GL_TEXTURE|# 0)
   (glBindTexture        GL_TEXTURE_2D texture)
   (glUniformMatrix4fv   move-loc 1 #f (get-view))
   (glDrawArrays         GL_TRIANGLES 0 6)
   (glBindVertexArray    0)
  ))

(define/memoize (load-texture* file) #:finalize (lambda (x) (glDeleteTextures 1 (u32vector x)))
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

;; TODO better way to draw tiles? instancing?
(define/memoize-partial draw-tiles (file horizontal-panes vertical-panes tiles transform) (x)
  ((define runs  (animate-texture file '(-1 -1) '(1 1) horizontal-panes vertical-panes))
   (define fbo   (u32vector-ref (glGenFramebuffers 1) 0))
   (glBindFramebuffer GL_FRAMEBUFFER fbo)
   (define tex (create-blank-texture 800 600))
   (glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 tex 0)
   (when (not (= GL_FRAMEBUFFER_COMPLETE (glCheckFramebufferStatus GL_FRAMEBUFFER)))
     (ftal^ "Unable to create framebuffer")
     (exit 3))
   (glViewport 0 0 800 600)
   (for/fold ([y* 0])
             ([tile-line tiles])
     (for/fold ([x* 0])
               ([tile tile-line])
       (let-values ([(x y) (ticker tile horizontal-panes vertical-panes)])
         (parameterize ([*view* (matrix* (matrix-transpose (matrix* transform (matrix-transpose (translate x* y*)))) (*view*))])
         (runs x y))
         (+ 2 x*)))
     (+ 2 y*))
    (glBindFramebuffer GL_FRAMEBUFFER 0)
    (glViewport 0 0 800 600))
  (
   (draw-texture-fullscreen tex)
   ))

(define (animate source bottom-left top-right horiz vertic tick every)
  (define-values (x y) (ticker (quotient tick every) horiz vertic))
  ((animate-texture source bottom-left top-right horiz vertic) x y))

(define (animate/xform transformation source bottom-left top-right horiz vertic tick every)
  (define-values (x y) (ticker (quotient tick every) horiz vertic))
  ((animate-texture source bottom-left top-right horiz vertic) x y))

(define/memoize (animate-texture/xform source bottom-left top-right horizontal-panes vertical-panes)
  (define runs (flatten
                 (for/list ([i horizontal-panes])
                   (for/list ([j vertical-panes])
                     (lambda ()
                       (draw-texture/uv source bottom-left top-right (list (/ i horizontal-panes)
                                                                           (/ j vertical-panes))
                                                                     (list (/ (add1 i) horizontal-panes)
                                                                           (/ (add1 j) vertical-panes))))))))
  (lambda (i j)
      ((list-ref runs (+ (modulo j vertical-panes) (* vertical-panes (modulo i horizontal-panes)))))))


;; Give the rectangle and the amount of panes, assumes the file is a texture atlas
;; Returns a function that takes x and y as in which pane to draw
(define/memoize (animate-texture source bottom-left top-right horizontal-panes vertical-panes)
  (define runs (flatten
                 (for/list ([i horizontal-panes])
                   (for/list ([j vertical-panes])
                     (lambda ()
                       (draw-texture/uv source bottom-left top-right (list (/ i horizontal-panes)
                                                                           (/ j vertical-panes))
                                                                     (list (/ (add1 i) horizontal-panes)
                                                                           (/ (add1 j) vertical-panes))))))))
  (lambda (i j)
      ((list-ref runs (+ (modulo j vertical-panes) (* vertical-panes (modulo i horizontal-panes)))))))

;; TODO the functions above

(define (wh->f32vector w h)
  (define w/2 (/ w 2))
  (define h/2 (/ h 2))
  (rectangle->f32vector (list (- w/2) (- h/2)) (list w/2 h/2)))

(define/memoize-partial #:inverted draw-texture-2 (fwh) (view)
  (
   (define file   (first fwh))
   (define width  (second fwh))
   (define height (third fwh))
   (define tex            (load-texture* file))
   (define vertexarray    (u32vector-ref (glGenVertexArrays 1) 0))
   (define vertexbuffer   (u32vector-ref (glGenBuffers 1) 0))
   (define program-id     (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                           (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
   (define move-loc       (glGetUniformLocation program-id "movement"))
   (define vertex-loc     (glGetAttribLocation program-id "vertex"))
   (define uv-loc         (glGetAttribLocation program-id "vertex_uv"))
   (define tex-loc              (glGetUniformLocation program-id "texture"))
   (define points*              (wh->f32vector width height))
   (define dim-vertex           2)
   (define dim-uv               2)
   (register-finalizer          tex (lambda (x) (glDeleteTextures 1 (u32vector x))))
   (register-finalizer          vertexarray (lambda (x) (glDeleteVertexArrays 1 (u32vector x))))
   (glBindVertexArray           vertexarray)
   (glBindBuffer                GL_ARRAY_BUFFER vertexbuffer)
   (glBufferData                GL_ARRAY_BUFFER
                                (* (f32vector-length points*) size-gl-float)
                                (f32vector->cpointer points*)
                                GL_STATIC_DRAW)
   (glBindTexture               GL_TEXTURE_2D tex)
   (glTexParameteri             GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER)
   (glTexParameteri             GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER)
   (glTexParameteri             GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
   (glTexParameteri             GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
   (glGenerateMipmap            GL_TEXTURE_2D)
   (glEnableVertexAttribArray   vertex-loc)
   (glEnableVertexAttribArray   uv-loc)
   (glVertexAttribPointer       vertex-loc dim-vertex GL_FLOAT #f 16 #f)
   (glVertexAttribPointer       uv-loc dim-uv GL_FLOAT #f 16 8)
   (glBindVertexArray           0)
   (glDeleteBuffers             1 (u32vector vertexbuffer))
   )
   ((glUseProgram (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                   (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
    (glBindVertexArray    vertexarray)
    (glActiveTexture      GL_TEXTURE0)
    (glUniform1i          tex-loc #|GL_TEXTURE|# 0)
    (glBindTexture        GL_TEXTURE_2D tex)
    (glUniformMatrix4fv   move-loc 1 #f (matrix->f32vector view))
    (glDrawArrays         GL_TRIANGLES 0 6)
    (glBindVertexArray    0)
    ))

;; Assumes the texture is a rectangle that fills the entire rectangle
(define/memoize-partial draw-texture (file bottom-left top-right) ()
  ((define tex            (load-texture* file))
   (define vertexarray    (u32vector-ref (glGenVertexArrays 1) 0))
   (define vertexbuffer   (u32vector-ref (glGenBuffers 1) 0))
   (define program-id     (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                           (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
   (define move-loc       (glGetUniformLocation program-id "movement"))
   (define vertex-loc     (glGetAttribLocation program-id "vertex"))
   (define uv-loc         (glGetAttribLocation program-id "vertex_uv"))
   (define tex-loc              (glGetUniformLocation program-id "texture"))
   (define points*              (rectangle->f32vector bottom-left top-right))
   (define dim-vertex           2)
   (define dim-uv               2)
   (register-finalizer          tex (lambda (x) (glDeleteTextures 1 (u32vector x))))
   (register-finalizer          vertexarray (lambda (x) (glDeleteVertexArrays 1 (u32vector x))))
   (glBindVertexArray           vertexarray)
   (glBindBuffer                GL_ARRAY_BUFFER vertexbuffer)
   (glBufferData                GL_ARRAY_BUFFER
                                (* (f32vector-length points*) size-gl-float)
                                (f32vector->cpointer points*)
                                GL_STATIC_DRAW)
   (glBindTexture               GL_TEXTURE_2D tex)
   (glTexParameteri             GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER)
   (glTexParameteri             GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER)
   (glTexParameteri             GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
   (glTexParameteri             GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
   (glGenerateMipmap            GL_TEXTURE_2D)
   (glEnableVertexAttribArray   vertex-loc)
   (glEnableVertexAttribArray   uv-loc)
   (glVertexAttribPointer       vertex-loc dim-vertex GL_FLOAT #f 16 #f)
   (glVertexAttribPointer       uv-loc dim-uv GL_FLOAT #f 16 8)
   (glBindVertexArray           0)
   (glDeleteBuffers             1 (u32vector vertexbuffer))
   )
   ((glUseProgram (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                   (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
    (glBindVertexArray    vertexarray)
    (glActiveTexture      GL_TEXTURE0)
    (glUniform1i          tex-loc #|GL_TEXTURE|# 0)
    (glBindTexture        GL_TEXTURE_2D tex)
    (glUniformMatrix4fv   move-loc 1 #f (get-view))
    (glDrawArrays         GL_TRIANGLES 0 6)
    (glBindVertexArray    0)
    ))

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
            (parameterize ([*view* (matrix* (translate (* x x-width) (* -1 nl height)) (*view*))])
              (animation i j))
            (values nl (add1 x))
             ))))))

(define/memoize-partial draw-texture/uv (file bottom-left    top-right
                                              bottom-left-uv top-right-uv) ()
  ((define tex            (load-texture* file))
   (define vertexarray    (u32vector-ref (glGenVertexArrays 1) 0))
   (define vertexbuffer   (u32vector-ref (glGenBuffers      1) 0))
   (define program-id     (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                           (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
   (define move-loc       (glGetUniformLocation program-id "movement"))
   (define tex-loc        (glGetUniformLocation program-id "texture"))
   (define vertex-loc     (glGetAttribLocation program-id "vertex"))
   (define uv-loc         (glGetAttribLocation program-id "vertex_uv"))
   (define dim-vertex     2)
   (define dim-uv         2)
   (define stride         (* dim-vertex dim-uv size-gl-float))
   (define points*        (rectangle->f32vector/uv bottom-left    top-right
                                                   bottom-left-uv top-right-uv))
   (register-finalizer          tex (lambda (x) (glDeleteTextures 1 (u32vector x))))
   (register-finalizer          vertexarray (lambda (x) (glDeleteVertexArrays 1 (u32vector x))))
   (glBindVertexArray           vertexarray)
   (glBindBuffer                GL_ARRAY_BUFFER vertexbuffer)
   (glBufferData                GL_ARRAY_BUFFER
                                (* (f32vector-length points*) size-gl-float)
                                (f32vector->cpointer points*)
                                GL_STATIC_DRAW)
   (glEnableVertexAttribArray   vertex-loc)
   (glEnableVertexAttribArray   uv-loc)
   (glVertexAttribPointer       vertex-loc dim-vertex GL_FLOAT #f stride #f)
   (glVertexAttribPointer       uv-loc dim-uv GL_FLOAT #f stride (* dim-vertex size-gl-float))
   (glBindVertexArray           0)
   (glDeleteBuffers             1 (u32vector vertexbuffer))
   ;;
   (glBindTexture      GL_TEXTURE_2D tex)
   (glTexParameteri    GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_BORDER)
   (glTexParameteri    GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_BORDER)
   ;; Border color if clamp-to-border
   ; (glTexParameterfv GL_TEXTURE_2D GL_TEXTURE_BORDER_COLOR {1 0 0 1})
   (glTexParameteri    GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
   (glTexParameteri    GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
   (glGenerateMipmap   GL_TEXTURE_2D)
   )
  ((glUseProgram (create-program* (load-shader* "source/shaders/draw-texture.vertex.glsl"   GL_VERTEX_SHADER)
                                  (load-shader* "source/shaders/draw-texture.fragment.glsl" GL_FRAGMENT_SHADER)))
   (glBindVertexArray    vertexarray)
   (glActiveTexture      GL_TEXTURE0)
   (glUniform1i          tex-loc #|GL_TEXTURE|# 0)
   (glBindTexture        GL_TEXTURE_2D tex)
   (glUniformMatrix4fv   move-loc 1 #f (get-view))
   (glDrawArrays         GL_TRIANGLES 0 6)
   (glBindVertexArray    0)
   ))
