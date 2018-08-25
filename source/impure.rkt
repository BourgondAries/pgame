#lang racket/base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Procedures for impure operations
;;
;; All of these procedures must somehow edit impure
;; state or perform some IO operation with the rest
;; of the system. All such operations are deemed impure.
;;
;; This file can contain some minor logic, but most pure
;; logic should be offloaded elsewhere.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(require racket/list racket/match syntax/parse/define
         (for-syntax racket/base racket/list racket/string
                     threading)
         ffi/vector finalizer math/matrix opengl opengl/util threading
         glfw3 logger memo nested-hash spipe
         "breakpoint.rkt" "drawing.rkt" "pure.rkt")

(define-syntax-parser push-view
  ([_ view:expr body:expr ...+]
   #'(parameterize ([*view* (matrix* view (*view*))])
      body ...)))

(define (do-fade window width height)
  (define texture (get-previous-frame width height))
  (glClearColor 0. 0. 0. 0.)
  (for ([i 40])
    (glClear GL_COLOR_BUFFER_BIT)
    (draw-texture-fullscreen texture)
    (fade (/ i 30))
    (glfwSwapBuffers window))
  (glClearColor 0.3 0.8 0.3 0.))

(define (collect-wasd keys)
  (or
    (if (hash-ref keys 'a #f) 'a #f)
    (if (hash-ref keys 'd #f) 'd #f)
    (if (hash-ref keys 'w #f) 'w #f)
    (if (hash-ref keys 's #f) 's #f)))

(define (check-door-collision x y)
  (and (< -20 x 20) (> 130 y 100)))

(define (dd persp)
  (parameterize ([*view* (matrix* persp (*view*))])
    (draw-texture "data/text/main-text2.png" '(-1 -1) '(1 1))))

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

(define grass (make-list 20 (make-list 20 49)))
(define (do-draw-tiles tick persp)
  (push-view persp
    ; (trce tick)
    (define-values (x y) (ticker (quotient tick 60) 8 15))
    ; (trce (quotient tick 60))
    ((animate-texture "data/basictiles.png" '(-1 -1) '(-0.5 -0.5) 8 15)
     x y)
    (draw-tiles "data/basictiles.png" 8 15 grass (scale 5/100) 0)
    (draw-shape '((-1 1) (0.5 1) (0.5 0.5))
                '((0 0 0 0) (0 1 0 0) (1 0 0 1)))
  ))

(define (clear-graphics)
  (glClear GL_COLOR_BUFFER_BIT))

(define (drawtext tick)
  ((animate-texture "data/text.png" '(-0.5 -0.5) '(0 0) 14 3) (floor (/ tick 12))
                                                              (- 2 (floor (/ tick (* 12 14)))))
  ((draw-text "data/text/main-text2.png" '(-1 0.0) '(-0.9 0.2) 19 5)
   "Nani the fuck\nis going on?"))

(define (drawtext2 view tick)
  (parameterize ([*view* (matrix* view (*view*))])
    ((animate-texture "data/text.png" '(-0.5 -0.5) '(0 0) 14 3) (floor (/ tick 12))
                                                                (- 2 (floor (/ tick (* 12 14)))))
    ((draw-text "data/text/main-text2.png" '(-1 0.0) '(-0.9 0.2) 19 5)
     "Nani the fuck\nis going on? (2)")))

(define (render-absolute)
  (draw-texture "data/simple-house.png" '(-0.3 0.3) '(0.3 1.4)))

(define (get-keys window)
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
  ((map-glfw-keys left-control right-control escape q e w a s d g b up down left right) window))

(define (clear-keys window table)
  (get-keys window)
  (glfwPollEvents)
  (for/fold ([table* table])
            ([(x y) table])
    (trce x y)
    (hash-set table* x #f)
  ))

(define (make-global-transform trn)
  ; (dbug trn)
  (if trn
    (let ([x (hash-ref trn 'x 0)]
          [y (hash-ref trn 'y 0)]
          [r (hash-ref trn 'r 0)]
          )
      (matrix*
        (matrix [[1 0 0 0]
                 [0 1 0 0]
                 [0 0 1 0]
                 [(/ x 20) (/ y 20) 0 1]])
        ;; rotation here
        (matrix [[0.1 0 0 0]
                 [0 0.1 0 0]
                 [0 0 0.1 0]
                 [0 0 0   1]])
        ))
    (identity-matrix 4)))

(define (draw global-trn iter last-direction mado)
  ; ((animate-texture "data/simple-house.png" '(-1 -1) '(0 0) 3 3)
   ; (sub1 iter) iter)
  (parameterize ([*view* (matrix* (if global-trn global-trn (identity-matrix 4)) (*view*))])
    (match last-direction
      ('s ((list-ref mado (floor (/ (modulo iter 40) 10))) ))
      ('a ((list-ref mado (+ 4 (floor (/ (modulo iter 40) 10)))) ))
      ('w ((list-ref mado (+ 8 (floor (/ (modulo iter 40) 10)))) ))
      ('d ((list-ref mado (+ 12 (floor (/ (modulo iter 40) 10)))) ))
      (_  ((list-ref mado (floor (/ (modulo iter 40) 10)))))
  )))

(define (draw-relative global-trn mado)
  (when mado
    (for ([x mado])
      (draw-texture/uv* x)
    )))

(define/memoize (draw-texture/uv* lst)
  (apply draw-texture/uv lst))

; (define (dextra xform 

(define (draw-coin tick)
  (animate "data/coin24.png" '(0.1 0.1) '(0.2 0.2) 61 1 tick 2))

(define (draw-portal tick)
  (animate "data/portalRings1.png" '(0.2 0.1) '(0.3 0.2) 4 5 tick 2))

(define ((every tick-modulus action) tick value)
  (if (zero? (modulo tick tick-modulus))
    (begin (action value) value)
    value
    ))

(define (get-window-size window)
  (glfwGetWindowSize window))

(define (sub state)
  (parameterize ([*view* (matrix* (rotate (nested-hash-ref state 'ae 'rotation)) (perspective (nested-hash-ref state 'io 'window-size 'width #:default #f)
                                                                                                (nested-hash-ref state 'io 'window-size 'height #:default #f))
                                               (*view*))])
    (H~>
      state
      (render-absolute   ())
      (context (io.transform)
        (draw              (ae.tick.direction-keys ae.last-direction io.animation.madotsuki) ())
        (draw-relative     (io.render.relative)                                              ()))
      (drawtext          (ae.tick.direction-keys))
      (draw-coin         (ae.tick.iteration))
      (draw-portal       (ae.tick.iteration)))))

#|
Every drawable thing requires: VIEW
Every ANIMATED drawable thing requires TICK & DIVISOR
Static: VIEW              (TEXTURE width height)
DumbAn: VIEW TICK MODULUS (TEXTURE width height vertical horizontal)
SmarAn: VIEW X    Y       (TEXTURE width height vertical horizontal)
Tilest: VIEW              (TEXTURE width height vertical horizontal ((3 9 4 5) (1 9 4 2)))
sttext: VIEW              (TEXTURE width height string)
dytext: VIEW TICK         (TEXTURE width height string)
|#
