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
         "breakpoint.rkt" "drawing.rkt" "initialization.rkt" "pure.rkt")


(define (clear-graphics)
  (glClear GL_COLOR_BUFFER_BIT))

(define (drawtext tick)
  ((animate-texture "data/text.png" '(-0.5 -0.5) '(0 0) 14 3) (floor (/ tick 12))
                                                              (- 2 (floor (/ tick (* 12 14)))))
  ((draw-text "data/text/main-text2.png" '(-1 0.0) '(-0.9 0.2) 19 5)
   "Nani the fuck\nis going on?"))

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
                 [0 0 0  1]])
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
      ((draw-texture/uv* x) #:transform global-trn)
    )))

(define/memoize (draw-texture/uv* lst)
  (apply draw-texture/uv lst))
