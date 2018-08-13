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
         "initialization.rkt" "breakpoint.rkt" "drawing.rkt" "initialization.rkt" "pure.rkt")

;; Handles all impure state changes
(define (impure state)
  (glClear GL_COLOR_BUFFER_BIT)
  (H~> state
    ((if* add1) (any-direction-keys? iter) (iter))
    (draw (transform iter last-direction animation.madotsuki))
    (render-absolute (render.absolute))
    (glfwSwapBuffers (window))
  ))

(define (render-absolute lst)
  (when lst
    (for ([i lst])
      ((draw-texture (first i) (second i) (third i)) 0))
    ))



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

  ((map-glfw-keys left-control right-control escape q e w a s d up down left right) window))

(define (make-global-transform trn)
  (dbug trn)
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
  (trce global-trn)
  ; ((animate-texture "data/simple-house.png" '(-1 -1) '(0 0) 3 3)
   ; (sub1 iter) iter)
  (match last-direction
    ('s ((list-ref mado (floor (/ (modulo iter 40) 10))) #:transform global-trn))
    ('a ((list-ref mado (+ 4 (floor (/ (modulo iter 40) 10)))) #:transform global-trn))
    ('w ((list-ref mado (+ 8 (floor (/ (modulo iter 40) 10)))) #:transform global-trn))
    ('d ((list-ref mado (+ 12 (floor (/ (modulo iter 40) 10)))) #:transform global-trn))
    (_  (erro "Unable to find direction")))

  ; ((list-ref sprites (floor (/ (modulo iter 40) 10))) 0)
  ; ((draw-texture "data/walking.png" '(-1 -1) '(0 0)) 1)
  ; ((draw-white-shape '((0.0 0.0 0.0)
  ;                      (0.0 0.1 0.0)
  ;                      (0.1 0.1 0.0)

  ;                      (0.0 0.0 0.0)
  ;                      (0.1 0.1 0.0)
  ;                      (0.1 0.0 0.0)
  ;                      ))
  ;  0 0 #:translation global-trn)
  )
