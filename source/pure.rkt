#lang racket/base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Procedures for pure operations
;;
;; None of the procedures in this file shall communicate with
;; the system, perform network IO, send messages to threads,
;; or change some state.
;;
;; This file is intended to contain pure logic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(require racket/list
         nested-hash spipe
         logger
         syntax/parse/define racket/function (for-syntax racket/base))

(define-syntax-parser expand-single
  ([_ value:expr fn:id]
   #'(fn value))
  ([_ value:expr (fn:expr arg ...)]
   #'(fn value arg ...)))
(define-syntax-parser context
  ([_ value:expr operand:expr ...]
   #'(let ([result value])
       (expand-single result operand) ...)))

(define-syntax-parser set-false
  ([_ value:expr ...+]
   #'(values ((const #f) value) ...)))

(define (pop-fsm fsm)
  (if (empty? fsm)
    empty
    (rest fsm)))

(define ((push-fsm symbol) fsm)
  (cons symbol fsm))

(define ((step-to value) x)
  (cond
    ([> x value] (sub1 x))
    ([= x value] x)
    ([< x value] (add1 x))))

(define ((set-fsm symbol) fsm)
  (if (empty? fsm)
      (list symbol)
      (cons symbol (rest fsm))))

(define (ticker v x y)
  (values (modulo v x)
          (quotient v x)))

;; Handles all pure state changes
(define (pure state)
  (H~>
    state
    (check-C-W-exit (keys.left-control keys.w) (should-exit?))
    (add1-if-true (keys.q transform.r) (transform.r))
    (sub1-if-true (keys.e transform.r) (transform.r))
    (add1-if-true (keys.w transform.y) (transform.y))
    (sub1-if-true (keys.a transform.x) (transform.x))
    (sub1-if-true (keys.s transform.y) (transform.y))
    (add1-if-true (keys.d transform.x) (transform.x))
    (sub1-if-true (keys.q rotation) (rotation))
    (add1-if-true (keys.e rotation) (rotation))
    ((if* (push-fsm 'menu)) (keys.escape fsm) (fsm))
  ))

(define-syntax-parser defines**
  ([_ (name:id val:expr) ...+]
   #'(begin (define name val) ...)))

(define (process-triggers hitbix triggers)
  (void)
  )

(define (check-collision position collidables)
  ; (trce position)
  0
  )

;; rect: #s(lx by rx ty)
(define (collides? rect-1 rect-2)
  (define (collides?* rect-1 rect-2)
    (defines**
      (r1-l (first  rect-1))
      (r1-b (second rect-1))
      (r1-r (third  rect-1))
      (r1-t (fourth rect-1))
      (r2-l (first  rect-2))
      (r2-b (second rect-2))
      (r2-r (third  rect-2))
      (r2-t (fourth rect-2)))
    (and
      (or
        (< r1-l r2-l r1-r)
        (< r1-l r2-r r1-r))
      (or
        (< r1-b r2-b r1-t)
        (< r1-b r2-t r1-t))))
  (or (collides?* rect-1 rect-2)
      (collides?* rect-2 rect-1)))

(define ((if* proc) condition value)
  (if condition
    (proc value)
    value))

;; Indicates whether we're walking or not
(define (any-direction-keys? keys)
  (or
    (hash-ref keys 'w #f)
    (hash-ref keys 'a #f)
    (hash-ref keys 's #f)
    (hash-ref keys 'd #f)))

;; Actually moves the character based on WASD keys
;; To what extent should functions like this assume tables?
(define (count-walking keys x y)
  (values
    (cond
      ([hash-ref keys 'a #f]  (sub1 x))
      ([hash-ref keys 'd #f]  (add1 x))
      (else                   x))
    (cond
      ([hash-ref keys 'w #f]  (add1 y))
      ([hash-ref keys 's #f]  (sub1 y))
      (else                   y))
    ))

(define (compute-walk-tick keys tick)
  (if (any-direction-keys? keys)
    (add1 tick)
    0))

(define (last-key last-direction wasd)
  (if wasd
    wasd
    (or last-direction 'd)))


(define (add1* n) (if n (add1 n) 0))

(define (should-exit? state)
  (nested-hash-ref state 'ae 'should-exit?))

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

(define (exit-c ? lst)
  (if ? '() lst))
