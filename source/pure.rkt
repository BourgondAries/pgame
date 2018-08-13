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
         nested-hash spipe)

(define (set-false x) #f)

(define (pop-fsm fsm)
  (if (empty? fsm)
    empty
    (rest fsm)))

(define ((push-fsm symbol) fsm)
  (cons symbol fsm))

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
    ((if* (push-fsm 'menu)) (keys.escape fsm) (fsm))
    (renderlist () (render.absolute))
  ))

(define (renderlist)
  (list
    '("data/simple-house.png" (-0.3 0.3) (0.3 1.4))
    ))


(define ((if* proc) condition value)
  (if condition
    (proc value)
    value))

(define (any-direction-keys? keys)
  (or
    (hash-ref keys 'w #f)
    (hash-ref keys 'a #f)
    (hash-ref keys 's #f)
    (hash-ref keys 'd #f)))

(define (last-key last-direction w a s d)
  (cond
    (w 'w)
    (a 'a)
    (s 's)
    (d 'd)
    (else (or last-direction 'd))))


(define (add1* n) (if n (add1 n) 0))

(define (should-exit? state)
  (nested-hash-ref state 'game 'should-exit?))

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
