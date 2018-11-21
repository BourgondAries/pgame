#lang racket/base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Procedures for pure operations
;;
;; None of the procedures in this file shall communicate with
;; the system, perform network IO, send messages to threads,
;; or change some state. Neither shall this file know about
;; project data.
;;
;; This file is intended to contain pure logic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(require logger nested-hash spipe
         racket/function
         racket/list
         syntax/parse/define
         (for-syntax racket/base))

(define/H~> walk
  (collect-wasd           (keys)                       (keys.wasd))
  (last-key               (last-direction keys.wasd)   (last-direction))
  (compute-walk-tick      (keys)                       tick.direction-keys)
  (add1-if-true           (keys.q)   transform.r)
  (sub1-if-true           (keys.e)   transform.r)
  (count-walking          (keys) transform.x transform.y)
  )

(define (collect-wasd keys)
  (or
    (if (hash-ref keys 'a #f) 'a #f)
    (if (hash-ref keys 'd #f) 'd #f)
    (if (hash-ref keys 'w #f) 'w #f)
    (if (hash-ref keys 's #f) 's #f)))


(define ((step-to value) x)
  (cond
    ([> x value] (sub1 x))
    ([= x value] x)
    ([< x value] (add1 x))))

(define (ticker v x y)
  (values (modulo v x)
          (quotient v x)))

;; Handles all pure state changes
(define (pure state)
  (H~>
    state
    (check-C-W-exit (keys.left-control keys.w) should-exit?)
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

(define-syntax-parser defines
  ([_ (name:id val:expr) ...+]
   #'(begin (define name val) ...)))

(define (process-triggers hitbix triggers)
  (void)
  )

(define (check-collision position collidables)
  ; (trce position)
  0
  )

(define (collides? rect-1 rect-2)
  (define (collides?* rect-1 rect-2)
    (defines
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
    (if (= tick 0)
      8
      (add1 tick))
    0))

(define (last-key last-direction wasd)
  (if wasd
    wasd
    (or last-direction 'd)))

(define (add1* n) (if n (add1 n) 0))

(define (or* . lst)
  (ormap identity lst))

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

(define ((cond-push what) condition lst)
  (if condition
    (cons what lst)
    lst))

(define ((cond-set what) condition lst)
  (if condition
    (cons what (rest lst))
    lst))

(define (check-C-W-exit left-control w previous)
  (or (and left-control w) previous))

(define (exit-c ? lst)
  (if ? '() lst))

(define (get-fsm state)
  (define name (first (hash-ref state 'fsm)))
  name)

(define (get-fsm-second state)
  (info (hash-ref state 'fsm))
  (define name (second (hash-ref state 'fsm)))
  name)

(define (current-state state)
  (define name (first (hash-ref state 'fsm)))
  (define t (apply nested-hash-ref state name))
  (trce t))

(define (current-state2 state name)
  (apply nested-hash-ref state name))

(define (current-s state)
  (define machines (map (curry current-state2 state) (hash-ref state 'fsm)))
  (trce machines))

(define (check-box x y hit?)
  (or hit? (collides? (list x y x y) '(10 10 30 30)))
  )

(define (pop state)
  (define cool (get-fsm state))
  (nested-hash-set* state cool (rest (nested-hash-ref* state cool)))
  )

(define ((push fn) state)
  (define cool (get-fsm state))
  (nested-hash-set* state cool (cons fn (nested-hash-ref* state cool)))
  )

(define (pop-parent state)
  (define cool (get-fsm-second state))
  (nested-hash-set* state cool (rest (nested-hash-ref* state cool)))
  )

(define ((if-empty-pop which) state)
  (if (empty? (nested-hash-ref* state which))
    (begin
      (trce (hash-ref state 'fsm))
      (nested-hash-set* state (get-fsm state)
                        (rest (nested-hash-ref* state (first (hash-ref state 'fsm)))))
      )
    state))

(define ((oscillate divisor) tick)
  (sin (/ tick divisor)))

(require (for-syntax logger racket/function racket/list racket/sequence))

(define-syntax-parser reprovide2*
  ([_ module-path:expr ...+]
   #'(begin
       (provide (all-from-out module-path ...))
       (require module-path ...))))

(define-syntax-parser define-recursive-load
  ; TODO 
  ([_ storage:id directory*:string]
   #'(define-recursive-load storage directory* identity))
  ([_ storage:id directory*:string fold:expr]
   (define this-file (syntax-source (attribute directory*)))
   (define-values (directory file root?) (split-path this-file))
   (define directories (build-path directory (syntax-e (attribute directory*))))
   (define all-files (filter file-exists? (sequence->list (in-directory directories))))
   (datum->syntax
     #'storage
     `(begin
        (module ,(attribute storage) racket/base
          ,@(map (lambda (f)
                   (define-values (dir f2 r) (split-path f))
                   `(require (only-in (file ,(path->string f)) ,(string->symbol (path->string (path-replace-extension f2 "")))))
                   )
            all-files)
          (define ,(attribute storage)
            (list
              ,@(map (lambda (f)
                      (define-values (dir f2 r) (split-path f))
                      `(list
                         (quote ,(string->symbol (path->string (path-replace-extension f2 ""))))
                         ,(string->symbol (path->string (path-replace-extension f2 ""))))
                      )
                    all-files)))
          (provide ,(attribute storage)))
        (require (quote ,(attribute storage)))
        )
     #'storage)
   ))

(define (functions->hash symbols)
  (let loop ([state (hasheq)]
             [symbols* symbols])
    (cond
      ([null? symbols*] state)
      (else
        (loop
          (hash-set state (caar symbols*) (cadar symbols*))
          (cdr symbols*)
          )))))
