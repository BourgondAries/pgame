#! /usr/bin/env racket
#lang racket/base

(require reloadable)

(define core
  (reloadable-entry-point->procedure
    (make-reloadable-entry-point 'core "source/core.rkt")))
(define breakpoint
  (reloadable-entry-point->procedure
    (make-reloadable-entry-point 'breakpoint "source/breakpoint.rkt")))

(reload!)

(define (main)
  (let loop ([state '()])
    (when state
      (loop (core state)))))

(module+ main
  (call-with-exception-handler
    breakpoint
    main))
