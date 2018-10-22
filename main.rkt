#! /usr/bin/env racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main entry point for pgame
;;
;; This file sets up the reloadable entry point and break
;; handler, allowing one to modify code while running
;; the program. See source/core.rkt for the entry point.
;;
;; The main loop quits if the state is #f.
;;
;; For documentation first run `raco pkg install pgame/`,
;; and then see `raco docs pgame`.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (core '()))

(module+ main
  (error-print-width 1000)
  (call-with-exception-handler
    breakpoint
    main))
