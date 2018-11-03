#lang racket/base

(require
  (for-syntax racket/base)
  syntax/parse/define)

(define-syntax-parser reprovide
  ([_ module-path:expr ...+]
   #'(begin
       (provide (all-from-out module-path ...))
       (require module-path ...))))

#| (define-syntax-parser states |#
#|   ([_] |#
#|    #:with (import ...) |#  
#|     (begin |#
#|      (current-directory "source/states") |#
#|      (map path->string (directory-list))) |#
#|    #'(reprovide import ...))) |#

#| (states) |#

(reprovide
  "drawing.rkt"
  "impure.rkt"
  "pure.rkt"
  "states/decode-tmx.rkt"
  "states/menu.rkt"
  "states/top-map.rkt"
  ffi/vector
  finalizer
  glfw3
  logger
  math/matrix
  memo
  nested-hash
  opengl
  opengl/util
  racket/list
  spipe
  syntax/parse/define
  threading
  )
