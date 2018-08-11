#lang info
(define collection "pgame")
(define deps '("base" "glfw3" "logger" "memo" "nested-hash" "opengl" "spipe" "reloadable"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/pgame.scrbl" ())))
(define pkg-desc "My Game")
(define version "0.1.0")
(define pkg-authors '("Kevin Robert Stravers"))
