#lang racket/base

(require
  (for-syntax racket/base racket/function racket/list syntax/location)
  racket/function
  racket/list
  syntax/parse
  syntax/parse/define)

(define-syntax-parser reprovide*
  ([_ module-path:expr ...+]
   #'(begin
       (provide (all-from-out module-path ...))
       (require module-path ...))))

(define-syntax-parser reprovide
  ([s]
   (define this-file (syntax-source (attribute s)))
   (define-values (directory file root?) (split-path this-file))
   (define entries (directory-list directory #:build? #t))
   (define directories (filter directory-exists? entries))
   (define files (filter (lambda (x)
                           (and
                             (not (equal? this-file x))
                             (file-exists? x)))
                         entries))
   (define (not-empty? x) (not (empty? x)))
   (define all-files
     (let loop ([files*        files]
                [directories*  directories])
       (cond
         [(empty? directories*) files*]
         [else
           (define iteration
             (filter not-empty? (flatten (map (curry directory-list #:build? #t) directories*))))
           (loop (append (filter file-exists? iteration) files*)
                 (filter directory-exists? iteration))])))
   (writeln all-files)
   (datum->syntax #'s `(reprovide* ,@(map (lambda (file)
                                          `(file ,(path->string file)))
                                        all-files)) #'s)
   ))

(reprovide)
