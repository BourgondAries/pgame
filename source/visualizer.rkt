#lang racket/base

(provide show-visualization)

(require racket/file racket/function racket/list racket/port racket/string racket/system
         logger)


(define (normalize graph) (sort (hash->list graph) symbol<? #:key car))

(define (visualize graph #:with-data [data #f])
  (define (visualize* data graph parent)
    (cond
      ([hash? graph]
       (for/list ([entry (normalize graph)])
         (cons
          (string-append "\"" (symbol->string parent) "\" -> \"" (symbol->string (car entry)) "\";")
          (visualize* data (cdr entry) (car entry))
         )))
      (else (if data (string-append "\"" (symbol->string parent) "\" -> \""
                           (string-trim
                             (with-output-to-string
                               (thunk (write (string-take (format "~v" graph) 18))))
                             "\"")
                           "\";")
              ""))))
  (format "digraph G {\n~a\n}"
    (string-join
      (flatten
        (for/list ([entry (normalize graph)])
          (visualize* data (cdr entry) (car entry))))
      "\n")))

(define (string-take str n)
  (define lstr (string->list str))
  (if (< (length lstr) n)
    str
    (list->string (take (string->list str) n))))

(define (show-visualization graph #:with-data [data #t])
  (define temporary-file "temporary/visualizer.dot")
  (with-output-to-file temporary-file #:exists 'replace
    (thunk (display (visualize graph #:with-data data))))
  (system* "/usr/bin/env" "bash" "-c" "dot -T eps temporary/visualizer.dot > temporary/visualizer.eps && evince temporary/visualizer.eps")
  )

