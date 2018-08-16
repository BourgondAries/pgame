#lang racket/base

(provide show-visualization)

(require racket/file racket/function racket/list racket/port racket/string racket/system
         logger)


(define (normalize graph) (sort (hash->list graph) symbol<? #:key car))
(define (normalize2 graph) (reverse (sort (hash->list graph) symbol<? #:key car)))

(struct A (parent label value id) #:prefab)
(define (visualize2 pid label graph id)
  (cond
    ([hash? graph]
     (for/fold ([build (list (A pid label (void) id))]
                [value (add1 id)])
               ([entry (normalize2 graph)])
       (let-values ([(build* count*) (visualize2 id (car entry) (cdr entry) value)])
         (values (cons build* build)
                 count*))))
    (else
      (values (A pid label graph id) (+ 2 id)))))

(define (escape x)
  (string-trim
    (with-output-to-string
      (thunk (write (string-take (format "~a" x) 18))))
    "\"")
  )

(define (gvis2 graph)
  (define-values (g _) (visualize2 0 'root graph 0))
  (string-append "digraph G { node [shape=oval]; " (annota->dor g) (annotated->dot g) " }"))

(define (annotated->dot agra)
  (cond
    ([list? agra]  (apply string-append (map annotated->dot agra)))
    ([A? agra]     (cond
                     ([= (A-parent agra) (A-id agra)] "") ; Root node
                     ([void? (A-value agra)] (string-append (escape (A-parent agra)) " -> " (escape (A-id agra)) ";\n"))
                     (else                   (string-append (escape (A-parent agra)) " -> " (escape (A-id agra)) "-> " (escape (add1 (A-id agra))) ";\n"
                                                            (escape (add1 (A-id agra))) "[style=filled, fillcolor=orange, shape=box, label=\"" (escape (A-value agra)) "\"];\n"))))
    (else (crit "Unable to read agraph"))
  ))

(define (annota->dor agra)
  (cond
    ([list? agra]  (apply string-append (map annota->dor agra)))
    ([A? agra]     (string-append (escape (A-id agra)) "[style=filled, fillcolor=green, label=\"" (escape (A-label agra)) "\"];\n"))
    (else (crit "Unable to read agraph"))
  ))

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
  ; (define (call-with-values (thunk (visualize2 0 'root graph 0)) (lambda (x y) (annotated->dot x)))
  (define temporary-file2 "temporary/visualizer2.dot")
  (with-output-to-file temporary-file2 #:exists 'replace
    (thunk (display (gvis2 graph))))
  (system* "/usr/bin/env" "bash" "-c" "dot -T eps temporary/visualizer2.dot > temporary/visualizer2.eps && evince temporary/visualizer2.eps")

  (define temporary-file "temporary/visualizer.dot")
  (with-output-to-file temporary-file #:exists 'replace
    (thunk (display (visualize graph #:with-data data))))
  (system* "/usr/bin/env" "bash" "-c" "dot -T eps temporary/visualizer.dot > temporary/visualizer.eps && evince temporary/visualizer.eps")
  )

