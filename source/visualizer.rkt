#lang racket/base

(provide show-visualization)

(require racket/file racket/function racket/list racket/port racket/string racket/system
         "pure.rkt"
         logger)

;; Settings for tuning the graph
(defines
  (font-size-small 6)
  (font-size-normal 12)
  (max-value-length 50)
  (smaller-font-value-length 30)
  )


;; Turn a table into a list and sort the result
(define (normalize graph) (reverse (sort (hash->list graph) symbol<? #:key car)))

;; An A-record
(struct AnnotatedElement () #:prefab)
(struct A AnnotatedElement (parent label value id) #:prefab)

;; Recursively converts each hash-table into a list of A
(define (visualize pid label graph id)
  (cond
    ([hash? graph]
     (for/fold ([build (list (A pid label (void) id))]
                [value (add1 id)])
               ([entry (normalize graph)])
       (let-values ([(build* count*) (visualize id (car entry) (cdr entry) value)])
         (values (cons build* build)
                 count*))))
    (else
      ;; Incrementing by two so we leave ID space for value nodes in the graph
      (values (A pid label graph id) (+ 2 id)))))

;; Turn any value into a string, ensuring that it's properly escaped
(define (escape x)
  (define (string-take str n)
    (define lstr (string->list str))
    (if (< (length lstr) n)
      str
      (list->string (take (string->list str) n))))
  (string-trim
    (with-output-to-string
      (thunk (write (string-take (format "~a" x) max-value-length))))
    "\""))

(define (generate-dot-code graph)
  (define-values (graph* _) (visualize 0 'root graph 0))
  (string-append "digraph G {\n\tranksep=2;\n\tnode [shape=oval];\n" (compute-labels graph*) (annotated-graph->dot graph*) "}"))

(define (annotated-graph->dot agra)
  (define (root-node? agra)
    (= (A-parent agra) (A-id agra)))
  (cond
    ([list? agra]  (apply string-append (map annotated-graph->dot agra)))
    ([A?    agra]  (cond
                     ([root-node? agra]      "")
                     ([void? (A-value agra)] (string-append "\t" (escape (A-parent agra)) " -> " (escape (A-id agra)) ";\n"))
                     (else                   (string-append "\t" (escape (A-parent agra)) " -> " (escape (A-id agra)) " -> " (escape (add1 (A-id agra))) ";\n"
                                                            "\t" (escape (add1 (A-id agra)))
                                                            (let* ([length (string-length (escape (A-value agra)))]
                                                                   [size   (if (> length smaller-font-value-length) font-size-small font-size-normal)])
                                                              (string-append
                                                                " [style=filled,fillcolor=orange,shape=box,fontsize=" (number->string size) ",label=\""))
                                                            (escape (A-value agra)) "\"];\n"))))
    (else (raise-argument-error 'annotated-graph->dot "(or/c list? AnnotatedElement?" agra))))

(define (compute-labels agra)
  (cond
    ([list? agra]  (apply string-append (map compute-labels agra)))
    ([A?    agra]  (string-append "\t" (escape (A-id agra)) " [style=filled,fillcolor=green,label=\"" (escape (A-label agra)) "\"];\n"))
    (else          (raise-argument-error 'compute-labels "(or/c list? AnnotatedElement?" agra))
  ))

(define (run-command)
  (not (system* "/usr/bin/env" "bash" "-c" "dot -T eps temporary/visualizer.dot > temporary/visualizer.eps && evince temporary/visualizer.eps")))

(define (show-visualization graph)
  (define temporary-file "temporary/visualizer.dot")
  (with-output-to-file temporary-file #:exists 'replace
    (thunk (display (generate-dot-code graph))))
  (define command-error
    (call-with-output-string
      (lambda (p) (parameterize ([current-error-port p])
                    (run-command)))))
  (when (non-empty-string? command-error)
    (crit+ command-error)))
