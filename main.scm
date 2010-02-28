(import input)
(import graph)
(import utilities)
(import visualization)
(import mutation)
(import constraints)
(import filters)
(import ordering)

(define (main)
  (random-source-randomize! default-random-source) ; Randomizes seed for UUID generation
  (visualize-graph '())
  (let loop
    ((archgraph-list (list (generate-graph-from-xml (input)))))
    (for-each
      (lambda
        (graph)
        (print-graph graph)
        (visualize-graph graph))
      archgraph-list)
    (loop
      (order-graph-list
        (filter-graph-list
          (make-graph-mutations (first-or-element archgraph-list))))))

  (representation-cleanup)

  (exit 0))
(main)
