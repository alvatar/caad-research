(import input)
(import graph)
(import utilities)
(import visualization)
(import mutation)
(import constraints)
(import filtering)
(import ordering)

(define (main)
  (visualize-graph '())
  (let loop
    ((archgraph-list (list (generate-graph-from-xml (input)))))
    (for-each
      (lambda
        (graph)
        ;(print-graph graph)
        (visualize-graph graph))
      archgraph-list)
    (loop
      (order-graph-list
        (filter-graph-list
          (make-graph-mutations (first-or-element archgraph-list)
                                constraints)
          constraints))))

  (representation-cleanup)

  (exit 0))
(main)
