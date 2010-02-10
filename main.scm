(import input)
(import graph)
(import visualization)
(import mutation)
(import constraints)
(import filtering)

(define (main)
  (visualize-graph '())
  (let loop
    ((archgraph (generate-graph-from-xml (input))))
    (visualize-graph archgraph)
    (filter-graph-list (mutate-graph archgraph constraints) constraints)
    (print-graph archgraph)
    (loop archgraph))

  (representation-cleanup)

  (exit 0))
(main)
