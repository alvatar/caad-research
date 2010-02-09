(import graph)
(import representation)
(import mutation)
(import filtering)

(define (main)

  (let*
    ((xml-file (open-input-file "arch.xml"))
     (xml-string (read-line xml-file #f))
     (close-port xml-file))

    (representation-init)
    (let
      ((archgraph (generate-graph-from-xml xml-string)))
      (represent archgraph)
      (filter (mutate archgraph)))
    (representation-cleanup)

  (exit 0)))

(main)
