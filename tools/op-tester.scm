;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operator testbed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(import (std string/xml-to-sxml
             misc/uuid
             srfi/1)
        ../context
        ../input
        ../graph-visualization
        ../graph-operations
        ../operators)

((lambda ()
   (let ((graph (input-from-xml "xml-input/two_rooms.xml")))
     (visualize-graph graph)
     (visualization:do-now)

     (let ((transformed-graph
            (op:merge-rooms (apply room+room->context
                                   graph
                                   (graph:find.rooms graph)))))
       (visualization:forget-all)
       (visualize-graph transformed-graph)
       (visualization:do-loop)))
   (exit 0)))
