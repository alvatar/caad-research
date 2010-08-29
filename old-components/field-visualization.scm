;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elements used by generations algorithms and strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        ../graph-operations
        ../core/list
        ../geometry/kernel
        ../geometry/query
        ../graph
        ../math/exact-algebra
        ../visualization)

;-------------------------------------------------------------------------------
; Visualization
;-------------------------------------------------------------------------------

;;; Field visualization

(define (visualize-field field size-vec)
  (visualization:do-later
    'fields
    (lambda (backend vis-env)
      (let* ((image (visualization:create-image backend)) ; TODO: created in other place
             (max-dim (max (vect2-x size-vec) (vect2-y size-vec)))
             (image-scale (vect2*
                            (make-vect2 max-dim max-dim)
                            (make-vect2 (inverse maxx) (inverse maxy)))))
        (visualization:scale backend image-scale)
        (visualization:image-set! image (u8-2dfield-data field))
        (visualization:paint-image backend image 0.5)
        (visualization:scale backend (vect2:1/vect2 image-scale)))))
  (visualization:layer-depth-set! 'fields 10))
