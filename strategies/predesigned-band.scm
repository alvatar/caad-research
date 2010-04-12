;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A predesigned band strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../analysis)
(import ../fields/entries)
(import ../fields/light)
(import ../fields/pipes)
(import ../fields/structure)
(import ../geometry)
(import ../generation-elements)
(import ../graph)
(import ../math)

;;; Create all necessary things to begin simulation

(define (describe-world-predesigned-band graph world)
  (let*
    ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
     (bb-vect (segment->vect2 (point-list->bounding-box limit-polygon)))
     (bb-x (vect2-u bb-vect))
     (bb-y (vect2-v bb-vect))
     (basic-set
      `(,(make-agent
           'entrance
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon))
               (agent-proc agent))))
        ,(make-agent
           'bath
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon))
               (agent-proc agent))))
        ,(make-agent
           'room1
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon))
               (agent-proc agent))))
        ,(make-agent
           'living
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon))
               (agent-proc agent))))
        ,(make-agent
           'kitchen
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon))
               (agent-proc agent))))))
     (more
      `(,(make-agent
           'distrib
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon))
               (agent-proc agent))))
        ,(make-agent
           'storage
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon))
               (agent-proc agent))))
        ,(make-agent
           'room2
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon))
               (agent-proc agent))))
        ,(make-agent
           'room3
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon))
               (agent-proc agent)))))))
    (values
      graph
      (make-world 
        (append basic-set more)
        (list
          (make-light-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon)
          (make-entries-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon)
          (make-structure-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon)
          (make-pipes-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))))))
