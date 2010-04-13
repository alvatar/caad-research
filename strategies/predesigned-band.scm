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
     (light-field (make-light-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     (entries-field (make-entries-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     (structure-field (make-structure-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     (pipes-field (make-pipes-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon)))
     (letrec
       ((agents (list
         (make-agent
           'entrance
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (let ((pos (car (agent-node-positions agent))))
                 (list (point-translation
                         (vect2+
                           (field->force-vect2 light-field pos)
                           (inverse-gravity-force-agents agents)))))
               (agent-proc agent))))
         (make-agent
           'bath
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (make-point 0.0 0.0))
               (agent-proc agent))))
         (make-agent
           'room1
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (make-point 0.0 0.0))
               (agent-proc agent))))
         (make-agent
           'living
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (make-point 0.0 0.0))
               (agent-proc agent))))
         (make-agent
           'kitchen
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (make-point 0.0 0.0))
               (agent-proc agent))))
         (make-agent
           'distrib
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (make-point 0.0 0.0))
               (agent-proc agent))))
         (make-agent
           'storage
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (make-point 0.0 0.0))
               (agent-proc agent))))
         (make-agent
           'room2
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (make-point 0.0 0.0))
               (agent-proc agent))))
         (make-agent
           'room3
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (make-point 0.0 0.0))
               (agent-proc agent)))))))
       (values
         graph
         (make-world 
           agents
           (list light-field
                 entries-field
                 structure-field
                 pipes-field))))))
