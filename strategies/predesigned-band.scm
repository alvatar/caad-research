;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A predesigned band strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../analysis)
(import ../geometry)
(import ../generation-elements)
(import ../graph)

;;; Create all necessary things to begin simulation

(define (describe-world-predesigned-band graph)
  (let*
    ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
     (basic-set
      `(,(make-agent
           'entrance
           (list (random-point-in-polygon limit-polygon)
                 (random-point-in-polygon limit-polygon)
                 (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (list (random-point-in-polygon limit-polygon)
                     (random-point-in-polygon limit-polygon)
                     (random-point-in-polygon limit-polygon))
               (agent-proc agent))))
        ,(make-agent
           'bath
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'room1
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'living
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'kitchen
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))))
     (more
      `(,(make-agent
           'distrib
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'storage
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'room2
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'room3
           (list (random-point-in-polygon limit-polygon))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))))
     (light-field (make-light-field graph graph-space-size-x graph-space-size-y)))
    (make-world 
      (append basic-set more)
      (list light-field))))
