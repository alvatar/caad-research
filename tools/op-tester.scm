;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An independent tool for visualizing xml statically
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(import (std string/xml-to-sxml))
(import ../web/parse/ssax-sxml/sxml-tools/sxpath)
(import (std misc/uuid))
(import (std srfi/1))

(import ../input)
(import ../graph-visualization)
(import ../graph)
(import ../visualization)
(import ../context)
(import ../core/functional)
(import ../core/list)
(import ../geometry/kernel)
(import ../math/exact-algebra)
(import ../analysis)
(import ../auxiliary-operations)




(define (op:remove-multiple graph le)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (remove (lambda (e)
             (any (lambda (e2) (equal? e2 e)) le))
           (graph-architecture graph))))

(define (op:fix-room-topology graph)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (map (lambda (e)
          (if (room? e)
              (sort-room-walls graph e)
              e))
        (graph-architecture graph))))

(define (op:split-room context-tree) ;graph context-selector constraints)
  (let ((graph (context-tree:first-in-level context-tree 0))
        (room (context-tree:first-in-level context-tree 1)) ; TODO: with a room list
        (walls (context-tree:level context-tree 2))
        (split-points (context-tree:level context-tree 3)))
    ;; Pick up contexts and generate uids
    (let ((first-wall (car walls))
          (second-wall (cadr walls))
          (first-split-point (car split-points))
          (second-split-point (cadr split-points))
          (new-wall-uid (make-uuid))
          (first-wall-uid-1-half (make-uuid))
          (first-wall-uid-2-half (make-uuid))
          (second-wall-uid-1-half (make-uuid))
          (second-wall-uid-2-half (make-uuid)))
      (receive (fore aft)
               (room-break
                graph
                room
                (wall-uid first-wall)
                (wall-uid second-wall))
               (let ((graph/new-elements
                      (make-graph
                       (make-uuid)
                       (graph-environment graph)
                       `(
                         ;; Substitution of old room by the 2 new ones
                         ,@(remove (lambda (e) (equal? e room)) (graph-architecture graph))
                         ,(make-room (make-uuid)
                                     `(,@(cdr fore)
                                       ,first-wall-uid-1-half
                                       ,new-wall-uid
                                       ,second-wall-uid-2-half))
                         ,(make-room (make-uuid)
                                     `(,@(cdr aft)
                                       ,second-wall-uid-1-half
                                       ,new-wall-uid
                                       ,first-wall-uid-2-half))
                         ;; Splitting wall
                         ,(make-wall
                           new-wall-uid
                           (list
                            (pseq:relative-position->point (wall-pseq first-wall) first-split-point)
                            (pseq:relative-position->point (wall-pseq second-wall) second-split-point))
                           '()
                           '())
                         ;; Split touched walls at the splitting point (add 2 new ones)
                         ,@(let ((create-first-splitted-wall (curry create-splitted-wall first-wall first-split-point)))
                             (if (pseq:is-end-point?
                                  (wall-list->pseq (map (lambda (u) (graph:find-wall/uid graph u)) (cdr fore)))
                                  (first (wall-pseq first-wall)))
                                 (create-first-splitted-wall first-wall-uid-1-half first-wall-uid-2-half)
                                 (create-first-splitted-wall first-wall-uid-2-half first-wall-uid-1-half)))
                         ,@(let ((create-second-splitted-wall (curry create-splitted-wall second-wall second-split-point)))
                             (if (pseq:is-end-point?
                                  (wall-list->pseq (map (lambda (u) (graph:find-wall/uid graph u)) (cdr aft)))
                                  (first (wall-pseq second-wall)))
                                 (create-second-splitted-wall second-wall-uid-1-half second-wall-uid-2-half)
                                 (create-second-splitted-wall second-wall-uid-2-half second-wall-uid-1-half)))))))
                 (op:fix-room-topology
                  ;; Remove touched walls
                 (op:remove-multiple
                   ;; Update references of rooms to old walls
                   (update-wall-refs-in-rooms
                    (update-wall-refs-in-rooms
                     graph/new-elements
                     (wall-uid first-wall)
                     (list first-wall-uid-1-half
                           first-wall-uid-2-half))
                    (wall-uid second-wall)
                    (list  second-wall-uid-1-half
                           second-wall-uid-2-half))
                   (list first-wall
                         second-wall))))))))







(define (xml->sxml-graph xml-string)
  (let ((sxml (xml-string->sxml xml-string)))
    (car
     ((sxpath '(ensanche floorPlan architecture)) sxml))))


(define (main)
  (let ((graph (sxml-graph->graph
                (xml->sxml-graph
                 (let*
                     ((xml-file (open-input-file "../data/arch_1.xml"))
                      (xml-string (read-line xml-file #f))
                      (close-port xml-file))
                   xml-string)))))
    (let loop ((graph graph))
      (visualize-graph graph)
      (visualization:do-now)
      (visualization:forget-all)
      (loop
       (let ((room (biggest-room graph)))
        (op:split-room
         (receive (points walls)
                  (room-line-intersection
                   graph
                   room
                   (point+direction->line (vect2+
                                           (vect2:random)
                                        ;                                          (make-vect2 0 0)
                                           (pseq:centroid (room->pseq graph room))) ; TODO: limit random bias
                                          (direction:perpendicular
                                           (segment->direction
                                            (pseq->segment
                                             (wall-pseq
                                              (find-longest-wall-in-room graph room)))))))
                  (make-context-tree `[,graph
                                       ()
                                       (,room
                                        ()
                                        (,(car walls)
                                         (,(cadr walls)
                                          ()
                                          (,(cadr points)
                                           ()
                                           ()))
                                         (,(car points)
                                          ()
                                          ())))])))))))

  (exit 0))
(main)
