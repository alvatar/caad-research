;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph. As a requirement, they
;;; must leave the graph coherent after operating on it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (compile-options force-compile: #t)

(import (std srfi/1
             srfi/11
             misc/uuid)
        context
        core/command
        core/functional
        core/list
        core/debugging
        ds/n-ary
        geometry/kernel
        math/exact-algebra
        graph-operations
        graph-repairing
        graph)

(%activate-checks)

;-------------------------------------------------------------------------------
; Basic operations
;-------------------------------------------------------------------------------

;;; Identity

(define (op:identity context #!optional arguments) context)

;;; Add element

(define (op:add context arguments)
  (let ((graph context))
    (%accept (graph? graph) "only context currently accepted for adding is a whole graph")
    ;; TODO: A context with the graph and a room would allow adding the element and reference
    (make-graph
     (graph-uid graph)
     (graph-environment graph)
     (cons arguments (graph-architecture graph)))))

;;; Remove element from graph

(define (op:remove context arguments)
  (let ((graph context)
        (element arguments))
    (%accept (graph? graph) "only context currently accepted for adding is a whole graph")
    ;; TODO: remove references, too. IMPORTANT
    (make-graph
     (graph-uid graph)
     (graph-environment graph)
     (remove (lambda-equal? element) (graph-architecture graph)))))

;;; Remove element-list from graph

(define (op:remove-multiple context arguments)
  (let ((graph context)
        (le arguments))
    (%accept (graph? graph) "only context currently accepted for adding is a whole graph")
    ;; TODO: remove references, too. IMPORTANT
    (make-graph
     (graph-uid graph)
     (graph-environment graph)
     (remove (lambda (e) (any (lambda-equal? e) le)) (graph-architecture graph)))))

;;; Rename element

(define (op:rename context arguments)
  (let ((graph context)
        (element (get-arg arguments 'element))
        (name (get-arg arguments 'name)))
    (%accept (graph? graph) "only context currently accepted for adding is a whole graph")
    (op:add
     (op:remove graph element)
     (cond
       ((room? element) ; TODO: this, of course, would be better off with an object system
        (make-room name
                   (room-walls element)))
       (else
        (error "only room renaming is implemented"))))))

;;; Move an element within a constraining subspace, without changing topology
;;; @context: a 2-layers context containing the constraining space and the element
;;; @arguments: movement vector (1d or 2d depending on the constraining space)

(define (op:move-invariant context arguments)
  (let ((graph (n-ary:extract-level context 0))
        (constraining-subspace (n-ary:extract-level context 1))
        (element (car (n-ary:extract-level context 2)))
        (movement-vect (get-arg arguments 'movement)))
    (%accept (wall? element) "only walls can be moved at the moment")
    ))

;;; Move several elements if they don't change topology when moved together

(define (op:move-multiple-invariant context arguments)
  context)

;-------------------------------------------------------------------------------
; Topological operations
;-------------------------------------------------------------------------------

;;; Move an element within a constraining subspace, but where topological changes
;;; are allowed
;;; @context: a 2-layers context containing the constraining space and the element
;;; @arguments: movement vector (1d or 2d depending on the constraining space)

(define (op:move context arguments)
  (let ((graph (n-ary:extract-level context 0))
        (constraining-subspace (n-ary:extract-level context 1))
        (element (car (n-ary:extract-level context 2)))
        (movement-vect (get-arg arguments 'movement)))
    (%accept (wall? element) "only walls can be moved at the moment")
    ))

;;; The generic CUT operation: given a graph and a set of points with a minimum
;;; length of 2, first select the 2 walls where those first and last points lay
;;; in case they are not given. Then split the room if they belong to the same
;;; one and the path doesn't intersect any other wall.

(define (op:cut context arguments)
  (define (cut-room-opposed graph room walls split-points)
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
               (graph:room-break graph
                                 room
                                 (wall-uid first-wall)
                                 (wall-uid second-wall))
               (let-values (((splitted-wall-1a splitted-wall-1b split-status)
                             (let ((create-walls (curry graph:split-wall
                                                        first-wall
                                                        first-split-point)))
                               (if (pseq:is-end-point?
                                    (graph:wall-list->pseq (map (lambda (u) (graph:find.wall/uid graph u)) (cdr fore)))
                                    (first (wall-pseq first-wall)))
                                   (create-walls first-wall-uid-1-half first-wall-uid-2-half)
                                   (create-walls first-wall-uid-2-half first-wall-uid-1-half))))
                            ((splitted-wall-2a splitted-wall-2b split-status)
                             (let ((create-walls (curry graph:split-wall
                                                        second-wall
                                                        second-split-point)))
                               (if (pseq:is-end-point?
                                    (graph:wall-list->pseq (map (lambda (u) (graph:find.wall/uid graph u)) (cdr aft)))
                                    (first (wall-pseq second-wall)))
                                   (create-walls second-wall-uid-1-half second-wall-uid-2-half)
                                   (create-walls second-wall-uid-2-half second-wall-uid-1-half)))))
                 (let
                     ((graph/new-elements
                       (make-graph
                        (make-uuid)
                        (graph-environment graph)
                        `(
                          ;; Substitution of old room by the 2 new ones
                          ,@(remove (lambda-equal? room) (graph-architecture graph))
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
                          ,(make-wall-plain
                            new-wall-uid
                            (list
                             (pseq:1d-coord->point (wall-pseq first-wall) first-split-point)
                             (pseq:1d-coord->point (wall-pseq second-wall) second-split-point)))
                          ;; Split touched walls at the splitting point (add 2 new ones)
                          ,splitted-wall-1a
                          ,splitted-wall-1b
                          ,splitted-wall-2a
                          ,splitted-wall-2b))))
                   (graph:fix-wall-order
                    ;; Remove touched walls
                    (op:remove-multiple
                     ;; Update references of rooms to old walls
                     (graph:update-wall-refs-in-rooms
                      (graph:update-wall-refs-in-rooms
                       graph/new-elements
                       (wall-uid first-wall)
                       (list first-wall-uid-1-half
                             first-wall-uid-2-half))
                      (wall-uid second-wall)
                      (list  second-wall-uid-1-half
                             second-wall-uid-2-half))
                     (list first-wall
                           second-wall))))))))
  
  (if (< (n-ary:depth context) 2)       ; no information about walls
      (error "unimplemented op:cut wall finding with point arguments")
      (let ((graph (n-ary:extract-level context 0))
            (rooms (n-ary:extract-level context 1))
            (walls (n-ary:extract-level context 2))
            (split-points (get-arg arguments 'split-points)))
        (%accept #t "you didn't pass split-points to op:cut as arguments" split-points)
        (%accept (= (length rooms) 1) "can only cut one room currently")
        (let ((wall1 (car walls))
              (wall2 (cadr walls)))
          (cond
           ((equal? wall1 wall2)        ; same wall
            (error "same wall splitting is not implemented"))
           ((graph:walls-are-connected? wall1 wall2) ; connected walls
            (error "connected walls splitting is not implemented"))
           (else                        ; opposing walls
            (cut-room-opposed graph
                              (car rooms)
                              walls
                              split-points)))))))

;;; Merge two rooms

(define (op:merge context #!optional arguments)
  (let ((graph (n-ary:extract-level context 0))
        (rooms (n-ary:extract-level context 1))
        (possible-merge-uid-1 (make-uuid))
        (possible-merge-uid-2 (make-uuid)))
    (%deny (< (length rooms) 2) "The number of rooms is less than 2")
    (let ((room-a (car rooms))
          (room-b (cadr rooms)))
      (let ((common-wall-uid-lis
             (graph:filter.common-room-walls room-a room-b)))
        (%deny (null? common-wall-uid-lis) "The rooms to merge don't have common walls")
        (let ((common-wall-uid (if (> (length common-wall-uid-lis) 2)
                                   (error "trying to merge rooms that share more than one wall")
                                   (car common-wall-uid-lis)))) ; TODO: consider when more than one wall is common
          (receive
           (wall-bifurcations-1 wall-bifurcations-2)
           (apply/values
            (lambda (wall-list)
              (filter (lambda (w) (let ((wuid (wall-uid w)))
                               (or (graph:room-wall-uid? room-a wuid)
                                   (graph:room-wall-uid? room-b wuid))))
                      wall-list))
            (graph:find.walls-connected-to graph
                                           (graph:find.wall/uid graph
                                                                common-wall-uid)))
           (let ((context-walls-1 (graph:try-to-merge-if-parallel-walls
                                   wall-bifurcations-1
                                   possible-merge-uid-1))
                 (context-walls-2 (graph:try-to-merge-if-parallel-walls
                                   wall-bifurcations-2
                                   possible-merge-uid-2)))
             (let ((identify-invariant-walls-in-room
                    (let ((modified-walls-uids `(,@(map (lambda (w) (wall-uid w)) wall-bifurcations-1)
                                                 ,@(map (lambda (w) (wall-uid w)) wall-bifurcations-2)
                                                 ,common-wall-uid)))
                      (lambda (room) (remove-any equal? modified-walls-uids (room-walls room))))))
               ;; The common wall and its bifurcations are removed anyway and the later re-added once fixed,
               ;; also the merged rooms are removed
               (graph:fix-wall-order
                (make-graph
                 (graph-uid graph)
                 (graph-environment graph)
                 `(,@(remove-any (lambda (a e)
                                   (or (and (room? e)
                                            (equal? a e))
                                       (and (wall? e)
                                            (equal? a (wall-uid e)))))
                                 `(,@(map (lambda (w) (wall-uid w)) wall-bifurcations-1)
                                   ,@(map (lambda (w) (wall-uid w)) wall-bifurcations-2)
                                   ,common-wall-uid
                                   ,@rooms)
                                 (graph-architecture graph))
                   ;; Add the fixed bifurcations (if parallel) and the room taking new walls into account
                   ,@context-walls-1
                   ,@context-walls-2
                   ,(make-room (make-uuid)
                               (append (map (lambda (w) (wall-uid w)) context-walls-1)
                                       (identify-invariant-walls-in-room room-a)
                                       (map (lambda (w) (wall-uid w)) context-walls-2)
                                       (identify-invariant-walls-in-room room-b))))))))))))))

;-------------------------------------------------------------------------------
; Boundary modifications
;-------------------------------------------------------------------------------

;;; Expand

(define (op:expand context arguments)
  (error "unimplemented"))

;;; Shrink

(define (op:shrink context arguments)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Post-operations
;-------------------------------------------------------------------------------

;;; Stabilize structure: add, move or replace pilars (TODO)

(define (op:stabilize-structure context arguments)
  (error "unimplemented"))

;;; Snap walls to closest structure

(define (op:snap-walls/structure context arguments)
  (error "unimplemented"))
