#lang racket

(require "f1-stack-queue-lists.rkt")

(define-struct node (value [left #:mutable] [right #:mutable]))
(define-struct binary-tree ([root #:mutable]))

(define (binary-tree-add BT val)
  (define (_insert-right ptrNode newNode)
    (define ptrRight (node-right ptrNode))
    (if (null? ptrRight)
        (set-node-right! ptrNode newNode)
        (_binary-tree-add ptrRight newNode)))
  (define (_insert-left ptrNode newNode)
    (define ptrLeft (node-left ptrNode))
    (if (null? ptrLeft)
        (set-node-left! ptrNode newNode)
        (_binary-tree-add ptrLeft newNode)))
  (define (_binary-tree-add ptrNode newNode)
    (define ptrValue (node-value ptrNode))
    (define newValue (node-value newNode))
    (if (< newValue ptrValue)
        (_insert-left ptrNode newNode)
        (_insert-right ptrNode newNode)))
  (define newNode (make-node val '() '()))
  (define root (binary-tree-root BT))
  (if (null? root)
      (set-binary-tree-root! BT newNode)
      (_binary-tree-add root newNode)))


(define (binary-tree-print-node-with-siblings node)
  (define (_print-node-with-siblings node)
    (define currentValue (node-value node))
    (define hasLeft (not (null? (node-left node))))
    (define hasRight (not (null? (node-right node))))
    (printf "Node(Value: ~a hasLeft: ~a hasRight: ~a)\n" currentValue hasLeft hasRight))
  (if (null? node)
      (displayln "-")
      (_print-node-with-siblings node)))


(define (binary-tree-find-node BT val)
  (define (_binary-tree-find-node node val)
    (define nodeValue (node-value node))
    (define hasLeftNode (not (null? (node-left node))))
    (define hasRightNode (not (null? (node-right node))))
    (define goLeft (and  (< val nodeValue) hasLeftNode))
    (define goRight (and (> val nodeValue) hasRightNode))
    (define leftNode (node-left node))
    (define rightNode (node-right node))
    (cond [(= nodeValue val) node]
          [goLeft (_binary-tree-find-node leftNode val)]
          [goRight (_binary-tree-find-node rightNode val)]
          [else '()]))
  (define root (binary-tree-root BT))
  (if (null? root)
      '()
      (_binary-tree-find-node root val)))


; (define (binary-tree-remove BT val)
;   (define (_binary-tree-remove node val)
;     '())
;   (define root (binary-tree-root BT))
;   (if (null? root)
;     '()
;     (_binary-tree-remove root val)



(define tree1 (make-binary-tree '()))

(define (dfs BT)
  (define (_dfs node ll)
    (unless (null? (node-left node))
      (_dfs (node-left node) ll))
    (linked-list-append ll (node-value node))
    (unless (null? (node-right node))
      (_dfs (node-right node) ll)))
  (define currentNode (binary-tree-root BT))
  (define ll (make-linked-list '()))
  (if (null? currentNode)
      ll
      (begin  (_dfs currentNode ll)
              ll)))


(displayln "===== Binary tree add and DFS =====")
(binary-tree-add tree1 50)
(displayln (linked-list->list (dfs tree1)))
(binary-tree-add tree1 25)
(displayln (linked-list->list (dfs tree1)))
(binary-tree-add tree1 75)
(displayln (linked-list->list (dfs tree1)))
(binary-tree-add tree1 35)
(displayln (linked-list->list (dfs tree1)))
(binary-tree-add tree1 15)
(displayln (linked-list->list (dfs tree1)))
(binary-tree-add tree1 85)
(displayln (linked-list->list (dfs tree1)))
(binary-tree-add tree1 65)
(displayln (linked-list->list (dfs tree1)))

(displayln "===== Finding and printing node =====")
(define tree2 (make-binary-tree '()))

(binary-tree-add tree2 8)

(binary-tree-add tree2 4)
(binary-tree-add tree2 12)


(binary-tree-add tree2 2)
(binary-tree-add tree2 5)
(binary-tree-add tree2 10)
(binary-tree-add tree2 14)


(binary-tree-add tree2 1)
(binary-tree-add tree2 3)
(binary-tree-add tree2 6)
(binary-tree-add tree2 7)
(binary-tree-add tree2 9)
(binary-tree-add tree2 11)
(binary-tree-add tree2 13)
(binary-tree-add tree2 15)

(define node9 (binary-tree-find-node tree2 2))
(define node9Left (node-left node9))
(define node9Right (node-right node9))
(binary-tree-print-node-with-siblings node9)
(binary-tree-print-node-with-siblings node9Left)
(binary-tree-print-node-with-siblings node9Right)