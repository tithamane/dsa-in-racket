#lang racket

#|
  #
  #   Node used in all the structures
  #
|#

(define-struct node (value [next #:mutable]))

#|
  #
  #      Create a basic stack implementation
  #
|#

; Stack struct
(define-struct stack ([root #:mutable]))

; Push value on to stack
(define (stack-push! S val)
  (define root (stack-root S))
  (define newNode (make-node val root))
  (set-stack-root! S newNode))

; Pop value from stack
(define (stack-pop! S)
  (define root (stack-root S))
  (if (null? root)
      '()
      (let (
            [newRoot (node-next root)])
        (set-stack-root! S newRoot)
        (node-value root))))

; Create list from stack
(define (stack->list S)
  (define (_stack->list node acc)
    (if (null? node)
        acc
        (let* ([value (node-value node)]
               [nextNode (node-next node)]
               [newAcc (cons value acc)])
          (_stack->list nextNode newAcc))))
  (define node (stack-root S))
  (define acc '())
  (reverse (_stack->list node acc)))

(displayln "===== Testing stack =====\n")
(define s1 (make-stack '()))
(displayln (stack->list s1))
(stack-push! s1 10)
(displayln (stack->list s1))
(stack-push! s1 20)
(displayln (stack->list s1))
(stack-push! s1 30)
(displayln (stack->list s1))
(stack-pop! s1)
(displayln (stack->list s1))
(stack-pop! s1)
(displayln (stack->list s1))
(stack-pop! s1)
(displayln (stack->list s1))

(displayln "\n===== End of stack test =====")


#|
  #
  #      Create a queue implementation
  #
|#

; Queue struct
(define-struct queue ([root #:mutable]))

; Add value to queue
(define (queue->enqueue! Q val)
  (define newNode (make-node val '()))
  (define (add-to-leaf ptr newNode)
    (define ptrNext (node-next ptr))
    (define ptrIsLeaf (null? ptrNext))
    (if ptrIsLeaf
        (set-node-next! ptr newNode)
        (add-to-leaf ptrNext newNode)))
  (define root (queue-root Q))
  (if (null? root)
      (set-queue-root! Q newNode)
      (add-to-leaf root newNode)))

; Remove value from queue
; Notice: Stack and Queue remove an item in the exact same way (Just noticed this)
(define (queue->dequeue! Q)
  (define ptr (queue-root Q))
  (set-queue-root! Q (node-next ptr))
  (node-value ptr))

; Create list from queue
(define (queue->list Q)
  (define (_queue->list node acc)
    (if (null? node)
        acc
        (let* ([nextNode (node-next node)]
               [nodeValue (node-value node)]
               [newAcc (cons nodeValue acc)])
          (_queue->list nextNode newAcc))))
  (define acc '())
  (define root (queue-root Q))
  (if (null? root)
      '()
      (reverse (_queue->list root acc))))

(displayln "===== Testing queue =====\n")
(define q1 (make-queue'()))
(displayln (queue->list q1))
(queue->enqueue! q1 10)
(displayln (queue->list q1))
(queue->enqueue! q1 20)
(displayln (queue->list q1))
(queue->enqueue! q1 30)
(displayln (queue->list q1))
(queue->dequeue! q1)
(displayln (queue->list q1))
(queue->dequeue! q1)
(displayln (queue->list q1))
(queue->dequeue! q1)
(displayln (queue->list q1))

#|
  #
  #      Create a basic linked-list implementation
  #
|#

(define-struct linked-list ([root #:mutable]))

; Add a value
(define (linked-list-append LL val)
  (define newNode (make-node val '()))
  (define (add-to-end ptr node)
    (define nextPtr (node-next ptr))
    (define ptrIsLeaf (null? nextPtr))
    (if ptrIsLeaf
        (set-node-next! ptr node)
        (add-to-end nextPtr node)))
  (define root (linked-list-root LL))
  (if (null? root)
      (set-linked-list-root! LL newNode)
      (add-to-end root newNode)))

;; Add item in an ordered fashing (Hopefully nothing was appended out of order)
; TODO: Can this be made more readable? It's not readable enough for me.
(define (linked-list-add LL val)
  (define root (linked-list-root LL))
  (define newNode (make-node val '()))
  (define (_set-node-next cur next)
    (set-node-next! cur next)
    cur)
  (define (_linked-list-add-in-order ptr newNode)
    (define newNodeValue (node-value newNode))
    (define ptrValue (node-value ptr))
    (define ptrValueLessThanNewValue (< ptrValue newNodeValue))
    (define ptrNext (node-next ptr))
    (if ptrValueLessThanNewValue
        (_set-node-next ptr (_linked-list-add-in-order ptrNext newNode))
        (_set-node-next newNode ptr)))
  (set-linked-list-root! LL (_linked-list-add-in-order root newNode)))


; Create a list from linked list
(define (linked-list->list LL)
  (define root (linked-list-root LL))
  (define (_linked-list->list ptr acc)
    (if (null? ptr)
        acc
        (let* ([value (node-value ptr)]
               [nextPtr (node-next ptr)]
               [newAcc (cons value acc)])
          (_linked-list->list nextPtr newAcc))))
  (define acc '())
  (if (null? root)
      acc
      (reverse (_linked-list->list root acc))))



; Removing a val from a linked list
; - If root is null then return
; - If root have value then set the next as head
; - Loop through list and remove the next node if it has the value
;; This works but it still looks NASTY
(define (linked-list-remove LL val)
  (define root (linked-list-root LL))
  (define (_linked-list-remove ptr val)
    (cond   [(null? ptr) '()]
            [(= (node-value ptr) val) (node-next ptr)]
            [else (begin
                    (set-node-next! ptr (_linked-list-remove (node-next ptr) val))
                    ptr)]))
  (cond   [(null? root) '()]
          [(= (node-value root) val) (set-linked-list-root! LL (node-next root))]
          [else (set-node-next! root (_linked-list-remove (node-next root) val))]))


(displayln "===== Testing linked-list =====\n")
(define ll1 (make-linked-list '()))
(displayln (linked-list->list ll1))
(linked-list-append ll1 10)
(displayln (linked-list->list ll1))
(linked-list-append ll1 20)
(displayln (linked-list->list ll1))
(linked-list-append ll1 30)
(displayln (linked-list->list ll1))
(displayln "Add values in order")
(linked-list-add ll1 25)
(displayln (linked-list->list ll1))
(linked-list-add ll1 15)
(displayln (linked-list->list ll1))
(linked-list-remove ll1 20)
(displayln (linked-list->list ll1))
(linked-list-remove ll1 15)
(displayln (linked-list->list ll1))
(linked-list-remove ll1 30)
(displayln (linked-list->list ll1))
(linked-list-remove ll1 10)
(displayln (linked-list->list ll1))
(linked-list-remove ll1 25)
(displayln (linked-list->list ll1))
(displayln "\n===== End of linked-list test =====")

