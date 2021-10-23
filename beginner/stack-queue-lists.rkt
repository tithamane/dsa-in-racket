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
  (define newRoot (make-node val (stack-root S)))
  (set-stack-root! S newRoot))

; Pop value from stack
(define (stack-pop! S)
  (if (null? (stack-root S))
      '()
      (let (
            [ptr (stack-root S)])
        (set-stack-root! S (node-next ptr))
        (node-value ptr))))

; Print the stack values as a list
(define (stack->list S)
  (define (_stack->list node acc)
    (if (null? node)
        acc
        (_stack->list (node-next node) (cons (node-value node) acc))))
  (let (
        [ptr (stack-root S)]
        [acc '()])
    (reverse (_stack->list ptr acc))))

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
  #      Create a queue stack implementation
  #
|#

; Queue struct
(define-struct queue ([root #:mutable]))

; Add value to enqueue
(define (queue->enqueue! Q val)
  (define newNode (make-node val '()))
  (define (add-to-leaf ptr newNode)
    (if (null? (node-next ptr))
        (set-node-next! ptr newNode)
        (add-to-leaf (node-next ptr) newNode)))
  (if (null? (queue-root Q))
      (set-queue-root! Q newNode)
      (add-to-leaf (queue-root Q) newNode)))

; Remove value from queue
; Notice: Stack and Queue remove an item in the exact same way (Just noticed this)
(define (queue->dequeue! Q)
  (define ptr (queue-root Q))
  (set-queue-root! Q (node-next ptr))
  (node-value ptr))

; Create array from queue
(define (queue->list Q)
  (define (_queue->list node acc)
    (if (null? node)
        acc
        (_queue->list (node-next node) (cons (node-value node) acc))))
  (let ([acc '()])
    (if (null? (queue-root Q))
        '()
        (reverse (_queue->list (queue-root Q) acc)))))

(displayln "===== Testing stack =====\n")
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

;; Create a basic linked-list implementation
