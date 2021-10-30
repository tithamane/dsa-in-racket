#lang racket

(require "utils.rkt")

(define-struct max-heap ([size #:mutable] [capacity #:mutable] [values #:mutable]))

(define (change-value-at-index L index value)
	(define (_change-value-at-index oldList currentIndex targetList)
    (cond 
    [(null? oldList) targetList]
    [(= currentIndex index)
      (_change-value-at-index (rest oldList) (+ currentIndex 1) (cons value targetList))]
      [else (change-value-at-index (rest oldList) (+ currentIndex 1)) (cons (first oldList) targetList)]))
  (t-reverse (_change-value-at-index L index '())))


; (define (max-heap-ensure-capacity H)
;   (define size (max-heap-size H))
;   (define currentCapacity (max-heap-size H))
;   (if (= currentCapacity size)
;     (t-grow-list L capacity)
;     L))


; (define (max-heap-add H val)
;   (define heapSize (max-heap-size H))
;   (define heapCapacity (max-heap-capacity H))
;   (when (= heapSize heapCapacity)



(displayln "===== Testing Current List Capacity =====")

(define heap1 (make-max-heap 0 8 (t-make-list 8)))
(displayln (max-heap-values heap1))

(displayln (max-heap-values heap1))
