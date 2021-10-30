#lang racket

(require "utils.rkt")

;; This code works but confuses me too.

(define-struct max-heap ([values #:mutable]))

; Swap two indexes in an array
(define (swap-at-indexes L firstIndex secondIndex)
  (define firstValue (list-ref L firstIndex))
  (define secondValue (list-ref L secondIndex))
  (define (_change-at-index l index value)
    (define firstPart (take l index))
    (define lastPart (drop l (+ 1 index)))
    (append firstPart (list value) lastPart))
  (if (= (t-length L) 2)
      (list secondValue firstValue)
      (let* ([firstValueSwapped (_change-at-index L firstIndex secondValue)]
             [secondValueSwapped (_change-at-index firstValueSwapped secondIndex firstValue)])
        secondValueSwapped)))



(define (max-heap-add H val)
  (define values (max-heap-values H))
  (define newValues (append values (list val)))
  (set-max-heap-values! H newValues)
  (define lastIndex (- (t-length newValues) 1))
  (when (> lastIndex 0)
    (max-heap-shuffle-up H lastIndex)))

(define (get-parent-index index)
  (if (odd? index)
      (- (/ (+ index 1) 2) 1)
      (- (/ index 2) 1)))


(define (max-heap-shuffle-up H startIndex)
  (define parentIndex (get-parent-index startIndex))
  (define values (max-heap-values H))
  (define parentValue (list-ref values parentIndex))
  (define indexValue (list-ref values startIndex))
  (when (> indexValue parentValue)
    (begin
      (set-max-heap-values! H (swap-at-indexes values parentIndex startIndex))
      (when (> parentIndex 0)
        (max-heap-shuffle-up H parentIndex)))))

;; (define (max-heap-shuffle-down H startIndex)
;;   '())


(displayln "===== Adding to a max-heap =====")

(define heap1 (make-max-heap '()))

(displayln (max-heap-values heap1))

(max-heap-add heap1 10)
(displayln (max-heap-values heap1))
(max-heap-add heap1 20)
(displayln (max-heap-values heap1))
(max-heap-add heap1 15)
(displayln (max-heap-values heap1))
(max-heap-add heap1 25)
(displayln (max-heap-values heap1))
(max-heap-add heap1 22)
(displayln (max-heap-values heap1))
(max-heap-add heap1 50)
(displayln (max-heap-values heap1))
