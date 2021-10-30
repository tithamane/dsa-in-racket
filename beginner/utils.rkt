#lang racket

(provide (all-defined-out))
#|
	The methods in this file might-exist in racket but I need to know how
	to implement thems so I'll recreate them and prefix them with "t" from my name.
|#


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





(define (t-reverse L)
  (define (_t-reverse oldList newList)
    (if (null? oldList)
        newList
        (let*  ([oldListHead (first oldList)]
                [oldListTail (rest oldList)]
                [updatedList (cons oldListHead newList)])
          (_t-reverse oldListTail updatedList))))
  (_t-reverse L '()))


(define (t-max L)
  (define (_t-max val l)
    (if (null? l)
        val
        (if (> val (first l))
            (_t-max val (rest l))
            (_t-max (first l) (rest l)))))
  (_t-max (first L) (rest L)))


(define (t-length L)
  (define (_t-length l acc)
    (if (null? l)
        acc
        (_t-length (rest l) (+ acc 1))))
  (_t-length L 0))

(define (t-make-list size)
  (define mainList '())
  (define (add-item counter l item)
    (if (= counter 0)
        l
        (add-item (- counter 1) (cons item l) '())))
  (add-item size mainList '()))

(define (t-grow-list L newSize)
  (define currentSize (t-length L))
  (if (= currentSize newSize)
      L
      (t-grow-list (append L '()) newSize)))
