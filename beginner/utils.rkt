#lang racket

(provide (all-defined-out))
#|
	The methods in this file might-exist in racket but I need to know how
	to implement thems so I'll recreate them and prefix them with "t" from my name.
|#


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
    (null? l)
    acc
    (_t-length (rest l) (+ acc 1)))
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
