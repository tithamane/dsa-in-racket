#lang racket

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