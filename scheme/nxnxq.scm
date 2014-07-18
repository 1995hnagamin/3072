(load "./routine.scm")
(load "./square.scm")

(define (mpp func lst)
  (cond
    ((null? lst) '())
    (else (cons (func (car lst)) (mpp func (cdr lst))))))

(define (largest-square boards)
  (apply max (mpp square boards)))

(define (swipeswipe board)
  (list (swipe-left board) (swipe-right board)
        (swipe-up   board) (swipe-down  board)))

(define (swipe4 board)
  (apply append (map swipeswipe (swipeswipe board))))

(define (largest-future16 board)
  (largest-square (swipe4 board)))

(define best-way (best-way-gen largest-future16))

;(play best-way)
