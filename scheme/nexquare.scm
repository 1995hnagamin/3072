(load "./routine.scm")

(define (mpp func lst)
  (cond
    ((null? lst) '())
    (else (cons (func (car lst)) (mpp func (cdr lst))))))

(define (largest-void boards)
  (apply max (mpp square boards)))

(define (largest-futures board)
  (largest-void (list (swipe-left board)
                     (swipe-right board)
                     (swipe-up board)
                     (swipe-down board))))

(define best-way (best-way-gen largest-futures))
