(load "./engine.scm")

(define (movegen swipe-xxx)
  (lambda ()
    (begin
      (set! b (swipe-xxx b))
      (printb b))))

(define L (movegen swipe-left))
(define R (movegen swipe-right))
(define U (movegen swipe-up))
(define D (movegen swipe-down))

(printb b)
