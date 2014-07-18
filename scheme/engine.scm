(use srfi-27)
(load "./util.scm")

(define (move-row-left row)
  (let ((l (filter (compose not zero?) row)))
    (append l (make-list (- 4 (length l)) 0))))

(define (move-row-right row)
  (let ((l (filter (compose not zero?) row)))
    (append (make-list (- 4 (length l)) 0) l)))

(define (fold-row-left row)
  (cond
    ((> 2 (length row)) row)
    ((= (car row) (cadr row)) (cons (* 2 (car row)) (fold-row-left (cddr row))))
    (else (cons (car row) (fold-row-left (cdr row))))))

(define (fold-row-right row)
  (reverse (fold-row-left (reverse row))))

(define (voids board)
  (fold + 0 (map count-zero board)))

(define (remzero lst)
  (rember 0 lst))

(define (t matrix)
  (cond
    ((null? (car matrix)) '())
    (else
      (cons (map car matrix) (t (map cdr matrix))))))

(define (swipe move fold)
  (lambda (board)
    (map move (map fold (map remzero board)))))

(define swipe-left (swipe move-row-left fold-row-left))

(define swipe-right (swipe move-row-right fold-row-right))

(define (swipe-tt swipe)
  (lambda (board)
    (t (swipe (t board)))))

(define swipe-up (swipe-tt swipe-left))
(define swipe-down (swipe-tt swipe-right))

(define (printrow row)
  (if (null? row)
    (newline)
    (begin
      (display (car row))
      (display "\t")
      (printrow (cdr row)))))

(define (printb board)
  (for-each printrow board))

(define (printbn board)
  (printb board)
  (newline))

(define b '((0 0 0 0)
            (0 0 0 0)
            (0 2 0 0)
            (0 0 0 2)))

(define (set-at x y value board)
  (list-set y (list-set x value (list-ref board y)) board))

(define (get-at x y board)
  (list-ref (list-ref board y) x))

(define (zero-panels row x y)
  (if (zero? (car row))
    (cons (x . y) (zero-panels (+ 1 x) y))
    (zero-panels (+ 1 x) y)))

(define (zero-panels board)
  (letrec ((find-zero (lambda (row x y)
                        (if (zero? (car row))
                          (cons (x . y) (zero-panels (+ 1 x) y))
                          (zero-panels (+ 1 x) y)))))
    (

(define (or24)
  (if (= 0 (random-integer 4)) 2 4))

(define (hurt board)
  (let ((x (random-integer 4))
        (y (random-integer 4)))
    (if (= 0 (get-at x y board))
      (set-at x y (or24) board)
      (hurt board))))
