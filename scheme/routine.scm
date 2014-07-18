(load "./game-engine.scm")

(define swipes (list swipe-right swipe-down swipe-up swipe-left))

(define (count-zero lst)
  (letrec ((count (lambda (l n)
                    (cond
                      ((null? l) n)
                      ((= 0 (car l)) (count (cdr l) (+ 1 n)))
                      (else (count (cdr l) n))))))
    (count lst 0)))

(define (find-max lst key)
  (cond
    ((null? (cdr lst)) (car lst))
    (else (max (car lst) (find-max (cdr lst) key)))))

(define (best-way-gen evaluator)
  (lambda (board choices)
    (let ((make-pair (lambda (func)
                       (cons (evaluator (func board)) func))))
      (find-max (map make-pair choices) car))))

(define (best-ways-gen evaluator)
  (lambda (board choices)
    (let* ((make-pair (lambda (func)
                        (cons (evaluator (func board)) func)))
           (mmc (map make-pair choices))
           (M (apply max (map car mmc))))
      (map cdr (filter (lambda (c) (= M (car c))) mmc)))))

(define (find-best-ways evaluator)
  (lambda (board choices)
    (let ((make-pair (lambda (func) (cons func (func board)))))
      

(define (swipe! swipe-xxx)
  (set! b (swipe-xxx b)))

(define (pp bway)
    (cond
      ((eq? bway swipe-right) (print "R"))
      ((eq? bway swipe-left ) (print "L"))
      ((eq? bway swipe-up   ) (print "U"))
      ((eq? bway swipe-down ) (print "D"))))

(define (step w)
  (let* ((choices (filter (lambda (swipe-xxx) ((swipable? b) swipe-xxx)) swipes))
         (bway (if (= 1 (length choices))
                 (car choices)
                 (cdr (w b choices)))))
    (begin
      (display b)
      (display " ")
      (pp bway)
      (swipe! bway)
      (set! b (hurt b)))))

(define (swipable? board)
  (lambda (swipe-xxx)
    (not (equal? board (swipe-xxx board)))))

(define (oor . args)
  (cond
    ((null? args) #f)
    ((car args) #t)
    (else (apply oor (cdr args)))))

(define (movable? board)
  (fold oor #f (map (swipable? board) swipes)))

(define (play w)
  (if (movable? b)
    (begin
      (step w)
      (play w))
    (print b)))

(define (step-by-step w)
  (if (movable? b)
    (begin
      (step w)
      (read-eval-print-loop)
      (step-by-step w))
    (display b)))

(define (SSB)
  (step-by-step best-way))
