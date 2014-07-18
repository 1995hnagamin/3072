(load "./routine.scm")

(define (find-max lst key)
  (cond
    ((null? (cdr lst)) (car lst))
    ((> (key (car lst)) (key (cadr lst)))
     (find-max (cons (car lst) (cddr lst)) key))
    (else (find-max (cdr lst) key))))

(define (vec2 lst)
  (fold + 0 (map (lambda (x) (* x x)) lst)))

(define (square board)
  (fold + 0 (map vec2 board)))
