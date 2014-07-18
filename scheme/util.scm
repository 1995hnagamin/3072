(define (rember a lst)
  (cond
    ((null? lst) '())
    ((= a (car lst)) (rember a (cdr lst)))
    (else (cons (car lst) (rember a (cdr lst))))))

(define (list-set k value lst)
  (cond
    ((= k 0) (cons value (cdr lst)))
    (else (cons (car lst) (list-set (- k 1) value (cdr lst))))))

(define (maxs key lst)
  (letrec ((find-maxs (lambda (l found-maxs Max)
                        (print (list "maxs" l found-maxs Max))
                        (cond
                          ((null? l) found-maxs)
                          ((> (key (car l)) Max) (find-maxs (cdr l) (list (car l)) (key (car l))))
                          ((= (key (car l)) Max) (find-maxs (cdr l) (cons (car l) found-maxs) Max))
                          (else (find-maxs (cdr l) found-maxs Max))))))
    (find-maxs lst '() (key (car lst)))))

(define (compose . funcs)
  (cond
    ((null? funcs) (lambda (x) x))
    (else (lambda (x) ((car funcs) ((apply compose (cdr funcs)) x))))))

(define (apply-all 
