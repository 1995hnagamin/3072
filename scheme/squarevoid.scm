(load "./routine.scm")
(load "./square.scm")

(define v (best-ways-gen voids))

(define x (best-way-gen square))

(define (best-way board choices)
  (x board (v board choices)))
