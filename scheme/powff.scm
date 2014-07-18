(load "./routine.scm")
(load "./square.scm")
(load "./nextep.scm")
(load "./nexquare.scm")
(load "./nxnxq.scm")

(define (vxk v x k)
  (let ((vv (best-ways-gen v))
        (xx (best-ways-gen x))
        (kk (best-way-gen  k)))
    (lambda (board choices)
      (kk board (xx board (vv board choices))))))

(define best-way (vxk largest-future16
                      largest-futurev
                      largest-futures))
