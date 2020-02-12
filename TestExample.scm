;TestExample.scm - Yang Yang

;; Simple implmentation on assert equal
;; TODO(tony): support more types
(define (assert-eq a b)
  (cond ((= a b) (display "pass\n"))
	(else (begin (display "fail\n") (exit 1)))))

(assert-eq 1 1)
(assert-eq 1 -1)

