;TestExample.scm - Yang Yang

;; Simple implmentation on assert equal
;; TODO(tony): support more types
(define (assert-eq a b)
  (cond ((= a b) (display "pass\n"))
	(else (begin
	      (display "fail\n")
	      (exit 1)))))

(define (assert-near a b)
  (cond ((< (abs (- a b)) 0.001) (display "pass\n"))
	(else (begin
	      (display "fail\n")
	      (exit 1)))))

(assert-eq 1 1)

;; Exercise 1.3
(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

(define (sum-of-squares-of-two-larger-numbers a b c)
  (cond ((and (>= a b) (>= c b)) (sum-of-squares a c))
        ((and (>= a c) (>= b c)) (sum-of-squares a b))
	(else (sum-of-squares b c))))

(assert-eq (sum-of-squares-of-two-larger-numbers 1 2 3) 13)
(assert-eq (sum-of-squares-of-two-larger-numbers 2 1 3) 13)
(assert-eq (sum-of-squares-of-two-larger-numbers 3 2 1) 13)

;; Exercise 1.5
;; FIXME: find the answer

;; Exercise 1.6
; I don't think new-if would make any difference though..

;; Newton's method

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (average a b)
    (/ (+ a b) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
(assert-near (sqrt 9) 3)
