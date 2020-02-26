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

(display "Exercise 1.3\n")

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

(display "Newton's method\n")

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

;; Exercise 1.9
;; The first is recursice. The second is iterative.

(display "Exercise 1.11\n")

(define (f1.11 n)
  (cond ((< n 3) n)
        (else (+ (f1.11 (- n 1)) (* 2 (f1.11 (- n 2))) (* 3 (f1.11 (- n 3)))))
  )
)

(assert-eq (f1.11 0) 0)
(assert-eq (f1.11 1) 1)
(assert-eq (f1.11 2) 2)
(assert-eq (f1.11 3) 4)
(assert-eq (f1.11 4) 11)

(define (f1.11-iter-impl n1 n2 n3 count)
  (if (= count 0)
      n1
      (f1.11-iter-impl (+ n1 n2 n2 n3 n3 n3) n1 n2 (- count 1))
  )
)
(define (f1.11-iter n)
  (if (< n 3)
      n
      (f1.11-iter-impl 2 1 0 (- n 2))
  )
)

(assert-eq (f1.11-iter 4) 11)
(assert-eq (f1.11-iter 10) (f1.11 10))

(display "Exercise 1.19\n")

(define (fib-ref n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (fib-fast n)
  (define (fib-iter a b p q count)
    (cond ((= count 0)
           b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* q q) (* p p))     ;compute p'
                     (+ (* q q ) (* 2 p q))  ;compute q'
                     (/ count 2)))
          (else
           (fib-iter (+ (* b q)
                        (* a q)
                        (* a p))
                     (+ (* b p)
                        (* a q))
                     p
                     q
                     (- count 1)))))
  (fib-iter 1 0 0 1 n))

(assert-eq (fib-fast 42) (fib-ref 42))
(assert-eq (fib-fast 100) (fib-ref 100))
(assert-eq (fib-fast 2020) (fib-ref 2020))
