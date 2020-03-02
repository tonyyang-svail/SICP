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

(display "Exercise 1.29\n")

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y_k k) (f (+ a (* k h))))
  (* (/ h 3)
     (+ (sum y_k 0 (lambda (x) (+ x 1)) n)       ;; y0 + y1 + y2 + y3 + ... + yn-1 + yn
        (sum y_k 1 (lambda (x) (+ x 1)) (- n 1)) ;;      y1 + y2 + y3 + ... + yn-1
        (sum y_k 1 (lambda (x) (+ x 2)) (- n 1)) ;;      y1   +    y3 + ... + yn-1
        (sum y_k 1 (lambda (x) (+ x 2)) (- n 1)) ;;      y1   +    y3 + ... + yn-1
     )
  ))

(display (integral cube 0 1 100))
(display "\n")
(display (integral cube 0 1 1000))
(display "\n")
;; The result is exactly 1/4.

(display "Exercise 1.31\n")

(define (calc-pi n)
  (define (factorial term a next b)
    (if (> a b)
        1
        (* (term a)
           (factorial term (next a) next b))))
  (* 4
     (factorial
       (lambda (x)
         (if (even? x)
             (/ (+ x 2) (+ x 3))
             (/ (+ x 3) (+ x 2))))
       0 (lambda (x) (+ x 1)) n)))
(display (exact->inexact (calc-pi 10000)))
(display "\n")

(display "Exercise 1.34\n")
;; (define (f g) (g 2))
;; (f f) gives error: object 2 is not applicable
;;    (f f)
;; => (f 2)
;; => (2 2) error!

(display "Exercise 1.35\n")
(define (fixed-point f first-guess)
  (define tolerance 0.000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (average a b)
    (/ (+ a b) 2))
  (define (try guess)
    (let ((next (average (f guess) guess)))
;    (let ((next (f guess)))
      (begin
;        (display next)
;        (newline)
        (if (close-enough? guess next)
            next
            (try next)))))
  (try first-guess))
(assert-near (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) 1.618)

(display "Exercise 1.36\n")
(assert-near (expt
               (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
               (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1))
	     1000)

(display "Exercise 1.37\n")
(define (cont-frac n d k)
  (define (cont-frac-iter n d i)
    (if (= i k)
        (/ (n i) (d i))
	(/ (n i) (+ (d i) (cont-frac-iter n d (+ i 1))))))
  (cont-frac-iter n d 1))
(assert-near (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8) 0.618)

(display "Exercise 1.38\n")
(assert-near (cont-frac
                (lambda (i) 1.0)
	        (lambda (i)
	          (if (= (modulo i 3) 2)
	              (* 2 (/ (+ i 1) 3))
	              1))
                10)
	       (- (exp 1.0) 2))

(display "Exercise 1.40\n")
(define (newtons-method g guess)
  (define (deriv g)
    (define dx 0.00001)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx)))
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x)
              ((deriv g) x)))))
  (fixed-point (newton-transform g)
               guess))
(assert-near (newtons-method (lambda (x) (+ (* x x x) x)) 1) 0.0)

(display "Exercise 1.41\n")
(define (double f) (lambda (x) (f (f x))))
(assert-eq (((double (double double)) (lambda (x) (+ x 1))) 5) 21)

(display "Exercise 1.42\n")
(define (compose f g) (lambda (x) (f (g x))))
(assert-eq ((compose
               (lambda (x) (* x x))
	       (lambda (x) (+ x 1))) 6) 49)
