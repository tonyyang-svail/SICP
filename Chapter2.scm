;Chapter2.scm - Yang Yang

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


(display "Exercise 2.2\n")

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")\n"))
(print-point (make-point 1 2))

(display "Exercise 2.6\n")
; (define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))
; (define zero (lambda (f) (lambda (x) x)))
; (define one  (lambda (f) (lambda (x) (f x))))
; (define two  (lambda (f) (lambda (x) (f (f x)))))

(display "Exercise 2.7\n")
(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(assert-eq (lower-bound (make-interval 1 2)) 1)
(define (upper-bound i) (cdr i))
(assert-eq (upper-bound (make-interval 1 2)) 2)

(display "Exercise 2.8\n")
(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))
(define (sub-interval x y)
  (add-interval x
                (make-interval
		  (- (upper-bound y))
		  (- (lower-bound y)))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (assert-eq-interval a b)
  (cond ((and (= (lower-bound a) (lower-bound b))
              (= (upper-bound a) (upper-bound b))) (display "pass\n"))
	(else (begin
	      (display "fail\n")
	      (exit 1)))))
(assert-eq-interval (make-interval -1 1)
                    (sub-interval (make-interval 1 2) (make-interval 1 2)))

(display "Exercise 2.10\n")
(define (div-interval x y)
  (cond ((<= (* (lower-bound y) (upper-bound y)) 0)
            (begin (display "divided by zero\n")))
        (else (mul-interval x
                            (make-interval
                             (/ 1.0 (upper-bound y))
                             (/ 1.0 (lower-bound y)))))))
(div-interval (make-interval 1 1) (make-interval -1 1))

(display "Exercise 2.14\n")
(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))
(define (display-interval x)
    (begin (display "[")
           (display (lower-bound x))
           (display ",")
	   (display (upper-bound x))
           (display "]")
	   (display "\n")))
(display-interval (par1 (make-interval 1 2) (make-interval 3 4)))
(display-interval (par2 (make-interval 1 2) (make-interval 3 4)))
; consider (div-interval A A) with (define A (make-interval 1 2))
; the arithmetic expression can be reduced to 1
; however, (div-interval A A) gives (make-interval (/ 1 2) 2)
