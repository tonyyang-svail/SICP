;Chapter3.scm - Yang Yang

; Util function
(define (assert-true x)
  (cond (x (display "pass\n"))
	(else (begin
	      (display "fail\n")
	      (exit 1)))))

(define (assert-eq a b)
  (cond ((equal? a b) (display "pass\n"))
	(else (begin
	      (display "fail\n")
	      (exit 1)))))

(define (assert-near a b)
  (cond ((< (abs (- a b)) 0.001) (display "pass\n"))
	(else (begin
	      (display "fail\n")
	      (exit 1)))))

(define nil '())

(display "Exercise 3.1\n")
(define (make-accumulator balance)
  (lambda (amount)
    (begin (set! balance (+ balance amount))
           balance)))
(define A (make-accumulator 5))
(assert-eq (A 10) 15)
(assert-eq (A 10) 25)

(display "Exercise 3.2\n")
(define (make-monitored func)
  (define (make-monitored-impl func init)
    (lambda (x)
      (if (eq? x 'how-many-calls?)
          init
          (begin (set! init (+ init 1))
                 (func x)))))
  (make-monitored-impl func 0))
(define s (make-monitored sqrt))
(assert-eq (s 'how-many-calls?) 0)
(assert-eq (s 49) 7)
(assert-eq (s 'how-many-calls?) 1)
(assert-eq (s 100) 10)
(assert-eq (s 'how-many-calls?) 2)

(display "Exercise 3.3\n")
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (lambda (x) 'incorrect-password))
	  ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request:
                 MAKE-ACCOUNT" m))))
  dispatch)
(define acc (make-account 100 'secret-password))
(assert-eq ((acc 'secret-password 'withdraw) 40) 60)
(assert-eq ((acc 'some-other-password 'deposit) 50) 'incorrect-password)

(display "Exercise 3.5\n")
