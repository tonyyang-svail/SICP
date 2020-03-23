;Chapter2.scm - Yang Yang

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

(display "Interval Util\n")
(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(assert-eq (lower-bound (make-interval 1 2)) 1)
(define (upper-bound i) (cdr i))
(assert-eq (upper-bound (make-interval 1 2)) 2)
(define (interval-eq a b)
  (and (= (lower-bound a) (lower-bound b))
       (= (upper-bound a) (upper-bound b))))

(display "List Util\n")
(define (list-deep-eq list1 list2)
  (if (and (null? list1) (null? list2))
      #t
      (and (= (length list1) (length list2))
           (if (pair? (car list1))
               (if (pair? (car list2))
                   (and (list-deep-eq (car list1) (car list2))
                        (list-deep-eq (cdr list1) (cdr list2)))
                   #f)
               (if (pair? (car list2))
                   #f
                   (and (= (car list1) (car list2))
                        (list-deep-eq (cdr list1) (cdr list2))))))))
(assert-true (list-deep-eq (list 1 2 3) (list 1 2 3)))
(assert-true (list-deep-eq (list (list 1) 2 3) (list (list 1) 2 3)))
(assert-true (not (list-deep-eq (list (list 1) 2 3) (list (list 1 2) 3))))
(assert-true (list-deep-eq (list 1 (list 2 3)) (list 1 (list 2 3))))
(define list-eq list-deep-eq)

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
(assert-true
  (interval-eq (make-interval -1 1)
               (sub-interval (make-interval 1 2) (make-interval 1 2))))

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

(display "Exercise 2.17\n")
(define (list-pair items)
  (if (null? (cdr items))
      items
      (list-pair (cdr items))))
(assert-true
  (list-eq (list-pair (list 1 2 3))
           (list 3)))

(display "Exercise 2.18\n")
(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items)) (list (car items)))))
(assert-true
  (list-eq (reverse (list 1 2 3))
           (list 3 2 1)))

(display "Exercise 2.20\n")
(define (my-filter f items)
  (if (null? items)
      items
      (if (f (car items))
          (cons (car items) (my-filter f (cdr items)))
          (my-filter f (cdr items)))))
(define (same-parity x . items)
  (define (f y) (= (modulo x 2) (modulo y 2)))
  (cons x (my-filter f items)))

(assert-true
  (list-eq (same-parity 1 2 3 4 5 6 7)
           (list 1 3 5 7)))
(assert-true
  (list-eq (same-parity 2 3 4 5 6 7)
           (list 2 4 6)))

(display "Exercise 2.27\n")
(define (deep-reverse items)
  (if (null? items)
      items
      (append (deep-reverse (cdr items))
              (if (pair? (car items))
	          (list (deep-reverse (car items)))
                  (list (car items))))))
(assert-true (list-deep-eq (deep-reverse ()) ()))
(assert-true (list-deep-eq (deep-reverse (list 1 (list 2 3)))
                           (list (list 3 2) 1)))
(assert-true (list-deep-eq (deep-reverse (list (list 1) (list 2 3) 4 (list 5 6) 7))
                           (list 7 (list 6 5) 4 (list 3 2) (list 1))))

(display "Exercise 2.28\n")
(define (fringe x)
  (if (null? x)
      x
      (append (if (pair? (car x))
                  (fringe (car x))
                  (list (car x)))
	      (fringe (cdr x)))))
(assert-true (list-deep-eq (fringe ()) ()))
(assert-true (list-deep-eq (fringe (list 1 2 3)) (list 1 2 3)))
(assert-true (list-deep-eq (fringe (list (list 1 2) 3)) (list 1 2 3)))
(assert-true (list-deep-eq (fringe (list 1 (list 2 3) 3)) (list 1 2 3 3)))
(assert-true (list-deep-eq (fringe (list 1 (list 2 (list 3)) 3)) (list 1 2 3 3)))

(display "Exercise 2.30\n")
(define tree-eq list-deep-eq)
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))
(assert-true (tree-eq
  (square-tree (list 1
                     (list 2 (list 3 4) 5)
                     (list 6 7)))
  (list 1
        (list 4 (list 9 16) 25)
        (list 36 49))))

(display "Exercise 2.31\n")
(define (tree-map func tree)
  (map (lambda (node)
         (if (pair? node)
             (tree-map func node)
             (func node)))
       tree))
(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))
(assert-true (tree-eq
  (square-tree (list 1
                     (list 2 (list 3 4) 5)
                     (list 6 7)))
  (list 1
        (list 4 (list 9 16) 25)
        (list 36 49))))
  
(display "Exercise 2.32\n")
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
(assert-eq (length (subsets (list 1 2 3))) 8)
(assert-eq (length (subsets (list 1 2 3 4))) 16)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(display "Exercise 2.33\n")
(define (acc-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) 
              nil sequence))
(assert-true (list-eq (acc-map square (list 1 2 3)) (list 1 4 9)))

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))
(assert-true (list-eq (acc-append (list 1 2) (list 3 4)) (list 1 2 3 4)))

(define (acc-length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0 sequence))
(assert-eq (acc-length (list 1 2 3 4)) 4)

(display "Exercise 2.34\n")
(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))
(assert-eq (horner-eval 2 (list 1 3 0 5 0 1)) 79)

(display "Exercise 2.35\n")
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
	             (if (pair? x)
	                 (count-leaves x)
			 1))
		   t)))
(assert-eq (count-leaves (list 1 2 3 4 5 6 7)) 7)
(assert-eq (count-leaves (list 1 (list 2 3) (list 4 (list 5 6)) 7)) 7)

(display "Exercise 2.36\n")
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
(assert-true
  (list-eq (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
           (list 22 26 30)))

(display "Exercise 2.37\n")
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(assert-true
  (list-eq (matrix-*-vector (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)) (list 1 0 0 2))
           (list 9 16 24)))
(define (transpose mat)
  (accumulate-n cons nil mat))
(assert-true
  (list-eq (transpose (list (list 1 2 3) (list 4 5 6)))
           (list (list 1 4) (list 2 5) (list 3 6))))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))
(assert-true
  (list-eq (matrix-*-matrix (list (list 1 2 3) (list 4 5 6)) (list (list 1 4) (list 2 5) (list 3 6)))
           (list (list 14 32) (list 32 77))))

(display "Exercise 2.40\n")
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))
(define (unique-pairs n)
  (accumulate
   append
   nil
   (map (lambda (i)
          (map (lambda (j)
                 (list i j))
               (enumerate-interval (+ i 1) n)))
        (enumerate-interval 1 n))))
(assert-true
  (list-eq (unique-pairs 3)
           (list (list 1 2) (list 1 3) (list 2 3))))

(display "Exercise 2.41\n")
(define (unique-triples n)
  (accumulate append nil
    (accumulate append nil
      (map (lambda (i)
             (map (lambda (j)
                    (map (lambda (k) (list i j k))
                         (enumerate-interval (+ j 1) n)))
                  (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 n)))))
(define (find-triple-sum-s s)
  (filter (lambda (t)
            (= s
	       (+ (car t)
	          (cadr t)
	          (cadr (cdr t)))))
          (unique-triples s)))
(assert-true
  (list-eq (find-triple-sum-s 10)
           (list (list 1 2 7) (list 1 3 6) (list 1 4 5) (list 2 3 5))))

(display "Symbolic Representation\n")
(assert-eq '(a b) (list 'a 'b))
; ' is simply a escape character for the Scheme interpreter

(display "Exercise 2.59\n")
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1)
                          set2)))
        (else (union-set (cdr set1)
                         set2))))
(assert-eq (union-set '() '()) '())
(assert-eq (union-set '(1) '()) '(1))
(assert-eq (union-set '(1 2) '()) '(1 2))
(assert-eq (union-set '(1) '(2)) '(1 2))
(assert-eq (union-set '(1 2 3 4 5) '(2 3 4 5 6)) '(1 2 3 4 5 6))

