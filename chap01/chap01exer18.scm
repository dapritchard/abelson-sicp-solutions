;; *Exercise 1.18:* Using the results of *Note Exercise 1-16:: and
;; *Note Exercise 1-17::, devise a procedure that generates an
;; iterative process for multiplying two integers in terms of adding,
;; doubling, and halving and uses a logarithmic number of steps.
;;
;; ------------------------------------------------------------------


(define (double x)
  (+ x x))


(define (half x)
  (arithmetic-shift x -1))


(define (prod a b)
  (prod-iter 0 a b))


;; iterative space multiplication function.  The main idea is to keep the value
;; of `x + (a * b)` constant throughout each call while reducing the size of
;; `b`.  When `b` is even, then we use the identity
;;
;;     x + (a * b) = x + ((2 * a) * (b / 2))
;;
;; to reduce `b`, and when `b` is odd then we use the identity
;;
;;     x + (a * b) = (x + a) + (a * (b - 1))
;;
;; to reduce `b`.  Since `b` is always reduced it will eventually reach 0, at
;; which point we have the form `x + a*0`.  Since this is an invariant quantity,
;; it follows that `x` is the desired value.

(define (prod-iter x a b)
  (cond ((= b 0) x)
	((even? b) (prod-iter x
			      (double a)
			      (half b)))
	(else (prod-iter (+ x a)
			 a
			 (- b 1)))))
