;; *Exercise 1.16:* Design a procedure that evolves an iterative
;; exponentiation process that uses successive squaring and uses a
;; logarithmic number of steps, as does `fast-expt'.  (Hint: Using the
;; observation that (b^(n/2))^2 = (b^2)^(n/2), keep, along with the
;; exponent n and the base b, an additional state variable a, and
;; define the state transformation in such a way that the product a
;; b^n is unchanged from state to state.  At the beginning of the
;; process a is taken to be 1, and the answer is given by the value
;; of a at the end of the process.  In general, the technique of
;; defining an "invariant quantity" that remains unchanged from state
;; to state is a powerful way to think about the design of iterative
;; algorithms.)
;;
;; -------------------------------------------------------------------


;; wrapper function to initialize the starting variables
(define (fast-expt b n)
  (fast-expt-iter 1 b n))


;; iterative space exponentiation function.  The main idea is to keep the value
;; of `a * b^n` constant throughout each call while reducing the size of `n`.
;; When `n` is even, then we use the identity
;;
;;     a * b^n = a * (b^2)^(n / 2)
;;
;; to reduce `n`, and when `n` is odd then we use the identity
;;
;;     a * b^n = (a * b) * b^(n - 1)
;;
;; to reduce `n`.  Since `n` is always reduced it will eventually reach 0, at
;; which point we have the form `a * b^0`.  Since this is an invariant quantity,
;; it follows that `a` is the desired value.

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter a
				   (square b)
				   (/ n 2)))
	(else (fast-expt-iter (* a b)
			      b
			      (- n 1)))))
