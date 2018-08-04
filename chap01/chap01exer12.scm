;; *Exercise 1.12:* The following pattern of numbers is called "Pascal's
;; triangle".
;;
;;              1
;;            1   1
;;          1   2   1
;;        1   3   3   1
;;      1   4   6   4   1
;;
;; The numbers at the edge of the triangle are all 1, and each number inside the
;; triangle is the sum of the two numbers above it.  Write a procedure that
;; computes elements of Pascal's triangle by means of a recursive process.
;;
;; -----------------------------------------------------------------------------


;; returns the binomial coefficient for parameters `n` and `k`.  Note the
;; binomial coefficient is also known as the "choose" function, since it gives
;; the number of ways of choosing k out of a total of n items (this is often
;; stated as "n choose k").
;;
;; In terms of the Pascal triangle, `n` is the row index and `k` is the column
;; index, using 0-based indexing.

(define (binom-coef n k)
  (cond ((= n 0) 1)
	((= k 0) 1)
	((= k n) 1)
	(else (+ (binom-coef (- n 1) k)
		 (binom-coef (- n 1) (- k 1))))))
