;; *Exercise 1.38:* In 1737, the Swiss mathematician Leonhard Euler published a
;; memoir `De Fractionibus Continuis`, which included a continued fraction
;; expansion for `e - 2`, where `e` is the base of the natural logarithms.  In
;; this fraction, the `N_i` are all 1, and the `D_i` are successively 1, 2, 1,
;; 1, 4, 1, 1, 6, 1, 1, 8, ....  Write a program that uses your `cont-frac`
;; procedure from *Note Exercise 1.37 to approximate `e`, based on Euler`s
;; expansion.
;;
;; -----------------------------------------------------------------------------


;; continued fraction approx. for `e` ------------------------------------------

;; calculates a k-term finite continued fraction where the values of `N_i` and
;; `D_i` are given by evaluating `n-fcn` and `d-fcn` at i.  Note that we add by
;; 0.0 during one of the calculations to ensure the use of floating point
;; operations (instead of fractional).
(define (cont-frac-iter n-fcn d-fcn k)
  (define (calc-inner-term n-k d-k inner-result)
    (/ n-k (+ 0.0 d-k inner-result)))
  (define (i-th-level i inner-result)
    (if (= 0 i)
	inner-result
	(i-th-level (1- i)
		    (calc-inner-term (n-fcn i)
				     (d-fcn i)
				     inner-result))))
  (i-th-level (1- k)
	      (/ (n-fcn k)
		 (d-fcn k))))


;; functions for the `N_i` and `D_i` calculations
(define (n-1-fcn k) 1)
(define (d-euler-e-fcn k)
  (if (not (= 0 (remainder (1+ k) 3)))
      1
      (* 2 (/ (1+ k) 3))))


;; uses Euler's continued fraction expression to approximate `e` using `k` terms
(define (e-cf k)
  (+ 2 (cont-frac-iter n-1-fcn d-euler-e-fcn k)))




;; test cases ------------------------------------------------------------------

;; should be approximately 2.718281828459045
(e-cf 100)
