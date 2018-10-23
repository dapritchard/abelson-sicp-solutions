;; *Exercise 1.39:* A continued fraction representation of the
;; tangent function was published in 1770 by the German mathematician
;; J.H. Lambert:
;;
;;                    x
;;      tan x = ---------------
;;                      x^2
;;              1 - -----------
;;                        x^2
;;                  3 - -------
;;                      5 - ...
;;
;; where x is in radians.  Define a procedure `(tan-cf x k)` that
;; computes an approximation to the tangent function based on
;; Lambert's formula.  `k` specifies the number of terms to compute,
;; as in *Note Exercise 1.37.
;;
;; -----------------------------------------------------------------------------


;; continued fraction approx. for `tan(x)` -------------------------------------

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
(define (n-tan-fcn x k)
  (if (= 1 k)
      x
      (* -1 (* x x))))
(define (d-tan-fcn k)
  (- (* 2 k) 1))


;; uses Lambert's continued fraction expression to approximate `tan(x)` using
;; `k` terms
(define (tan-cf x k)
  (define (n-tan-x-fcn k)
    (n-tan-fcn x k))
  (cont-frac-iter n-tan-x-fcn d-tan-fcn k))




;; test cases ------------------------------------------------------------------

;; should be 0
(tan-cf 0 100)

;; should be approximately 1.557407724654902
(tan-cf 1 100)

;; should be approximately -2.185039863261519
(tan-cf 2 100)

;; should be approximately -0.1425465430742778
(tan-cf 3 100)

;; should be approximately 0
(tan-cf 3.141592653589793 100)
