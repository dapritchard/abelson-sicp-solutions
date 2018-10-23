;; *Exercise 1.37:*
;;
;;   a. An infinite "continued fraction" is an expression of the form
;;
;;                      N_1
;;           f = ---------------------
;;                          N_2
;;               D_1 + ---------------
;;                              N_3
;;                     D_2 + ---------
;;                           D_3 + ...
;;
;;      As an example, one can show that the infinite continued fraction
;;      expansion with the n_i and the D_i all equal to 1 produces 1/phi,
;;      where phi is the golden ratio (described in section *Note 1.2.2).  One
;;      way to approximate an infinite continued fraction is to truncate the
;;      expansion after a given number of terms.  Such a truncation--a so-called
;;      finite continued fraction "k-term finite continued fraction"--has the
;;      form
;;
;;                  N_1
;;           -----------------
;;                     N_2
;;           D_1 + -----------
;;                 ...    N_K
;;                     + -----
;;                        D_K
;;
;;      Suppose that `n` and `d` are procedures of one argument (the term index
;;      i) that return the n_i and D_i of the terms of the continued fraction.
;;      Define a procedure `cont-frac` such that evaluating `(cont-frac n d k)`
;;      computes the value of the k-term finite continued fraction.  Check your
;;      procedure by approximating 1/phi using
;;
;;           (cont-frac (lambda (i) 1.0)
;;                      (lambda (i) 1.0)
;;                      k)
;;
;;      for successive values of `k`.  How large must you make `k` in order to
;;      get an approximation that is accurate to 4 decimal places?
;;
;;  b. If your `cont-frac` procedure generates a recursive process, write one
;;     that generates an iterative process.  If it generates an iterative
;;     process, write one that generates a recursive process.
;;
;; -----------------------------------------------------------------------------




;; part a (linear version) -----------------------------------------------------

;; ````````````````````````````````````````````````````````````````````````````
;; we can either count up starting from 1 towards `k` or count down starting
;; from `k` towards 1.  It turns out that starting from 1 and counting up
;; naturally leads to a recursive version
;; .............................................................................

;; linear version.  Calculates a k-term finite continued fraction where the
;; values of `N_i` and `D_i` are given by evaluating `n-fcn` and `d-fcn` at i.
;; Note that we add by 0.0 during one of the calculations to ensure the use of
;; floating point operations (instead of fractional).
(define (cont-frac n-fcn d-fcn k)
  (define (i-th-level i)
    (if (> i k)
	0
	(/ (n-fcn i)
	   (+ 0.0
	      (d-fcn i)
	      (i-th-level (1+ i))))))
  (i-th-level 1))




;; part b (iterative version) --------------------------------------------------

;; `````````````````````````````````````````````````````````````````````````````
;; counting down from `k` naturally leads to an iterative version
;; .............................................................................

;; iterative version.  Calculates a k-term finite continued fraction where the
;; values of `N_i` and `D_i` are given by evaluating `n-fcn` and `d-fcn` at i.
;; Note that we add by 0.0 during one of the calculations to ensure the use of
;; floating point operations (instead of fractional).
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




;; define helper function ------------------------------------------------------

(define MAX-ITER 1000000)
(define (1- x) (- x 1))

;; test whether the difference between `actual` and `target` is less than `eps`
(define (close-enough? actual target eps)
  (< (abs (- actual target)) eps))


;; repeatedly evaluate `f`, a function that takes a single integer input, until
;; the difference beteen the result of `f` and `target` is less than `eps`.  The
;; values passed to `f` start at 1 and increase by 1 each time, and the return
;; value is the number of attempts that are necessary for an evaluation clsoe to
;; `target` is found.
(define (find-first-close f target eps)
  (define (try i)
    (cond ((> i MAX-ITER) (error "sequence did not converge"))
	  ((close-enough? (f i) target eps) i)
	  (#t (try (1+ i)))))
  (try 1))




;; test cases ------------------------------------------------------------------

;; the continued fraction expression for 1 / phi
(define (n-1-fcn i) 1)
(define (d-1-fcn i) 1)
(define (frac-phi-inv k)
  (cont-frac-iter n-1-fcn d-1-fcn k))


;; the continued fraction expression for pi.  See
;; \url{http://people.math.binghamton.edu/dikran/478/Ch7.pdf} for details.
(define (sqaure x) (* x x))
(define (n-pi-fcn i)
  (square (1- (* 2 i))))
(define (d-pi-fcn i)
  6)
(define (frac-pi k)
  (+ 3 (cont-frac-iter n-pi-fcn d-pi-fcn k)))


;; 1 / phi:  should be approximately 0.6180339887498949
(frac-phi-inv 100)

;; pi:  should be approximately 3.1415926535897932
(frac-pi 10000)

;; the size of `k` needed to approximate the value of 1 / phi to 4 decimal places
(find-first-close frac-phi-inv 0.6180339887498949 0.00001)
