;; *Exercise 1.7:* The `good-enough?' test used in computing square
;; roots will not be very effective for finding the square roots of
;; very small numbers.  Also, in real computers, arithmetic operations
;; are almost always performed with limited precision.  This makes
;; our test inadequate for very large numbers.  Explain these
;; statements, with examples showing how the test fails for small and
;; large numbers.  An alternative strategy for implementing
;; `good-enough?' is to watch how `guess' changes from one iteration
;; to the next and to stop when the change is a very small fraction
;; of the guess.  Design a square-root procedure that uses this kind
;; of end test.  Does this work better for small and large numbers?
;;
;; ----------------------------------------------------------------------


;; define the square root routines from the text -------------------------------

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve-verbose guess x)
                 x)))

(define (improve-verbose guess x)
  (average-verbose guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

;; add some print statements to
(define (average-verbose x y)
  (display "x:   ")
  (display x)
  (newline)
  (display "y:   ")
  (display y)
  (newline)
  (display "res: ")
  (display (/ (+ x y) 2))
  (newline)
  (/ (+ x y) 2))


;; some test cases -------------------------------------------------------------

;; sqrt(10^{-8}) = 10^{-4}.  The result of this procedure is about 0.03125.
(sqrt-iter 1.0 0.00000001)

;; sqrt(10^13) does not converge
(sqrt-iter 1.0 10000000000000)

;; discussion:

;; In the case of the small input, the constant being compared against for
;; `good-enough` is just too big compared to the desired result so the relative
;; error is huge.
;;
;; In the case of the large input, you get two large numbers that are very close
;; together in the call to `average and the machine doesn't have enough
;; precision to find a number between the two, so as a result it just picks one
;; of the two original numbers, in this case the guess.  This in turn causes the
;; new value of guess (i.e. the value returned from `improve`) to be the same as
;; the previous value of guess, and hence the algorithm never converges.


;; modified comparison function ------------------------------------------------

(define (sqrt-iter-relative guess x)
  (if (good-enough-relative? guess x)
      guess
      (sqrt-iter-relative (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough-relative? guess x)
  (< (abs (/ (- (square guess) x)
	     guess))
     0.001))

(define (average x y)
  (/ (+ x y) 2))

;; some test cases.  In the small values case this strategy helps because it
;; forces a small difference between the two values.  In the large values case
;; this strategy helps because the convergence criteria is satisfied before the
;; machine precision limit is met.
(sqrt-iter-relative 1.0 2)
(sqrt-iter-relative 1.0 0.00000001)
(sqrt-iter-relative 1.0 10000000000000)
