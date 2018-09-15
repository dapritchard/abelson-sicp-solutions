;; *Exercise 1.21:* Use the `smallest-divisor` procedure to find the smallest
;; divisor of each of the following numbers: 199, 1999, 19999.
;;
;; -----------------------------------------------------------------------------


;; driver function to call the recursive `find-driver` function with starting
;; seed of 2 (i.e the first number to check against `n` as a divisor)
(define (smallest-divisor n)
  (find-divisor n 2))


;; test if `a` evenly divides `b`
(define (divides? a b)
  (= (remainder b a) 0))


;; test if `n` is a prime number, since by definition if the smallest divisor of
;; a number (other than 1) is itself, then the number is prime.
(define (prime? n)
  (= n (smallest-divisor n)))

;; returns the smallest divisor of `n` that is greater than or equal to than
;; `test-divisor`
;;
;; the main idea of the function is that we seed `find-divisor` with a value of
;; 2 as the input for `test-divisor`.  Then each call to `find-divisor` tests to
;; see if the current value of `test-divisor` evenly divides `n`, and if not,
;; recursively calls the function to test the next smallest integer.  If no
;; divisor is found, then the search can stop once the square of `test-divisor`
;; is larger than `n`.
;;
;; the above logic is coded into the `cond` statement as follows.  First, the
;; current value of `test-divisor` is tested to see if its square is larger than
;; `n`.  If it is, then the value of `n` is returned by the function (i.e. `n`
;; is the smallest divisor of `n`).  If not, then the call to `divides?` tests
;; to see if the current value is a divisor for `n`, and if it is indeed a
;; divisor then the value of `test-divisor` is returned.  Otherwise
;; `find-divisor` is recursively called with the next value to test as a divisor.
;;
;; thus we see that `find-divisor is recursively called from the `else` portion
;; of the `cond` statment until a number is returned by one of the first two
;; `cond` statements.  Once a value is returned by the function then it is
;; passed back to each recursive call until it reaches the top level and returns
;; to the original caller.

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))




;; check the primality of the numbers in the prompt ----------------------------

;; returns 199
(smallest-divisor 199)

;; returns 1999
(smallest-divisor 1999)

;; returns 7
(smallest-divisor 19999)
