;; *Exercise 1.23:* The `smallest-divisor` procedure shown at the start of this
;; section does lots of needless testing: After it checks to see if the number
;; is divisible by 2 there is no point in checking to see if it is divisible by
;; any larger even numbers.  This suggests that the values used for
;; `test-divisor` should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9,
;; ....  To implement this change, define a procedure `next` that returns 3 if
;; its input is equal to 2 and otherwise returns its input plus 2.  Modify the
;; `smallest-divisor` procedure to use `(next test-divisor)` instead of `(+
;; test-divisor 1)`.  With `timed-prime-test` incorporating this modified
;; version of `smallest-divisor`, run the test for each of the 12 primes found
;; in *Note Exercise 1-22.  Since this modification halves the number of test
;; steps, you should expect it to run about twice as fast.  Is this expectation
;; confirmed?  If not, what is the observed ratio of the speeds of the two
;; algorithms, and how do you explain the fact that it is different from 2?
;;
;; -----------------------------------------------------------------------------


;; new or updated functions ----------------------------------------------------

;; provides the next number to check for primality.  the value of 2 is a special
;; case because it is the only even number that is a prime.  Otherwise, we can
;; assume that the input is an odd number so that adding 2 to it yeilds the next
;; smallest odd number.
(define (next x)
  (if (= 2 x)
      3
      (+ 2 x)))

;; NOTE: UPDATED VERSION.  Returns the smallest divisor of `n` that is greater
;; than or equal to than `test-divisor`
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))




;; procedures provided in Section 1.2.6 ----------------------------------------

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




;; procedures provided in the Exercise 1.22 prompt -----------------------------

;; prints `n` and checks to see if `n` is prime.  If `n` is prime, the procedure
;; prints three asterisks followed by the amount of time used in performing the
;; test.
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))


;; if `n` is prime then calls `report-time` with the elapsed runtime as the
;; argument, otherwise do nothing
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))


;; prints the value of `elapsed-time`
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))




;; procedure created for Exercise 1.22 -----------------------------------------

;; displays whether each odd number between `range-start` and `range-end`
;; (inclusive) is prime, and for values that are indeed primes then it displays
;; the length of time needed to perform the calculation

(define (search-for-primes range-start range-end)

  (define (timed-prime-test-driver curr-oddval range-end)
    (timed-prime-test curr-oddval)
    (search-for-primes-odd (+ 2 curr-oddval) range-end))

  (define (search-for-primes-odd curr-oddval range-end)
    (if (<= curr-oddval range-end)
	(timed-prime-test-driver curr-oddval range-end)))

  (if (<= range-start range-end)
      (if (even? range-start)
	  (search-for-primes-odd (1+ range-start) range-end)
	  (search-for-primes-odd range-start range-end))))




;; check new runtimes ----------------------------------------------------------

(search-for-primes    1000000000    1000000021)  ; 1e9
(search-for-primes   10000000000   10000000061)  ; 1e10
(search-for-primes  100000000000  100000000057)  ; 1e11
(search-for-primes 1000000000000 1000000000063)  ; 1e12




;; runtime output --------------------------------------------------------------

;; average runtimes for the various problem sizes (in microseconds?).  See the
;; computing systems specs after the runtimes.
;;
;;                                    ratio
;;     size  prev. ave.  new ave.  prev. / new
;;     ----  ---------   --------  -----------
;;      1e9       0.03       0.02         1.50
;;     1e10       0.08       0.06         1.33
;;     1e11       0.25       0.21         1.20
;;     1e12       0.80       0.63         1.26
;;
;; We see that the speedup is not close to 2.  This is presumably due to the
;; fact that we are replacing the function `+` with the slower `next` function
;; (slower since it requires a branch and a call to `+` for all inputs but `2`).

;; CPU specs for the above runtimes as provided by the output for `lscpu`
;;
;; Architecture:          x86_64
;; CPU op-mode(s):        32-bit, 64-bit
;; Byte Order:            Little Endian
;; CPU(s):                4
;; On-line CPU(s) list:   0-3
;; Thread(s) per core:    2
;; Core(s) per socket:    2
;; Socket(s):             1
;; NUMA node(s):          1
;; Vendor ID:             GenuineIntel
;; CPU family:            6
;; Model:                 142
;; Model name:            Intel(R) Core(TM) i5-7200U CPU @ 2.50GHz
;; Stepping:              9
;; CPU MHz:               700.022
;; CPU max MHz:           3100.0000
;; CPU min MHz:           400.0000
;; BogoMIPS:              5424.00
;; Virtualization:        VT-x
;; L1d cache:             32K
;; L1i cache:             32K
;; L2 cache:              256K
;; L3 cache:              3072K
;; NUMA node0 CPU(s):     0-3
