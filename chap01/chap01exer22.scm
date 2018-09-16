;; *Exercise 1.22:* Most Lisp implementations include a primitive called
;; `runtime` that returns an integer that specifies the amount of time the
;; system has been running (measured, for example, in microseconds).  The
;; following `timed-prime-test` procedure, when called with an integer n, prints
;; n and checks to see if n is prime.  If n is prime, the procedure prints three
;; asterisks followed by the amount of time used in performing the test.
;;
;; Using this procedure, write a procedure `search-for-primes` that checks the
;; primality of consecutive odd integers in a specified range.  Use your
;; procedure to find the three smallest primes larger than 1000; larger than
;; 10,000; larger than 100,000; larger than 1,000,000.  Note the time needed to
;; test each prime.  Since the testing algorithm has order of growth of
;; \Theta(\sqrt{n}), you should expect that testing for primes around 10,000
;; should take about \sqrt(10) times as long as testing for primes around 1000.
;; Do your timing data bear this out?  How well do the data for 100,000 and
;; 1,000,000 support the \sqrt(n) prediction?  Is your result compatible with
;; the notion that programs on your machine run in time proportional to the
;; number of steps required for the computation?


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


;; returns the smallest divisor of `n` that is greater than or equal to than
;; `test-divisor`
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))




;; new procedures --------------------------------------------------------------

;; convenience function similar to built-in `1+` function
(define (1- n) (- n 1))


;; return the value of the smallest prime number that is at least as big as the
;; input for `start-val`
(define (find-next-prime start-val)
  (if (prime? start-val)
      start-val
      (find-next-prime (1+ start-val))))


;; finds the value of the `n`-th smallest prime number among the set of prime
;; numbers that are at least as big as the input for `start-val`
(define (find-nth-prime start-val n)
  (if (= 1 n)
      (find-next-prime start-val)
      (find-nth-prime (1+ (find-next-prime start-val)) (1- n))))


;; displays whether each odd number between `range-start` and `range-end`
;; (inclusive) is prime, and for values that are indeed primes then it displays
;; the length of time needed to perform the calculation
;;
;; the `if` expression in the body of `search-for-primes` is run once and simply
;; finds the next smallest odd number before calling `search-for-primes-odd`.
;; This function merely checks to see if the number to be tested is still in the
;; range (i.e is no more than `range-end`), and then calls
;; `timed-prime-test-driver` to perform the test and to call
;; `search-for-primes-odd` again with the next odd number.
;;
;; the use of `timed-prime-test-driver` is merely a device to allow us to call
;; two expressions after testing the range in `search-for-primes-odd`, using
;; only the syntax that we have been introduced to so far

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




;; compare runtimes for various numbers ----------------------------------------

;; find the third smallest prime number starting at the following values
(find-nth-prime    1000000000 3)  ; 1e9
(find-nth-prime   10000000000 3)  ; 1e10
(find-nth-prime  100000000000 3)  ; 1e11
(find-nth-prime 1000000000000 3)  ; 1e12

(search-for-primes    1000000000    1000000021)  ; 1e9
(search-for-primes   10000000000   10000000061)  ; 1e10
(search-for-primes  100000000000  100000000057)  ; 1e11
(search-for-primes 1000000000000 1000000000063)  ; 1e12




;; runtime output --------------------------------------------------------------

;; average runtimes for the various problem sizes (in microseconds?).  See the
;; computing systems specs after the runtimes.
;;
;;     size  ave. time  ratio
;;     ----  ---------  -----
;;      1e9       0.03
;;     1e10       0.08   2.67
;;     1e11       0.25   3.13
;;     1e12       0.80   3.20
;;
;; Recall that the algorithm has \Theta(sqrt(n)) complexity for prime numbers.  Since
;;
;;     sqrt(10 * n)     sqrt(10) * sqrt(n)
;;     ------------  =  ------------------  =  sqrt(10),
;;        sqrt(n)            sqrt(n)
;;
;; we would expect that the ratio for the runtimes between numbers that are a
;; factor of 10 apart would be roughly sqrt(10), which is approximately 3.16.
;; The values in the above table bear this expectation out.


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
