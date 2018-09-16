;; *Exercise 1.24:* Modify the `timed-prime-test` procedure of *NoteExercise
;; 1.22 to use `fast-prime`? (the Fermat method), and test each of the 12 primes
;; you found in that exercise.  Since the Fermat test has \Theta(log(n)) growth,
;; how would you expect the time to test primes near 1,000,000 to compare with
;; the time needed to test primes near 1000?  Do your data bear this out?  Can
;; you explain any discrepancy you find?
;;
;; -----------------------------------------------------------------------------


;; Fermat's Little Theorem discussion ------------------------------------------

;; *Fermat's Little Theorem:* Let `n` be a prime number, and `a` is a positive
;; integer with `a < n`.  Then `a^n` is congruent to `a` modulo `n`.
;;
;; Two numbers are said to be "congruent modulo" `n` if they both have the same
;; remainder when divided by n.  So the above theorem is equivalent to `a^n mod
;; n = a mod n = a`.
;;
;; Furthermore, it is claimed in the text that if on the other hand `n` is not a
;; prime number, for most positive integers `a < n` then `a^n mod n \ne a`.
;;
;; Thus we can devise a test for primality as follows.  Pick a value `a < n` at
;; random.  If `a^n mod n \ne a` then we know the number is not prime.  If
;; however they are equal, then we know that `n` is most likely a prime number.
;; If we want to be even more sure that `n` is a prime number then we can repeat
;; the test with different choices of `a`.




;; Fermat test routines from Section 1.2.6 -------------------------------------

;; compute `base^exp mod m`
;;
;; The main idea is to keep the value of `base^exp mod m` constant throughout
;; each call while reducing the size of `exp`.  When `exp` is even, then we use
;; the identity
;;
;;     base^exp mod m = (base^(exp / 2))^2 mod m
;;
;;                    = ((base^(exp / 2)) mod m)^2 mod m
;;
;; to reduce `exp`, and the `(base^(exp / 2)) mod m` term can then be evaluated
;; by a recursive call to `expmod`.  Note that the second equality is due to the
;; general rule `a * b mod m = ((a mod m) * (b mod m)) mod m` for `a = b =
;; base^(exp / 2)`.
;;
;; When `exp` is odd then we use the identity
;;
;;     base^exp mode m = base * base^(exp - 1) mod m
;;
;;                     = ((base mod m) * (base^(exp - 1) mod m)) mod m
;;
;;                     = (base * (base^(exp - 1) mod m)) mod m
;;
;; to reduce `exp`, and the `base^(exp - 1) mod m` term can then be evaluated by
;; a recursive call to `expmod`.  Note that the last equality is due to the fact
;; that `(a * (b mod m)) mod m = ((a mod m) * ((b mod m) mod m) mod m = ((a mod
;; m) * (b mod m)) mod m`.
;;
;; Since `exp` is always reduced it will eventually reach 0, at which point we
;; have the base case of `base^0 mod m = 1`.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))


;; selects a random integer from 1 to `n - 1` inclusive, and returns `#t` if
;; `a^n mod n = a`, or `#f` otherwise

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


;; tests the primality for the input for `n`.  The function returns either `#t`
;; which means that `n` is highly likely to be a prime number, or `#f` which
;; means that it is guaranteed to not be a prime number.  The input for `times
;; has the effect of increasing the certainty that a return value of `#t` is a
;; true positive.
;;
;; `fast-prime?` uses a probabilistic method based on Fermat's Little Theorem to
;; evaluate the primality of the value specified by `n`.  The input for `times`
;; specifies how many times that a test based on Fermat's Little Theorem is
;; performed.

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))




;; modified procedure for Exercise 1.24 ----------------------------------------

;; prints `n` and checks to see if `n` is prime.  If `n` is prime, the procedure
;; prints three asterisks followed by the amount of time used in performing the
;; test.
(define (timed-prime-test n times)
  (newline)
  (display n)
  (start-prime-test n times (runtime)))


;; if `n` is prime (with high likelihood) then calls `report-time` with the
;; elapsed runtime as the argument, otherwise do nothing
(define (start-prime-test n times start-time)
  (if (fast-prime? n times)
      (report-prime (- (runtime) start-time))))


;; prints the value of `elapsed-time`
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))




;; check new runtimes ----------------------------------------------------------

;; these prime numbers were found during Exercises 1.22 and 1.23.  However, the
;; numbers are too small to measure the runtime using the `runtime` procedure.
(timed-prime-test     1000000007  10)  ; 1e9
(timed-prime-test     1000000009  10)  ; 1e9
(timed-prime-test     1000000021  10)  ; 1e9
(timed-prime-test    10000000019  10)  ; 1e10
(timed-prime-test    10000000033  10)  ; 1e10
(timed-prime-test    10000000061  10)  ; 1e10
(timed-prime-test   100000000003  10)  ; 1e11
(timed-prime-test   100000000019  10)  ; 1e11
(timed-prime-test   100000000057  10)  ; 1e11
(timed-prime-test  1000000000039  10)  ; 1e12
(timed-prime-test  1000000000061  10)  ; 1e12
(timed-prime-test  1000000000063  10)  ; 1e12

;; these primes were found using the `NextPrime` function on Wolfram Alpha
;; 1e250 + 1227
(timed-prime-test 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001227 10)
;; 1e500 + 961
(timed-prime-test 100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000961 10)
;; 1e1000 + 453
(timed-prime-test 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000453 10)




;; procedure timings -----------------------------------------------------------

;;   size   time  ratio
;;  -----   ----  -----
;;   1e250  0.09
;;   1e500  0.23   2.56
;;  1e1000  1.25   5.46
;;
;; this limited amount of data does seem to bear out that the procedure is
;; running in logarithmic time
