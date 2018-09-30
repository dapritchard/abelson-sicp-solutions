;; *Exercise 1.28:* One variant of the Fermat test that cannot be fooled is
;; called the "Miller-Rabin test" (Miller 1976; Rabin 1980).  This starts from
;; an alternate form of Fermat's Little Theorem, which states that if `n` is a
;; prime number and `a` is any positive integer less than `n`, then `a` raised
;; to the `(n - 1)`st power is congruent to 1 modulo `n`.  To test the primality
;; of a number `n` by the Miller-Rabin test, we pick a random number `a < n` and
;; raise `a` to the `(n - 1)`st power modulo `n` using the `expmod` procedure.
;; However, whenever we perform the squaring step in `expmod`, we check to see
;; if we have discovered a "nontrivial square root of 1 modulo `n`," that is, a
;; number not equal to 1 or `n - 1` whose square is equal to 1 modulo `n`.  It
;; is possible to prove that if such a nontrivial square root of 1 exists, then
;; `n` is not prime.  It is also possible to prove that if `n` is an odd number
;; that is not prime, then, for at least half the numbers `a < n`, computing
;; `a^(n - 1)` in this way will reveal a nontrivial square root of 1 modulo `n`.
;; (This is why the Miller-Rabin test cannot be fooled.)  Modify the `expmod`
;; procedure to signal if it discovers a nontrivial square root of 1, and use
;; this to implement the Miller-Rabin test with a procedure analogous to
;; `fermat-test`.  Check your procedure by testing various known primes and
;; non-primes.  Hint: One convenient way to make `expmod` signal is to have it
;; return 0.
;;
;; -----------------------------------------------------------------------------


;; initial note ----------------------------------------------------------------

;; Nota bene: there is seemingly a typographical error in this prompt.  One of
;; the lines says to check whether a number's square is "equal to 1 modulo `n`,"
;; however it presumably means to check whether a number's square is "congruent
;; to 1 modulo `n`."




;; Miller-Rabin test fcns ------------------------------------------------------

;; check if `x mod m = 1`
(define (congruent-1? x m)
  (= 1 (remainder x m)))


;; check if either `x = 1` or `x = m - 1`
(define (trivial-congruent-1? x m)
  (or (= 1 x) (= 1 (- m x))))


;; conditionally returns `x^2`, unless `x^2` is a nontrivial solution to `x^2
;; mod m = 1` in which case a sentinal value of 0 is returned.  "Nontrivial" is
;; defined to be any solution with the exception of the trivial solutions `x =
;; 1` and `x = m - 1` (note that `m - 1` is a trivial solution since `(m - 1)^2
;; mod m = (m^2 - 2m + 1) mod m = (m * (m - 2) + 1) mod m = 1`).
(define (square-check-modulo x m)
  (if (and (not (trivial-congruent-1? x m))
	   (congruent-1? (square x) m))
      0
      (square x)))


;; conditionally computes `base^exp mod m`, unless a nontrivial square root of 1
;; modulo `n` is found by any recursive call to `square-check-modulo` along the
;; way, in which case a sentinal value of 0 is returned.

;;
;; a nontrivial square root of 1 modulo `n` is defined to be any value, say `x`,
;; such that `x^2 mod m = 1` with the exception of `x = 1` or `x = m - 1`.
;;
;; note that if any call to `square-check-modulo` returns 0, then all recursive
;; calls higher up the the call tree will also return 0 since the square of 0
;; modulo `m` has the value of 0.
(define (miller-rabin-expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square-check-modulo (miller-rabin-expmod base (/ exp 2) m) m)
		    m))
	(else (remainder (* base (miller-rabin-expmod base (- exp 1) m))
			 m))))


;; perform the Miller-Rabin test for primality for `n` using the value of `a` for
;; the congruency check
(define (miller-rabin-test? a n)
  (not (= 0 (miller-rabin-expmod a (- n 1) n))))


;; conditionally performs the Miller-Rabin test for primality for `n`, using the
;; values of `a + 2, a + 4, a + 6, ...` until either we have discovered that `n`
;; is not a prime in which case we return `#f`, or else `a >= n` in which case
;; we return `#t`
(define (miller-rabin-driver-prime? a n)
  (cond ((>= a n) #t)
	((not (miller-rabin-test? a n)) #f)
	(else (miller-rabin-driver-prime? (+ 2 a) n))))




;; conditionally perform the Miller-Rabin test for the value of 2 and every odd
;; number less than `n`.  If at any point it is discovered that `n` is not a
;; prime then the function returns `#f`, otherwise `#t` is returned.  Note that
;; since this is half of the values smaller than `n`, this guarantees that `n`
;; is indeed a prime number.
(define (miller-rabin-prime? n)
  (if (miller-rabin-test? 2 n)
      (miller-rabin-driver-prime? 3 n)
      #f))




;; perform the Miller-Rabin test on some inputs --------------------------------

;; the first 13 natural numbers
(miller-rabin-prime? 1)
(miller-rabin-prime? 2)  ;; <-- false negative
(miller-rabin-prime? 3)
(miller-rabin-prime? 4)
(miller-rabin-prime? 5)
(miller-rabin-prime? 6)  ;; <-- false positive
(miller-rabin-prime? 7)
(miller-rabin-prime? 8)
(miller-rabin-prime? 9)
(miller-rabin-prime? 10)  ;; <-- false positive
(miller-rabin-prime? 11)
(miller-rabin-prime? 12)
(miller-rabin-prime? 13)

;; the first 6 Carmichael numbers (i.e these are non-primes)
(miller-rabin-prime?  561)
(miller-rabin-prime? 1105)
(miller-rabin-prime? 1729)
(miller-rabin-prime? 2465)
(miller-rabin-prime? 2821)
(miller-rabin-prime? 6601)

;; these are all prime numbers
(miller-rabin-prime? 103991)
(miller-rabin-prime? 103993)
(miller-rabin-prime? 103997)
(miller-rabin-prime? 104003)
(miller-rabin-prime? 104009)
(miller-rabin-prime? 104021)
(miller-rabin-prime? 104033)
(miller-rabin-prime? 104047)
(miller-rabin-prime? 104053)
(miller-rabin-prime? 104059)

;; these are all non-prime numbers
(miller-rabin-prime? 103992)
(miller-rabin-prime? 103994)
(miller-rabin-prime? 103998)
(miller-rabin-prime? 104004)
(miller-rabin-prime? 104010)
(miller-rabin-prime? 104022)
(miller-rabin-prime? 104034)
(miller-rabin-prime? 104048)
(miller-rabin-prime? 104054)  ;; <-- false positive
(miller-rabin-prime? 104060)
