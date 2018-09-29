;; *Exercise 1.27:* Demonstrate that the Carmichael numbers listed in *Note

;; Footnote 1.47 really do fool the Fermat test.  That is, write a procedure
;; that takes an integer `n` and tests whether `a^n` is congruent to a modulo
;; `n` for every `a < n`, and try your procedure on the given Carmichael
;; numbers.
;;
;; *Footnote 1.47:* Numbers that fool the Fermat test are called "Carmichael
;; numbers", and little is known about them other than that they are extremely
;; rare.  There are 255 Carmichael numbers below 100,000,000.  The smallest few
;; are 561, 1105, 1729,  2465, 2821, and 6601.  In testing primality of very
;; large numbers chosen at random, the chance of stumbling upon a value that
;; fools the Fermat test is less than the chance that cosmic radiation will
;; cause the computer to make an error in carrying out a "correct" algorithm.
;; Considering an algorithm to be inadequate for the first reason but not for
;; the second illustrates the difference between mathematics and engineering.
;;
;; -----------------------------------------------------------------------------


;; fcn to perform the Fermat test for every a < n ------------------------------

;; performs the Fermat test for every positive integer less than `n` until
;; either a value is found that fails a test for primacy, or all such numbers
;; have been tested.  In the former case, the value that fails a test is
;; returned, and in the latter case the value `n` is returned.
(define (exhaustive-fermat n)
  (define (exhaustive-fermat-iter a n)
    (if (or (= a n) (not (fermat-test a n)))
	a
	(exhaustive-fermat-iter (1+ a) n)))
  (exhaustive-fermat-iter 2 n))


;; compute `base^exp mod m`
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))


;; returns `#t` if `a^n mod n = a`, or `#f` otherwise
(define (fermat-test a n)
  (= (expmod a n n) a))




;; Fermat test for smallest few Carmichael nums --------------------------------

;; the factors of the  Carmichael numbers are obtained using external software

(exhaustive-fermat  561)  ;; factors 3, 11, 17, 33, 51, 187
(exhaustive-fermat 1105)  ;; factors 5, 13, 17, 65, 85, 221
(exhaustive-fermat 1729)  ;; factors 7, 13, 19, 91, 133, 247
(exhaustive-fermat 2465)  ;; factors 5, 17, 29, 85, 145, 493
(exhaustive-fermat 2821)  ;; factors 7, 13, 31, 91, 217, 403
(exhaustive-fermat 6601)  ;; factors 7, 23, 41, 161, 287, 943
