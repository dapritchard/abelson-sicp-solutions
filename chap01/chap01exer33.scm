;; *Exercise 1.33:* You can obtain an even more general version of `accumulate`
;; (*Note Exercise 1-32) by introducing the notion of a "filter" on the terms to
;; be combined.  That is, combine only those terms derived from values in the
;; range that satisfy a specified condition.  The resulting
;; `filtered-accumulate` abstraction takes the same arguments as accumulate,
;; together with an additional predicate of one argument that specifies the
;; filter.  Write `filtered-accumulate` as a procedure.  Show how to express the
;; following using `filtered-accumulate`:
;;
;;   a. the sum of the squares of the prime numbers in the interval a to b
;;   (assuming that you have a `prime?` predicate already written)
;;
;;   b. the product of all the positive integers less than n that are relatively
;;   prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1).
;;
;; -----------------------------------------------------------------------------


;; define `prime?` (see Exercise 1.22) -----------------------------------------

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




;; create filtered accumulate fcn ----------------------------------------------

;;
(define (filtered-accumulate combiner keep? null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (keep? a)
		    (term a)
		    null-value)
		(filtered-accumulate combiner keep? null-value term (next a) next b))))


(define (filtered-accumulate-iter combiner keep? null-value term a next b)
  (define (iter a result)
   (if (> a b)
       result
       (iter (next a) (combiner result (if (keep? a)
					   (term a)
					   null-value)))))
  (iter a null-value))



;; part a ----------------------------------------------------------------------

;; calculate the square of `x`
(define (square x) (* x x))


;; calculate \sum_{i = a}^{b} I(i is prime) * a^2
(define (sum-prime-squares-iter a b)
  (filtered-accumulate-iter + prime? 0 square a 1+ b))




;; part b ----------------------------------------------------------------------

;; obtain the greatest common denominator of integers `a` and `b`.  See Section
;; 1.2.5.
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; check if `a` and `b` are relatively prime, (i.e if GCD(a, b) = 1).
(define (rel-prime? a b)
  (= 1 (gcd a b)))


;; calculate the product of all the positive integers less than n that are
;; relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) =
;; 1).
(define (prod-rel-prime n)
  (define (rel-prime-to-n? k)
    (rel-prime? k n))
  (filtered-accumulate-iter * rel-prime-to-n? 1 identity 1 1+ n))




;; test cases ----------------------------------------------------------

(define (less-than-5? x) (< x 5))
(define (identity x) x)


;; sum_{i = 1}^{10} I(i < 5) * i = 10
(filtered-accumulate + less-than-5? 0 identity 1 1+ 10)
(filtered-accumulate-iter + less-than-5? 0 identity 1 1+ 10)

;; sum_{i = 1}^{10} I(i mod 2 = 0) * i = 30
(filtered-accumulate + even? 0 identity 1 1+ 10)
(filtered-accumulate-iter + even? 0 identity 1 1+ 10)

;; sum_{i = 1}^{10} I(i is prime) * i^2 = 1^2 + 2^2 + 3^2 + 5^2 + 7^2 = 88
(sum-prime-squares 1 10)
(sum-prime-squares-iter 1 10)
