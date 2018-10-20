;; *Exercise 1.32:*
;;
;;   a. Show that `sum` and `product` (*Note Exercise 1-31) are both special
;;   cases of a still more general notion called `accumulate` that combines a
;;   collection of terms, using some general accumulation function:
;;
;;           (accumulate combiner null-value term a next b)
;;
;;   `Accumulate` takes as arguments the same term and range specifications as
;;   `sum` and `product`, together with a `combiner` procedure (of two
;;   arguments) that specifies how the current term is to be combined with the
;;   accumulation of the preceding terms and a `null-value` that specifies what
;;   base value to use when the terms run out.  Write `accumulate` and show how
;;   `sum` and `product` can both be defined as simple calls to `accumulate`.
;;
;;   b. If your `accumulate` procedure generates a recursive process, write one
;;   that generates an iterative process.  If it generates an iterative process,
;;   write one that generates a recursive process.
;;
;; -----------------------------------------------------------------------------


;; part a (linear version) -----------------------------------------------------

;; linear version.  Takes the functions `combiner`, `term`, and `next`, and
;; values `null-value`, `a`, and `b` as inputs.  Return value is the result of
;; applying `combiner` to the values of `term` evaluated at successive values of
;; `next` applied to `a`, with the sum ending when the value of `(next (next
;; ... (next a)...))` is larger than `b`.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))




;; part b (iterative version) --------------------------------------------------

;; iterative version.  Takes the functions `combiner`, `term`, and `next`, and
;; values `null-value`, `a`, and `b` as inputs.  Return value is the result of
;; applying `combiner` to the values of `term` evaluated at successive values of
;; `next` applied to `a`, with the sum ending when the value of `(next (next
;; ... (next a)...))` is larger than `b`.
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner result (term a)))))
  (iter a null-value))




;; test cases ------------------------------------------------------------------

(define (identity x) x)
(define (square x) (* x x))


;; sum_{i = 1}^{10} i = 55
(accumulate + 0 identity 1 1+ 10)
(accumulate-iter + 0 identity 1 1+ 10)

;; sum_{i = 1}^{4} i^2 = 30
(accumulate + 0 square 1 1+ 4)
(accumulate-iter + 0 square 1 1+ 4)

;; prod_{i = 1}^{5} i = 120
(accumulate * 1 identity 1 1+ 5)
(accumulate-iter * 1 identity 1 1+ 5)
