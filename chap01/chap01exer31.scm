;; *Exercise 1.31:*
;;
;;   a. The `sum' procedure is only the simplest of a vast number of similar
;;      abstractions that can be captured as higher-order procedures. Write an
;;      analogous procedure called `product' that returns the product of the
;;      values of a function at points over a given range.  Show how to define
;;      `factorial' in terms of `product'.  Also use `product' to compute
;;      approximations to \pi using the formula
;;
;;           pi   2 * 4 * 4 * 6 * 6 * 8 ...
;;           -- = -------------------------
;;            4   3 * 3 * 5 * 5 * 7 * 7 ...
;;
;;   b. If your `product' procedure generates a recursive process, write one
;;   that generates an iterative process.  If it generates an iterative process,
;;   write one that generates a recursive process.
;;
;; -----------------------------------------------------------------------------


;; part a (linear version) -----------------------------------------------------

;; linear version.  Takes the functions `term` and `next` and values `a` and `b`
;; as inputs.  Return value is the product of the values of `term` evaluated at
;; successive values of `next` applied to `a`, with the sum ending when the
;; value of `(next (next ... (next a)...))` is larger than `b`.
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))




;; part b (iterative version) --------------------------------------------------

;; iterative version.  Takes the functions `term` and `next` and values `a` and
;; `b` as inputs.  Return value is the product of the values of `term` evaluated
;; at successive values of `next` applied to `a`, with the sum ending when the
;; value of `(next (next ... (next a)...))` is larger than `b`.
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))




;; test cases ------------------------------------------------------------------

;; identity function
(define (identity x) x)


;; factorial function using `product`
(define (factorial n)
  (if (<= n 1)
      1
      (product identity 2 1+ n)))


;; factorial function using `product-iter`
(define (factorial-iter n)
  (if (<= n 1)
      1
      (product-iter identity 2 1+ n)))



(factorial 0)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

(factorial-iter 0)
(factorial-iter 1)
(factorial-iter 2)
(factorial-iter 3)
(factorial-iter 4)
(factorial-iter 5)
