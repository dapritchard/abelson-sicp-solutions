;; *Exercise 1.6:* Alyssa P. Hacker doesn't see why `if' needs to be
;; provided as a special form.  "Why can't I just define it as an
;; ordinary procedure in terms of `cond'?" she asks.  Alyssa's friend
;; Eva Lu Ator claims this can indeed be done, and she defines a new
;; version of `if':
;;
;;      (define (new-if predicate then-clause else-clause)
;;        (cond (predicate then-clause)
;;              (else else-clause)))
;;
;; Eva demonstrates the program for Alyssa:
;;
;;      (new-if (= 2 3) 0 5)
;;      5
;;
;;      (new-if (= 1 1) 0 5)
;;      0
;;
;; Delighted, Alyssa uses `new-if' to rewrite the square-root program:
;;
;;      (define (sqrt-iter guess x)
;;        (new-if (good-enough? guess x)
;;                guess
;;                (sqrt-iter (improve guess x)
;;                           x)))
;;
;; What happens when Alyssa attempts to use this to compute square
;; roots?  Explain.
;;
;; ----------------------------------------------------------------------


;; version 1 -------------------------------------------------------------------

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(sqrt-iter 1.0 2)


;; version 2 -------------------------------------------------------------------

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter-v2 guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

;; infinite depth recursion!
(sqrt-iter 1.0 2)


;; discussion ------------------------------------------------------------------

;; When `sqrt-iter` is called, the interpreter evaluates all of the arguments to
;; `new-if` since it performs applicative order evealuation.  So even though
;; `cond` is a special form, the arguments are evaluated _before_ they even make
;; it to the special form.
;;
;; As a result, every call to `sqrt-iter-v2` results in a recursive call to
;; `sqrt-iter`, and thus the recursion never ends.
