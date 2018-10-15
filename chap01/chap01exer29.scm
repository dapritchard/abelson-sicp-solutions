;; *Exercise 1.29:* Simpson's Rule is a more accurate method of numerical
;; integration than the method illustrated above.  Using Simpson's Rule, the
;; integral of a function f between a and b is approximated as
;;
;;      h
;;      - (y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + ... + 2y_(n-2) + 4y_(n-1) + y_n)
;;      3
;;
;; where h = (b - a)/n, for some even integer n, and y_k = f(a + kh).
;; (Increasing n increases the accuracy of the approximation.)
;; Define a procedure that takes as arguments f, a, b, and n and
;; returns the value of the integral, computed using Simpson's Rule.
;; Use your procedure to integrate `cube' between 0 and 1 (with n =
;; 100 and n = 1000), and compare the results to those of the
;; `integral' procedure shown above.
;;
;; -----------------------------------------------------------------------------


;; functions provided in section 1.3.1 -----------------------------------------

;; takes the functions `term` and `next` and values `a` and `b` as inputs.
;; Return value is the sum of the values of of `term` evaluated at successive
;; values of `next` applied to `a`, with the sum ending when the value of `(next
;; (next ... (next a)...))` is larger than `b`.
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))


;; approximates the definite integral of function `f` with lower bound `a` and
;; upper bound `b`, with rectangle widths of size `dx` by using the midpoint
;; Riemann sum.
;;
;; Recall that the midpoint Riemann sum is the value given by the sums of the
;; areas of consecutive rectangles each with width `dx` and height given by the
;; value of the curve at the point `(x_i + x_{i + 1} / 2` and where `x_{i + 1} =
;; x_i + dx`.
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))




;; Simpson integral routine ----------------------------------------------------

;; approximates the definite integral of function `f` with lower bound `a` and
;; upper bound `b` and with step size `(b - a) / 2` by using Simpson's rule
(define (integral-simpson f a b n)

  ;; bind `h` to input stepsize defined as `(b - a) / 2`
  (define h (/ (- b a) n))

  ;; create function to increment an input `z` by `2 * h`
  (define (increment-2-h z)
    (+ z (* 2 h)))

  ;; evaluate the calculation using Simpson's rule
  (* (/ h 3.0)
     (+ (f a)
	(* 4 (sum f (+ a h) increment-2-h (- b h)))
	(* 2 (sum f (+ a h h) increment-2-h (- b h)))
	(f b))))





;; test routines ---------------------------------------------------------------

(define (cube x)
  (* x x x))


(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(integral-simpson cube 0 1 100)
(integral-simpson cube 0 1 1000)
