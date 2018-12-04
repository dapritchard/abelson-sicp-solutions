;; *Exercise 1.40:* Define a procedure `cubic' that can be used together with
;; the `newtons-method` procedure in expressions of the form
;;
;;      (newtons-method (cubic a b c) 1)
;;
;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.
;;
;; -----------------------------------------------------------------------------


;; previously existing fcns from Section 1.3.4 ---------------------------------

;; a measure of closeness to be used as a parameter for defining when
;; convergence has occurred
(define tolerance 0.00001)
;; the size of the change in used to approximate the derivative of a function
(define dx 0.00001)


;; search for a fixed point for the function `f`, starting with the value of
;; `first-guess`.  Note that this function should probably have a stopping
;; criterion for function / value pairs for which onvergence does not occur.
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))


;; uses Newton's method to find a fixed point for function `g`, using an initial
;; value given by `guess`
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


;; takes an input function `g` and returns a function with input `x` that
;; calculates `x - (f(x) / (d/dx f(x)))`
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))


;; takes an input function `g` and returns a function with input `x` that
;; calculates an approximation of the derivative of `g` evaluated at `x`
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))




;; new code to create `cubic` fcn ----------------------------------------------

;; takes values for `a`, `b`, and `c`, and returns a function that takes an
;; input `x` and returns the value given by `x^3 + ax^2 + bx + c`.
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x )
       c)))




;; test cases ------------------------------------------------------------------

;; should have a value of 1.0
(newtons-method (cubic 1 -2 0) 1.0)

;; find the value and verify that it is a root
(newtons-method (cubic 1 -3 0) 1.0)
((cubic 1 -3 0) 1.302775637742353)

;; find the value and verify that it is a root
(newtons-method (cubic 1 -3 2) 1.0)
((cubic 1.0 -3 2) -2.5115471416945176)
