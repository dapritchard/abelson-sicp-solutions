;; *Exercise 1.36:* Modify `fixed-point` so that it prints the sequence of
;; approximations it generates, using the `newline` and `display` primitives
;; shown in Exercise 1.22.  Then find a solution to x^x = 1000 by finding a
;; fixed point of `x |-> log(1000)/log(x)`.  (Use Scheme's primitive `log`
;; procedure, which computes natural logarithms.)  Compare the number of steps
;; this takes with and without average damping.  (Note that you cannot start
;; `fixed-point` with a guess of 1, as this would cause division by `log(1) =
;; 0`.)
;;
;; -----------------------------------------------------------------------------


;; create the `fixed-point` procedure ------------------------------------------

;; a measure of closeness to be used as a parameter for defining when
;; convergence has occurred
(define tolerance 0.00001)


;; search for a fixed point for the function `f`, starting with the value of
;; `first-guess`.  Note that this function should probably have a stopping
;; criterion for function / value pairs for which convergence does not occur.
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess ctr)
    (display ctr)
    (display "  ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next (1+ ctr)))))
  (try first-guess 1))




;; without damping fixed-point solution ----------------------------------------

;; We observe that
;;
;;     x^x = 1000
;;         <==>  x log(x) = log(1000)
;;         <==> x = log(1000) / log(x),
;;
;; and that the derivative of the above expression is given by
;;
;;     d/dx log(1000) / log(x) = -log(1000) / (x * [ log(x) ]^2).
;;
;; Experimentation finds that `log(1000) / log(4) > 4`, and since the function
;; is monotone decreasing we can infer that a fixed point solution exists and is
;; greater than 4.  Furthermore, the absolute value of `-log(1000) / (x * [
;; log(x) ]^2)` is also monotone decreasing, and when evaluated at 4 the result
;; is absolute value is less than 1, so we can conclude that repeatedly applying
;; the transformation will converge to the solution.

(define (f-wo-damping x)
  (/ (log 1000) (log x)))




;; with damping fixed-point solution -------------------------------------------

;; We observe that
;;
;;     x = log(1000) / log(x)
;;         <==>  2x = x + (log(1000) / log(x))
;;         <==>  x = (x + (log(1000) / log(x))) / 2,
;;
;; and that the derivative of the above expression is given by
;;
;;     d/dx (x + (log(1000) / log(x))) / 2
;;         = (1 - (log(1000) / (x * [ log(x) ]^2))) / 2.
;;
;; The value of the derivative is monotone increasing and is bounded from above
;; by 0.5.  Furthermore, when the derivative is evaluated at 4 the result is
;; greater than -1, so we can conclude that repeatedly applying the
;; transformation will converge to the solution.

(define (f-with-damping x)
  (/ (+ x
	(f-wo-damping x)
     2))




;; test cases ------------------------------------------------------------------


(fixed-point f-wo-damping 5.0)

(fixed-point f-with-damping 5.0)
