;; a measure of closeness to be used as a parameter for defining when
;; convergence has occurred
(define tolerance 0.00001)


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


;; define the function given in the prompt
(define (f x)
  (+ 1 (/ 1 x)))


;; each of these starting points converges to phi, so for this problem the
;; starting value need not need to be close at all to the fixed point to achieve
;; convergence
(fixed-point f 2.0)
(fixed-point f -1000.0)
(fixed-point f 1000.0)
