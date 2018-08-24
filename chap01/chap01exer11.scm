;; *Exercise 1.11:* A function f is defined by the rule that f(n) = n
;; if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3.
;; Write a procedure that computes f by means of a recursive process.
;; Write a procedure that computes f by means of an iterative
;; process.
;;
;; ------------------------------------------------------------------


;; recursive definition of `f`
(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
	 (* 2 (f-recur (- n 2)))
	 (* 3 (f-recur (- n 3))))))


;; iterative version.  Calls the workhorse function `f-iter` with `f(2)`,
;; `f(1)`, `f(0)`, and `n - 2` as inputs.
(define (f n)
  (if (< n 3)
      n
      (f-iter 2 1 0 (- n 2))))

;; the inputs `a`, `b`, and `c` are expected to be the values of `f(k)`, `f(k -
;; 1)`, and `f(k - 2)` for some value of `k >= 2`, and the value of `count` is
;; expected to be such that the target calculation is `f(k + count)`.
;;
;; the main idea is that each call to `f-iter` calculates `f(k + 1)` from the
;; inputs and retains the previously calculated values of `f(k)` and `f(k -1)`
;; by passing them as inputs to the next call to `f-iter`.
(define (f-iter a b c count)
  (if (= count 0)
      a
      (f-iter (+ a (* 2 b) (* 3 c))
	      a
	      b
	      (- count 1))))
