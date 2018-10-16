;; *Exercise 1.30:* The `sum` procedure above generates a linear recursion.  The
;; procedure can be rewritten so that the sum is performed iteratively.  Show
;; how to do this by filling in the missing expressions in the following
;; definition:
;;
;;      (define (sum term a next b)
;;        (define (iter a result)
;;          (if <??>
;;              <??>
;;              (iter <??> <??>)))
;;        (iter <??> <??>))
;;
;; -----------------------------------------------------------------------------


;; create `sum` function -------------------------------------------------------

;; iterative version of `sum`.  takes the functions `term` and `next` and values
;; `a` and `b` as inputs.  Return value is the sum of the values of of `term`
;; evaluated at successive values of `next` applied to `a`, with the sum ending
;; when the value of `(next (next ... (next a)...))` is larger than `b`.
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))




;; test cases ------------------------------------------------------------------

(define (identity x) x)
(define (square x) (* x x))

(sum identity 1 1+ 1)
(sum identity 1 1+ 2)
(sum identity 1 1+ 3)
(sum identity 1 1+ 4)
(sum identity 1 1+ 5)

(sum square 1 1+ 1)
(sum square 1 1+ 2)
(sum square 1 1+ 3)
(sum square 1 1+ 4)
(sum square 1 1+ 5)
