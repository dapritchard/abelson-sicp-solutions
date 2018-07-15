;; *Exercise 1.8:* Newton's method for cube roots is based on the
;; fact that if y is an approximation to the cube root of x, then a
;; better approximation is given by the value
;;
;;      x/y^2 + 2y
;;      ----------
;;          3
;;
;; Use this formula to implement a cube-root procedure analogous to
;; the square-root procedure.  (In section *Note 1-3-4:: we will see
;; how to implement Newton's method in general as an abstraction of
;; these square-root and cube-root procedures.)
;;
;; ----------------------------------------------------------------


;; define the cube root routine ------------------------------------------------

(define (cuberoot-iter guess x)
  (if (good-enough? guess x)
      guess
      (cuberoot-iter (improve guess x)
		     x)))

(define (improve guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (/ (- (cube guess) x)
	     guess))
     0.00001))

(define (cube x) (* x x x))


;; some test cases -------------------------------------------------------------

(cuberoot-iter 1.0 (cube 3.5))

;; (10^{-9})^{1/3} = 0.001.  The result of this procedure is about
;; 0.002324 so this approximation is not so good.
(cuberoot-iter 1.0 0.00000001)

;; (10^13)^{1/3} = 21544.3469 approximately.  The result of this procedure is
;; about 21544.3469 so this is good.
(cuberoot-iter 1.0 10000000000000)
