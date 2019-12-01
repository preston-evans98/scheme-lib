;; Load utilities
(load "utils.scm")

;; Adapt BiwaScheme to MIT Scheme
(define print display)
(define mod remainder)

;; ---------------- USEFUL PRIMITIVES -----------------------
(define (square x) (* x x))
(define (average . items)
  (define (iter total lst item-count)
    (if (null? lst)
      (/ total item-count)
      (iter (+ total (car lst)) (cdr lst) (+ item-count 1))))
  (iter 0 items 0))
(define (cube x) (* x x x))
(define (even? n) (= 0 (mod n 2)))
(define (odd? n) (= 1 (mod n 2)))
(define (positive? n) (< 0 n))
(define (negative? n) (> 0 n))
(define (inc x) (+ x 1))
(define (identity x) x)
(define (average-damp f)
  (lambda (x) (average x (f x))))
(define (abs x)
  (if (>= x 0)
    x
    (- x)))


;; ---------------- USEFUL PROCEDURES -----------------------
;; Sum f(x) from a to b where the x[n+1] is next(x[n])
;; Accumulate is defined in utils.scm
(define (sum f a next b)
    (accumulate + 0 f a next b))

;; Sum integers from a to b
(define (sum-integers a b)
  (sum identity a inc b))

;; A slow formula for Pi / 8 due to Lebniz
;; (* 8 (pi-sum 1 10000)) = 3.14139
(define (pi-sum a b)
  (if (> a b)
    0 
    (+ (/ 1.0 (* a (+ 2 a)))
      (pi-sum (+ a 4) b))))



;; ---------------- EFFICIENT POWERS and FIBONACCI -----------------------
;; Log n tail-recursive x**n 
(define (pow x n)
  (define (iter base exp acc)
    (cond ((<= exp 1) (* base acc))
      ((even? exp)
        (iter (square base) (/ exp 2) acc))
      (else 
        (iter base (- exp 1) (* base acc)))))
    (iter x n 1))

;; Log n tail recursive x**n mod m. Memory use is linear with largest of {x, n, modulus}
(define (expt-mod x n modulus)
  (define (iter base exp acc)
    (cond ((= exp 0) 
        (mod (* base acc) modulus))
      ((= exp 1) 
        (mod (* base acc) modulus))
      ((even? exp)
        (iter (mod (square base) modulus) (/ exp 2) acc))
      (else 
        (iter base (- exp 1) (mod (* base acc) modulus)))))
    (iter x n 1))

;; Log N tail recursive fibonacci sequence calculator
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
    ((even? count)
      (fib-iter a b 
                (+ (* p p) (* q q)); compute p'
                (+ (* q q) (* q p 2)); compute q'
                (/ count 2)))
    (else (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q)) p q (- count 1)))))


;; ---------------- EFFICIENT ROOTS AND FIXED POINTS-----------------------
; Half interval method for finding roots. Logarithmic time
(define (half-interval-method f a b)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
      (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
              (search f neg-point midpoint))
            ((negative? test-value)
              (search f midpoint pos-point))
            (else midpoint))))))
    
  (let ((a-value (f a)) (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
        (search f a b))
      ((and (positive? a-value) (negative? b-value))
        (search f b a))
      (else (error "values are not of opposite sign" a b)))))
  
(define (fixed-point f first-guess)
  (define tolerance .00001)
  (define (close-enough? x y) (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

;; ---------------- PRIMES -----------------------
;; Fermat's test for primality
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))
;; Efficient test for primality based on Fermat
(define (fast-prime n times)
  (cond ((>= 0 times) #t)
    ((fermat-test n) (fast-prime n (- times 1)))
    (else #f)))

;; A function to demonstrate that carmichael numbers really do 
;; pass fermat's test for primality on every n
;; Try it with the smallest carmichael numbers - 561, 1105, 1729, 2465, 2821, or 6601
(define (carmichael-test n)
  (define (test a)
    (= (expmod a n n) a))
  (define (iter a)
    (cond ((= a n) #t)
    ((test a) (iter (+ a 1)))
    (else #f)))
  (iter 1))

;; Miller-rabin test for primality, which cannot be fooled
(define (miller-rabin n)
  (define (m-r-exptmod base exp modulus)
    (cond ((= 0 exp) 1)
      ((even? exp) 
        (mod 
          (m-r-square (m-r-exptmod base (/ exp 2) modulus)) 
          modulus))
        (else 
          (mod 
            (* base (m-r-exptmod base (- exp 1) modulus))
            modulus))))
  (define (m-r-square x)
    (cond ((or (= x 1) (= x (- n 1)))
        (square x))
      ((= 1 (mod (square x) n))
        0)
      (else (square x))
    ))
  (define (test a)
    (= (m-r-exptmod a (- n 1) n) 1))
  (test (+ 1 (random-integer (- n 1)))))

;; A function to test for primes using miller-rabin. times=256 should be more than sufficient
(define (fast-mr-prime n times)
  (if (>= 0 times)
      #t
    (and (miller-rabin n) (fast-mr-prime n (- times 1)))))

;; A function to verify a purported list of primes, p
(define (check-primes p)
  (cond ((null? p) 
    #t)
    ((fast-mr-prime (car p) 256) (check-primes (cdr p)))
    (else #f)))

;; Simple exhaustive search for divisor from div to sqrt(n)
(define (find-divisor n div)
  (cond ((= 0 (mod n div))
    div)
    ((> (square div) n) n)
    (else (find-divisor n (+ div 1)))))
(define (smallest-divisor n) (find-divisor n 2))


;; ---------------- CALCULUS -----------------------
;; Use Newton's method to find derivative

(define (newtons-method f guess)
  (define (deriv g)
    (define dx 0.00001)
    (lambda (x) (/ ( - (g (+ x dx)) (g x)) dx)))
  (define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g ) x)))))
  (fixed-point (newton-transform f) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))


;; Approximate the area under a function from A to B using N terms
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (iter k acc)
    (cond ((= k n) (/ (* h (+ acc (f (+ a (* k h))))) 3))
    ((= k 0) 
      (iter (+ k 1) (+ acc (f a))))
    ((even? k)
      (iter (+ k 1) (+ acc (* 4 (f (+ a (* k h)))))))
    (else
      (iter (+ k 1) (+ acc (* 2 (f (+ a (* k h)))))))))
  (iter 0 0))
