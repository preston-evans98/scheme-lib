;; Adapt BiwaScheme to MIT Scheme
(define print display)
(define mod remainder)

;; Useful primitives
(define (square x) (* x x))
(define (even? n) (= 0 (mod n 2)))
(define (odd? n) (= 1 (mod n 2)))

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
    (cond ((<= exp 1) (mod (* base acc) modulus))
      ((even? exp)
        (iter (mod (square base) modulus) (/ exp 2) acc))
      (else 
        (iter base (- exp 1) (mod (* base acc) modulus)))))
    (iter x n 1))

;; Log N fibonacci sequence calculator
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


;; PRIMEs

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

;; Simple exhaustive search for divisor from div to sqrt(n)
(define (find-divisor n div)
  (cond ((= 0 (mod n div))
    div)
    ((> (square div) n) n)
    (else (find-divisor n (+ div 1)))))
(define (smallest-divisor n) (find-divisor n 2))
