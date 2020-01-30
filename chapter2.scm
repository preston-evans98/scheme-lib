(load "math.scm")


(define (real-part-rectangular z)
    (car z))
(define (imag-part-rectangular z)
    (cdr z))
(define (magnitude-rectangular z)
    (sqrt (+ 
        (square (real-part-rectangular z)) 
        (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
    (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
    (attach-tag 'rectangular (cons x y)))
    
(define (make-from-mag-ang-rectangular r a)
    (attach-tag 'rectangular
        (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
    (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
    (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z)
    (car z))
(define (angle-polar z)
    (cdr z))
(define (make-from-real-imag-polar x y)
    (attach-tag 'polar (cons 
        (sqrt (+ (square x) (square y)))
        (atan y x))))
(define (make-from-mag-ang-polar r z)
    (attach-tag 'polar (cons r z)))
        

(define (attach-tag tag contents)
    (if (number? contents)
        contents
        (cons tag contents)))
(define (type-tag datum)
    (cond ((number? datum) 'scheme-number)
        ((pair? datum)
            (car datum))
        (else (error "Bad tagged datum TYPE-TAG" datum))))
(define (contents datum)
    (cond ((number? datum) datum)
        ((pair? datum)
            (cdr datum))
        (else (error "Bad tagged datum CONTENTS" datum))))
(define (rectangular? datum)
    (eq? (type-tag datum) 'rectangular))
(define (polar? datum)
    (eq? (type-tag datum)  'polar))

(define (real-part z)
    (cond ((polar? z)
        (real-part-polar (contents z)))
        ((rectangular? z)
            (real-part-rectangular (contents z)))
        (else 
            (error "Unknown type: REAL-PART" z))))
(define (imag-part z)
    (cond ((polar? z)
        (imag-part-polar (contents z)))
        ((rectangular? z)
            (imag-part-rectangular (contents z)))
        (else 
            (error "Unknown type: IMAG-PART" z))))
(define (magnitude z)
    (cond ((polar? z)
        (magnitude-polar (contents z)))
        ((rectangular? z)
            (magnitude-rectangular (contents z)))
        (else 
            (error "Unknown type: MAGNITUDE" z))))
(define (angle z)
    (cond ((polar? z)
        (angle-polar (contents z)))
        ((rectangular? z)
            (angle-rectangular (contents z)))
        (else 
            (error "Unknown type: ANGLE" z))))

(define make-from-real-imag make-from-real-imag-rectangular)
(define make-from-mag-ang make-from-mag-ang-polar)

; (define a (make-from-real-imag 1 2))
; (print a)
; (print (magnitude-rectangular a))
; (display (real-part-rectangular a))

(define (add-complex z1 z2)
    (make-from-real-imag 
        (+ (real-part z1) (real-part z2))
        (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
    (make-from-real-imag 
        (- (real-part z1) (real-part z2))
        (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2) 
    (make-from-mag-ang 
        (* (magnitude z1) (magnitude z2))
        (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
    (make-from-mag-ang 
        (/ (magnitude z1) (magnitude z2))
        (- (angle z1) (angle z2))))


;  ----------------------- SYMBOLIC DIFFERENTIATION ---------------
; (define (variable? x) (symbol? x))
; (define (same-variable? v1 v2)
;     (and (variable? v1) (variable? v2) (eq? v1 v2)))
; (define (make-sum a1 a2) (list '+ a1 a2))
; (define (make-product m1 m2) (list '* m1 m2))
; (define (sum? x) (and (pair? x) (eq? (car x) '+)))
; (define (addend s) (cadr s))
; (define (augend s) (caddr s))
; (define (sum? x) (and (pair? x) (eq? (car x) '*)))
; (define (multiplier p) (cadr p))
; (define (multiplicand p) (caddr p))

; (define (deriv exp var)
;     (cond ((number? exp) 0)
;         ((variable? exp) 
;             (if (same-variable? var exp) 1 0))
;         ((sum? exp)
;             (make-sum (deriv (addend exp)) (deriv (augend exp))))
;         ((product? exp)
;             (make-sum (make-product (multiplier exp) (deriv (multiplicand exp)))
;                 (make-product (deriv (multiplier exp)) (multiplicand exp))))
            
;         (else (error "unkown expression type DERIV:" exp))))


(define (install-derive-sum-package) 
    (define (make-sum a1 a2) (list '+ a1 a2))
    (define (addend s) (car s))
    (define (augend s) (cadr s))
    (define (derive-sum exp var)
        (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
    (put 'deriv '(+) derive-sum))
(define (install-derive-product-package)
    (define (make-product a1 a2) (list '* a1 a2))
    (define (multiplier p) (car p))
    (define (multiplicand p) (cadr p))
    (define (derive-product exp var)
           (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var) (multiplicand exp))))
    (put 'deriv '(*) derive-product))
(define (install-derive-exponent-package)
    (define (make-exponent a1 a2) (list '^ a1 a2))
    (define (base p) (car p))
    (define (exponent p) (cadr p))
    (define (derive-exponent exp var)
        (make-product 
            (make-product (exponent exp) (make-exponent (base exp) (make-sum (exponent exp) (- 1))))
            (deriv (base exp) var)))
    (put 'deriv '(^) derive-exponent))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))



(define (deriv exp var)
    (cond ((number? exp) 0)
        ((variable? exp) 
            (if (same-variable? var exp) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


(define (make-from-mag-ang r a)
    (define (dispatch op)
        (cond ((eq? op 'real-part))
                (* r (cos a))
            ((eq? op 'imag-part) (* r (sin a)))
            ((eq? op 'magnitude) r)
            ((eq? op 'angle) a)))
    dispatch)


; (define (install-scheme-number-package)
;     (define (tag x) (attach-tag 'scheme-number x)) 
;     (put 'add '(scheme-number scheme-number)
;         (lambda (x y) (tag (+ x y))))
;     (put 'sub '(scheme-number scheme-number)
;         (lambda (x y) (tag (- x y))))
;     (put 'mul '(scheme-number scheme-number)
;         (lambda (x y) (tag (* x y))))
;     (put 'div '(scheme-number scheme-number)
;         (lambda (x y) (tag (/ x y))))
;     (put 'make 'scheme-number (lambda (x) (tag x))) 'done)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y)) 
(define (mul x y) (apply-generic 'mul x y)) 
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags))) (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
            (list op type-tags))))))


(define (install-scheme-number-package)
    (define (tag x) x) 
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number (lambda (x) (tag x))) 
    (put 'equ? 'scheme-number =)
    'done
)

(define (install-rational-package) 
    ;; internal procedures
    (define (numer x) (car x)) 
    (define (denom x) (cdr x)) 
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                (* (numer y) (denom x)))
                (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
            (* (numer y) (denom x)))
            (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y)))) 
            
    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                (* (denom x) (numer y))))
    ;; interface to rest of the system
    (define (tag x) (attach-tag 'rational x)) 
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y)))) 
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y)))) 
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y)))) 
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational
        (lambda (n d) (tag (make-rat n d))))
    (put 'equ? 'rational (lambda (a b) (and (= (numer a) (number b)) (= (denom a) (denom b)))))
    'done
)
(define (make-rational n d)
    ((get 'make 'rational) n d))

(define (install-complex-package)
    ;; imported procedures from rectangular and polar packages 
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y)) 
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a)) 
    
    ;; internal procedures
    (define (add-complex z1 z2)
        (make-from-real-imag 
            (+ (real-part z1) (real-part z2))
            (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
            (- (imag-part z1) (imag-part z2)))) 
    (define (mul-complex z1 z2)
        (make-from-mag-ang 
            (* (magnitude z1) (magnitude z2))
            (+ (angle z1) (angle z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang 
            (/ (magnitude z1) (magnitude z2))
            (- (angle z1) (angle z2)))) 
    ;; interface to rest of the system
    
    (define (tag z) (attach-tag 'complex z)) 258
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)
    (put 'equ? '(complex) (lambda (a b) (and (= (magnitude a) (magnitude b)) (= (angle a) (angle b)))))
    'done
)

(define (install-polynomial-package)
    (define (make-poly variable term-list)
        (cons variable term-list))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))


    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: ADD-POLY" (list p1 p2))))

    (define (add-terms l1 l2)
        (cond ((empty-termlist? l1) l2)
            ((empty-termlist? l2) l1)
            (else 
                (let ((t1 (first-term l1)) (t2 (first-term l2)))
                    (cond ((> (order t1) (order t2))
                            (adjoin-term t1 (add-terms (rest-terms l1) l2)))
                        ((> (order t1) (order t2)) 
                            (adjoin-term t2 (add-terms l1 (rest-terms l2))))
                        (else (adjoin-term 
                            (make-term (order t1) 
                                (add (coef t1) (coef t2)))
                            (add-terms (rest-terms l1) (rest-terms l2)))))))))
            
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (mul-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: MUL-POLY" (list p1 p2))))
    (define (mul-terms L1 L2) (if (empty-termlist? L1) (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L) (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
                (adjoin-term
                    (make-term (+ (order t1) (order t2))
                            (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms t1 (rest-terms L))))))
    (define (adjoin-term t set)
        (if (or (null? set) (> (order t) (order (first-term set))))
            (cons term set)
            (adjoin-set t (cdr set))))


    (define (tag x) (attach-tag 'polynomial x))
    (put 'add '(polynomial polynomial) (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial) (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
    'done
)
