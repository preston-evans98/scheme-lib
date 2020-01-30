;; (define-interface utils-interface 
;;    (export accumulate 
;;            filtered-accumulate)) 
;; (define-structure utils utils-interface 
;;    (open scheme) 
;;    (files utils ))

(define (accumulate combiner null-value term a next b)
    (define (iter current total)
        (if (> current b) 
            total
            (iter (next current) (combiner (term current) total))))
    (iter a null-value))

(define (filtered-accumulate filter? combiner null-value term a next b)
   (define (iter current total)
        (cond ((> current b)
                total)
            ((filter? current)
                (iter (next current) (combiner (term current) total)))
            (else 
                (iter (next current) total))))
    (iter a null-value))
