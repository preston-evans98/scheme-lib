; This is free and unencumbered software released into the public domain.

; Anyone is free to copy, modify, publish, use, compile, sell, or
; distribute this software, either in source code form or as a compiled
; binary, for any purpose, commercial or non-commercial, and by any
; means.

; In jurisdictions that recognize copyright laws, the author or authors
; of this software dedicate any and all copyright interest in the
; software to the public domain. We make this dedication for the benefit
; of the public at large and to the detriment of our heirs and
; successors. We intend this dedication to be an overt act of
; relinquishment in perpetuity of all present and future rights to this
; software under copyright law.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; OTHER DEALINGS IN THE SOFTWARE.

; For more information, please refer to <https://unlicense.org>

(load "math.scm")

;  ------------------------------- Mutation Examples ------------------------------
                
; ---------- make monitored function --------------
(define (make-monitored f)
    (let ((calls 0))
        (define (mf x)
            (cond ((eq? x 'how-many-calls?) calls)
                ((eq? x 'reset-count) (set! calls 0))
                (else (begin 
                    (set! calls (+ calls 1))
                    (f x)))))
    mf))

; -------------- Bank Account Example ---------------------

(define (make-account balance password)
    (let ((amt balance))
        (define (dispatch sym)
            (cond ((eq? sym 'withdraw)
                (lambda (amt) (if (<= amt balance) 
                    (begin (set! balance (- balance amt)) balance)
                    "Insufficient Funds")))
                ((eq? sym 'deposit)
                    (lambda (amt) (begin 
                        (set! balance (+ balance amt))
                        balance)))
                ((eq? sym 'check-password)
                    #t)
                (else (error "Unknown request: MAKE-ACCOUNT" sym))))
        (lambda (pwd op) 
            (if (eq? pwd password)
                (dispatch op)
                (lambda (amt) "Incorrect Password")))))

(define (make-joint account old-pass new-pass)
    (if (account old-pass 'check-password)
        (lambda (pwd op)
            (if (eq? pwd new-pass)
                (account old-pass op)
                (account pwd op)))))

(define (make-withdraw initial-amount) 
    (let ((balance initial-amount))
        (lambda (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount)) balance)
                "Insufficient funds"))))

; --------- mutation based make-withdraw -----------
; (define (make-withdraw balance) 
;     (lambda (amount)
;         (if (>= balance amount)
;             (begin (set! balance (- balance amount))
;                         balance)
;                     "Insufficient funds")))


; ---------  bank account tests -----------
; (define peter-acc (make-account 100 'rosebud))
; (print ((peter-acc 'rosebud 'deposit) 10))
; (print ((peter-acc 'rosebud 'withdraw) 20))
; (define paul-acc (make-joint peter-acc 'rosebud 'new))
; (print ((paul-acc 'new 'withdraw) 10))



; ------------------------------ Monte Carlo Simulation Primitives ------------------------------

(define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
        (cond ((= trials-remaining 0)
            (/ trials-passed trials))
        ((experiment)
            (iter (- trials-remaining 1)
                (+ trials-passed 1)))
        (else (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

(define (random-in-range low high) 
    (let ((range (- high low)))
        (+ low (random range))))

; ------------------------------ Monte Carlo Area of a Circle ------------------------------
(define (in-circle? x y)
        (<= (+ (square x) (square y)) 1))

(define (estimate-integral predicate? x1 x2 y1 y2 trials)
    (monte-carlo trials 
        (lambda ()
            (predicate? (random-in-range x1 x2) (random-in-range y1 y2)))))



; ------------------ Mystery Function -------------
(define (mystery x)
    (define (loop x y)
        (if (null? x)
            y
            (let ((temp (cdr x)))
                (set-cdr! x y)
                (loop temp x))))
    (loop x '()))


; (define v (list 'a 'b 'c 'd))
; (define w (mystery v))
; (print v)
; (print w)


; ----------------- Count Number of Pairs in a List --------------------

;  --------------- Naive Count Pairs (given) ----------
(define (count-pairs x) (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; ----------- Correct Count Pairs operation -------------
(define (count-pairs x)
    (let ((visited ()))
        (define (visited? x lst)
            (cond 
                ((null? lst)
                    #f)
                ((eq? x (car lst)) #t) 
                (else (visited? x (cdr lst)))))
        (define (check x)
            (cond ((null? x) 0)
                ((not (pair? x)) 0)
                ((visited? x visited) 0)
                (else 
                    (begin
                        (set! visited (cons x visited))
                        (+ 1
                        (check (car x))
                        (check (cdr x)))))))
        
        (check x)))
        
(define (check-for-loop lst)
    (define (in? lst x)
            (cond 
                ((null? lst)
                    #f)
                ((eq? x (car lst)) #t) 
                (else (in? (cdr lst) x))))
    (define (check lst visited)
        (cond ((null? lst) #f)
            ((in? visited (car lst)) #t)
            (else (check (cdr lst) (cons (car lst) visited)))))
    (check (cdr lst) ()))


; ------------- Demonstrate Count Pairs -------------

; ; A 3-pair object
; (print (count-pairs (list 'a 'b' c))) ; prints 3
; ; A 3-pair object which the Naive implementation counts as 4
; (define four (cons 'c (list 'a 'b)))
; (set-car! four (cddr four))
; (print (count-pairs four)) ; prints 4 with Naive, 3 with correct

; ; A 3-pair object which the Naive implementation counts as 7
; (define temp (cons 'b (cons 'c ())))
; (set-car! temp (cdr temp))
; (define seven (cons 'a temp))
; (set-car! seven (cdr seven))
; ; (print seven)
; (print (count-pairs seven))
; ; (print (check-for-loop seven))

; ; A 3-pair object which the Naive implementation counts as infinite
; (define infinite (list 'a 'b 'c))
; (set-cdr! (cddr infinite) infinite)
; (print (check-for-loop infinite))
; (print (count-pairs infinite))

; ----------------------- QUEUE ---------------
(define (make-queue) (cons () ()))
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))
(define (empty-queue? q) (null? (front-ptr q)))
(define (front-queue q) 
    (if (empty-queue? q)
        (error "FRONT queue called on empty queue" q)
        (car (front-ptr q))))
(define (insert-queue! q item)
    (let ((new (cons item ())))
        (cond 
            ((empty-queue? q)
                (set-front-ptr! q new)
                (set-rear-ptr! q new)
                q)
            (else (set-cdr! (rear-ptr q) new)
                (set-rear-ptr! q new)
                q))))
(define (insert-front-queue! q item)
    (let ((new (cons item ())))
        (cond 
            ((empty-queue? q)
                (set-front-ptr! q new)
                (set-rear-ptr! q new)
                q)
            (else (set-front-ptr! q (cons item (front-ptr q)))))))
(define (delete-queue! q) 
    (if (empty-queue? q)
        (error "DELETE! queue called on empty queue" q)
        (begin (set-front-ptr! q (cdr (front-ptr q))) 
            q)))
(define (print-queue q)
    (print (car q)))

;  -------------- Queue Tests ---------------
; (define q (make-queue))
; (insert-front-queue! q 'middle)
; (insert-front-queue! q 'beginning)
; (insert-queue! q 'end)
; (print-queue q)  ; Should print (beginning middle end)
; (delete-queue! q)
; (print-queue q)  ; Should print (middle end)
; (delete-queue! q)
; (print-queue q)  ; Should print (end)
; (delete-queue! q)
; (print-queue q)  ; Should print ()
; (delete-queue! q) ; WARN: Should throw error

;  --------- Implementation of Queue in Message passing style ------------
; (define (make-queue)
;     (let ((front-ptr ()) (rear-ptr ()))
;         (define (dispatch op) 
;             (cond 
;                 ((eq? op 'empty-queue?) (lambda () (null? front-ptr)))
;                 ((eq? op 'front-queue)
;                     (lambda () (if (null? front-ptr)
;                         (error "FRONT called on empty queue")
;                         (car front-ptr))))
;                 ((eq? op 'insert-queue!)
;                     (lambda (item) (if (null? front-ptr)
;                         (begin (set! front-ptr (cons item ()))
;                             (set! rear-ptr front-ptr))
;                         (begin (set-cdr! rear-ptr (cons item ()))
;                             (set! rear-ptr (cdr rear-ptr))))))
;                 ((eq? op 'delete-queue!)
;                     (lambda () (if (null? front-ptr)
;                         (error "DELETE! called on empty queue")
;                         (let ((first (car front-ptr)))
;                             (set! front-ptr (cdr front-ptr))
;                             first))))
;                 ((eq? op 'print-queue)
;                     (lambda () (print front-ptr)))))
;     dispatch))



; ------------------------------------ DEQUE ----------------------------------

;  ------------------ DEQUE NODE ---------------
(define (new-node . args)  
    (if (null? args)
        (cons (cons () ()) ())
        (cons (cons (car args) ()) ())
    ))
(define (prev-node n)
    (cdr (car n)))
(define (next-node n)
    (cdr n))
(define (next-node n)
    (cdr n))
(define (value-node n)
    (car (car n)))
(define (set-prev-node! n val)
    (set-cdr! (car n) val))
(define (set-next-node! n val)
    (set-cdr! n val))
(define (set-value-node! n val)
    (set-car! (car n) val))

(define (make-deque)
    (let ((head (new-node 'head))
            (tail (new-node 'tail)))
        (set-next-node! head tail)
        (set-prev-node! tail head)
        (list head tail)))

;  ------------------ DEQUE  ---------------
(define (front-deque d) (car d))
(define (rear-deque d) (cadr d))
(define (empty-deque? q)
    (eq? (next-node (front-deque d)) (rear-deque d)))
(define (front-insert-deque! d val)
    (let ((head (front-deque d)) (new (new-node val)) (first-node (next-node (front-deque d))))
        (set-next-node! new first-node)
        (set-prev-node! first-node new)
        (set-prev-node! new head)
        (set-next-node! head new)))
(define (rear-insert-deque! d val)
    (let ((tail (rear-deque d)) (new (new-node val)) (last-node (prev-node (rear-deque d))))
        (set-prev-node! new last-node)
        (set-next-node! last-node new)
        (set-next-node! new tail)
        (set-prev-node! tail new)))
(define (print-deque d)
    (define (print-node node head tail)
        (cond ((eq? node head)
                (begin (display "{" )
                    (print-node (next-node node) head tail)))
            ((eq? node tail)
                (display "}\n" ))
            ((eq? (next-node node) tail)
                (begin (display (value-node node))
                    (print-node (next-node node) head tail)))
            (else 
                (begin (display (value-node node)) (display ", ")
                    (print-node (next-node node) head tail)))))
    (print-node (front-deque d) (front-deque d) (rear-deque d)))
(define (front-delete-deque! d)
    (if (empty-deque? d)
        (error "FRONT_DELETE_DEQUE: No items to delete" d)
        (let* ((head (front-deque d)) (first-node (next-node head)) (second-node (next-node first-node)))
            (set-next-node! head second-node)
            (set-prev-node! second-node head)
            first-node)))
(define (rear-delete-deque! d)
    (if (empty-deque? d)
        (error "REAR_DELETE_DEQUE: No items to delete" d)
        (let* ((tail (rear-deque d)) (last-node (prev-node tail)) (pen-node (prev-node last-node)))
            (set-next-node! pen-node tail)
            (set-prev-node! tail pen-node)
            last-node)))

;  ---------------- DEQUE TESTS ----------------
; (define d (make-deque))
; (front-insert-deque! d 'Iamnew)
; (front-insert-deque! d 'Iamnewer)
; (rear-insert-deque! d 'Iamlast)

; (print-deque d)
; (rear-delete-deque! d)
; (print-deque d)
; (rear-delete-deque! d)
; (print-deque d)
; (rear-delete-deque! d)
; (print-deque d)

; (define (lookup))
; -------------------------------- LinkedList Table ----------------------------------
(define (make-table . args)
    (if (null? args)
        (list 'table)
        (list (car args))))
(define (lookup key table) 
    (let ((record (assoc key (cdr table))))
        (if record 
            (cdr record)
            #f)))
(define (insert! key val table)
    (let ((record (assoc key (cdr table))))
        (if record
            (set-cdr! record val)
            (set-cdr! table (cons (cons key val) (cdr table)))))
    'ok)

; -------------------------------- LinkedList N-dimensional Table ----------------------------------

(define (lookup-n keys table)
    (cond ((null? (cdr keys))
        (lookup (car keys) table))
        (else 
            (let ((record (assoc (car keys) (cdr table))))
                (if record
                    (lookup-n (cdr keys) record)
                    #f)))))

(define (insert-n! keys val table)
    (if (null? (cdr keys))
        (insert! (car keys) val table)
        (let ((record (assoc (car keys) (cdr table))))
            (if record
                (insert-n! (cdr keys) val record)
                (begin 
                    (insert! (car keys) () table)
                    (insert-n! keys val table))))))


; ------------------------ N-dimensional table tests -------------------
; (define t (make-table))
; (insert-n! '(a * Z -) 2 t)
; (insert-n! '(a * Z +) 4 t)
; (insert-n! '(a * A +) 3 t)
; (print t)
; (print (lookup-n '(a * A +) t))



;  -----------------------------------  CIRCUIT  SIMULATION  -----------------------------------
(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
    'ok))
(define (full-adder a b cin sum cout)
    (let ((s (make-wire)) (c1 (make-wire)) (c2 make0wire))
        (half-adder b cin s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 cout)
    'ok))

(define (inverter input output)
    (define (invert-input)
        (let ((new-value (logical-not (get-signal input))))
            (after-delay inverter-delay 
                (lambda () (set-signal! output new-value)))))
    (add-action! input invert-input) 'ok)
(define (logical-not input)
    (cond ((= input 0) 1)
            ((= input 1) 0)
            (else (error "Invalid Signal" s))))
(define (and-gate a b out)
    (define (and-action-procedure)
        (let ((new-value (logical-and (get-signal a) (get-signal b))))
            (after-delay and-gate-delay 
                (lambda () (set-signal! out new-value)))))
    (add-action! a and-action-procedure)
    (add-action! b and-action-procedure)
    'ok)
(define (logical-and a1 a2)
    (cond ((and (= a1 1) (= a2 1)) 1)
        ((not (and (number? a1) (number? a2) )) (error "Invalid Signal: AND " a1 a2))
        (else 0)))
(define (or-gate a b out)
    (define (or-action-procedure)
        (let ((new-value (logical-or (get-signal a) (get-signal b))))
            (after-delay or-gate-delay 
                (lambda () (set-signal! out new-value)))))
        (add-action! a or-action-procedure)
        (add-action! b or-action-procedure)
        'ok)
(define (logical-or a1 a2)
    (cond ((not (and (number? a1) (number? a2))) (error "Invalid Signal: OR " a1 a2))
        ((or (= a1 1) (= a2 1)) 1)
        (else 0)))
(define (fake-or a b out)
    (let 
        ((c (make-wire)) (d (make-wire)) (e (make-wire)) (f (make-wire)))
        (inverter a c)
        (inverter b d)
        (and-gate d e f)
        (inverter f out)))
(define (make-wire)
    (let ((signal-value 0) (action-procedures ()))
        (define (set-my-signal! new-value)
            (if (not (= signal-value new-value))
                (begin (set! signal-value new-value)
                    (call-each action-procedures))
                'done
                ))
        (define (accept-action-procedure! proc)
            (set! action-procedures (cons proc action-procedures))
            (proc))
        (define (dispatch m)
            (cond ((eq? m 'get-signal) signal-value)
                ((eq? m 'set-signal!) set-my-signal!)
                ((eq? m 'add-action!) accept-action-procedure!)
                (else (error "Unknown Operation: WIRE" m))))
        dispatch))
(define (call-each procedures)
    (if (null? procedures)
        'done
        (begin ((car procedures))
            (call-each (cdr procedures)))))
(define (get-signal wire) (wire 'get-signal)) 
(define (set-signal! wire new-value)
    ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
    (add-to-agenda! 
        (+ delay (current-time the-agenda))
        action the-agenda))
(define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (begin ((first-agenda-item the-agenda))
            (remove-first-agenda-item! the-agenda)
            (propagate))))
(define (probe name wire)
    (add-action! wire 
        (lambda () (print name " " (current-time the-agenda) " New-value = " (get-signal wire)))))

(define (make-time-segment time queue)
    (cons time queue))
(define (segment-time s) (car s)) 
(define (segment-queue s) (cdr s))
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda) 
    (define (belongs-before? segments)
        (or (null? segments) (< time (segment-time (car segments)))))
    (define (make-new-time-segment time action)
        (let ((q (make-queue)))
            (insert-queue! q action)
            (make-time-segment time q)))
    (define (add-to-segments! segments)
      (if (= (segment-time (car segments)) time)
          (insert-queue! (segment-queue (car segments)) action)
          (let ((rest (cdr segments)))
            (if (belongs-before? rest)
                (set-cdr! segments (cons (make-new-time-segment time action) (cdr segments)))
                (add-to-segments! rest)))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
          (set-segments! agenda (cons (make-new-time-segment time action) segments))
          (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
        (delete-queue! q)
        (if (empty-queue? q)
            (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty: FIRST-AGENDA-ITEM")
        (let ((first-seg (first-segment agenda)))
            (set-current-time! agenda
                (segment-time first-seg))
            (front-queue (segment-queue first-seg)))))



; (define the-agenda (make-agenda))
; (define inverter-delay 2)
; (define and-gate-delay 3)
; (define or-gate-delay 5)
; (define input-1 (make-wire))
; (define input-2 (make-wire))
; (define sum (make-wire))
; (define carry (make-wire))


