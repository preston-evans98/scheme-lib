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

;----------------  Circuit Simulator Tests ------------

; (define the-agenda (make-agenda))
; (define inverter-delay 2)
; (define and-gate-delay 3)
; (define or-gate-delay 5)
; (define input-1 (make-wire))
; (define input-2 (make-wire))
; (define sum (make-wire))
; (define carry (make-wire))

; (probe 'sum sum)
; (probe 'carry carry)
; (half-adder input-1 input-2 sum carry)
; (set-signal! input-1 1)
; (propagate) ; expected output sum 8 New-value = 1
; (set-signal! input-2 1)
; (propagate) ; carry 11 New-value = 1 and sum 16 New-value = 0


; -------------------- CONSTRAINT SYSTEMS ---------------
(define (celsius-fahrenheit-converter c f)
    (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (adder a1 a2 sum)
    (define (process-new-value)
      (cond ((and (has-value? a1) (has-value? a2))
                (set-value! sum (+ (get-value a1) (get-value a2)) me))
            ((and (has-value? a1) (has-value? sum))
                (set-value! a2 (- (get-value sum) (get-value a1)) me))
            ((and (has-value? a2) (has-value? sum))
                (set-value! a1 (- (get-value sum) (get-value a2)) me)))) 
    (define (process-forget-value)
        (forget-value! sum me)
        (forget-value! a1 me)
        (forget-value! a2 me)
        (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value) (process-new-value))
            ((eq? request 'I-lost-my-value) (process-forget-value))
            (else (error "Unknown Request: Adder" request))))
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
              (and (has-value? m2) (= (get-value m2) 0)))
                (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2))
       (set-value! product (* (get-value m1) (get-value m2)) me))
      ((and (has-value? m2) (has-value? product))
       (set-value! m1 (/ (get-value product) (get-value m2)) me))
      ((and (has-value? m1) (has-value? product)) 
       (set-value! m2 (/ (get-value product) (get-value m1)) me))))
    (define (process-forget-value)
      (forget-value! m1 me)
      (forget-value! m2 me)
      (forget-value! product me)
      (process-new-value))
    (define (me request)
      (cond ((eq? request 'I-have-a-value) (process-new-value))
            ((eq? request 'I-lost-my-value) (process-forget-value))
            (else (error "Unknown request: MULTIPLIER" request))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me)
(define (inform-about-value constraint)
    (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
    (constraint 'I-lost-my-value))
(define (constant value connector)
  (define (me request)
    (error "Unknown request: Connector" request))
  (connect connector me)
  (set-value! connector value me)
  me)
(define (probe name connector)
  (define (print-probe value)
    (print "Probe:" name " = " value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unkown request: PROBE" request))))
  (connect connector me)
  me)
(define (make-connector)
  (let ((value false) (informant false) (constraints ()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints (cons new-constraint constraints)))
      (if (has-value? me) 
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: Connector" request))))
    me)) 
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
   ((connector 'connect) new-constraint))

; ------------------ TEST CONSTRAINT PRIMITIVES -----------
; (define C (make-connector))
; (define F (make-connector))
; (define Sum (make-connector))
; (celsius-fahrenheit-converter C F)
; (set-value! F 212 'user)
; (probe "Celsius temp" C)
; (probe "Fahrenheit temp" F)
; (forget-value! F 'user)
; (set-value! C 32 'user)
; (forget-value! C 'user)

(define (averager a b c)
  (let ((x (make-connector))
        (two (make-connector)))
    (adder a b x)
    (constant 2 two)
    (multiplier c two x)
    'ok))
; ------------ TEST AVERAGER ---------------
; (define C (make-connector))
; (define F (make-connector))
; (define Sum (make-connector))
; (averager C F Sum)
; (probe "C" C)
; (probe "F" F)
; (probe "Avg" Sum)
; (set-value! C 10 'user)
; (set-value! F 20 'user)
; (forget-value! C 'user)
; (set-value! Sum 40 'user)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER" (get-value b))
            (set-value! a (sqrt b) me))
        (if (has-value? a) 
            (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
      (cond ((eq? request 'I-have-a-value) (process-new-value))
            ((eq? request 'I-lost-my-value) (process-forget-value))
            (else (error "Unknown Request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)
    
; -------------- Test Squarer  ---------------
; (define C (make-connector))
; (define F (make-connector))
; (squarer C F)
; (probe "C" C)
; (probe "F" F)
; (set-value! C -10 'user)
; (set-value! F 10 'user)
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))
(define (cv x)
  (let ((c (make-connector)))
    (constant x c)
    c))
(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5)) x)
      (cv 32)))
; ---------- TEST c+, c*, etc. ----------
; (define C (make-connector))
; (define F (celsius-fahrenheit-converter C))
; (probe "Fahrenheit temp" F)
; (set-value! C 32 'user)

; ------------------------------ STREAMS ----------------------------
(define the-empty-stream ())
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each print s))
(define (print-stream s n)
  (if (= n 0)
      the-empty-stream
      (begin (print (stream-car s))
      (print-stream (stream-cdr s) (- n 1)))))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream)) (cons-stream (stream-car stream)
                                                (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream 
        (apply proc (map stream-car argstreams))
        (apply stream-map 
               (cons proc (map stream-cdr argstreams))))))
(define (show x)
  (print x)
  x)
; (define x (stream-map show (stream-enumerate-interval 0 20)))
; (stream-ref x 7)
; (print "Seven")
; (stream-ref x 5)
(define (scale-stream s n)
  (stream-map (lambda (x) (* x n)) s))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))
(define fibs (cons-stream 1 (add-streams (cons-stream 0 fibs) fibs)))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))
(define (partial-sums s)
  (define new (cons-stream (stream-car s) (add-streams new (stream-cdr s))))
    new)
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((> s1car s2car) 
                       (cons-stream s2car (merge s1 (stream-cdr s2))))
                      ((< s1car s2car)
                       (cons-stream s1car (merge (stream-cdr s1) s2)))
                      (else (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))
(define evens (scale-stream integers 2))
(define threes (scale-stream integers 3))
(define fives (scale-stream integers 5))
(define s (cons-stream 1 (merge evens (merge threes fives))))
(define (expand num den radix)
    (cons-stream
        (quotient (* num radix) den)
        (expand (remainder (* num radix) den) den radix)))
(define (integrate-series s)
  (mul-streams s (stream-map (lambda (x) (/ 1 x)) integers)))

(define cosine-series 
  (cons-stream 1 
    (integrate-series (stream-map (lambda (x) (- x)) sin-series))))
(define sin-series (cons-stream 0 (integrate-series cosine-series)))
(define (sine x terms)
  (define (iter sum stream n)
    (if (= n terms)
      sum
      (iter (+ sum (* (stream-car stream) (pow x n))) (stream-cdr stream) (+ n 1))))
  (iter 0 sin-series 0))
; (print (sine  (/ 3.141592 2) 15))
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s2) (stream-car s1))
    (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                 (scale-stream (stream-cdr s2) (stream-car s1)))
        (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define (invert-unit-series s)
  (let ((sr (stream-cdr s)))
    (define x (cons-stream 1 (stream-map (lambda (x) (- x)) (mul-series sr x))))
    x))
(define inverse-cos 
  (invert-unit-series cosine-series))
(define (div-series a b)
  (if (= (stream-car b) 0)
      (error "Can't divide by series starting with zero" b)
      (mul-series a (invert-unit-series b))))
(define (accumulate-stream s x terms)
  (define (iter sum stream n)
    (if (= n terms)
      sum
      (iter (+ sum (* (stream-car stream) (pow x n))) (stream-cdr stream) (+ n 1))))
  (iter 0 s 0))
(define tangent (div-series sin-series cosine-series))
; (print (accumulate-stream tangent (/ 3.141592 2) 20))
; (print-stream cosine-series 10)
; (print "----------")
; (print-stream (div-series cosine-series cosine-series) 10)

; ---------------------------- Iterations as Streams -------------------------
; --------- estimate sqrts, pi, logs as accelerated streams-------
(define (pi-summands n) 
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x) 
  (define guesses (cons-stream 1.0 
    (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)
(define reciprocals
  (cons-stream 1.0 (stream-map / ones (stream-cdr integers))))
(define alternating
  (cons-stream 1 (stream-map - alternating)))
(define alternating-reciprocals
  (mul-streams alternating reciprocals))
(define log-stream (partial-sums alternating-reciprocals))
; ------------------- Interleave streams -------------
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
           (interleave s2 (stream-cdr s1)))))
; (define (pairs s t)
;   (cons-stream (list (stream-car s) (stream-car t))
;     (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
;                 (pairs (stream-cdr s) (stream-cdr t)))))
; ------------------- Generate all pairs from (Si, Tj) where S, T are streams, i <= j-------------
(define (pairs s t)
  (cons-stream (list (stream-car s) (stream-car t))
    (interleave (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
                (pairs (stream-cdr s) (stream-cdr t)))))
; ------- Same as above, but for all triples from streams S, T, U ---------------
(define (triples s t u)
  (cons-stream (list (stream-car s) (stream-car t) (stream-car u))
    (interleave (stream-cdr (stream-map (lambda (x) (cons (stream-car s) x)) (pairs t u)))
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
; ------------Generate all pythagorean triples ------
(define pythagorus (stream-filter 
    (lambda (triplet) (= (+ (square (car triplet)) (square (cadr triplet))) 
                         (square (caddr triplet))))
    (triples integers integers integers)))
; ------------ Merge two streams in some order ------
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1-car (stream-car s1)) (s2-car (stream-car s2)))
                (if (< (weight s1-car) (weight s2-car))
                    (cons-stream s1-car (merge-weighted (stream-cdr s1) s2 weight))
                    (cons-stream s2-car (merge-weighted (stream-cdr s2) s1 weight)))))))
(define (weighted-pairs s t weight)
    (cons-stream (list (stream-car s) (stream-car t))
      (merge-weighted (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
        (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
        weight)))

; --------------------Primitives for generating ramanujan, etc. --------------
; (print-stream log-stream 15)
; (print-stream (euler-transform log-stream) 15)
; (print-stream (accelerated-sequence euler-transform log-stream) 15)
; (print-stream (pairs integers integers) 15)
; (print-stream pythagorus 3)
; (print-stream (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))) 15)
(define (divisible-2-3-5 x)
  (or (= (remainder x 2) 0)
       (= (remainder x 3) 0)
       (= (remainder x 5) 0)))
; (define usable-integers (stream-filter (lambda (x) (not (divisible-2-3-5 x))) integers))
; (print-stream usable-integers 10)
; (print-stream (weighted-pairs usable-integers usable-integers (lambda (x) (+ (* 2 (car x))
                                                                ; (* 3 (cadr x))
                                                                ; (* 5 (car x) (cadr x))))) 15)

(define (s-cubes x) (+ (cube (car x)) (cube (cadr x))))
(define (s-squares x) (+ (square (car x)) (square (cadr x))))
(define sum-cube-stream (weighted-pairs integers integers s-cubes))
; ----------- Generate ramanujan numbers -------
(define (ramanujan s weight)
  (if (= (weight (stream-car s)) (weight (stream-car (stream-cdr s))))
        (cons-stream (weight (stream-car s)) (ramanujan (stream-cdr s) weight))
        (ramanujan (stream-cdr s) weight)))
; ----------- Generate numbers that are sums of square in 3 different ways-------
(define (3-sum-squares s weight)
  (let ((a (stream-car s)) 
        (b (stream-car (stream-cdr s))) 
        (c (stream-car (stream-cdr (stream-cdr s)))))
  (if (= (weight a) (weight b) (weight c))
         (cons-stream (list a b c ":" (weight c))
            (3-sum-squares (stream-cdr s) weight))
        (3-sum-squares (stream-cdr s) weight))))
; (print-stream (ramanujan sum-cube-stream s-cubes) 6 )
; (print-stream (3-sum-squares (weighted-pairs integers integers s-squares) s-squares) 6 )
; (print-stream sum-cube-stream 10)
;--------------- Stream definition of integral ---------
(define (integral integrand initial-value dt)
   (define int
     (cons-stream initial-value
                  (add-streams (scale-stream integrand dt)
                               int))) int)
; (print-stream (integral integers 0 .5) 10)
;--------------- Two small curicuti simulations ---------
(define (RC R C dt)
  (define (output current-stream v0)
    (add-streams (scale-stream current-stream R)
                 (integral (scale-stream current-stream (/ 1.0 C)) v0 dt)))
  output)
(define RC1 (RC 5 1 0.5))
; (print-stream (RC1 integers 0) 10)
;--------------- Sign Change detector for streams---------
(define (sign-change-detector a b)
  (cond ((and (positive? a) (negative? b)) -1)
        ((and (positive? b) (negative? a)) 1)
        (else 0)))
(define sense-data (cons-stream 1 (cons-stream 2 (cons-stream 1.5 (cons-stream 1 (cons-stream 0.5
    (cons-stream -0.1 (cons-stream -2 (cons-stream -3 (cons-stream -2
     (cons-stream -0.5 (cons-stream 0.2 (cons-stream 3 (cons-stream 4 ()))))))))))))))
(define zero-crossings 
  (stream-map sign-change-detector
    sense-data
    (stream-cdr sense-data)))
; (print-stream zero-crossings 11)
; (define (make-zero-crossings input-stream last-value last-avg)
;   (let ((avpt (/ (+ (stream-car input-stream)
;                     last-value)
;                  2)))
;     (cons-stream
;       (sign-change-detector last-avg avpt)
;       (make-zero-crossings
;         (stream-cdr input-stream) (stream-car input-stream) avpt))))
; (print-stream (make-zero-crossings sense-data 0 0) 12)
; ---------------- Average damp a stream (window  = 2) ---------
(define (smooth stream)
  (define (iter s prev)
    (cons-stream (average (stream-car s) prev)
        (iter (stream-cdr s) (stream-car s))))
  (iter stream 0))
(define (make-zero-crossings input-stream)
    (stream-map sign-change-detector (smooth input-stream) (stream-cdr (smooth input-stream))))
; (print-stream (make-zero-crossings sense-data) 11)
; (print-stream (smooth sense-data) 10)
;---------------------- delayed stream definition of integral ------------
(define (integral delayed-integrand initial-value dt)
  (cons-stream
    initial-value
    (let ((integrand (force delayed-integrand)))
        (if (stream-null? integrand)
            the-empty-stream
            (integral (delay (stream-cdr integrand))
                      (+ (* dt (stream-car integrand))
                         initial-value)
                      dt)))))
; (print-stream (solve square 1 1) 5)
; ---------------------- first order differential equation solver --------------
(define (solve f y0 dt)
   (define y (integral (delay dy) y0 dt))
   (define dy (stream-map f y))
   y)
; (print-stream (solve square 1 1) 5)
; ---------------------- quadratic 2nd order differential equation solver --------------
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)
; (print (stream-ref (solve (lambda (y) y) 1 .0001) 10000))
; ( print (stream-ref (solve-2nd 1 1 1 1 .0001) 1000) )
; ---------------------- general 2nd order differential equation solver --------------
(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f y dy))
  y)
; (print (stream-ref (solve-2nd (lambda (x y) (+ x y)) 1 1 .0001) 1000))

; ----------------------- Cirucuit Simulation--------------------
(define (rlc R L C dt)
  (define (circuit vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1.0 C)))
    (define dil (add-streams (scale-stream il (/ (- 0 R) L))
                             (scale-stream vc (/ 1 L))))
    (cons vc il))
  circuit)

; ------------------------- Stream with Reset command ---------------- 
; (print-stream (car ((rlc 1 1 .2 .1) 10 0)) 10)
; (define (generator msgs)
;   (define (iter num msgs)
;     (if (not (eq? (stream-car msgs) 'reset))
;         (cons-stream num (iter (+ num 1) (stream-cdr msgs)))
;         (cons-stream 0 (iter 1 (stream-cdr msgs)))))
;   (iter 0 msgs))
; (print-stream (generator cmds ) 10)

; ------------------------ Monte Carlo Area of a circle (Stream version) -------------
(define (random-range-stream a b)
  (stream-map random-in-range (scale-stream ones a) (scale-stream ones b)))
(define (monte-carlo experiment-stream)
  (define s (cons-stream 0 (add-streams s (stream-map (lambda (x) (if x 1 0)) experiment-stream ))))
  (stream-map (lambda (x y) (/ x y) )
   (stream-cdr s) integers))
(define (estimate-integral predicate? x1 x2 y1 y2)
  (monte-carlo (stream-map predicate? (random-range-stream x1 x2) (random-range-stream y1 y2))))
; (print (* 4.0 (stream-ref (estimate-integral in-circle? -1.0 1.0 -1.0 1.0) 10000)))

