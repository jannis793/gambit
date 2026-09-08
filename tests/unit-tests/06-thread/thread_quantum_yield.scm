(include "#.scm")

;; Explicit heartbeats make time-slice accounting independent of CPU speed.
;; The long timer interval keeps real timer delivery out of these short tests.
(define saved-heartbeat (make-f64vector 1))
(##get-heartbeat-interval! saved-heartbeat 0)
(define saved-quantum (thread-quantum (current-thread)))
(##set-heartbeat-interval! 1000.0)
(thread-quantum-set! (current-thread) 3000.0)

(define processor (current-processor))
(define (make-local-thread thunk)
  (let ((thread (make-thread thunk)))
    (cond-expand
     (enable-smp (##thread-pin! thread processor))
     (else #f))
    thread))

;; A yield with no other runnable thread must also start a fresh quantum.
(thread-yield!)
(##thread-heartbeat!)
(##thread-heartbeat!)
(thread-yield!)
(define events '())
(define (record! event) (set! events (cons event events)))
(define worker (make-local-thread (lambda () (record! 'worker))))
(thread-start! worker)
(##thread-heartbeat!)
(record! 'one)
(##thread-heartbeat!)
(record! 'two)
(##thread-heartbeat!)
(record! 'three)
(thread-join! worker)
(test-equal '(one two worker three) (reverse events))

;; Every quantum, not just the first, must last for three heartbeats.
(set! events '())
(define (make-worker name)
  (make-local-thread
   (lambda ()
     (let loop ((n 0))
       (if (< n 9)
           (begin
             (record! name)
             (##thread-heartbeat!)
             (loop (+ n 1))))))))
(define a (make-worker 'a))
(define b (make-worker 'b))
(thread-start! a)
(thread-start! b)
(thread-join! a)
(thread-join! b)
(test-equal '(a a a b b b a a a b b b a a a b b b) (reverse events))

(##set-heartbeat-interval! (f64vector-ref saved-heartbeat 0))
(thread-quantum-set! (current-thread) saved-quantum)
