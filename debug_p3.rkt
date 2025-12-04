#lang rosette
(require "verify.rkt" "model.rkt" "ring_buffer.rkt")

(define (event-by-id trace eid)
  (define evt (for/first ([e trace] #:when (= (event-id e) eid)) e))
  (unless evt (error 'event-by-id "missing event ~a" eid))
  evt)

(define (check-p3-specific mode)
  (define (trace-builder rvals)
    (define trace (make-trace-p3 rvals))
    ;; Force reading TAIL = 1
    (define tail-read (event-by-id trace 5))
    (assert (= (event-val tail-read) 1))
    trace)
  (analyze-scenario trace-builder mode))

(define result (check-p3-specific 'sc))
(displayln (scenario->report result))

