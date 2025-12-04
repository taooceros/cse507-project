#lang rosette

(require "model.rkt")
(provide (all-defined-out))

;; Addresses (two slots) and wrap-around head/tail
(define DATA0 0)
(define DATA1 1)
(define TAIL 2)
(define HEAD 3)

;; Helper to index read values
(define (rv rvals idx) (list-ref rvals idx))

;; P1: Correct synchronization (tail publishes with REL, consumer acquires)
(define (initial-events)
  (list
  (mk-write -4 0 DATA0 0 #:mode 'SC)
  (mk-write -3 0 DATA1 0 #:mode 'SC)
  (mk-write -2 0 TAIL 0 #:mode 'SC)
  (mk-write -1 0 HEAD 0 #:mode 'SC)))

(define (make-trace-p1 rvals)
  (append
   (initial-events)
   (list
    ;; Producer publishes slot 0 then slot 1
    (mk-write 1 1 DATA0 1 #:mode 'SC)
    (mk-write 2 1 TAIL 1 #:mode 'REL)
    (mk-write 3 1 DATA1 2 #:mode 'SC)
    (mk-write 4 1 TAIL 2 #:mode 'REL)

    ;; Consumer round 1
    (mk-read 5 2 TAIL (rv rvals 0) #:mode 'ACQ)
    (mk-read 6 2 DATA0 (rv rvals 1) #:mode 'SC)
    (mk-write 7 2 HEAD 1 #:mode 'SC)

    ;; Consumer round 2
    (mk-read 8 2 TAIL (rv rvals 2) #:mode 'ACQ)
    (mk-read 9 2 DATA1 (rv rvals 3) #:mode 'SC)
    (mk-write 10 2 HEAD 0 #:mode 'SC))))

;; P2a: Missing release (tail writes are plain, consumer still acquires)
(define (make-trace-p2a rvals)
  (append
   (initial-events)
   (list
    (mk-write 1 1 DATA0 1 #:mode 'SC)
    (mk-write 2 1 TAIL 1 #:mode 'SC)
    (mk-write 3 1 DATA1 2 #:mode 'SC)
    (mk-write 4 1 TAIL 2 #:mode 'SC)

    (mk-read 5 2 TAIL (rv rvals 0) #:mode 'ACQ)
    (mk-read 6 2 DATA0 (rv rvals 1) #:mode 'SC)
    (mk-write 7 2 HEAD 1 #:mode 'SC)
    (mk-read 8 2 TAIL (rv rvals 2) #:mode 'ACQ)
    (mk-read 9 2 DATA1 (rv rvals 3) #:mode 'SC)
    (mk-write 10 2 HEAD 0 #:mode 'SC))))

;; P2b: Producer releases, but consumer fails to acquire
(define (make-trace-p2b rvals)
  (append
   (initial-events)
   (list
    (mk-write 1 1 DATA0 1 #:mode 'SC)
    (mk-write 2 1 TAIL 1 #:mode 'REL)
    (mk-write 3 1 DATA1 2 #:mode 'SC)
    (mk-write 4 1 TAIL 2 #:mode 'REL)

    (mk-read 5 2 TAIL (rv rvals 0) #:mode 'SC)
    (mk-read 6 2 DATA0 (rv rvals 1) #:mode 'SC)
    (mk-write 7 2 HEAD 1 #:mode 'SC)
    (mk-read 8 2 TAIL (rv rvals 2) #:mode 'SC)
    (mk-read 9 2 DATA1 (rv rvals 3) #:mode 'SC)
    (mk-write 10 2 HEAD 0 #:mode 'SC))))

(define (make-trace-p3 rvals)
  (append
   (initial-events)
   (list
    (mk-write 1 1 TAIL 1 #:mode 'REL)
    (mk-write 2 1 DATA0 1 #:mode 'SC)
    (mk-write 3 1 DATA1 2 #:mode 'SC)
    (mk-write 4 1 TAIL 2 #:mode 'REL)

    (mk-read 5 2 TAIL (rv rvals 0) #:mode 'SC)
    (mk-read 6 2 DATA0 (rv rvals 1) #:mode 'SC)
    (mk-write 7 2 HEAD 1 #:mode 'SC)
    (mk-read 8 2 TAIL (rv rvals 2) #:mode 'SC)
    (mk-read 9 2 DATA1 (rv rvals 3) #:mode 'SC)
    (mk-write 10 2 HEAD 0 #:mode 'SC))))
