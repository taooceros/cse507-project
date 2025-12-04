#lang rosette

(provide (all-defined-out))
(require racket/list)

;; Events carry an optional memory-order tag: 'SC, 'REL, or 'ACQ.
(struct event (id thread-id type addr val mode) #:transparent)

(define (mk-read id tid addr val #:mode [mode #f])
  (event id tid 'read addr val mode))

(define (mk-write id tid addr val #:mode [mode #f])
  (event id tid 'write addr val mode))

;; Basic predicates
(define (is-read? e) (equal? (event-type e) 'read))
(define (is-write? e) (equal? (event-type e) 'write))

(define (mode-sc? e) (equal? (event-mode e) 'SC))
(define (mode-rel? e) (equal? (event-mode e) 'REL))
(define (mode-acq? e) (equal? (event-mode e) 'ACQ))

;; Program order: per-thread id ordering.
(define (po trace e1 e2)
  (and (equal? (event-thread-id e1) (event-thread-id e2))
       (< (event-id e1) (event-id e2))))

;; Collectors
(define (get-writes trace addr)
  (filter (lambda (e) (and (is-write? e)
                           (equal? (event-addr e) addr)))
          trace))

(define (get-reads trace)
  (filter is-read? trace))

;; From-read relation helper used downstream.
(define (fr trace rf co e1 e2)
  (ormap (lambda (w-prime)
           (and (rf w-prime e1)
                (co w-prime e2)))
         trace))

;; Rank utilities for cyclicity proofs.
(define (acyclic? rel trace)
  (define ranks
    (for/list ([e trace])
      (define-symbolic* r integer?)
      r))

  (define (get-rank e)
    (list-ref ranks (index-of trace e)))

  (and ranks
       (for/and ([e1 trace] [e2 trace])
         (implies (rel e1 e2)
                  (< (get-rank e1) (get-rank e2))))))

;; Relaxed program-order relation: only events marked SC enforce order.
(define (ppo-relaxed trace e1 e2)
  (and (po trace e1 e2)
       (or (mode-sc? e1)
           (mode-sc? e2))))

;; Sequentially consistent program order: full per-thread ordering.
(define (ppo-sc trace e1 e2)
  (po trace e1 e2))

;; Well-formedness checks.
(define (well-formed-rf trace rf)
  (for/and ([r trace])
    (if (is-read? r)
        (ormap (lambda (w)
                 (and (rf w r)
                      (equal? (event-addr w) (event-addr r))
                      (equal? (event-val w) (event-val r))))
                trace)
        #t)))

(define (well-formed-co trace co)
  (for/and ([w1 trace] [w2 trace])
    (implies (co w1 w2)
             (and (equal? (event-addr w1) (event-addr w2))
                  (is-write? w1)
                  (is-write? w2)))))

(define (consistent? trace rf co)
  (define (union r1 r2)
    (lambda (x y)
      (or (r1 x y) (r2 x y))))

  (define fr-rel (lambda (x y) (fr trace rf co x y)))
  (define rf-rel (lambda (x y) (rf x y)))
  (define co-rel (lambda (x y) (co x y)))
  (define ppo-rel (lambda (x y) (ppo-relaxed trace x y)))

  (define combined
    (union ppo-rel
           (union rf-rel
                  (union co-rel fr-rel))))

  (and (well-formed-rf trace rf)
       (well-formed-co trace co)
       (acyclic? combined trace)))

;; Release-acquire helper: if a REL write synchronises with an ACQ read of the
;; same address/value, then all writes program-order-before the release must
;; become visible before the acquire (rank ordering + enforced rf pairing).
(define (release-acquire-visibility trace rf get-rank)
  (define releases
    (filter (lambda (e) (and (is-write? e) (mode-rel? e))) trace))
  (define acquires
    (filter (lambda (e) (and (is-read? e) (mode-acq? e))) trace))

  (for/and ([w releases])
    (for/and ([r acquires])
        (implies (equal? (event-addr w) (event-addr r))
           (and (rf w r)
          (equal? (event-val r) (event-val w))
          (for/and ([w-pre trace]
              #:when (and (is-write? w-pre)
              (equal? (event-thread-id w-pre)
                (event-thread-id w))
              (po trace w-pre w)))
            (< (get-rank w-pre) (get-rank r))))))))
