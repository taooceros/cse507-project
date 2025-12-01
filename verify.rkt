#lang rosette

(require "model.rkt" "ring_buffer.rkt")

(define BUFFER-SIZE 4) ;; Small scope assumption for counterexample search.
(define BOUNDED-BITWIDTH 4) ;; Reduced integer width keeps SMT search tractable.

(define (all-writes trace)
  (filter is-write? trace))

(define (verify-ring-buffer model-type make-trace)
  (current-bitwidth BOUNDED-BITWIDTH)

  (define-symbolic* r_tail_val integer?)
  (define-symbolic* r_data_val integer?)

  (define trace (make-trace r_tail_val r_data_val))

  (define init-writes
    (list (mk-write -1 -1 DATA 0)
          (mk-write -2 -1 TAIL 0)
          (mk-write -3 -1 HEAD 0)))

  (define full-trace (append init-writes trace))
  (define reads (get-reads full-trace))
  (define writes (all-writes full-trace))

  (parameterize ([current-model model-type])
    (define-values (rf-constraints rf-rel rf-choices)
      (synthesize-rf full-trace writes reads))

    (define write-ranks
      (for/list ([w writes])
        (define-symbolic* rank integer?)
        rank))

    (define (get-co-rank w)
      (list-ref write-ranks (index-of writes w)))

    (define co-constraints
      (for/and ([w1 writes] [w2 writes])
        (and
         (implies (and (equal? (event-addr w1) (event-addr w2))
                       (not (equal? w1 w2)))
                  (not (= (get-co-rank w1) (get-co-rank w2))))
         (implies (and (equal? (event-addr w1) (event-addr w2))
                       (< (event-id w1) 0)
                       (> (event-id w2) 0))
                  (< (get-co-rank w1) (get-co-rank w2))))))

    (define (co-rel w1 w2)
      (and (member w1 writes)
           (member w2 writes)
           (equal? (event-addr w1) (event-addr w2))
           (< (get-co-rank w1) (get-co-rank w2))))

    (define ranks
      (for/list ([e full-trace])
        (define-symbolic* r integer?)
        r))

    (define (get-rank e)
      (list-ref ranks (index-of full-trace e)))

    (define acyclic-constraints
      (for/and ([e1 full-trace] [e2 full-trace])
        (let ([rel (or (ppo full-trace e1 e2)
                       (rf-rel e1 e2)
                       (co-rel e1 e2)
                       (fr full-trace rf-rel co-rel e1 e2))])
          (implies rel (< (get-rank e1) (get-rank e2))))))

    (define model-constraints
      (and (well-formed-rf full-trace rf-rel)
           (well-formed-co full-trace co-rel)
           acyclic-constraints))

    (define violation
      (and (= r_tail_val 1)
           (not (= r_data_val 1))))

    (define formula
      (and rf-constraints co-constraints model-constraints violation))

    (define sol (solve (assert formula)))

    (if (unsat? sol)
        sol
        (begin
          (printf "Model found under ~a.\n" model-type)
          (printf "  r_tail_val: ~a\n" (evaluate r_tail_val sol))
          (printf "  r_data_val: ~a\n" (evaluate r_data_val sol))
          sol))))

(define (report-outcome label model-type solution)
  (if (unsat? solution)
      (printf "  ~a: verified under ~a (no stale read).\n" label model-type)
      (printf "  ~a: counterexample under ~a.\n" label model-type)))

(define (run-verification label make-trace)
  (printf "Verifying ~a...\n" label)
  (define sc-solution (verify-ring-buffer 'sc make-trace))
  (report-outcome label 'SC sc-solution)
  (define rdma-solution (verify-ring-buffer 'rdma make-trace))
  (report-outcome label 'RDMA rdma-solution)
  (newline))

(run-verification "P1 (fenced)" make-trace-p1)
(run-verification "P2 (no fences)" make-trace-p2)
