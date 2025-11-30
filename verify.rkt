#lang rosette

(require "model.rkt" "ring_buffer.rkt")

(define (verify-ring-buffer make-trace)
  ;; Clear state
  (current-bitwidth #f) ;; Use infinite precision for integers
  
  ;; 1. Create symbolic values for reads
  (define-symbolic* r_tail_val integer?)
  (define-symbolic* r_data_val integer?)
  
  (define trace (make-trace r_tail_val r_data_val))
  
  ;; Initial writes
  (define init-writes 
    (list (mk-write -1 -1 DATA 0)
          (mk-write -2 -1 TAIL 0)
          (mk-write -3 -1 HEAD 0)))
          
  (define full-trace (append init-writes trace))
  
  ;; 2. Synthesize RF
  (define reads (filter (lambda (e) (equal? (event-type e) 'read)) full-trace))
  (define writes (filter (lambda (e) (or (equal? (event-type e) 'write) 
                                         (equal? (event-type e) 'rdma-write))) full-trace))

  (printf "Reads: ~a\n" reads)
  (printf "Writes: ~a\n" writes)

  (define read-sources
    (for/list ([r reads])
      (define-symbolic* idx integer?)
      idx))
  
  (define (check-rf r idx)
    (for/or ([w writes] [i (in-naturals)])
      (and (equal? idx i)
           (equal? (event-addr r) (event-addr w))
           (equal? (event-val r) (event-val w)))))

  (define rf-constraints
    (and (check-rf (list-ref reads 0) (list-ref read-sources 0))
         (check-rf (list-ref reads 1) (list-ref read-sources 1))))

  (define (rf-rel w r)
    (if (and (member r reads) (member w writes))
        (let ([idx (list-ref read-sources (index-of reads r))])
          (equal? w (list-ref writes idx)))
        #f))

  ;; 3. Synthesize CO
  ;; We only care about CO for same address.
  (define write-ranks
    (for/list ([w writes])
      (define-symbolic* rank integer?)
      rank))
      
  (define (get-co-rank w)
    (list-ref write-ranks (index-of writes w)))
    
  (define co-constraints
    (for/and ([w1 writes] [w2 writes])
      (and
       ;; Total order logic
       (implies (and (equal? (event-addr w1) (event-addr w2))
                     (not (equal? w1 w2)))
                (not (equal? (get-co-rank w1) (get-co-rank w2))))
       ;; Init is first
       (implies (and (equal? (event-addr w1) (event-addr w2))
                     (< (event-id w1) 0) ;; w1 is init
                     (> (event-id w2) 0)) ;; w2 is not init
                (< (get-co-rank w1) (get-co-rank w2))))))
               
  (define (co-rel w1 w2)
    (and (member w1 writes) (member w2 writes)
         (equal? (event-addr w1) (event-addr w2))
         (< (get-co-rank w1) (get-co-rank w2))))

  ;; Debug: Implement acyclic locally to expose ranks
  (define ranks (for/list ([e full-trace]) (define-symbolic* r integer?) r))
  
  (define (get-rank e) (list-ref ranks (index-of full-trace e)))
  
  (define acyclic-constraints
    (for/and ([e1 full-trace] [e2 full-trace])
      (let ([rel (or (ppo full-trace e1 e2)
                     (rf-rel e1 e2)
                     (co-rel e1 e2)
                     (fr full-trace rf-rel co-rel e1 e2))])
        (implies rel (< (get-rank e1) (get-rank e2))))))
        
  (define model-constraints (and (well-formed-rf full-trace rf-rel)
                                 (well-formed-co full-trace co-rel)
                                 acyclic-constraints))

  (define violation
    (and (equal? r_tail_val 1)
         (not (equal? r_data_val 1))))

  (define sol (solve (assert (and rf-constraints co-constraints model-constraints violation))))
  
  (if (unsat? sol)
      sol
      (begin
        (printf "Model found. Ranks:\n")
        (for ([e full-trace] [r ranks])
          (printf "Event ~a (Type ~a): Rank ~a\n" (event-id e) (event-type e) (evaluate r sol)))
        (printf "r_tail_val: ~a\n" (evaluate r_tail_val sol))
        (printf "r_data_val: ~a\n" (evaluate r_data_val sol))
        sol)))

(printf "Verifying P1 (Correct)...\n")
(define sol-p1 (verify-ring-buffer make-trace-p1))
(if (unsat? sol-p1)
    (printf "P1 Verified! No violation found.\n")
    (begin
      (printf "P1 Failed! Counterexample found.\n")
      (print sol-p1)
      ;; Print ranks if available in model?
      ;; Ranks are symbolic variables created inside verify-ring-buffer.
      ;; They are not returned.
      ;; To see them, I should return them or print them inside solve?
      ;; But solve returns a model.
      ;; I can evaluate ranks against the model.
      ))

(printf "\nVerifying P2 (Incorrect)...\n")
(define sol-p2 (verify-ring-buffer make-trace-p2))
(if (unsat? sol-p2)
    (printf "P2 Verified? (Unexpected)\n")
    (begin
      (printf "P2 Failed! Counterexample found (Expected).\n")
      ;; We could print the trace here
      ))
