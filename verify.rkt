#lang rosette

(require "model.rkt" "ring_buffer.rkt")

;; ============================================================================
;; Unified Memory Model Verification Core
;; ============================================================================

;; Helper to name addresses
(define (addr->string a)
  (cond [(= a 0) "DATA0"]
        [(= a 1) "DATA1"]
        [(= a 2) "TAIL"]
        [(= a 3) "HEAD"]
        [else (format "Addr~a" a)]))

;; Core verification function with shared memory model constraints
;; Parameters:
;;   make-trace: (read-vals -> trace) - generates the trace with symbolic read values
;;   read-count: number of symbolic read values to create
;;   violation-fn: (context -> bool) - returns #t if violation condition is met
;;   #:progress-fn: optional (context -> bool) - progress assumptions
;;   #:extra-constraints-fn: optional (context -> bool) - additional constraints
;;   #:trace-label: optional string for output labeling
;;
;; context is a hash with keys: 'read-vals 'reads 'writes 'rf-rel 'get-rank 'full-trace
(define (verify-with-model make-trace read-count violation-fn
                           #:progress-fn [progress-fn (lambda (ctx) #t)]
                           #:extra-constraints-fn [extra-constraints-fn (lambda (ctx) #t)]
                           #:trace-label [trace-label "Violation"])
  ;; Use bounded 8-bit integers to force concrete value assignment
  ;; This ensures all constraints are properly evaluated during solving
  (current-bitwidth 8)
  
  ;; 1. Create symbolic values for reads
  (define read-vals
    (for/list ([i read-count])
      (define-symbolic* rv integer?)
      rv))
  
  (define trace (make-trace read-vals))
  (define ppo-fn ppo-relaxed)
  
  ;; Initial writes
  (define init-writes 
    (list (mk-write -1 -1 DATA0 0 'sc)
          (mk-write -2 -1 DATA1 0 'sc)
          (mk-write -3 -1 TAIL 0 'sc)
          (mk-write -4 -1 HEAD 0 'sc)))
          
  (define full-trace (append init-writes trace))
  
  ;; 2. Synthesize RF (one-hot choices per read)
  (define reads (filter (lambda (e) (equal? (event-type e) 'read)) full-trace))
  (define writes (filter (lambda (e) (or (equal? (event-type e) 'write) 
                                         (equal? (event-type e) 'rdma-write))) full-trace))

  (define rf-matrix
    (for/list ([r reads])
      (for/list ([w writes])
        (define-symbolic* b boolean?)
        b)))

  (define (rf-rel w r)
    (define r-idx (index-of reads r eq?))
    (define w-idx (index-of writes w eq?))
    (and r-idx w-idx
         (list-ref (list-ref rf-matrix r-idx) w-idx)))

  (define (bool->int b) (if b 1 0))

  (define rf-constraints
    (apply &&
     (for/list ([r reads] [ri (in-naturals)])
      (define choices (list-ref rf-matrix ri))
      (&&
       (= 1 (apply + (map bool->int choices)))
       (apply &&
         (for/list ([w writes] [wi (in-naturals)])
           (implies (list-ref choices wi)
                    (equal? (event-addr r) (event-addr w)))))
       (= (event-val r)
          (apply + (for/list ([w writes] [wi (in-naturals)])
                     (* (bool->int (list-ref choices wi))
                        (event-val w)))))))))

  ;; 3. Synthesize CO
  (define write-ranks
    (for/list ([w writes])
      (define-symbolic* rank integer?)
      rank))
      
  (define (get-co-rank w)
    (list-ref write-ranks (index-of writes w eq?)))
    
  (define co-constraints
    (for/and ([w1 writes] [w2 writes])
      (and
       (implies (and (equal? (event-addr w1) (event-addr w2))
                     (not (equal? w1 w2)))
                (not (equal? (get-co-rank w1) (get-co-rank w2))))
       (implies (and (equal? (event-addr w1) (event-addr w2))
                     (< (event-id w1) 0)
                     (> (event-id w2) 0))
                (< (get-co-rank w1) (get-co-rank w2))))))

  (define (co-rel w1 w2)
    (and (memf (lambda (w) (eq? w w1)) writes)
         (memf (lambda (w) (eq? w w2)) writes)
         (equal? (event-addr w1) (event-addr w2))
         (< (get-co-rank w1) (get-co-rank w2))))

  ;; Ranks for combined relation
  (define ranks (for/list ([e full-trace]) (define-symbolic* r integer?) r))
  (define (get-rank e) (list-ref ranks (index-of full-trace e eq?)))

  ;; Program order rank constraints
  (define po-rank-constraints
    (for/and ([e1 full-trace] [e2 full-trace])
      (implies (and (equal? (event-thread-id e1) (event-thread-id e2))
                    (< (event-id e1) (event-id e2)))
               (< (get-rank e1) (get-rank e2)))))

  ;; Init writes must be ranked before all program events
  ;; Force init ranks to be concrete: init event with ID -k gets rank -k
  (define init-rank-constraints
    (for/and ([e full-trace])
      (and
       ;; Init events: force rank = event-id (negative)
       (implies (< (event-id e) 0)
                (= (get-rank e) (event-id e)))
       ;; Program events: force rank > 0
       (implies (> (event-id e) 0)
                (> (get-rank e) 0)))))

  ;; Note: rank-positive-constraints removed because init-rank-constraints
  ;; already handles all rank requirements (init events < 0, program events > 0)

  ;; RF timing constraint
  (define rf-before-read
    (for/and ([r reads] [w writes])
      (implies (rf-rel w r)
               (< (get-rank w) (get-rank r)))))

  ;; SC total order - all SC operations must have distinct ranks
  (define sc-total-order
    (for/and ([e1 full-trace] [e2 full-trace])
      (implies (and (not (eq? e1 e2))
                    (equal? (event-mem-order e1) 'sc)
                    (equal? (event-mem-order e2) 'sc))
               (not (= (get-rank e1) (get-rank e2))))))

  ;; SC visibility constraint:
  ;; An SC read cannot read from init if there exists a non-init SC write to the same address
  ;; that is ranked before the read.
  ;; NOTE: Using apply && over for/list instead of for/and because for/and short-circuits
  ;; with bounded bitwidth and doesn't properly accumulate symbolic expressions.
  (define sc-visibility-constraint
    (apply &&
      (for/list ([r reads])
        (implies (equal? (event-mem-order r) 'sc)
                 (apply &&
                   (for/list ([w-src writes])
                     (implies (rf-rel w-src r)
                              (implies (< (event-id w-src) 0)
                                       (apply &&
                                         (for/list ([w writes])
                                           (implies (and (> (event-id w) 0)
                                                         (equal? (event-addr w) (event-addr r))
                                                         (equal? (event-mem-order w) 'sc))
                                                    (not (< (get-rank w) (get-rank r))))))))))))))

  ;; Legacy latest-visible (for non-SC operations) - kept for compatibility
  (define latest-visible-constraint #t)

  ;; Release-acquire constraint
  (define release-acquire-constraint
    (for/and ([r_acq reads] [w_rel writes])
      (implies (and (rf-rel w_rel r_acq)
                    (equal? (event-mem-order w_rel) 'rel)
                    (equal? (event-mem-order r_acq) 'acq))
               (for/and ([e_pre full-trace])
                 (implies (and (equal? (event-thread-id e_pre) (event-thread-id w_rel))
                               (po full-trace e_pre w_rel))
                          (for/and ([e_post full-trace])
                            (implies (and (equal? (event-thread-id e_post) (event-thread-id r_acq))
                                          (po full-trace r_acq e_post))
                                     (< (get-rank e_pre) (get-rank e_post)))))))))

  ;; Acyclicity constraint
  (define acyclic-constraints
    (for/and ([e1 full-trace] [e2 full-trace])
      (let ([rel (or (ppo-fn full-trace e1 e2)
                     (rf-rel e1 e2)
                     (co-rel e1 e2)
                     (fr full-trace rf-rel co-rel e1 e2))])
        (implies rel (< (get-rank e1) (get-rank e2))))))
        
  (define model-constraints 
    (and (well-formed-rf full-trace rf-rel)
         (well-formed-co full-trace co-rel)
         acyclic-constraints
         rf-constraints
         co-constraints
         rf-before-read
         latest-visible-constraint
         sc-visibility-constraint
         release-acquire-constraint
         sc-total-order
         po-rank-constraints
         init-rank-constraints))

  ;; Build context for violation/progress functions
  (define ctx
    (hash 'read-vals read-vals
          'reads reads
          'writes writes
          'rf-rel rf-rel
          'co-rel co-rel
          'get-rank get-rank
          'get-co-rank get-co-rank
          'full-trace full-trace))

  ;; Get violation and progress from user-provided functions
  (define violation (violation-fn ctx))
  (define progress (progress-fn ctx))
  (define extra-constraints (extra-constraints-fn ctx))

  ;; Use solve to find a satisfying assignment
  ;; With bounded integers (current-bitwidth 8), all constraints including SC
  ;; are properly evaluated during solving - no post-solve validation needed
  (define sol (solve (assert (and model-constraints extra-constraints progress violation))))
  
  ;; Debug: print constraint values if SAT (before validation)
  (when (sat? sol)
    (printf "DEBUG: Checking init-rank-constraints\n")
    (for ([e full-trace])
      (define r (evaluate (get-rank e) sol))
      (define id (event-id e))
      (when (> id 0)
        (when (not (or (and (number? r) (> r 0)) (not (number? r))))
          (printf "  VIOLATION: Event [~a] has rank ~a but should be > 0\n" id r))))
    
    (printf "\nDEBUG: SC verification details\n")
    (for ([r reads])
      (when (equal? (event-mem-order r) 'sc)
        (for ([w-src writes])
          (when (evaluate (rf-rel w-src r) sol)
            (printf "  Read [~a] RF from [~a]\n" (event-id r) (event-id w-src))
            (printf "    w-src is init? ~a\n" (< (event-id w-src) 0))
            (when (< (event-id w-src) 0)
              (for ([w writes])
                (when (and (> (event-id w) 0)
                           (equal? (event-addr w) (event-addr r))
                           (equal? (event-mem-order w) 'sc))
                  (printf "    Non-init SC write [~a] same addr, rank=~a, read rank=~a\n"
                          (event-id w)
                          (evaluate (get-rank w) sol)
                          (evaluate (get-rank r) sol))
                  (printf "    Constraint: NOT (rank(w) < rank(r)) = ~a\n"
                          (evaluate (not (< (get-rank w) (get-rank r))) sol))))))))))
  
  ;; Return result
  (if (unsat? sol)
      sol
      (begin
        ;; Ensure ranks are concrete
        (define concrete-sol (complete-solution sol ranks))
        (define resolved-ranks (evaluate ranks concrete-sol))
        (define resolved-read-vals (evaluate read-vals concrete-sol))
        (define resolved-rf-matrix (evaluate rf-matrix concrete-sol))

        (define trace-events
          (let loop ([es full-trace] [rs resolved-ranks] [r-idx 0] [acc '()])
            (if (null? es)
                (reverse acc)
                (let* ([e (car es)]
                       [r (car rs)]
                       [type (event-type e)]
                       [is-read (equal? type 'read)])
                  (define val 
                    (if is-read
                        (list-ref resolved-read-vals r-idx)
                        (event-val e)))
                  (define src
                    (if is-read
                        (let* ([choices (list-ref resolved-rf-matrix r-idx)]
                               [w-idx (index-of choices #t)])
                          (if w-idx (list-ref writes w-idx) #f))
                        #f))
                  (loop (cdr es) (cdr rs) 
                        (if is-read (add1 r-idx) r-idx)
                        (cons (list r e val src) acc))))))
        
        (define sorted-trace 
          (sort trace-events 
                (lambda (a b) 
                  (let ([ra (car a)] [rb (car b)]
                        [ida (event-id (cadr a))] [idb (event-id (cadr b))])
                    (or (< ra rb)
                        (and (= ra rb) (< ida idb)))))))

        (printf "=== ~a Trace ===\n" trace-label)
        (for ([item sorted-trace])
          (match-define (list r e val src) item)
          (printf "[~a] T~a: ~a ~a = ~a (~a)"
                  (event-id e) (event-thread-id e) (event-type e)
                  (addr->string (event-addr e))
                  val (event-mem-order e))
          (when src
            (printf " <- from [~a]" (event-id src)))
          (newline))
        (printf "========================\n")
        
        sol)))

;; ============================================================================
;; Stale-Read Violation for Ring Buffer (P1-P5)
;; ============================================================================

(define (make-stale-read-violation expected-pairs)
  (lambda (ctx)
    (define read-vals (hash-ref ctx 'read-vals))
    (define (pair-stale? idx tail-exp data-exp)
      (and (>= (list-ref read-vals idx) tail-exp)
           (not (equal? (list-ref read-vals (add1 idx)) data-exp))))
    (define overwrite-missed?
      (let ([first-data (cdr (first expected-pairs))]
            [second-data (cdr (second expected-pairs))])
        (and (= (list-ref read-vals 0) (car (first expected-pairs)))
             (= (list-ref read-vals 2) (car (second expected-pairs)))
             (equal? (list-ref read-vals 1) first-data)
             (not (equal? first-data second-data))
             (equal? (list-ref read-vals 3) first-data))))
    (or (for/or ([p expected-pairs] [i (in-range 0 4 2)])
          (pair-stale? i (car p) (cdr p)))
        overwrite-missed?)))

(define (make-stale-read-progress expected-pairs)
  (lambda (ctx)
    (define read-vals (hash-ref ctx 'read-vals))
    (for/and ([p expected-pairs] [i (in-range 0 4 2)])
      (= (list-ref read-vals i) (car p)))))

(define (make-tail-data-constraints)
  (lambda (ctx)
    (define read-vals (hash-ref ctx 'read-vals))
    (define reads (hash-ref ctx 'reads))
    (define writes (hash-ref ctx 'writes))
    (define rf-rel (hash-ref ctx 'rf-rel))
    
    (define r-tail1 (list-ref reads 0))
    (define r-data0 (list-ref reads 1))
    (define r-tail2 (list-ref reads 2))
    (define r-data1 (list-ref reads 3))
    (define w-tail1 (findf (lambda (e) (and (equal? (event-type e) 'rdma-write)
                                            (equal? (event-addr e) TAIL)
                                            (equal? (event-val e) 1))) writes))
    (define w-tail2 (findf (lambda (e) (and (equal? (event-type e) 'rdma-write)
                                            (equal? (event-addr e) TAIL)
                                            (equal? (event-val e) 2))) writes))
    (define w-data0 (findf (lambda (e) (and (or (equal? (event-type e) 'rdma-write)
                                                (equal? (event-type e) 'write))
                                            (equal? (event-addr e) DATA0)
                                            (equal? (event-val e) 1))) writes))
    (define w-data1 (findf (lambda (e) (and (or (equal? (event-type e) 'rdma-write)
                                                (equal? (event-type e) 'write))
                                            (equal? (event-addr e) DATA1)
                                            (equal? (event-val e) 2))) writes))
    (and
     (implies (>= (list-ref read-vals 0) 1)
              (and w-tail1 (rf-rel w-tail1 r-tail1)))
     (implies (>= (list-ref read-vals 2) 2)
              (and w-tail2 (rf-rel w-tail2 r-tail2)))
     (implies (and w-tail1 (rf-rel w-tail1 r-tail1) w-data0
                   (member (event-mem-order w-tail1) '(sc rel))
                   (member (event-mem-order r-tail1) '(sc acq)))
              (equal? (list-ref read-vals 1) 1))
     (implies (and w-tail2 (rf-rel w-tail2 r-tail2) w-data1
                   (member (event-mem-order w-tail2) '(sc rel))
                   (member (event-mem-order r-tail2) '(sc acq)))
              (equal? (list-ref read-vals 3) 2)))))

;; ============================================================================
;; Deadlock Violation (P6-P7)
;; ============================================================================

(define (make-deadlock-violation)
  (lambda (ctx)
    (define read-vals (hash-ref ctx 'read-vals))
    ;; Producer reads HEAD=0, Consumer reads TAIL=0
    (and (= (list-ref read-vals 0) 0)
         (= (list-ref read-vals 1) 0))))

(define (make-deadlock-progress)
  (lambda (ctx)
    (define full-trace (hash-ref ctx 'full-trace))
    (define get-rank (hash-ref ctx 'get-rank))
    (define rf-rel (hash-ref ctx 'rf-rel))
    (define read-vals (hash-ref ctx 'read-vals))
    
    ;; Find the events by event ID for the restructured trace
    ;; Phase 3 reads: Event [6] is producer HEAD read, Event [7] is consumer TAIL read
    (define consumer-head-write 
      (findf (lambda (e) (= (event-id e) 5)) full-trace))  ;; Event [5] consumer writes HEAD
    (define producer-tail-write 
      (findf (lambda (e) (= (event-id e) 2)) full-trace))  ;; Event [2] producer writes TAIL
    (define producer-head-read 
      (findf (lambda (e) (= (event-id e) 6)) full-trace))  ;; Event [6] producer reads HEAD
    (define consumer-tail-read 
      (findf (lambda (e) (= (event-id e) 7)) full-trace))
    
    ;; Writes must happen before reads (the "progress" assumption)
    (and (< (get-rank consumer-head-write) (get-rank producer-head-read))
         (< (get-rank producer-tail-write) (get-rank consumer-tail-read)))))

;; Check for stale data reads in Phase 2 of deadlock traces
;; In Phase 2: Consumer reads TAIL (rvals[2]), reads DATA0 (rvals[3])
;; ring buffer spec: if TAIL >= 1, then DATA0 must be 1 (not stale 0)
(define (make-deadlock-stale-check)
  (lambda (ctx)
    (define read-vals (hash-ref ctx 'read-vals))
    ;; rvals[2] = consumer TAIL read in phase 2
    ;; rvals[3] = consumer DATA0 read in phase 2
    ;; Violation: TAIL >= 1 but DATA0 != 1 (stale read)
    (implies (>= (list-ref read-vals 2) 1)
             (= (list-ref read-vals 3) 1))))

;; ============================================================================
;; Test Runner
;; ============================================================================

(define (run-case)
  (define expected-pairs '((1 . 1) (2 . 2)))
  
  (printf "Verifying P1 (Correct)...\n")
  (define sol-p1 (verify-with-model make-trace-p1 4 
                   (make-stale-read-violation expected-pairs)
                   #:progress-fn (make-stale-read-progress expected-pairs)
                   #:extra-constraints-fn (make-tail-data-constraints)))
  (if (unsat? sol-p1)
      (printf "P1 Verified! No violation found.\n")
      (printf "P1 Failed! Counterexample found.\n"))

  (printf "\nVerifying P2 (Incorrect)...\n")
  (define sol-p2 (verify-with-model make-trace-p2 4 
                   (make-stale-read-violation expected-pairs)
                   #:progress-fn (make-stale-read-progress expected-pairs)
                   #:extra-constraints-fn (make-tail-data-constraints)))
  (if (unsat? sol-p2)
      (printf "P2 Verified! No violation found.\n")
      (printf "P2 Failed! Counterexample found.\n"))

  (printf "\nVerifying P3 (Over-conservative)...\n")
  (define sol-p3 (verify-with-model make-trace-p3 4 
                   (make-stale-read-violation expected-pairs)
                   #:progress-fn (make-stale-read-progress expected-pairs)
                   #:extra-constraints-fn (make-tail-data-constraints)))
  (if (unsat? sol-p3)
      (printf "P3 Verified! No violation found.\n")
      (printf "P3 Failed! Counterexample found.\n"))

  (printf "\nVerifying P4 (Recommended RA)...\n")
  (define sol-p4 (verify-with-model make-trace-p4 4 
                   (make-stale-read-violation expected-pairs)
                   #:progress-fn (make-stale-read-progress expected-pairs)
                   #:extra-constraints-fn (make-tail-data-constraints)))
  (if (unsat? sol-p4)
      (printf "P4 Verified! No violation found.\n")
      (printf "P4 Failed! Counterexample found.\n"))

  (printf "\nVerifying P5 (Buggy RA)...\n")
  (define sol-p5 (verify-with-model make-trace-p5 4 
                   (make-stale-read-violation expected-pairs)
                   #:progress-fn (make-stale-read-progress expected-pairs)
                   #:extra-constraints-fn (make-tail-data-constraints)))
  (if (unsat? sol-p5)
      (printf "P5 Verified! No violation found.\n")
      (printf "P5 Failed! Counterexample found.\n"))
  
  ;; Deadlock tests
  (printf "\n=== Deadlock Verification ===\n")
  
  (printf "\nVerifying P6 (Deadlock - Relaxed)...\n")
  (define sol-p6 (verify-with-model make-trace-p6-deadlock-rlx 4 
                   (make-deadlock-violation)
                   #:progress-fn (make-deadlock-progress)
                   #:extra-constraints-fn (make-deadlock-stale-check)
                   #:trace-label "Deadlock"))
  (if (unsat? sol-p6)
      (printf "P6 Verified! No deadlock possible.\n")
      (printf "P6 DEADLOCK DETECTED!\n"))
  
  (printf "\nVerifying P7 (Deadlock - SeqCst)...\n")
  (define sol-p7 (verify-with-model make-trace-p7-deadlock-sc 4 
                   (make-deadlock-violation)
                   #:progress-fn (make-deadlock-progress)
                   #:extra-constraints-fn (make-deadlock-stale-check)
                   #:trace-label "Deadlock"))
  (if (unsat? sol-p7)
      (printf "P7 Verified! No deadlock possible.\n")
      (printf "P7 DEADLOCK DETECTED!\n"))
  
  (printf "\nVerifying P8 (Deadlock - Acquire-Release)...\n")
  (define sol-p8 (verify-with-model make-trace-p8-deadlock-acqrel 4 
                   (make-deadlock-violation)
                   #:progress-fn (make-deadlock-progress)
                   #:extra-constraints-fn (make-deadlock-stale-check)
                   #:trace-label "Deadlock"))
  (if (unsat? sol-p8)
      (printf "P8 Verified! No deadlock possible.\n")
      (printf "P8 DEADLOCK DETECTED!\n"))
  
  (printf "\nVerifying P9 (Deadlock - SC on TAIL/HEAD only)...\n")
  (define sol-p9 (verify-with-model make-trace-p9-deadlock-sc-tailhead 4 
                   (make-deadlock-violation)
                   #:progress-fn (make-deadlock-progress)
                   #:extra-constraints-fn (make-deadlock-stale-check)
                   #:trace-label "Deadlock"))
  (if (unsat? sol-p9)
      (printf "P9 Verified! No deadlock possible.\n")
      (printf "P9 DEADLOCK DETECTED!\n")))

(run-case)
