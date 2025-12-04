#lang rosette
(require racket/list racket/string racket/format)
(require "model.rkt" "ring_buffer.rkt")

;; Verification driver wiring together the memory-model predicates with
;; the ring-buffer traces. Solves for candidate executions and reports
;; whether a stale consumer read is possible under various modes.

(provide analyze-scenario
         scenario->report
         check-p1
         check-p2a
         check-p2b)

;; fresh-int: allocate a brand-new symbolic integer for ranking/choice slots.
(define (fresh-int)
  (define-symbolic* tmp integer?)
  tmp)

;; event-by-id: find the unique event with id eid or raise if absent.
(define (event-by-id trace eid)
  (define evt (for/first ([e trace] #:when (= (event-id e) eid)) e))
  (unless evt (error 'event-by-id "missing event ~a" eid))
  evt)

;; event->summary: project an event to a readable association list.
(define (event->summary e)
  (list
   (list 'id (event-id e))
   (list 'thread (event-thread-id e))
   (list 'type (event-type e))
   (list 'addr (event-addr e))
   (list 'val (event-val e))
   (list 'mode (event-mode e))))

(define (format-trace trace)
  (define sorted (sort trace < #:key event-id))
  (string-join
   (for/list ([e sorted])
     (format "  [~a] Thread ~a: ~a ~a (Val: ~a, Mode: ~a)"
             (~r (event-id e) #:min-width 3)
             (event-thread-id e)
             (~a (event-type e) #:width 5)
             (~a (event-addr e) #:width 5)
             (~a (event-val e) #:width 2)
             (event-mode e)))
   "\n"))

(define (last-before lst pred?)
  ;; last-before: return the most recent list item that satisfies pred?.
  (for/fold ([candidate #f]) ([item lst])
    (if (pred? item) item candidate)))

(define (first-after lst pred?)
  ;; first-after: return the earliest list item that satisfies pred?.
  (for/first ([item lst] #:when (pred? item)) item))

(define (build-rf-function trace writes reads)
  ;; build-rf-function: synthesize a symbolic reads-from relation over trace.
  (define choices (for/list ([r reads]) (fresh-int)))
  (define write-count (length writes))
  (for ([choice choices])
    (assert (<= 0 choice))
    (assert (< choice write-count)))
  (values
   (lambda (w r)
     (for/or ([read reads]
              [choice choices])
       (and (equal? r read)
            (equal? w (list-ref writes choice)))))
   choices))

(define (build-co-function trace writes)
  ;; build-co-function: assign symbolic total orders to writes per location.
  (define ranks (for/list ([w writes]) (fresh-int)))
  (for* ([i (in-range (length writes))]
         [j (in-range (length writes))]
         #:when (< i j)
         #:when (equal? (event-addr (list-ref writes i))
                         (event-addr (list-ref writes j))))
    (assert (not (= (list-ref ranks i) (list-ref ranks j)))))
  (values
   (lambda (w1 w2)
     (define i (index-of writes w1))
     (define j (index-of writes w2))
     (and i j
          (equal? (event-addr w1) (event-addr w2))
          (< (list-ref ranks i) (list-ref ranks j))))
   ranks))

(define (build-rank-function trace relation-suite)
  ;; build-rank-function: produce ranks proving combined relations acyclic.
  (define ranks (for/list ([e trace]) (fresh-int)))
  (define (get-rank e)
    (list-ref ranks (index-of trace e)))
  (for ([rel relation-suite])
    (define relation (first rel))
    (for* ([e1 trace] [e2 trace])
      (assert (implies (relation e1 e2)
                       (< (get-rank e1) (get-rank e2))))))
  (values ranks get-rank))

;; scenario-mode->settings: select program-order and RA enforcement flags.
(define (scenario-mode->settings mode)
  (case mode
    [(sc) (values ppo-sc #t)]
  [(ra) (values ppo-relaxed #t)]
    [(relaxed) (values ppo-relaxed #f)]
    [else (error 'analyze-scenario "unknown mode ~a" mode)]))

;; analyze-scenario: build constraints for a given trace template and mode.
(define (analyze-scenario trace-builder mode)
  ;; Instantiate a trace, synthesize rf/co, enforce model constraints,
  ;; and ask Rosette for a stale-read witness under the requested mode.
  (define-values (ppo enforce-ra?) (scenario-mode->settings mode))
  (define-symbolic rv0 rv1 rv2 rv3 integer?)
  (define rvals (list rv0 rv1 rv2 rv3))
  (define trace (trace-builder rvals))
  (define writes (filter is-write? trace))
  (define reads (get-reads trace))
  (define releases (filter (lambda (e) (and (is-write? e) (mode-rel? e))) trace))
  (define acquires (filter (lambda (e) (and (is-read? e) (mode-acq? e))) trace))

  (define-values (rf rf-choices) (build-rf-function trace writes reads))
  (define-values (co co-ranks) (build-co-function trace writes))

  (assert (well-formed-rf trace rf))
  (assert (well-formed-co trace co))

  (define relation-suite
    (list
     (list (lambda (x y) (ppo trace x y)) 'ppo)
     (list (lambda (x y) (rf x y)) 'rf)
     (list (lambda (x y) (co x y)) 'co)
     (list (lambda (x y) (fr trace rf co x y)) 'fr)))

  (define-values (event-ranks get-rank)
    (build-rank-function trace relation-suite))

  (when enforce-ra?
    ;; Force ACQ reads to observe paired REL writes and propagate data.
    (assert (release-acquire-visibility trace rf get-rank))
    (when (and (pair? releases) (pair? acquires))
      (for ([r acquires])
        (define w-sync
          (last-before releases
                       (lambda (w) (< (event-id w) (event-id r)))))
        (when w-sync
          (assert (rf w-sync r))
          (define next-acquire
            (first-after acquires
                         (lambda (a) (> (event-id a) (event-id r)))))
          (define data-reads
            (filter (lambda (e)
                      (and (is-read? e)
                           (= (event-thread-id e) (event-thread-id r))
                           (member (event-addr e) (list DATA0 DATA1))
                           (> (event-id e) (event-id r))
                           (or (not next-acquire)
                               (< (event-id e) (event-id next-acquire)))))
                    trace))
          (for ([dr data-reads])
            (define producer-source
              (last-before writes
                           (lambda (w)
                             (and (= (event-thread-id w) 1)
                                  (= (event-addr w) (event-addr dr))
                                  (< (event-id w) (event-id w-sync))))))
            (when producer-source
              (assert (rf producer-source dr))))))))

  ;; Stale read manifests if consumer reads pull from the initial slot writes.
  (define d0-init (event-by-id trace -4))
  (define d1-init (event-by-id trace -3))
  (define t0-read (event-by-id trace 5))
  (define d0-read (event-by-id trace 6))
  (define t1-read (event-by-id trace 8))
  (define d1-read (event-by-id trace 9))
  (define stale?
    (or (and (>= (event-val t0-read) 1) (rf d0-init d0-read))
        (and (>= (event-val t1-read) 2) (rf d1-init d1-read))))
  (assert stale?)

  (define result (solve #t))
  (define sat-model? (sat? result))
  (define status (if sat-model? 'sat 'unsat))

  ;; eval*: materialize symbolic value v under the solved model.
  (define (eval* v)
    (evaluate v result))

  (define data-summary
    (if sat-model?
        (list
         (list 'tail1 (eval* rv0))
         (list 'data0 (eval* rv1))
         (list 'tail2 (eval* rv2))
         (list 'data1 (eval* rv3)))
        '()))

  (define rf-summary
    (if sat-model?
        (for/list ([r reads]
                   [choice rf-choices])
          (list (event-id r)
                (event-id (list-ref writes (eval* choice)))) )
        '()))

  (define co-summary
    (if sat-model?
        (for/list ([w writes]
                   [rank co-ranks])
          (list (event-id w) (eval* rank)))
        '()))

  (define evaluated-trace
    (if sat-model?
        (evaluate trace result)
        '()))

  (define counterexample
    (if sat-model?
        (for/list ([e evaluated-trace]) (event->summary e))
        '()))

  (hash 'status status
        'mode mode
        'stale stale?
        'trace trace
        'read-values data-summary
        'rf rf-summary
        'co co-summary
        'solution result
        'ranks (if sat-model?
                   (for/list ([e trace]
                              [rank event-ranks])
                     (list (event-id e) (eval* rank)))
                   '())
        'counterexample counterexample
        'model-trace evaluated-trace))

;; scenario->report: turn a scenario hash into a human-readable summary.
(define (scenario->report scenario)
  (define status (hash-ref scenario 'status))
  (define mode (hash-ref scenario 'mode))
  (cond
    [(eq? status 'unsat)
     (format "Mode ~a: UNSAT (Verified - no stale reads)." mode)]
    [else
     (define trace (hash-ref scenario 'model-trace))
     (format "Mode ~a: SAT (Stale read detected!)\nTrace:\n~a"
             mode
             (format-trace trace))]))

;; check-p1: analyze the fully synchronized producer/consumer schedule.
(define (check-p1 mode)
  (analyze-scenario make-trace-p1 mode))

;; check-p2a: analyze the trace missing producer release semantics.
(define (check-p2a mode)
  (analyze-scenario make-trace-p2a mode))

;; check-p2b: analyze the trace missing consumer acquire semantics.
(define (check-p2b mode)
  (analyze-scenario make-trace-p2b mode))

(define (check-p3 mode)
  (analyze-scenario make-trace-p3 mode))


(displayln (scenario->report (check-p2b 'sc)))
; (displayln (scenario->report (check-p3 'sc)))

