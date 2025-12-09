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

;; P1: Correct Synchronization without QP/flush (ordered locally)
;; rvals: [tail0 data0 tail1 data1]
(define (make-trace-p1 rvals)
  (list
   ;; Producer publishes slot 0 and slot 1
   (mk-rdma-write 1 1 DATA0 1 'sc)
   (mk-rdma-write 2 1 TAIL 1 'sc)
   (mk-rdma-write 3 1 DATA1 2 'sc)
   (mk-rdma-write 4 1 TAIL 2 'sc)

   ;; Consumer round 1
   (mk-read 5 2 TAIL (rv rvals 0) 'sc)
   (mk-read 6 2 DATA0 (rv rvals 1) 'sc)
   (mk-write 7 2 HEAD 1 'sc)

   ;; Consumer round 2 (wrap head back to 0)
   (mk-read 8 2 TAIL (rv rvals 2) 'sc)
   (mk-read 9 2 DATA1 (rv rvals 3) 'sc)
   (mk-write 10 2 HEAD 0 'sc)))

;; P2: Missing ordering; tail may expose before data visible
(define (make-trace-p2 rvals)
  (list
   ;; Producer: tail updates issued before data is guaranteed visible
   (mk-rdma-write 1 1 DATA0 1 'rlx)
   (mk-rdma-write 2 1 TAIL 1 'rlx)
   (mk-rdma-write 3 1 DATA1 2 'rlx)
   (mk-rdma-write 4 1 TAIL 2 'rlx)

   ;; Consumer
   (mk-read 5 2 TAIL (rv rvals 0) 'rlx)
   (mk-read 6 2 DATA0 (rv rvals 1) 'rlx)
   (mk-write 7 2 HEAD 1 'rlx)
   (mk-read 8 2 TAIL (rv rvals 2) 'rlx)
   (mk-read 9 2 DATA1 (rv rvals 3) 'rlx)
   (mk-write 10 2 HEAD 0 'rlx)))

;; P3: Overly strong RA (all writes release, reads acquire)
(define (make-trace-p3 rvals)
  (list
   ;; Producer: every write uses release (over-conservative)
   (mk-rdma-write 1 1 DATA0 1 'rel)
   (mk-rdma-write 2 1 TAIL 1 'rel)
   (mk-rdma-write 3 1 DATA1 2 'rel)
   (mk-rdma-write 4 1 TAIL 2 'rel)

   ;; Consumer: all reads acquire
   (mk-read 5 2 TAIL (rv rvals 0) 'acq)
   (mk-read 6 2 DATA0 (rv rvals 1) 'acq)
   (mk-write 7 2 HEAD 1 'rlx)
   (mk-read 8 2 TAIL (rv rvals 2) 'acq)
   (mk-read 9 2 DATA1 (rv rvals 3) 'acq)
   (mk-write 10 2 HEAD 0 'rlx)))

;; P4: Recommended RA usage (data relaxed, tail release, tail reads acquire)
(define (make-trace-p4 rvals)
  (list
   ;; Producer: data relaxed, tail release
   (mk-rdma-write 1 1 DATA0 1 'rlx)
   (mk-rdma-write 2 1 TAIL 1 'rel)
   (mk-rdma-write 3 1 DATA1 2 'rlx)
   (mk-rdma-write 4 1 TAIL 2 'rel)

   ;; Consumer: tail acquire, data relaxed
   (mk-read 5 2 TAIL (rv rvals 0) 'acq)
   (mk-read 6 2 DATA0 (rv rvals 1) 'rlx)
   (mk-write 7 2 HEAD 1 'rlx)
   (mk-read 8 2 TAIL (rv rvals 2) 'acq)
   (mk-read 9 2 DATA1 (rv rvals 3) 'rlx)
   (mk-write 10 2 HEAD 0 'rlx)))

;; P5: Misused RA (first tail publish missing release; stale first slot)
(define (make-trace-p5 rvals)
  (list
   ;; Producer: first tail mistakenly relaxed (no release), second tail uses release
   (mk-rdma-write 1 1 DATA0 1 'rlx)
   (mk-rdma-write 2 1 TAIL 1 'rlx) ;; missing release here
   (mk-rdma-write 3 1 DATA1 2 'rlx)
   (mk-rdma-write 4 1 TAIL 2 'rel)

   ;; Consumer: tail reads acquire both times
   (mk-read 5 2 TAIL (rv rvals 0) 'acq)
   (mk-read 6 2 DATA0 (rv rvals 1) 'rlx)
   (mk-write 7 2 HEAD 1 'rlx)
   (mk-read 8 2 TAIL (rv rvals 2) 'acq)
   (mk-read 9 2 DATA1 (rv rvals 3) 'rlx)
   (mk-write 10 2 HEAD 0 'rlx)))

;; Deadlock scenario: Both threads complete their respective writes, then both
;; try to read and see stale values (both would sleep indefinitely).
;;
;; Scenario timeline:
;; 1. Producer writes DATA0=1, writes TAIL=1 (completing a produce cycle)
;; 2. Consumer reads TAIL=1, reads DATA0=1, writes HEAD=1 (completing a consume cycle)
;; 3. NOW both check again:
;;    - Producer reads HEAD (hoping to see 1 from consumer's write)
;;    - Consumer reads TAIL (hoping to see 2 from producer's next write, but producer hasn't written)
;;
;; For deadlock: Producer sees HEAD=0 (stale), Consumer sees TAIL=0 (stale, before producer's write)
;; This is only possible with weak memory ordering.

;; P6: Deadlock scenario (Relaxed) - SHOULD deadlock
(define (make-trace-p6-deadlock-rlx rvals)
  (list
   ;; Phase 1: Producer writes data and updates tail
   (mk-rdma-write 1 1 DATA0 1 'rlx)
   (mk-rdma-write 2 1 TAIL 1 'rlx)

   ;; Phase 2: Consumer consumes and writes head
   (mk-read 3 2 TAIL (rv rvals 2) 'rlx)  ;; Consumer checks TAIL (should be 1)
   (mk-read 4 2 DATA0 (rv rvals 3) 'rlx) ;; Consumer reads DATA0
   (mk-write 5 2 HEAD 1 'rlx)            ;; Consumer writes HEAD=1

   ;; Phase 3: Both check again for next round - this is where deadlock can occur
   (mk-read 6 1 HEAD (rv rvals 0) 'rlx)  ;; Producer reads HEAD
   (mk-read 7 2 TAIL (rv rvals 1) 'rlx))) ;; Consumer reads TAIL

;; P7: Deadlock scenario (SeqCst) - Should NOT deadlock
(define (make-trace-p7-deadlock-sc rvals)
  (list
   ;; Phase 1: Producer writes data and updates tail
   (mk-rdma-write 1 1 DATA0 1 'sc)
   (mk-rdma-write 2 1 TAIL 1 'sc)

   ;; Phase 2: Consumer consumes and writes head
   (mk-read 3 2 TAIL (rv rvals 2) 'sc)  ;; Consumer checks TAIL
   (mk-read 4 2 DATA0 (rv rvals 3) 'sc) ;; Consumer reads DATA0
   (mk-write 5 2 HEAD 1 'sc)            ;; Consumer writes HEAD=1

   ;; Phase 3: Both check again - SC should prevent observing stale values
   (mk-read 6 1 HEAD (rv rvals 0) 'sc)  ;; Producer reads HEAD
   (mk-read 7 2 TAIL (rv rvals 1) 'sc))) ;; Consumer reads TAIL

;; P8: Deadlock scenario (Acquire-Release) - SHOULD deadlock
;; Acquire-release ordering provides synchronization between release stores
;; and acquire loads on the SAME address, but NOT across different addresses.
;; Producer's release on TAIL doesn't synchronize with Consumer's acquire on HEAD.
(define (make-trace-p8-deadlock-acqrel rvals)
  (list
   ;; Phase 1: Producer writes data (relaxed) and updates tail (release)
   (mk-rdma-write 1 1 DATA0 1 'rlx)
   (mk-rdma-write 2 1 TAIL 1 'rel)     ;; Release semantics

   ;; Phase 2: Consumer reads tail (acquire), reads data, writes head (release)
   (mk-read 3 2 TAIL (rv rvals 2) 'acq)  ;; Acquire semantics
   (mk-read 4 2 DATA0 (rv rvals 3) 'rlx)
   (mk-write 5 2 HEAD 1 'rel)            ;; Release semantics

   ;; Phase 3: Both check again - acq-rel is insufficient to prevent deadlock
   ;; Producer acquires HEAD but there's no synchronization with Consumer's TAIL release
   (mk-read 6 1 HEAD (rv rvals 0) 'acq)  ;; Acquire on HEAD
   (mk-read 7 2 TAIL (rv rvals 1) 'acq)));; Acquire on TAIL

;; P9: Deadlock scenario (SC on TAIL/HEAD only) - Should NOT deadlock
;; Uses SeqCst only on TAIL and HEAD operations, relaxed for DATA.
;; SC on synchronization variables provides total order needed to prevent deadlock.
(define (make-trace-p9-deadlock-sc-tailhead rvals)
  (list
   ;; Phase 1: Producer writes data (relaxed) and updates tail (SC)
   (mk-rdma-write 1 1 DATA0 1 'rlx)     ;; Relaxed for DATA
   (mk-rdma-write 2 1 TAIL 1 'sc)       ;; SC for TAIL

   ;; Phase 2: Consumer reads tail (SC), reads data (relaxed), writes head (SC)
   (mk-read 3 2 TAIL (rv rvals 2) 'sc)  ;; SC for TAIL
   (mk-read 4 2 DATA0 (rv rvals 3) 'rlx);; Relaxed for DATA
   (mk-write 5 2 HEAD 1 'sc)            ;; SC for HEAD

   ;; Phase 3: Both check again - SC on TAIL/HEAD should prevent deadlock
   (mk-read 6 1 HEAD (rv rvals 0) 'sc)  ;; SC for HEAD
   (mk-read 7 2 TAIL (rv rvals 1) 'sc)));; SC for TAIL

