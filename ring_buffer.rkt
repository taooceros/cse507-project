#lang rosette

(require "model.rkt")
(provide (all-defined-out))

;; Addresses
(define DATA 0)
(define TAIL 1)
(define HEAD 2)

;; P1: Correct Synchronization
(define (make-trace-p1 r_tail_val r_data_val)
  (list
   ;; Thread 1: Producer (RDMA Writes)
   (mk-rdma-write 1 1 DATA 1)      ;; Write Data = 1
   (mk-fence 2 1)             ;; Fence
   (mk-rdma-write 3 1 TAIL 1)      ;; Write Tail = 1

   ;; Thread 2: Consumer (Local Reads)
   (mk-read 4 2 TAIL r_tail_val)       ;; Read Tail
   (mk-fence 5 2)             ;; Fence
   (mk-read 6 2 DATA r_data_val)       ;; Read Data
   (mk-fence 7 2)             ;; Fence
   (mk-write 8 2 HEAD 1)      ;; Write Head = 1
   ))

;; P2: Incorrect Synchronization (Missing Fences)
(define (make-trace-p2 r_tail_val r_data_val)
  (list
   ;; Thread 1: Producer (RDMA Writes)
   (mk-rdma-write 1 1 DATA 1)      ;; Write Data = 1
   ;; Missing Fence
   (mk-rdma-write 2 1 TAIL 1)      ;; Write Tail = 1

   ;; Thread 2: Consumer (Local Reads)
   (mk-read 3 2 TAIL r_tail_val)       ;; Read Tail
   ;; Missing Fence
   (mk-read 4 2 DATA r_data_val)       ;; Read Data
   (mk-write 5 2 HEAD 1)      ;; Write Head = 1
   ))
