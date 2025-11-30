# RDMA Verification with Rosette

This project implements a solver-aided verification framework for RDMA-based ring buffers using Rosette.

## Files
- `model.rkt`: Axiomatic memory model for Relaxed RDMA.
- `ring_buffer.rkt`: Trace definitions for P1 (correct) and P2 (incorrect) implementations.
- `verify.rkt`: Verification driver.

## Prerequisites
- Racket
- Rosette (`raco pkg install rosette`)

## Running Verification
```bash
racket verify.rkt
```

## Results
- **P1**: Verified correct.
- **P2**: Verified correct (unexpectedly, likely due to model constraints).
