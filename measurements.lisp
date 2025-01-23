(in-package #:org.shirakumo.machine-state.measurements)

(define-measurement (storage-io rate-measurement) ((device T))
  (nth-value 0 (machine-state:storage-io-bytes device)))

(define-measurement (storage-read rate-measurement) ((device T))
  (nth-value 1 (machine-state:storage-io-bytes device)))

(define-measurement (storage-write rate-measurement) ((device T))
  (nth-value 2 (machine-state:storage-io-bytes device)))

(define-measurement storage-% ((device T))
  (multiple-value-bind (free total) (machine-state:storage-room device)
    (float (* 100 (/ (- total free) total)) 0d0)))

(define-measurement storage-free ((device T))
  (nth-value 0 (machine-state:storage-room device)))

(define-measurement storage-used ((device T))
  (multiple-value-bind (free total) (machine-state:storage-room device)
    (- total free)))

(define-measurement storage-total ((device T))
  (nth-value 1 (machine-state:storage-room device)))

(define-measurement (network-io rate-measurement) ((device T))
  (nth-value 0 (machine-state:network-io-bytes device)))

(define-measurement (network-read rate-measurement) ((device T))
  (nth-value 1 (machine-state:network-io-bytes device)))

(define-measurement (network-write rate-measurement) ((device T))
  (nth-value 2 (machine-state:network-io-bytes device)))

(define-measurement memory-% ()
  (multiple-value-bind (used total) (machine-state:machine-room)
    (float (* 100 (/ used total)) 0d0)))

(define-measurement memory-free ()
  (multiple-value-bind (used total) (machine-state:machine-room)
    (- total used)))

(define-measurement memory-used ()
  (nth-value 0 (machine-state:machine-room)))

(define-measurement memory-total ()
  (nth-value 1 (machine-state:machine-room)))

(define-measurement uptime ()
  (machine-state:machine-uptime))

(define-measurement (cpu-% diff-measurement) ((core T))
  (multiple-value-bind (idle total) (machine-state:machine-time core)
    (float (* 100 (/ (- total idle) total)) 0d0)))

(define-measurement (cpu-idle diff-measurement) ((core T))
  (nth-value 0 (machine-state:machine-time core)))

(define-measurement (cpu-busy diff-measurement) ((core T))
  (multiple-value-bind (idle total) (machine-state:machine-time core)
    (- total idle)))

(define-measurement heap-% ()
  (multiple-value-bind (free total) (machine-state:gc-room)
    (float (* 100 (/ (- total free) total)) 0d0)))

(define-measurement heap-free ()
  (nth-value 0 (machine-state:gc-room)))

(define-measurement heap-used ()
  (multiple-value-bind (free total) (machine-state:gc-room)
    (- total free)))

(define-measurement heap-total ()
  (nth-value 1 (machine-state:gc-room)))

(define-measurement (process-busy rate-measurement) ()
  (machine-state:process-time))

(define-measurement process-size ()
  (machine-state:process-room))

(define-measurement (process-io rate-measurement) ()
  (nth-value 0 (machine-state:process-io-bytes)))

(define-measurement (process-read rate-measurement) ()
  (nth-value 1 (machine-state:process-io-bytes)))

(define-measurement (process-write rate-measurement) ()
  (nth-value 2 (machine-state:process-io-bytes)))

(define-measurement (gc-busy rate-measurement) ()
  (machine-state:gc-time))

(define-measurement gpu-% ()
  (multiple-value-bind (free total) (machine-state:gpu-room)
    (float (* 100 (/ (- total free) total)) 0d0)))

(define-measurement gpu-free ()
  (nth-value 0 (machine-state:gpu-room)))

(define-measurement gpu-used ()
  (multiple-value-bind (free total) (machine-state:gpu-room)
    (- total free)))

(define-measurement (gpu-busy rate-measurement) ()
  (machine-state:gpu-time))
