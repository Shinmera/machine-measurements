(in-package #:org.shirakumo.machine-state.measurements)

;; protocol.lisp
(docs:define-docs
  (type measurement
    "

See MEASURE
See LAST-TIME
See LAST-VALUE")

 (function measure
   "

See MEASUREMENT")

 (function last-time
   "

See MEASUREMENT")

 (function last-value
   "

See MEASUREMENT"))

;; measurements.lisp
(docs:define-docs
  (type storage-io
    "

See MEASUREMENT")

  (type storage-read
    "

See MEASUREMENT")

  (type storage-write
    "

See MEASUREMENT")

  (type storage-%
    "

See MEASUREMENT")

  (type storage-free
    "

See MEASUREMENT")

  (type storage-used
    "

See MEASUREMENT")

  (type storage-total
    "

See MEASUREMENT")

  (type network-io
    "

See MEASUREMENT")

  (type network-read
    "

See MEASUREMENT")

  (type network-write
    "

See MEASUREMENT")

  (type memory-%
    "

See MEASUREMENT")

  (type memory-free
    "

See MEASUREMENT")

  (type memory-used
    "

See MEASUREMENT")

  (type memory-total
    "

See MEASUREMENT")

  (type uptime
    "

See MEASUREMENT")

  (type cpu-%
    "

See MEASUREMENT")

  (type cpu-idle
    "

See MEASUREMENT")

  (type cpu-busy
    "

See MEASUREMENT")

  (type heap-%
    "

See MEASUREMENT")

  (type heap-free
    "

See MEASUREMENT")

  (type heap-used
    "

See MEASUREMENT")

  (type heap-total
    "

See MEASUREMENT")

  (type process-busy
    "

See MEASUREMENT")

  (type process-size
    "

See MEASUREMENT")

  (type process-io
    "

See MEASUREMENT")

  (type process-read
    "

See MEASUREMENT")

  (type process-write
    "

See MEASUREMENT")

  (type gc-busy
    "

See MEASUREMENT")

  (type gpu-%
    "

See MEASUREMENT")

  (type gpu-free
    "

See MEASUREMENT")

  (type gpu-used
    "

See MEASUREMENT")

  (type gpu-busy
    "

See MEASUREMENT"))
