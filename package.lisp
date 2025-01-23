(defpackage #:org.shirakumo.machine-state.measurements
  (:use #:cl)
  (:local-nicknames
   (#:machine-state #:org.shirakumo.machine-state)
   (#:precise-time #:org.shirakumo.precise-time))
  (:export
   #:storage-io
   #:storage-read
   #:storage-write
   #:storage-%
   #:storage-free
   #:storage-used
   #:storage-total
   #:network-io
   #:network-read
   #:network-write
   #:memory-%
   #:memory-free
   #:memory-used
   #:memory-total
   #:uptime
   #:cpu-%
   #:cpu-idle
   #:cpu-busy
   #:heap-%
   #:heap-free
   #:heap-used
   #:heap-total
   #:process-busy
   #:process-size
   #:process-io
   #:process-read
   #:process-write
   #:gc-busy
   #:gpu-%
   #:gpu-free
   #:gpu-used
   #:gpu-busy))
