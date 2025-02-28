(in-package #:org.shirakumo.machine-state.measurements)

;; protocol.lisp
(docs:define-docs
  (type measurement
    "Base type for all measurements.

See MEASURE
See LAST-TIME
See LAST-VALUE")

 (function measure
   "Perform a measurement.

Returns three values:
  - The measured value as a double-float or (unsigned-byte 64)
  - The time difference in seconds since the last measurement as a
    double-float
  - The measurement object

See MEASUREMENT")

 (function last-time
   "Returns the last time the measurement was performed.

This is not a wall clock, but rather some monotonic time stamp that
was taken the last time MEASURE was called.

See MEASUREMENT")

 (function last-value
   "Returns the last value that was measured.

Note that this is usually a \"raw\" value before it was converted to a
more easy-to-consume variant as is returned by MEASURE.

See MEASUREMENT"))

;; measurements.lisp
(docs:define-docs
  (type storage-io
    "Measures the storage IO rate in bytes/s

DEVICE should be the device name to measure, or T for all devices.

See ORG.SHIRAKUMO.MACHINE-STATE:STORAGE-IO-BYTES
See MEASUREMENT")

  (type storage-read
    "Measures the storage read rate in bytes/s

DEVICE should be the device name to measure, or T for all devices.

See ORG.SHIRAKUMO.MACHINE-STATE:STORAGE-IO-BYTES
See MEASUREMENT")

  (type storage-write
    "Measures the storage write rate in bytes/s

DEVICE should be the device name to measure, or T for all devices.

See ORG.SHIRAKUMO.MACHINE-STATE:STORAGE-IO-BYTES
See MEASUREMENT")

  (type storage-%
    "Measures the storage usde space percentage.

DEVICE should be the device name to measure, or T for all devices.

See ORG.SHIRAKUMO.MACHINE-STATE:STORAGE-ROOM
See MEASUREMENT")

  (type storage-free
    "Measures the storage free space in bytes.

DEVICE should be the device name to measure, or T for all devices.

See ORG.SHIRAKUMO.MACHINE-STATE:STORAGE-ROOM
See MEASUREMENT")

  (type storage-used
    "Measures the storage used space in bytes

DEVICE should be the device name to measure, or T for all devices.

See ORG.SHIRAKUMO.MACHINE-STATE:STORAGE-ROOM
See MEASUREMENT")

  (type storage-total
    "Measures the storage total space in bytes

DEVICE should be the device name to measure, or T for all devices.

See ORG.SHIRAKUMO.MACHINE-STATE:STORAGE-ROOM
See MEASUREMENT")

  (type network-io
    "Measures the network IO rate in bytes/s

DEVICE should be the device name to measure, or T for all devices.

See ORG.SHIRAKUMO.MACHINE-STATE:NETWORK-IO-BYTES
See MEASUREMENT")

  (type network-read
    "Measures the storage read rate in bytes/s

DEVICE should be the device name to measure, or T for all devices.
See ORG.SHIRAKUMO.MACHINE-STATE:NETWORK-IO-BYTES
See MEASUREMENT")

  (type network-write
    "Measures the storage write rate in bytes/s

DEVICE should be the device name to measure, or T for all devices.

See ORG.SHIRAKUMO.MACHINE-STATE:NETWORK-IO-BYTES
See MEASUREMENT")

  (type memory-%
    "Measures the memory used percentage

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-ROOM
See MEASUREMENT")

  (type memory-free
    "Measures the memory free space in bytes

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-ROOM
See MEASUREMENT")

  (type memory-used
    "Measures the memory used space in bytes

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-ROOM
See MEASUREMENT")

  (type memory-total
    "Measures the memory total space in bytes

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-ROOM
See MEASUREMENT")

  (type uptime
    "Measures the time since the machine started in seconds

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-UPTIME
See MEASUREMENT")

  (type cpu-%
    "Measures the CPU core utilization percentage

CORE should be the core number to measure, or T for all cores.

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-TIME
See MEASUREMENT")

  (type cpu-idle
    "Measures the CPU idle time

CORE should be the core number to measure, or T for all cores.

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-TIME
See MEASUREMENT")

  (type cpu-busy
    "Measures the CPU busy time

CORE should be the core number to measure, or T for all cores.

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-TIME
See MEASUREMENT")

  (type heap-%
    "Measures the GC used space percentage

See ORG.SHIRAKUMO.MACHINE-STATE:GC-ROOM
See MEASUREMENT")

  (type heap-free
    "Measures the GC free space in bytes

See ORG.SHIRAKUMO.MACHINE-STATE:GC-ROOM
See MEASUREMENT")

  (type heap-used
    "Measures the GC used space in bytes

See ORG.SHIRAKUMO.MACHINE-STATE:GC-ROOM
See MEASUREMENT")

  (type heap-total
    "Measures the GC total space in bytes

See ORG.SHIRAKUMO.MACHINE-STATE:GC-ROOM
See MEASUREMENT")

  (type process-busy
    "Measures the processor time the process has used in seconds

See ORG.SHIRAKUMO.MACHINE-STATE:PROCESS-TIME
See MEASUREMENT")

  (type process-size
    "Measures the process size in bytes

See ORG.SHIRAKUMO.MACHINE-STATE:PROCESS-ROOM
See MEASUREMENT")

  (type process-io
    "Measures the process IO rate in bytes/s

See ORG.SHIRAKUMO.MACHINE-STATE:PROCESS-IO-BYTES
See MEASUREMENT")

  (type process-read
    "Measures the process read rate in bytes/s

See ORG.SHIRAKUMO.MACHINE-STATE:PROCESS-IO-BYTES
See MEASUREMENT")

  (type process-write
    "Measures the process write rate in bytes/s

See ORG.SHIRAKUMO.MACHINE-STATE:PROCESS-IO-BYTES
See MEASUREMENT")

  (type gc-busy
    "Measures the time spent in GC in seconds

See ORG.SHIRAKUMO.MACHINE-STATE:GC-TIME
See MEASUREMENT")

  (type gpu-%
    "Measures the GPU memory utilization percentage

See ORG.SHIRAKUMO.MACHINE-STATE:GPU-ROOM
See MEASUREMENT")

  (type gpu-free
    "Measures the GPU memory free space in bytes

See ORG.SHIRAKUMO.MACHINE-STATE:GPU-ROOM
See MEASUREMENT")

  (type gpu-used
    "Measures the GPU memory used space in bytes

See ORG.SHIRAKUMO.MACHINE-STATE:GPU-ROOM
See MEASUREMENT")

  (type gpu-busy
    "Measures the GPU busy time in seconds

See ORG.SHIRAKUMO.MACHINE-STATE:GPU-TIME
See MEASUREMENT")

  (type battery-%
    "Measures the battery charge in percentage

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-BATTERY
See MEASUREMENT")

  (type battery-charge
    "Measures the battery charge

See ORG.SHIRAKUMO.MACHINE-STATE:MACHINE-BATTERY
See MEASUREMENT")

  (function storage-io
    "Create a storage-io instance

See STORAGE-IO (type)")

  (function storage-read
    "Create a storage-read instance

See STORAGE-READ (type)")

  (function storage-write
    "Create a storage-write instance

See STORAGE-WRITE (type)")

  (function storage-%
    "Create a storage-% instance

See STORAGE-% (type)")

  (function storage-free
    "Create a storage-free instance

See STORAGE-FREE (type)")

  (function storage-used
    "Create a storage-used instance

See STORAGE-USED (type)")

  (function storage-total
    "Create a storage-total instance

See STORAGE-TOTAL (type)")

  (function network-io
    "Create a network-io instance

See NETWORK-IO (type)")

  (function network-read
    "Create a network-read instance

See NETWORK-READ (type)")

  (function network-write
    "Create a network-write instance

See NETWORK-WRITE (type)")

  (function memory-%
    "Create a memory-% instance

See MEMORY-% (type)")

  (function memory-free
    "Create a memory-free instance

See MEMORY-FREE (type)")

  (function memory-used
    "Create a memory-used instance

See MEMORY-USED (type)")

  (function memory-total
    "Create a memory-total instance

See MEMORY-TOTAL (type)")

  (function uptime
    "Create a uptime instance

See UPTIME (type)")

  (function cpu-%
    "Create a cpu-% instance

See CPU-% (type)")

  (function cpu-idle
    "Create a cpu-idle instance

See CPU-IDLE (type)")

  (function cpu-busy
    "Create a cpu-busy instance

See CPU-BUSY (type)")

  (function heap-%
    "Create a heap-% instance

See HEAP-% (type)")

  (function heap-free
    "Create a heap-free instance

See HEAP-FREE (type)")

  (function heap-used
    "Create a heap-used instance

See HEAP-USED (type)")

  (function heap-total
    "Create a heap-total instance

See HEAP-TOTAL (type)")

  (function process-busy
    "Create a process-busy instance

See PROCESS-BUSY (type)")

  (function process-size
    "Create a process-size instance

See PROCESS-SIZE (type)")

  (function process-io
    "Create a process-io instance

See PROCESS-IO (type)")

  (function process-read
    "Create a process-read instance

See PROCESS-READ (type)")

  (function process-write
    "Create a process-write instance

See PROCESS-WRITE (type)")

  (function gc-busy
    "Create a gc-busy instance

See GC-BUSY (type)")

  (function gpu-%
    "Create a gpu-% instance

See GPU-% (type)")

  (function gpu-free
    "Create a gpu-free instance

See GPU-FREE (type)")

  (function gpu-used
    "Create a gpu-used instance

See GPU-USED (type)")

  (function gpu-busy
    "Create a gpu-busy instance

See GPU-BUSY (type)")

  (function battery-%
    "Create a battery-% instance

See BATTERY-% (type)")

  (function battery-charge
    "Create a battery-charge instance

See BATTERY-% (type)"))
