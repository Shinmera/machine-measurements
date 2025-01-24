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
See MEASUREMENT"))
