(asdf:defsystem machine-measurements
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Perform measurements about CPU time, memory usage, etc."
  :homepage "https://shinmera.github.io/machine-measurements/"
  :bug-tracker "https://github.com/shinmera/machine-measurements/issues"
  :source-control (:git "https://github.com/shinmera/machine-measurements.git")
  :serial T
  :components ((:file "package")
               (:file "protocol")
               (:file "measurements")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :machine-state
               :precise-time))
