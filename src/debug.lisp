(in-package :clsh)

(defvar *debug-levels*
  '(:verbose :debug :warning :error :fatal))

(defvar *debug-level* :warnin)          ;default

(defun set-debug-level (level)
  (if (not (member level *debug-levels*))
      (error 'unknown-debug-level :level level)
      (setq *debut-level* level)))

(defun debug-format (level str &rest (args nil))
  ;; output to *error-output*
  (if (member level *debug-levels*)
      (progn
        (format *error-output* (debug-level-format level))
        (apply #'format *error-output* str args))
      (error 'unknown-debug-level :level level)))

(defun debug-level-format (level)
  (case level
    (:verbose
     "[verbose] ")
    (:debug
     "[debug] ")
    (:warning
     "[warning] ")
    (:error
     "[error] ")
    (:fatal
     "[fatal] ")
    (t
     (error 'unknown-debug-level :level level))))
