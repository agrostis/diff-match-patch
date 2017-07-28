(in-package #:diff-match-patch)

(defun print-diff (a b &key (test #'eql) (out *standard-output*) (format :plain) (color T))
  "Runs (DIFF A B) and prints the diff in a pretty format.  FORMAT can be
   :PLAIN to indicate additions and removals by {+...+} and [-...-], or NIL
   if not.  COLOR indicates whether to enabled colored output via ANSI
   escape codes."
  (iter
    (for (op x) in (diff a b :test test))
    (when color
      (case op
        (:+ (write-string #.(cl-ansi-text:make-color-string :green) out))
        (:- (write-string #.(cl-ansi-text:make-color-string :red) out))))
    (when (eq format :plain)
      (case op
        (:+ (write-string "{+" out))
        (:- (write-string "[-" out))))
    (format out "~A" x)
    (when (eq format :plain)
      (case op
        (:+ (write-string "+}" out))
        (:- (write-string "-]" out))))
    (when color
      ;; TODO: could be optimised to only reset if necessary
      (write-string #.cl-ansi-text:+reset-color-string+ out))))
