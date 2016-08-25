(cl:defpackage #:diff-match-patch
  (:nicknames #:dmp)
  (:use #:cl #:iterate)
  (:export #:*diff-timeout* #:*diff-edit-cost* #:*diff-check-lines-length*
           #:*match-threshold* #:*match-distance*
           #:*patch-delete-threshold* #:*patch-margin* #:*max-bits*
           ;; diff.lisp
           #:diff #:mapdiff #:diff-origin #:diff-destination
           #:translate-position #:levenshtein #:index-abbreviator
           ;; match.lisp
           #:match
           ;; patch.lisp
           #:hunk #:hunk-diffs #:hunk-start-a #:hunk-start-b
           #:hunk-length-a #:hunk-length-b
           #:make-patch #:apply-patch #:print-patch #:read-patch))

(in-package #:diff-match-patch)

(defparameter *diff-timeout* 5.0
  "Number of seconds to map a diff before giving up (NIL for infinity).")

(defparameter *diff-edit-cost* 4
  "Cost of an empty edit operation in terms of edit characters.")

(defparameter *diff-check-lines-length* 100
  "When a diff is computed between strings, how long should they be to
   to qualify for line-level-diff optimization. On strings whose length
   is greater than this value, a line-level diff is run first to identify
   the changed areas. NIL = don't optimize.")

(defparameter *match-threshold* 0.5
  "At what point is no match declared (0.0 = perfection, 1.0 = very loose).")

(defparameter *match-distance* 1000
  "How far to search for a match (0 = exact location, 1000+ = broad match).
   A match this many characters away from the expected location will add
   1.0 to the score (0.0 is a perfect match).")

(defparameter *patch-delete-threshold* 0.5
  "When deleting a large block of text (over ~64 characters), how close do
   the contents have to be to match the expected contents. (0.0 = perfection,
   1.0 = very loose).  Note that Match_Threshold controls how closely the
   end points of a delete need to match.")

(defparameter *patch-margin* 4
  "Chunk size for context length.")

(defparameter *max-bits* #.(integer-length most-positive-fixnum)
  "The number of bits in an int, used to limit the width of match patterns
   when applying an inexact patch. Common Lisp having bigints allows to set
   this value to NIL (meaning unlimited width), but keeping it as it is
   gives a slight optimization by using fixints only.")
