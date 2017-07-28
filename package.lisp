;;;; Google Diff, Match and Patch library by Neil Fraser
;;;; Ported into Common Lisp by Boris Smilga, with modifications
;;;; Original project: see http://code.google.com/p/google-diff-match-patch/

;; Copyright 2006 Google Inc.
;; Copyright 2016 Boris Smilga
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;   http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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
           #:make-patch #:apply-patch
           #:write-chars-patch #:read-chars-patch
           #:write-lines-patch #:read-lines-patch
           ;; print.lisp
           #:print-diff))

;;;; Global parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:diff-match-patch)

(defparameter *diff-timeout* 5.0
  "Number of seconds to refine a diff before giving up (NIL for infinity).")

(defparameter *diff-edit-cost* 4
  "Cost of an empty edit operation in terms of edit characters. Used to
   detect operationally trivial equalities when cleaning up diff lists.")

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
   1.0 = very loose).  Note that *MATCH-THRESHOLD* controls how closely the
   end points of a delete need to match.")

(defparameter *patch-margin* 4
  "Chunk size for context length.")

(defparameter *max-bits* #.(integer-length most-positive-fixnum)
  "The number of bits in an int, used to limit the width of match patterns
   when applying an inexact patch. Common Lisp having bigints allows to set
   this value to NIL (meaning unlimited width), but keeping it as it is
   optimizes performance by using fixints only.")
