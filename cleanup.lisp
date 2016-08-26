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

;;;; Cleanup functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:diff-match-patch)

(declaim (optimize (speed 3) (safety 0)))

(defun filter-diffs (diffs)
  (remove-if #'empty diffs :key #'cadr))

(defun edits-xor (dels adds)
  (if (empty adds)
      (if (not (empty dels)) `(:- ,dels))
      (if (empty dels) `(:+ ,adds))))

(defun cleanup-merge.join-differences (diffs test)
  "For every run of differences in the list DIFFS (separated by equalities),
   concatenate additions with additions and deletions with deletions to get
   two sequences, and factor out their common affixes, joining them unto the
   surrounding equalities."
  (iter (until (endp diffs))
        (for (values dels adds equality rest) := (next-diffs diffs))
        (with ^equality := nil)
        (if (and (empty dels) (empty adds))
            (unless (empty equality)
              (setq ^equality (join* ^equality equality)))
            (dlet* (((values prefix dels* adds* suffix)
                     (affix-match dels adds test)))
              (unless (empty prefix) (setq ^equality (join* ^equality prefix)))
              (unless (empty suffix) (setq equality (join* suffix equality)))
              (nconcing
                (filter-diffs `((:= ,^equality) (:- ,dels*) (:+ ,adds*))))
              (setq ^equality equality)))
        (if (and (endp rest) (not (empty ^equality)))
            (collect `(:= ,^equality)))
        (setq diffs rest)))

(defun cleanup-merge.shift-differences (diffs test)
  "For every unpaired edit in the list DIFFS which is surrounded by equalities
   on both sides, try to eliminate either the left or the right equality by
   ``shifting through'' the edit.  E. g. =A -BA =C is equivalent to -AB =AC,
   =A -BC =B is equivalent to =AB -CB."
  (macrolet ((shift-left (a b c)
               `(setf ,b (join* ,a (narrow ,b 0 (- (length ,a))))
                      ,a (join* ,a ,c)))
             (shift-right (a b c)
               `(setf ,b (join* (narrow ,b (length ,c)) ,c)
                      ,c (join* ,a ,c))))
    (iter (until (endp diffs))
          (with diffs-orig := diffs) (with changed := nil)
          (with ^equality := nil)
          (for (values dels adds equality rest) := (next-diffs diffs))
          (for (op edit) := (edits-xor dels adds))
          (cond ((and ^equality equality edit (suffixp ^equality edit test))
                 (shift-left ^equality edit equality)
                 (collect `(,op ,edit) :into diffs*)
                 (setq changed t))
                ((and ^equality equality edit (prefixp equality edit test))
                 (shift-right ^equality edit equality)
                 (setq ^equality nil)
                 (nconcing `((:= ,equality) (,op ,edit)) :into diffs*)
                 (setq changed t))
                (t (nconcing
                     (filter-diffs `((:= ,^equality) (:- ,dels) (:+ ,adds)))
                     :into diffs*)
                   (setq ^equality equality)))
          (setq diffs rest)
          (when (and (endp rest) (not (empty ^equality)))
            (collect `(:= ,^equality) :into diffs*))
          (finally (return (if changed diffs* diffs-orig))))))

(defun cleanup-merge (diffs test)
  "Reorder and merge like edit sections in the list DIFFS.  Merge equalities.
   Any edit section can move as long as it doesn't cross an equality."
  (iter (for joined := (cleanup-merge.join-differences diffs test))
        (for shifted := (cleanup-merge.shift-differences joined test))
        (if (eq joined shifted) (return shifted) (setq diffs shifted))))

(defun cleanup-semantic.next-short-equality (diffs prev-equalities)
  "Find the first equality in DIFFS which is no longer than either all the
   deletions or all the insertions on its left side, and ditto for those on
   its right side.  Return two values: (1) a three-element list, with (1a)
   differences on the left of the equality in reverse order, (1b) the
   content of the equality, and (1c) differences on the right in direct
   order; and (2) the list PREV-EQUALITIES unto which have been consed
   similar three-element lists representing equalities in DIFF before the
   one we'd found."
  (iter (for (head . rest) :on diffs)
        (for (op x) := head)
        (with lookback := nil) (with equality := nil) (with ret)
        (with edit-lengths-left) (with edit-lengths-right := (cons 0 0))
        (if (eq op :=)
            (progn
              (when equality (push ret prev-equalities))
              (setq edit-lengths-left edit-lengths-right
                    edit-lengths-right (cons 0 0)
                    ret (list lookback x rest)
                    lookback nil
                    equality x))
            (progn
              (case op
                (:- (incf (car edit-lengths-right) (length x)))
                (:+ (incf (cdr edit-lengths-right) (length x))))
              (if (and equality
                       (flet ((maxc (p) (max (car p) (cdr p))))
                         (<= (length equality)
                             (min (maxc edit-lengths-left)
                                  (maxc edit-lengths-right)))))
                  (return (values ret prev-equalities)))
              (push head lookback)))))

(defun cleanup-semantic.eliminate-short-equalities (diffs)
  "Eliminate every equality in DIFFS which is no longer than either all the
   deletions or all the insertions on its left side, and ditto for those on
   its right side, by replacing it with a delete-add pair."
  (iter (with diffs-orig := diffs) (with changed := nil)
        (with equalities := nil)
        (for (values (lookback equality rest) equalities+)
          := (cleanup-semantic.next-short-equality diffs equalities))
        (flet ((reattach (diffs prev-equality)
                 (dlet* (((lookback equality) prev-equality))
                   (revappend* lookback `(:= ,equality) diffs))))
          (if equality
              (let ((prev-equality (pop equalities+)))
                (setq diffs (revappend* lookback
                              `(:- ,equality) `(:+ ,equality) rest)
                      equalities equalities+
                      changed t)
                (if prev-equality
                    (setq diffs (reattach diffs prev-equality))))
              (return
                (if changed
                    (reduce #'reattach equalities :initial-value diffs)
                    diffs-orig))))))

(defun cleanup-semantic.boundary-score (str pos
                                        &key (start 0) (end (length str)))
  "Compute a score from 6 to 0 determining to which extent the position POS
   in the portion of STR between START and END is a logical boundary."
  (macrolet ((counting-newlines ((n &rest steps) &body clauses)
               `(iter (for c :in-string str ,@steps)
                  (while (newline-char-p c))
                  (if (eq c #\Newline) (count 1 :into ,n))
                  ,@clauses)))
    (let ((c-before (and (< start pos) (<= pos end) (aref str (1- pos))))
          (c-after (and (<= start pos) (< pos end) (aref str pos))))
      (max ;; Score for logical boundary after C-BEFORE
           (cond ((null c-before) 6)
                 ((newline-char-p c-before)
                  (if (counting-newlines (n :from (1- pos) :downto start)
                        (thereis (>= n 2)))
                      5 4))
                 ((whitespace-char-p c-before) 2)
                 ((not (alphanumericp c-before)) 1)
                 (t 0))
           ;; Score for logical boundary before C-AFTER
           (cond ((null c-after) 6)
                 ((newline-char-p c-after)
                  (if (counting-newlines (n :from pos :below end)
                        (thereis (>= n 2)))
                      5 4))
                 ((whitespace-char-p c-after)
                  (if (or (null c-before)
                          (not (or (alphanumericp c-before)
                                   (whitespace-char-p c-before))))
                      3 2))
                 ((not (alphanumericp c-after)) 1)
                 (t 0))))))

(defun cleanup-semantic.shift-lossless (diffs test)
  "For every unpaired edit in the list DIFFS which is surrounded by
   equalities on both sides, try to realign the edit by ``shifting it
   through'' to a logical boundary."
  (iter (until (endp diffs))
        (with ^equality := nil)
        (for (values dels adds equality rest) := (next-diffs diffs))
        (for (op edit) := (edits-xor dels adds))
        (for (values ^equality* edit* equality*) :=
          (when (and ^equality equality edit)
            (iter (with buf := (join* ^equality edit equality))
                  (with width := (length edit))
                  (with buf-start := (or (mismatch ^equality edit
                                           :from-end t :test test)
                                         0))
                  (with buf-end := (- (length buf) width))
                  (for window :index-of-string buf
                              :from buf-start :to buf-end)
                  (for window-end := (+ window width))
                  (while (or (= window buf-end)
                             (funcall test
                               (aref buf window) (aref buf window-end))))
                  (for score := (max (cleanup-semantic.boundary-score
                                       buf window :end window-end)
                                     (cleanup-semantic.boundary-score
                                       buf window-end :start window)))
                  (maximizing score :into best-score)
                  (reducing window
                    :by (lambda (bw w) (if (< score best-score) bw w))
                    :into best-window :initial-value buf-start)
                  (finally
                    (unless (= best-window (length ^equality))
                      (return (split-at buf best-window
                                        (+ best-window width))))))))
        (if edit*
            (progn
              (unless (empty ^equality*) (collect `(:= ,^equality*)))
              (collect `(,op ,edit*))
              (setq ^equality equality*))
            (progn
              (nconcing
                (filter-diffs `((:= ,^equality) (:- ,dels) (:+ ,adds))))
              (setq ^equality equality)))
        (setq diffs rest)
        (when (and (endp rest) (not (empty ^equality)))
          (collect `(:= ,^equality)))))

(defun cleanup-semantic.eliminate-overlaps (diffs test)
  "Find overlaps between adjacent deletions and insertions in DIFFS and
   extract them by inserting equalities, provided the overlap is as big as
   the remainder of the edits. E. g. -ABCXXX +XXXDEF is the same as -ABC
   =XXX +DEF; -XXXABC +DEFXXX is the same as +DEF =XXX -ABC."
  (iter (until (endp diffs))
    (for (values dels adds equality rest) := (next-diffs diffs))
    (cond
      ((empty dels) (unless (empty adds) (collect `(:+ ,adds))))
      ((empty adds) (unless (empty dels) (collect `(:- ,dels))))
      (t (let ((ov1 (overlap dels adds test))
               (ov2 (overlap adds dels test)))
           (flet ((collect-overlap (op-a a op-b b ov)
                    (nconcing
                      (filter-diffs `((,op-a ,(narrow a 0 (- ov)))
                                      (:= ,(narrow b 0 ov))
                                      (,op-b ,(narrow b ov)))))))
             (cond
               ((and (>= ov1 ov2) (> ov1 0)
                     (or (>= ov1 (/ (length dels) 2))
                         (>= ov1 (/ (length adds) 2))))
                (collect-overlap :- dels :+ adds ov1))
               ((and (>= ov2 ov1) (> ov2 0)
                     (or (>= ov2 (/ (length dels) 2))
                         (>= ov2 (/ (length adds) 2))))
                (collect-overlap :+ adds :- dels ov2))
               (t (nconcing `((:- ,dels) (:+ ,adds)))))))))
    (unless (empty equality) (collect `(:= ,equality)))
    (setq diffs rest)))

(defun cleanup-semantic (diffs test)
  "Reduce the number of edits by eliminating semantically trivial
   equalities."
  (let* ((no-shorts (cleanup-semantic.eliminate-short-equalities diffs))
         (merged (if (eq no-shorts diffs) diffs
                     (cleanup-merge no-shorts test)))
         (shifted (cleanup-semantic.shift-lossless merged test))
         (no-overlaps (cleanup-semantic.eliminate-overlaps shifted test)))
    no-overlaps))

(defun cleanup-for-efficiency (diffs test)
  "Reduce the number of edits by eliminating operationally trivial
   equalities."
  (iter (until (endp diffs))
        (with diffs-orig := diffs) (with changed := nil)
        (with ^equality := nil) (with ^dels := nil) (with ^adds := nil)
        (with ^^equality := nil) (with ^^dels := nil) (with ^^adds := nil)
        (for (values dels adds equality rest) := (next-diffs diffs))
        (for score :=
          (cond
            ((or (>= (length ^equality) *diff-edit-cost*)
                 (and (empty dels) (empty adds)))
               -1)
            ((empty ^equality)
               0)
            (t (- (count-if-not #'empty (list ^adds ^dels adds dels))
                  (if (>= (length ^equality) (/ *diff-edit-cost* 2))
                      1 0)))))
        (cond
          ((< score 3)
             (nconcing
               (filter-diffs `((:- ,^^dels) (:+ ,^^adds) (:= ,^^equality)))
               :into diffs*)
             (if (< score 0)
                 (progn
                   (nconcing
                     (filter-diffs `((:- ,^dels) (:+ ,^adds) (:= ,^equality)))
                     :into diffs*)
                   (setq ^^dels nil ^^adds nil ^^equality nil))
                 (setq ^^dels ^dels ^^adds ^adds ^^equality ^equality))
             (if (endp rest)
                 (nconcing
                   (filter-diffs `((:- ,dels) (:+ ,adds) (:= ,equality)))
                   :into diffs*)
                 (setq ^dels dels ^adds adds ^equality equality))
             (setq diffs rest))
          ((or (empty ^dels) (empty ^adds))
             (setq diffs `(,@(filter-diffs `((:- ,^dels) (:+ ,^adds)))
                           (:- ,^equality) (:+ ,^equality) . ,diffs))
             (setq ^equality ^^equality ^dels ^^dels ^adds ^^adds)
             (setq ^^equality nil ^^dels nil ^^adds nil)
             (setq changed t))
          (t (nconcing (filter-diffs
                         `((:- ,^^dels) (:+ ,^^adds) (:= ,^^equality)
                           (:- ,^dels) (:+ ,^adds)))
                       :into diffs*)
             (setq diffs `((:- ,^equality) (:+ ,^equality) . ,diffs))
             (setq ^equality nil ^dels nil ^adds nil
                   ^^equality nil ^^dels nil ^^adds nil)
             (setq changed t)))
        (finally
          (return (if changed (cleanup-merge diffs* test) diffs-orig)))))
