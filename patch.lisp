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

;;;; Patch functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:diff-match-patch)

(declaim (optimize (speed 3) (safety 0)))

(defclass hunk ()
  ((diffs :accessor hunk-diffs :initarg :diffs :type list)
   (start-a :accessor hunk-start-a :initarg :start-a :type (integer 0))
   (start-b :accessor hunk-start-b :initarg :start-b :type (integer 0))
   (length-a :accessor hunk-length-a :initarg :length-a :type (integer 0))
   (length-b :accessor hunk-length-b :initarg :length-b :type (integer 0)))
  (:documentation "A wrapper for an edit or group of adjacent edits."))

(defun make-hunk (diffs start-a start-b length-a length-b context)
  "Create a hunk instance, ensuring that it includes a unique context."
  (iter (for padding :from 0 :by *patch-margin*)
        (with l := (length context))
        (with end-b := (+ start-b length-a))
        (for p-start := #1=(max (- start-b padding) 0))
        (for p-end := #2=(min (+ end-b padding) l))
        (while (or (not *max-bits*)
                   (< (- p-end p-start)
                      (- *max-bits* (* 2 *patch-margin*)))))
        (while (flet ((q (rev)
                        (if (and (= p-start 0) (= p-end l)) 0
                            (search context context
                              :start1 p-start :end1 p-end :from-end rev))))
                 (/= (q t) (q nil))))
        (finally (incf padding *patch-margin*)
                 (setf p-start #1# p-end #2#)
                 (when (< p-start start-b)
                   (setf diffs `((:= ,(narrow context p-start start-b))
                                 ,@diffs))
                   (let ((m (- start-b p-start)))
                     (incf length-a m) (incf length-b m)
                     (decf start-a m) (decf start-b m)))
                 (when (> p-end end-b)
                   (setf diffs `(,@diffs
                                 (:= ,(narrow context end-b p-end))))
                   (let ((m (- p-end end-b)))
                     (incf length-a m) (incf length-b m)))
                 (return
                   (make-instance 'hunk
                     :diffs diffs
                     :start-a start-a :start-b start-b
                     :length-a length-a :length-b length-b)))))

(defun copy-hunk (hunk &optional diffs start-a start-b length-a length-b)
  "Return a copy of HUNK with zero or more slot values replaced by
   arguments."
  (make-instance 'hunk
    :diffs (or diffs (hunk-diffs hunk))
    :start-a (or start-a (hunk-start-a hunk))
    :start-b (or start-b (hunk-start-b hunk))
    :length-a (or length-a (hunk-length-a hunk))
    :length-b (or length-b (hunk-length-b hunk))))

(defun make-patch.cleanup (diffs test)
  "Cleanup DIFFS based on their size and type of elements."
  (if (endp (cddr diffs)) diffs
      (let ((sem (if (characterp (elt (cadar diffs) 0))
                     (cleanup-semantic diffs test)
                     diffs)))
        (cleanup-for-efficiency sem test))))

(defun make-patch.next-hunk (diffs start-a start-b
                             left-context right-context)
  "Extract the next patch hunk from DIFFS."
  (iter (for (values dels adds equality rest) := (next-diffs diffs))
        (for (hunk-start-a hunk-start-b) :do-next
          (unless (and (empty dels) (empty adds))
            (if-first-time
              (setf hunk-start-a start-a hunk-start-b start-b))))
        (for del-len := (length dels))
        (for add-len := (length adds))
        (for eq-len := (length equality))
        (with dbl-margin := (* *patch-margin* 2))
        (with context := (join* left-context right-context))
        (when (plusp del-len)
          (collect `(:- ,dels) :into hunk-diffs)
          (summing del-len :into hunk-length-a)
          (incf start-a del-len)
          (setf right-context (narrow right-context del-len)))
        (when (plusp add-len)
          (collect `(:+ ,adds) :into hunk-diffs)
          (summing add-len :into hunk-length-b)
          (incf start-b add-len)
          (setf left-context (join* left-context adds)))
        (when (plusp eq-len)
          (when (and (<= eq-len dbl-margin) hunk-diffs (not (endp rest)))
            (collect `(:= ,equality) :into hunk-diffs)
            (summing eq-len :into hunk-length-a)
            (summing eq-len :into hunk-length-b))
          (incf start-a eq-len)
          (incf start-b eq-len)
          (setf left-context (join* left-context equality)
                right-context (narrow right-context eq-len)))
        (until (or (endp (setq diffs rest))
                   (and hunk-diffs (>= eq-len dbl-margin))))
        (finally
          (let ((hunk
                  (if hunk-diffs
                      (make-hunk hunk-diffs hunk-start-a hunk-start-b
                                 hunk-length-a hunk-length-b context))))
            (return
              (values hunk start-b start-b diffs
                      left-context right-context))))))

(defun make-patch (&rest args)
  "When called with two seuqences A and B, compute the diff between them and
   convert it into a list of hunk objects.  Alternatively, the diff can be
   passed explicitly by keyword argument, and sequences reconstructed from
   it."
  (let* ((kw-args (member-if #'keywordp args))
         (seqs-args (ldiff args kw-args)))
    (destructuring-bind (&optional (a nil a?) (b nil b?)) seqs-args
      (destructuring-bind (&key (diffs nil diffs?) (test #'eql)) kw-args
        (cond
          ((and diffs? (not a?))
           (setq a (diff-origin diffs)))
          ((and (not diffs?) a? b?)
           (setq diffs (make-patch.cleanup (diff a b :test test) test)))
          ((and (not diffs?) (not a?))
           (error "Insufficient args for MAKE-PATCH: ~S" args)))
          (iter (initially
                  (setq start-a 0 start-b 0)
                  (dsetq (values left-ctx right-ctx) (split-at a 0)))
                (for (values hunk start-a start-b rest left-ctx right-ctx)
                  :next (make-patch.next-hunk diffs start-a start-b
                                              left-ctx right-ctx))
                (when hunk (collect hunk))
                (until (endp (setq diffs rest))))))))

(defun format-hunk-header (hunk out)
  (with-slots (start-a start-b length-a length-b) hunk
    (format out "@@ -~? +~? @@~%"
            #1="~[~A,0~;~*~A~:;~*~A,~3:*~A~]"
            (list length-a start-a (1+ start-a))
            #1#
            (list length-b start-b (1+ start-b)))))

(defun write-chars-patch (patch &optional (out *standard-output*))
  "Write the formatted representation of the given PATCH (a hunk or list of
   hunks, encoding a difference of character strings) to the stream OUT."
  (etypecase patch
    (list (mapc (lambda (hunk) (write-chars-patch hunk out)) patch))
    (hunk (with-slots (diffs) patch
            (when (not (eq (diff-seq-type diffs) 'string))
              (error "Non-string patch in WRITE-CHARS-PATCH"))
            (format-hunk-header patch out)
            (iter (for (op x) :in diffs)
                  (princ (if (eq op :=) #\Space op) out)
                  (write-line-urlencode x out))))))

(defun write-lines-patch (patch &optional (out *standard-output*))
  "Write the formatted representation of the given PATCH (a hunk or list of
   hunks, encoding a difference of line sequences) to the stream OUT."
  (etypecase patch
    (list (mapc (lambda (hunk) (write-lines-patch hunk out)) patch))
    (hunk (with-slots (diffs) patch
            (when (not (eq (nth-value 1 (diff-seq-type diffs)) 'string))
              (error "Non-line-sequence patch in WRITE-LINES-PATCH"))
            (format-hunk-header patch out)
            (iter (for (op x) :in diffs)
                  (let ((leader (if (eq op :=) #\Space op)))
                    (iter (for s :in-sequence x)
                          (princ leader out)
                          (write-line-urlencode s out))))))))

(let ((header-re
        (ppcre:create-scanner
          "@@ -(\\d+)(?:,(\\d+))? \\+(\\d+)(?:,(\\d+))? @@")))
  (defun parse-hunk-header (in)
    (dlet* ((header (read-line in nil))
            ((values ok g) (ppcre:scan-to-strings header-re header)))
      (if ok
          (flet ((int (i) (if #1=(aref g i) (parse-integer #1#) 1)))
            (let* ((la (int 1))
                   (a0 (if (zerop la) (int 0) (1- (int 0))))
                   (lb (int 3))
                   (b0 (if (zerop lb) (int 2) (1- (int 2)))))
              (make-instance 'hunk
                :start-a a0 :length-a la :start-b b0 :length-b lb)))
          (error "Invalid hunk header: ~A" header)))))

(defun read-chars-patch (&optional (in *standard-input*))
  "Read a formatted representation of patch encoding a difference of
   character strings from the stream IN and return it as a list of hunks."
  (iter (for c0 := (peek-char nil in nil))
        (with hunk := nil) (with diffs := '()) (with op)
        (when (and (or (not c0) (char= c0 #\@)) hunk)
          (if (iter (for (op) :in diffs) (thereis (member op '(:+ :-))))
              (progn (setf (hunk-diffs hunk) (nreverse diffs)
                           diffs '())
                     (collect hunk))
              (error "Empty diffs")))
        (while c0)
        (cond
          ((char= c0 #\@)
           (setq hunk (parse-hunk-header in)))
          ((setq op (case c0 (#\+ :+) (#\- :-) (#\Space :=)))
           (read-char in)
           (push `(,op ,(read-line-urldecode in)) diffs))
          (t (error "Invalid diff line: ~A" (read-line in nil))))))

(defun read-lines-patch (&optional (in *standard-input*) (seq-type 'vector))
  "Read a formatted representation of patch encoding a difference of
   line sequences from the stream IN and return it as a list of hunks."
  (iter (for c0 := (peek-char nil in nil))
        (with hunk := nil) (with diffs := '()) (with op)
        (with leader := #\ ) (with leader-op := nil) (with lines := '())
        (when (and (or (not c0) (char/= c0 leader)) lines)
          (push (list leader-op (coerce (nreverse lines) seq-type)) diffs)
          (setq lines '()))
        (when (and (or (not c0) (char= c0 #\@)) hunk)
          (if (iter (for (op) :in diffs) (thereis (member op '(:+ :-))))
              (progn (setf (hunk-diffs hunk) (nreverse diffs)
                           diffs '())
                     (collect hunk))
              (error "Empty diffs")))
        (while c0)
        (cond
          ((char= c0 #\@)
           (setq hunk (parse-hunk-header in)))
          ((setq op (case c0 (#\+ :+) (#\- :-) (#\Space :=)))
           (read-char in)
           (setq leader c0 leader-op op)
           (push (read-line-urldecode in) lines))
          (t (error "Invalid diff line: ~A" (read-line in nil))))))

(defun trivial-padding (patch length)
  "Return a padding sequence for PATCH, of the given LENGTH."
  (unless (null patch)
    (with-slots (diffs) (car patch)
      (iter (with seq-type := (diff-seq-type diffs))
            (with mkelt := (typecase (elt (cadar diffs) 0)
                             (number #'identity)
                             (character #'code-char)
                             (t #'gensym)))
            (for i :from 1 :to length)
            (collect (funcall mkelt i) :into padding)
            (finally (return (coerce padding seq-type)))))))

(defun add-padding (patch padding)
  "Return a copy of PATCH with PADDING added to its first and last hunk."
  (iter (with padding-length := (length padding))
        (for (hunk . rest) :on patch)
        (for firstp := (first-time-p))
        (for lastp := (endp rest))
        (for hunk* :=
          (with-slots (diffs start-a length-a start-b length-b) hunk
            (dlet* (((op x) (and firstp (car diffs)))
                    (l (length x)))
              (cond ((and firstp (not (eq op :=)))
                       (copy-hunk hunk
                         `((:= ,padding) ,@diffs)
                         nil nil
                         #1=(+ length-a padding-length)
                         #2=(+ length-b padding-length)))
                    ((and firstp (< l padding-length))
                       (copy-hunk hunk
                         `((,op ,(join* (narrow padding l) x))
                           ,@(rest diffs))
                         (+ start-a l) (+ start-b l)
                         #3=(+ length-a (- padding-length l))
                         #4=(+ length-b (- padding-length l))))
                    (t (copy-hunk hunk nil
                         (+ start-a padding-length)
                         (+ start-b padding-length)))))))
        (collect
          (with-slots (diffs start-a length-a start-b length-b) hunk*
            (dlet* (((op x) (and lastp (car (last diffs))))
                    (l (length x)))
              (cond ((and lastp (not (eq op :=)))
                       (copy-hunk hunk*
                         `(,@diffs (:= ,padding))
                         nil nil
                         #1# #2#))
                    ((and lastp (< l padding-length))
                       (copy-hunk hunk*
                         `(,@(butlast diffs)
                           (,op ,(join* x (narrow padding 0 (- l)))))
                         nil nil
                         #3# #4#))
                    (t hunk*)))))))

(defun split-big-hunks (patch)
  "Return a copy of PATCH with all big hunks split. (Big hunks are hunks
   whose length exceeds the maximum limit of the match algorithm.)"
  (if *max-bits*
      (iter (for hunk :in patch)
            (if (<= (hunk-length-a hunk) *max-bits*)
                (collect hunk)
                (nconcing (split-big-hunk hunk *max-bits*))))
      patch))

(defun split-big-hunk (hunk size)
  "Split HUNK into pieces not exceeding SIZE and return them as a list of
   hunks."
  (with-slots (diffs start-a start-b length-a length-b) hunk
    (iter :split
          (with type := (diff-seq-type diffs))
          (with big-diffs := diffs) (with lcontext := '())
          (with start-a* := start-a) (with start-b* := start-b)
          (until (endp big-diffs))
          (iter (with l* := (length lcontext))
                (with length-a* := l*) (with length-b* := l*)
                (with some-edits := nil)
                (with hunk* := (make-instance 'hunk
                                 :start-a (- start-a* l*)
                                 :start-b (- start-b* l*)))
                (with diffs* := '())
                (until (or (endp big-diffs)
                           (>= length-a* (- size *patch-margin*))))
                (for (op x) := (pop big-diffs))
                (for l := (length x))
                (initially
                  (unless (empty lcontext) (push `(:= ,lcontext) diffs*)))
                (cond
                  ((eq op :+)
                     (incf length-b* l) (incf start-b* l)
                     (push `(:+ ,x) diffs*))
                  ((and (eq op :-) (eq (caar diffs*) :=) (> l (* size 2)))
                     (incf length-a* l) (incf start-a* l)
                     (push `(:- ,x) diffs*))
                  (t (dlet* ((p (index (- size length-a* *patch-margin*) l))
                             ((values x1 x2) (split-at x p)))
                       (incf length-a* p) (incf start-a* p)
                       (when (eq op :=)
                         (incf length-b* p) (incf start-b* p))
                       (push `(,op ,x1) diffs*)
                       (unless (empty x2) (push `(,op ,x2) big-diffs)))))
                (if (not (eq op :=)) (setq some-edits t))
                (finally
                  (let ((rcontext (narrow (diff-origin big-diffs)
                                          0 *patch-margin*)))
                    (setq l* (length rcontext))
                    (unless (empty rcontext)
                      (incf length-a* l*)
                      (incf length-b* l*)
                      (when (eq (caar diffs*) :=)
                        (setq rcontext (join* (cadr (pop diffs*)) rcontext)))
                      (push `(:= ,rcontext) diffs*))
                    (setq diffs* (diff-coerce (nreverse diffs*) type)
                          hunk* (copy-hunk hunk* diffs* nil nil
                                           length-a* length-b*)
                          lcontext (narrow (diff-destination diffs*)
                                           (- (+ l* *patch-margin*))
                                           (and (plusp l*) (- l*))))
                    (when some-edits
                      (in :split (collect hunk*)))))))))

(defun apply-hunk (hunk seq delta test)
  "Apply one HUNK to the sequence SEQ with an offset DELTA between expected
   and actual position.  Return three values: (1) a boolean indicating if
   the hunk could be applied; (2) SEQ, or its modified copy; and (3) DELTA,
   possibly modified."
  (with-slots (diffs start-a start-b length-a length-b) hunk
    (let* ((expected-pos (+ start-b delta))
           (orig (diff-origin diffs))
           (l (length orig))
           (big-del (and *max-bits* (> l *max-bits*)))
           (orig1 (if big-del (narrow orig 0 *max-bits*) orig))
           (start (match seq orig1 expected-pos :test test))
           (orig2 (if (and big-del start) (narrow orig (- *max-bits*))))
           (end (and orig2
                     (match seq orig2 (+ expected-pos l (- *max-bits*))
                            :test test)))
           (applicable (and start
                            (or (not big-del) (and end (< start end))))))
      (if applicable
          (flet ((apply-diff (diff pos transl)
                   (dlet* (((op x) diff) (l (length x)) (pos* (+ pos l)))
                     (case op
                       (:= (values seq pos*))
                       (:+ (dlet* (((values pfx sfx)
                                    (split-at seq (funcall transl pos))))
                             (values (join* pfx x sfx) pos*)))
                       (:- (dlet* (((values pfx sfx)
                                    (split-at seq (funcall transl pos)
                                              (funcall transl pos*) t)))
                             (values (join* pfx sfx) pos)))))))
            (let* ((end* (if end (+ end *max-bits*) (+ start l)))
                   (orig* (narrow seq start end*)))
              (if (equal orig* orig)
                  (dlet* (((values pfx sfx) (split-at seq start end* t)))
                    (setq seq (join* pfx (diff-destination diffs) sfx)))
                  (let ((diffs* (diff orig orig* :test test)))
                    (if (and (or (not *max-bits*) (> l *max-bits*))
                             (> (/ (levenshtein diffs*) l)
                                *patch-delete-threshold*))
                        (setq applicable nil)
                        (iter (with diffs** :=
                                (if (eq (diff-seq-type diffs*) 'string)
                                    (cleanup-semantic.shift-lossless diffs*)
                                    diffs*))
                              (with translate :=
                                (lambda (pos)
                                  (+ (translate-position diffs** pos)
                                     start)))
                              (with pos := 0)
                              (for diff :in diffs)
                              (dsetq (values seq pos)
                                     (apply-diff diff pos translate)))))))
            (setq delta (- start expected-pos)))
          (decf delta (- length-b length-a)))
      (values applicable seq delta))))

(defun apply-patch (patch seq &key (padding #'trivial-padding) (test #'eql))
  "Apply PATCH (a list of hunks) to the sequence SEQ.  Return two values:
   (1) SEQ, or its modified copy; and (2) a list of booleans, of the same
   length as PATCH, indicating for every hunk if it could be applied."
  (if (null patch)
      (values seq '())
      (let* ((padding*
               (etypecase padding
                 (sequence padding)
                 (function (funcall padding patch *patch-margin*))))
             (pad-length
               (length padding*)))
        (iter (for hunk :in (split-big-hunks (add-padding patch padding*)))
              (initially
                (setq seq* (join* padding* seq padding*)
                      delta 0))
              (for (values applicable seq* delta) :=
                (apply-hunk hunk seq* delta test))
              (collect applicable :into applied)
              (finally
                (return
                  (values (narrow seq* pad-length (- pad-length))
                          applied)))))))
