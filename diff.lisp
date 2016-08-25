(in-package #:diff-match-patch)

(declaim (optimize (speed 3) (safety 0)))

(declaim (ftype (function (list function) list)
                cleanup-merge cleanup-semantic))

(defvar %diff-deadline% nil)

(defmacro with-diff-deadline ((&optional (timeout '*diff-timeout*))
                              &body body)
  (let (($timeout (gensym)) ($current (gensym)))
    `(let* ((,$timeout ,timeout)
            (,$current (get-internal-seconds))
            (%diff-deadline%
              (or %diff-deadline%
                  (and ,$timeout (+ ,$current ,$timeout)))))
       ,@body)))

(defun past-diff-deadline ()
  (and %diff-deadline%
       (>= (get-internal-seconds) %diff-deadline%)))

(defun affix-match (a b test)
  "If A and B are elementwise equal, return either of them.  Otherwise,
   return four values: (1) the common prefix of A and B; (2, 3) the middle
   parts of A and B between the common prefix and the common suffix; and
   (4) the common suffix."
  (let ((prefix-end (mismatch a b :test test)))
    (if (null prefix-end) b
        (let* ((suffix-start-a
                 (mismatch a b :test test :from-end t
                   :start1 prefix-end :start2 prefix-end))
               (suffix-start-b
                 (- (length b) (- (length a) suffix-start-a))))
          (values (narrow b 0 prefix-end)
                  (narrow a prefix-end suffix-start-a)
                  (narrow b prefix-end suffix-start-b)
                  (narrow a suffix-start-a))))))

(defun mapdiff (fn diffs)
  "Return a copy of edit operation list DIFFS in which the content of every
   operation has been replaced by its image under FN."
  (iter (for (op x) :in diffs) (collect `(,op ,(funcall fn x)))))

(defun diff-seq-type (diffs)
  "Return the type of sequences in the elements of DIFFS."
  (seq-type (cadar diffs)))

(defun diff-coerce (diffs type)
  "Coerce the sequences in the elements of DIFFS to TYPE."
  (if (eq (diff-seq-type diffs) type)
      diffs
      (mapdiff (lambda (x) (coerce x type)) diffs)))

(defun diff (a b &key (test #'eql))
  "Find the differences between two sequences A and B of elements comparable
   under the function TEST. Return a list of edit operations of the form (OP
   X), where OP is one of the keywords :+ :- :=, and X is a subsequence of A
   or B."
  (with-diff-deadline ()
    (cond
      ;; Some trivial cases
      ((empty a) (if (empty b) '() `((:+ ,b))))
      ((empty b) `((:- ,a)))
      ((eq a b) `((:= ,b)))
      (t (dlet* (((values prefix a* b* suffix)
                  (affix-match (coerce a 'vector) (coerce b 'vector) test)))
           (if (null a*) `((:= ,prefix))
               (let* ((type (seq-type a))
                      (diffs (cleanup-merge
                               `(,@(unless (empty prefix) `((:= ,prefix)))
                                 ,@(cond
                                     ((empty a*) `((:+ ,b*)))
                                     ((empty b*) `((:- ,a*)))
                                     (t (diff-compute a* b* test)))
                                 ,@(unless (empty suffix) `((:= ,suffix))))
                               test)))
                 (diff-coerce diffs type))))))))

(defun diff-compute (a b test)
  "Find the differences between two sequences A and B of elements comparable
   under TEST, assuming that they don't share a common preffix or suffix and
   that neither is empty."
  (or ;; One input is completely inside the other.
      (dlet* (((values long short) (long-short a b))
              (infix-start (search short long :test test)))
        (cond
          (infix-start
           (let ((prefix (subseq long 0 infix-start))
                 (suffix (subseq long (+ infix-start (length short))))
                 (op (if (eq long a) :- :+)))
             `((,op ,prefix) (:= ,short) (,op ,suffix))))
          ;; Short input of just one element which is not inside long input
          ;; is by definition distinct from any element of the long input
          ((= (length short) 1)
           `((:- ,a) (:+ ,b)))))
      ;; The inputs have a substantial common middle portion which can be
      ;; found very fast: split and recurse.
      (dlet* (((values prefix-a prefix-b middle suffix-a suffix-b)
               (and *diff-timeout* (half-match a b test))))
        (when middle
          `(,@(diff prefix-a prefix-b :test test)
            (:= ,middle)
            ,@(diff suffix-a suffix-b :test test))))
      ;; Line-level diff optimization can be performed.
      (when (and (stringp a) (stringp b) *diff-check-lines-length*
                 (> (length a) *diff-check-lines-length*)
                 (> (length b) *diff-check-lines-length*))
        (diff-line-mode a b test))
      ;; The inputs have a common middle portion computable within the
      ;; deadline: split and recurse.
      (dlet* (((values a1 a2 b1 b2) (diff-bisect a b test)))
        (when (and (or a1 a2) (or b1 b2))
          (nconc (diff a1 b1 :test test)
                 (diff a2 b2 :test test))))
      ;; The fallback case.
      `((:- ,a) (:+ ,b))))

(defun half-match (a b test)
  "If the sequences A and B share a common infix which is at least half the
   length of the longer sequence, return five values: (1, 2) the prefixes of
   A and B up to (3) the common middle, and (4, 5) the suffixes of A and B
   after it.  Otherwise, return NIL."
  (dlet* (((values long short) (long-short a b)))
    (when (>= (* 2 (length short)) (length long) 4)
      (flet ((find-infix (h q)
               (let* ((pivot (truncate (+ (length long) h) q))
                      (pivot* (+ pivot (truncate (length long) 4))))
                 (iter (with infix-start-long := nil)
                       (with infix-start-short := nil)
                       (with best-infix-length := 0)
                       (for j := (search long short :test test
                                   :start1 pivot :end1 pivot*
                                   :start2 (if j (1+ j) 0)))
                       (while j)
                       (for p := (mismatch long short :test test
                                   :start1 pivot :start2 j))
                       (for s := (mismatch long short :test test
                                   :end1 pivot :end2 j :from-end t))
                       (for infix-length := (+ (if p (- p pivot) 0)
                                               (if s (- pivot s) 0)))
                       (when (> infix-length best-infix-length)
                         (setq best-infix-length infix-length
                               infix-start-long s
                               infix-start-short (- j (- pivot s))))
                       (finally
                         (return
                           (if (>= (* best-infix-length 2) (length long))
                               (values infix-start-long infix-start-short
                                       best-infix-length))))))))
        (dlet* (((values sl1 ss1 l1) (find-infix 3 4))
                ((values sl2 ss2 l2) (find-infix 1 2))
                ((values sl ss l) (if (and sl1 (or (not sl2) (> l1 l2)))
                                      (values sl1 ss1 l1)
                                      (values sl2 ss2 l2)))
                ((values sa sb) (if (eq long a) (values sl ss)
                                    (values ss sl))))
          (when l
            (values (subseq a 0 sa) (subseq b 0 sb)
                    (subseq a sa (+ sa l))
                    (subseq a (+ sa l)) (subseq b (+ sb l)))))))))

(defun index-abbreviator ()
  "Return two unary functions: one to abbreviate things to integers, the
   other to restore them from integer abbreviations."
  (let ((table (make-hash-table :test 'equal))
        (untable (make-adjustable-vector)))
    (values (lambda (x)
                 (or #1=(gethash x table)
                     (setf #1# (vector-push-extend x untable))))
            (lambda (index)
              (and (< -1 index (length untable))
                   (aref untable index))))))

(defun diff-line-mode (a b test)
  "Find the differences between the multiline texts A and B by comparing
   them as sequences of lines, then recursively comparing just the
   non-matching line runs by the general method."
  (dlet* (((values abbr unabbr) (index-abbreviator))
          (a* (map 'vector abbr (split-lines a)))
          (b* (map 'vector abbr (split-lines b)))
          (diffs (cleanup-semantic
                   (mapdiff (mapjoin unabbr) (diff a* b* :test #'=))
                   test)))
    (iter (for (values dels adds middle rest) := (next-diffs diffs))
          (setq diffs rest)
          (if (not (and (empty dels) (empty adds)))
              (nconcing (let ((*diff-check-lines-length* nil))
                          (diff dels adds :test test))
                :into diffs*))
          (if middle
              (collect `(:= ,middle) :into diffs*)
              (leave diffs*)))))

(defun diff-bisect (a b test)
  "Use E. Myers' (1986) algorithm to find the ``middle snake'' (equality) of
   an optimal diff of A and B, split the sequences just beyond the snake,
   and return four values: the two parts of A and the two parts of B.  If
   the timeout is hit, or if the two sequences have no common elements,
   return NIL."
  (let* ((la (length a))
         (lb (length b))
         (delta (- la lb))
         (frontp (oddp delta))
         (max-d (truncate (+ la lb 1) 2))
         (lv (* max-d 2))
         (v1 #1=(let ((v (make-array lv
                           :element-type `(integer -1 ,lv)
                           :initial-element -1)))
                  (setf (aref v (1+ max-d)) 0)
                  v))
         (v2 #1#))
    (flet ((d-step (from-end d start end v v* common-affix mirror)
             (iter (for k :from (- start d) :to (- d end) :by 2)
                   (for offset := (+ max-d k))
                   (for x := (if (or (= k (- d))
                                     (and (/= k d)
                                          (< #2=(aref v (1- offset))
                                             #3=(aref v (1+ offset)))))
                                 #3# (1+ #2#)))
                   (for y := (- x k))
                   ;(print `(@@iter :from-end ,from-end :start ,start :end ,end :k ,k :offset ,offset :x ,x :y ,y) *trace-output*)
                   (when (and (< x la) (< y lb))
                     (let ((q (funcall common-affix x y)))
                       (incf x q)
                       (incf y q)))
                   (setf (aref v offset) x)
                   ;(when from-end (print `(@@iter :from-end ,from-end :------ :x ,x :y ,y :frontp ,frontp) *trace-output*))
                   (cond ((> x la) (incf end 2))
                         ((> y lb) (incf start 2))
                         ((not (eq frontp from-end))
                          (let ((offset* (+ max-d delta (- k))))
                            (dlet* (((values x* y* limit)
                                     (and (>= offset* 0) (< offset* lv)
                                          (not (minusp (aref v* offset*)))
                                          (funcall mirror x y offset*))))
                              ;(when from-end (print `(@@iter :from-end ,from-end :------ :offset* ,offset* :x* ,x* :y* ,y* :limit ,limit) *trace-output*))
                              (when (and x* (>= x* limit))
                                (dlet* (((values a1 a2) (split-at a x*))
                                        ((values b1 b2) (split-at b y*)))
                                  (return-from diff-bisect
                                    (values a1 a2 b1 b2))))))))
                   (finally (return (values start end)))))
           (common-prefix (x y)
             (let ((m (mismatch a b :start1 x :start2 y :test test)))
               (- (or m la) x)))
           (common-suffix (x y)
             (let* ((x* (- la x)) (y* (- lb y))
                    (m (mismatch a b :end1 x* :end2 y*
                                     :from-end t :test test)))
               (- x* (or m 0))))
           (mirror-fwd (x y offset)
             (values x y (- la (aref v2 offset))))
           (mirror-back (x y offset)
             (declare (ignore y))
             (let ((x* (aref v1 offset)))
               (values x* (- (+ max-d x*) offset) (- la x)))))
      (iter (for d :from 0 :below max-d)
            (until (past-diff-deadline))
            (with f-start := 0) (with f-end := 0)
            (with b-start := 0) (with b-end := 0)
            (dsetq (values f-start f-end)
                   (d-step nil d f-start f-end v1 v2
                           #'common-prefix #'mirror-fwd))
            (dsetq (values b-start b-end)
                   (d-step t d b-start b-end v2 v1
                           #'common-suffix #'mirror-back))))))

(flet ((filter-concat (diffs exclude)
         (iter (for (op x) :in diffs)
           (unless (eq op exclude) (collect x :into parts))
           (finally
             (let ((type (diff-seq-type diffs)))
               (return (apply #'concatenate type parts)))))))

  (defun diff-origin (diffs)
    "Given a difference list, rebuild the first of the compared sequences."
    (filter-concat diffs :+))

  (defun diff-destination (diffs)
    "Given a difference list, rebuild the second of the compared sequences."
    (filter-concat diffs :-))

)

(defun next-diffs (diffs)
  "Extract all differences from the beginning of the list DIFFS, up to the
   first equality or the end of the list, whichever comes first. Return
   four values: (1) the concatenation of all deleted sequences, (2) the
   concatenation of all added sequences, (3) the equal sequence (or NIL if
   the end of DIFFS was reached), and (4) the rest of DIFFS after the
   equality."
  (iter (for rest :on diffs)
        (for (op x) := (car rest))
        (for equality := (and (eq op :=) x))
        (until equality)
        (case op
          (:+ (collect x :into adds))
          (:- (collect x :into dels)))
        (finally (return (values (join dels) (join adds)
                                 equality (cdr rest))))))

(defun translate-position (diffs position)
  "Given the difference list DIFFS between two sequences and a POSITION in
   the first sequence, return the equivalent position in the second
   sequence."
  (flet ((val (n op excluded) (if (eq op excluded) 0 n)))
    (iter (for (op x) :in diffs)
          (for l := (length x))
          (initially (setq la 0 lb 0))
          (for la :next (incf la (val l op :+)))
          (for lb :next (incf lb (val l op :-)))
          (for ^la :previous la :initially 0)
          (for ^lb :previous lb :initially 0)
          (with deleted := nil)
          (when (> la position)
            (return (if (eq op :-) ^lb (+ ^lb (- position ^la)))))
          (finally
            (return lb)))))

(defun levenshtein (diffs)
  "Given the difference list DIFFS between two sequences, return the
   corresponding Levenshtein distance."
  (iter (for (values dels adds equality rest) := (next-diffs diffs))
        (summing (max (length dels) (length adds)))
        (until (endp (setq diffs rest)))))
