(in-package #:diff-match-patch)

(declaim (optimize (speed 3) (safety 0)))

(defun match (space pattern position &key (test #'eql))
  "Locate the best instance of PATTERN in SPACE near POSITION."
  (cond ((equal pattern space) 0)
        ((empty space) nil)
        (t (let* ((space* (coerce space 'vector))
                  (pattern* (coerce pattern 'vector))
                  (position* (index position (length space*))))
             (if (mismatch pattern* space*
                   :start2 position*
                   :end2 (min (+ position* (length pattern*))
                              (length space*))
                   :test test)
                 (match-bitap space* pattern* position* test)
                 position*)))))

(defun match-bitap.alphabet (pattern test)
  "Compute the alphabet for the Bitap algorithm, as a hash table mapping
   elements of PATTERN to integer hashes."
  (let ((t-test (macrolet ((is-fn (x . ff)
                             `(member (coerce ,x 'function) (list ,@ff))))
                  (cond
                    ((is-fn test #'eq #'eql #'equal #'equalp) test)
                    ((is-fn test #'= #'char= #'string=) #'equal)
                    (t #'equalp)))))
    (iter (for x :in-sequence pattern)
          (for i :upfrom 0)
          (with table := (make-hash-table :test t-test))
          (with l := (length pattern))
          (setf (gethash x table)
                (logior (gethash x table 0) (ash 1 (- l i 1))))
          (finally (return table)))))

(defun match-bitap (space pattern position test)
  "Use the Bitap algorithm to locate the best instance of PATTERN in SPACE
   near POSITION."
  (let* ((slength (length space))
         (plength (length pattern))
         (alphabet (match-bitap.alphabet pattern test)))
    (flet ((score (errors pos)
             ;; Score for a match at POS with the given number of ERRORS.
             (let ((accuracy (/ errors plength))
                   (proximity (abs (- position pos))))
               (if (and *match-distance* (not (zerop *match-distance*)))
                   (+ accuracy (/ proximity *match-distance*))
                   (if (plusp proximity) 1 accuracy))))
           (ash1 (x)
             (logior (ash x 1) 1)))
      (iter (with threshold :=
              (let ((exact-match
                      (or (search pattern space :test test :start2 position)
                          (search pattern space :test test :from-end t
                            :end2 (min (+ position plength) slength)))))
                (if exact-match
                    (min *match-threshold* (score 0 exact-match))
                    *match-threshold*)))
            (with mask := (ash 1 (1- plength)))
            (with best-match-pos := nil)
            (with bin-max := (+ plength slength))
            (for x :in-sequence pattern)
            (for e :upfrom 0)
            (for (values start end) :=
              (iter (with bin-min := 0)
                    (with bin-mid := bin-max)
                    (while (< bin-min bin-mid))
                    (if (<= (score e (+ position bin-mid)) threshold)
                        (setq bin-min bin-mid)
                        (setq bin-max bin-mid))
                    (setq bin-mid (+ (truncate (- bin-max bin-min) 2)
                                     bin-min))
                    (finally
                      (setq bin-max bin-mid)
                      (return
                        (values (max 1 (- position bin-mid -1))
                                (+ (min (+ position bin-mid) slength)
                                   plength))))))
            (for rd :=
              (make-array (+ end 2)
                :element-type 'integer :initial-element 0))
            (for ^rd :previous rd)
            (setf (aref rd (1+ end)) (1- (ash 1 e)))
            (iter (for j :downfrom end)
                  (while (>= j start))
                  (for char-match :=
                    (if (> j slength) 0
                        (gethash (aref space (1- j)) alphabet 0)))
                  (for match :=
                    (if (zerop e)
                        #1=(logand (ash1 (aref rd (1+ j))) char-match)
                        (logior
                          #1#
                          (ash1 (logior #2=(aref ^rd (1+ j)) (aref ^rd j)))
                          #2#)))
                  (setf (aref rd j) match)
                  (unless (zerop (logand match mask))
                    (let ((score (score e (1- j))))
                      (unless (> score threshold)
                        (setq threshold score
                              best-match-pos (1- j))
                        (if (> best-match-pos position)
                            (setq start
                                  (max 1 (- (* position 2) best-match-pos)))
                            (finish))))))
            (when (> (score (1+ e) position) threshold) (finish))
            (finally (return best-match-pos))))))
