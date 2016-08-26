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

;;;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:diff-match-patch)

(declaim (inline get-internal-seconds seq-type make-adjustable-vector
                 narrow empty prefixp suffixp join mapjoin join*
                 revappend* whitespace-char-p newline-char-p)
         (optimize (speed 3) (safety 0)))

(defun get-internal-seconds ()
  (/ (get-internal-real-time)
     (float internal-time-units-per-second)))

(defmacro dlet* (bindings &body body)
  (let ((vars nil))
    (labels ((vars (x)
               (typecase x
                 (symbol (unless (member x '(values nil)) (pushnew x vars)))
                 (cons (progn (vars (car x)) (vars (cdr x)))))))
      (let ((dsetqs
              (iter (for (pattern form) :in bindings)
                    (vars pattern)
                    (collecting `(dsetq ,pattern ,form)))))
        `(let ,vars ,@dsetqs ,@body)))))

(defun seq-type (x)
  (cond ((listp x) 'list)
        ((stringp x) 'string)
        ((vectorp x) 'vector)
        (t (error "Not a sequence"))))

(defun make-adjustable-vector (&optional (size 7))
  (make-array size :fill-pointer 0 :adjustable t))

(defun index (i l)
  (max (min i l) 0))

(defun narrow (seq start &optional end)
  (if start
      (let* ((l (length seq))
             (start* (index (if (minusp start) (+ l start) start) l))
             (end* (index (cond ((null end) l)
                                ((minusp end) (+ l end))
                                (t end))
                          l)))
        (if (and (= start* 0) (= end* l))
            seq
            (subseq seq start* end*)))
      (subseq seq 0 0)))

(defun empty (x)
  (or (null x) (and (not (consp x)) (zerop (length x)))))

(defun prefixp (a b &optional (test #'eql))
  (let ((l (length a)))
    (and (<= l (length b))
         (null (mismatch a b :test test :end2 l)))))

(defun suffixp (a b &optional (test #'eql))
  (let ((l (- (length b) (length a))))
    (and (>= l 0)
         (null (mismatch a b :test test :from-end t :start2 l)))))

(defun split-at (seq position
                 &optional (position2 nil pos2-supplied) (drop-middle nil))
  (if position
      (if pos2-supplied
          (if drop-middle
              (values #1=(subseq seq 0 position)
                      #2=(subseq seq position2))
              (values #1# (subseq seq position position2) #2#))
          (values #1# (subseq seq position)))
      seq))

(defun split-lines (str)
  (iter (with l := (length str))
        (for i :from 0 :to l)
        (with last-break := 0)
        (if (< i l)
            (when (equal (aref str i) #\Newline)
              (let ((br (1+ i)))
                (collect (narrow str last-break br))
                (setq last-break br)))
            (if (< last-break i)
                (collect (narrow str last-break))))))

(defun join (parts)
  (let ((parts* (remove nil parts)))
    (cond ((null parts*) #())
          ((null (cdr parts*)) (car parts*))
          (t (apply #'concatenate (seq-type (car parts*)) parts*)))))

(defun mapjoin (fn &optional parts)
  (let ((mapfn (lambda (parts) (join (map 'list fn parts)))))
    (if parts (mapfn parts) mapfn)))

(defun join* (&rest parts)
  (join parts))

(defun overlap (a b &optional (test #'eql))
  (let ((l (min (length a) (length b))))
    (if (zerop l) 0
        (flet ((oveq (a b l -l)
                 (null (mismatch a b :start1 -l :end2 l :test test))))
          (let ((-l (- (length a) l)))
            (if (oveq a b l -l) l
                (iter (with best := 0)
                      (with l* := 1) (with -l* := (- (length a) l*))
                      (while (< l* l))
                      (for m := (search a b :start1 -l* :end2 l))
                      (until (null m))
                      (incf l* m) (decf -l* m)
                      (when (or (zerop m) (oveq a b l* -l*))
                        (setq best l*) (incf l*) (decf -l*))
                      (finally (return best)))))))))

(defun long-short (a b)
  (if (> (length a) (length b)) (values a b) (values b a)))

(defun revappend* (x &rest yy)
  (revappend x (apply #'list* yy)))

(defun whitespace-char-p (c)
  (member c '(#\Space #\Tab #\Page)))

(defun newline-char-p (c)
  (member c '(#\Newline #\Return #\Linefeed)))

(defun write-char-urlencode (c out)
  (flet ((hexc (i)
           (code-char
            (+ i (if (< i 10)
                     #.(char-code #\0)
                     #.(- (char-code #\A) 10))))))
    (if (or (alphanumericp c) (char> c #.(code-char 127))
            (find c " !#$&'()*+,-./:;=?@_~"))
        (write-char c out)
        (dlet* (((values i j) (truncate (char-code c) 16)))
          (write-char #\% out)
          (write-char (hexc i) out)
          (write-char (hexc j) out)))))

(defun read-char-urldecode (in)
  (flet ((read-hex ()
           (let* ((c (peek-char nil in nil))
                  (i (cond
                       ((not c)
                        nil)
                       ((char<= #\0 c #\9)
                        (- (char-code c) #.(char-code #\0)))
                       ((char<= #\a c #\f)
                        (+ (- (char-code c) #.(char-code #\a)) 10))
                       ((char<= #\A c #\F)
                        (+ (- (char-code c) #.(char-code #\A)) 10)))))
             (and i (values (read-char in nil) i)))))
    (let ((c (read-char in nil)))
      (when c
        (if (char= c #\%)
            (dlet* (((values c1 i1) (read-hex))
                    ((values c2 i2) (and i1 (read-hex))))
              (cond
                (i2 (code-char (logior (ash i1 4) i2)))
                (i1 (unread-char c1 in) c)
                (t c)))
            c)))))
