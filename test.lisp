(cl:defpackage #:diff-match-patch-test
  (:nicknames #:dmp-test)
  (:use #:cl #:fiveam)
  (:export #:run-all))

(in-package #:dmp-test)

(cl-interpol:enable-interpol-syntax)

(def-suite diff-match-patch)

(in-suite diff-match-patch)

(test cleanup-merge
  (flet ((cm (diffs) (dmp::cleanup-merge diffs #'char=)))
    (is-every equal
      ;; Null case
      ((cm '()) '())
      ;; No change case
      ((cm '((:= "a") (:- "b") (:+ "c"))) '((:= "a") (:- "b") (:+ "c")))
      ;; Merge equalities
      ((cm '((:= "a") (:= "b") (:= "c"))) '((:= "abc")))
      ;; Merge deletions
      ((cm '((:- "a") (:- "b") (:- "c"))) '((:- "abc")))
      ;; Merge insertions
      ((cm '((:+ "a") (:+ "b") (:+ "c"))) '((:+ "abc")))
      ;; Merge interweave
      ((cm '((:- "a") (:+ "b") (:- "c") (:+ "d") (:= "e") (:= "f")))
       '((:- "ac") (:+ "bd") (:= "ef")))
      ;; Prefix and suffix detection
      ((cm '((:- "a") (:+ "abc") (:- "dc")))
       '((:= "a") (:- "d") (:+ "b") (:= "c")))
      ;; Prefix and suffix detection with equalities
      ((cm '((:= "x") (:- "a") (:+ "abc") (:- "dc") (:= "y")))
       '((:= "xa") (:- "d") (:+ "b") (:= "cy")))
      ;; Slide edit left
      ((cm '((:= "a") (:+ "ba") (:= "c")))
       '((:+ "ab") (:= "ac")))
      ;; Slide edit right
      ((cm '((:= "c") (:+ "ab") (:= "a")))
       '((:= "ca") (:+ "ba")))
      ;; Slide edit left recursive
      ((cm '((:= "a") (:- "b") (:= "c") (:- "ac") (:= "x")))
       '((:- "abc") (:= "acx")))
      ;; Slide edit right recursive
      ((cm '((:= "x") (:- "ca") (:= "c") (:- "b") (:= "a")))
       '((:= "xca") (:- "cba"))) )))

(test cleanup-semantic-lossless
  (flet ((sl (diffs) (dmp::cleanup-semantic.shift-lossless diffs #'char=)))
    (is-every equal
      ;; Null case
      ((sl '()) '())
      ;; Blank lines
      ((sl '((:= #?"AAA\r\n\r\nBBB") (:+ #?"\r\nDDD\r\n\r\nBBB")
             (:= #?"\r\nEEE")))
       '((:= #?"AAA\r\n\r\n") (:+ #?"BBB\r\nDDD\r\n\r\n")
         (:= #?"BBB\r\nEEE")))
      ;; Line boundaries
      ((sl '((:= #?"AAA\r\nBBB") (:+ #?" DDD\r\nBBB") (:= " EEE")))
       '((:= #?"AAA\r\n") (:+ #?"BBB DDD\r\n") (:= "BBB EEE")))
      ;; Word boundaries
      ((sl '((:= "The c") (:+ "ow and the c") (:= "at.")))
       '((:= "The ") (:+ "cow and the ") (:= "cat.")))
      ;; Alphanumeric boundaries
      ((sl '((:= "The-c") (:+ "ow-and-the-c") (:= "at.")))
       '((:= "The-") (:+ "cow-and-the-") (:= "cat.")))
      ;; Hitting the start
      ((sl '((:= "a") (:- "a") (:= "ax")))
       '((:- "a") (:= "aax")))
      ;; Hitting the end
      ((sl '((:= "xa") (:- "a") (:= "a")))
       '((:= "xaa") (:- "a")))
      ;; Sentence boundaries
      ((sl '((:= "The xxx. The ") (:+ "zzz. The ") (:= "yyy.")))
       '((:= "The xxx.") (:+ " The zzz.") (:= " The yyy."))))))

(test cleanup-semantic
  (flet ((cs (diffs) (dmp::cleanup-semantic diffs #'char=)))
    (is-every equal
      ;; Null case
      ((cs '()) '())
      ;; No elimination #1
      ((cs '((:- "ab") (:+ "cd") (:= "12") (:- "e")))
       '((:- "ab") (:+ "cd") (:= "12") (:- "e")))
      ;; No elimination #2
      ((cs '((:- "abc") (:+ "ABC") (:= "1234") (:- "wxyz")))
       '((:- "abc") (:+ "ABC") (:= "1234") (:- "wxyz")))
      ;; Simple elimination
      ((cs '((:- "a") (:= "b") (:- "c")))
       '((:- "abc") (:+ "b")))
      ;; Backpass elimination
      ((cs '((:- "ab") (:= "cd") (:- "e") (:= "f") (:+ "g")))
       '((:- "abcdef") (:+ "cdfg")))
      ;; Multiple eliminations
      ((cs '((:+ "1") (:= "A") (:- "B") (:+ "2") (:= "_") (:+ "1") (:= "A")
             (:- "B") (:+ "2")))
       '((:- "AB_AB") (:+ "1A2_1A2")))
      ;; Word boundaries
      ((cs '((:= "The c") (:- "ow and the c") (:= "at.")))
       '((:= "The ") (:- "cow and the ") (:= "cat.")))
      ;; No overlap elimination
      ((cs '((:- "abcxx") (:+ "xxdef")))
       '((:- "abcxx") (:+ "xxdef")))
      ;; Overlap elimination
      ((cs '((:- "abcxxx") (:+ "xxxdef")))
       '((:- "abc") (:= "xxx") (:+ "def")))
      ;; Reverse overlap elimination
      ((cs '((:- "xxxabc") (:+ "defxxx")))
       '((:+ "def") (:= "xxx") (:- "abc")))
      ;; Two overlap eliminations
      ((cs '((:- "abcd1212") (:+ "1212efghi") (:= "----") (:- "A3")
             (:+ "3BC")))
       '((:- "abcd") (:= "1212") (:+ "efghi") (:= "----") (:- "A") (:= "3")
         (:+ "BC"))))))

(test cleanup-for-efficiency
  (flet ((ce (diffs &key (cost 4))
           (let ((dmp:*diff-edit-cost* cost))
             (dmp::cleanup-for-efficiency diffs #'char=))))
    (is-every equal
      ;; Null case
      ((ce '())
       '())
      ;; No elimination
      ((ce '((:- "ab") (:+ "12") (:= "wxyz") (:- "cd") (:+ "34")))
       '((:- "ab") (:+ "12") (:= "wxyz") (:- "cd") (:+ "34")))
      ;; Four-edit elimination
      ((ce '((:- "ab") (:+ "12") (:= "xyz") (:- "cd") (:+ "34")))
       '((:- "abxyzcd") (:+ "12xyz34")))
      ;; Three-edit elimination
      ((ce '((:+ "12") (:= "x") (:- "cd") (:+ "34")))
       '((:- "xcd") (:+ "12x34")))
      ;; Backpass elimination
      ((ce '((:- "ab") (:+ "12") (:= "xy") (:+ "34") (:= "z")
             (:- "cd") (:+ "56")))
       '((:- "abxyzcd") (:+ "12xy34z56")))
      ;; High cost elimination
      ((ce '((:- "ab") (:+ "12") (:= "wxyz") (:- "cd") (:+ "34"))
           :cost 5)
       '((:- "abwxyzcd") (:+ "12wxyz34"))))))

(test diff-half-match
  (flet ((hm (a b &key (timeout 1))
           (dmp::with-diff-deadline (timeout)
             (multiple-value-list (dmp::half-match a b #'char=)))))
    (is-every equal
      ;; No match
      ((hm "1234567890" "abcdef") '(nil))
      ((hm "12345" "23") '(nil))
      ;; Single match
      ((hm "1234567890" "a345678z") '("12" "a" "345678" "90" "z"))
      ((hm "a345678z" "1234567890") '("a" "12" "345678" "z" "90"))
      ((hm "abc56789z" "1234567890") '("abc" "1234" "56789" "z" "0"))
      ((hm "a23456xyz" "1234567890") '("a" "1" "23456" "xyz" "7890"))
      ;; Multiple matches
      ((hm "121231234123451234123121" "a1234123451234z")
       '("12123" "a" "1234123451234" "123121" "z"))
      ((hm "x-=-=-=-=-=-=-=-=-=-=-=-=" "xx-=-=-=-=-=-=-=")
       '("" "x" "x-=-=-=-=-=-=-=" "-=-=-=-=-=" ""))
      ((hm "-=-=-=-=-=-=-=-=-=-=-=-=y" "-=-=-=-=-=-=-=yy")
       '("-=-=-=-=-=" "" "-=-=-=-=-=-=-=y" "" "y"))
      ;; Non-optimal halfmatch.  Optimal diff would be
      ;; -q+x=H-i+e=lloHe+Hu=llo-Hew+y not -qHillo+x=HelloHe-w+Hulloy
      ((hm "qHilloHelloHew" "xHelloHeHulloy")
       '("qHillo" "x" "HelloHe" "w" "Hulloy")) )))

(test diff-bisect
  (flet ((db (a b &key (timeout 1.0))
           (dmp::with-diff-deadline (timeout)
             (multiple-value-bind (a1 a2 b1 b2) (dmp::diff-bisect a b #'char=)
               (if (and (or a1 a2) (or b1 b2))
                   (nconc (dmp:diff a1 b1 :test #'char=)
                          (dmp:diff a2 b2 :test #'char=))
                   `((:- ,a) (:+ ,b)))))))
    (is (equal (db "cat" "map") '((:- "c") (:+ "m") (:= "a") (:- "t") (:+ "p"))))
    (is (equal (db "cat" "map" :timeout 0) '((:- "cat") (:+ "map"))))))

(test diff-line-mode
  (flet ((str-mul (str factor)
           (loop for n :from factor :above 0
                 for str* := str :then (concatenate 'string str* str)
                 finally (return str*)))
         (ds (a b &key (check 100))
           (let ((dmp:*diff-check-lines-length* check))
             (dmp:diff a b :test #'char=)))
         (rebuild (diffs)
           (loop for (op x) :in diffs
                 unless (eq op :+) collect x :into a
                 unless (eq op :-) collect x :into b
                 finally (return
                           (cons (apply #'concatenate 'string a)
                                 (apply #'concatenate 'string b))))))
    (is-every equal
      ;; Simple line-mode
      ((ds #1=(str-mul #2=#?"1234567890\n" 13)
           #3=(str-mul #4=#?"abcdefghij\n" 13))
       (ds #1# #3# :check nil))
      ;; Single line-mode
      ((ds #5=(str-mul "1234567890" 13)
           #6=(str-mul "abcdefghij" 13))
       (ds #5# #6# :check nil))
      ;; Overlap line-mode
      ((rebuild
         (ds #2#
             #7=(concatenate 'string
                  #4# #2# #2# #2# #4# #2# #2# #2# #4# #2# #2# #2# #4#)))
       (rebuild (ds #2# #7# :check nil))))))

(test diff-main
  (flet ((ds (a b &key (timeout 1.0))
           (let ((dmp:*diff-timeout* timeout))
             (dmp:diff a b :test #'char=))))
    (is-every equal
      ;; Null case
      ((ds "" "") '())
      ;; Equality
      ((ds "abc" "abc")
       '((:= "abc")))
      ;; Simple insertion
      ((ds "abc" "ab123c")
       '((:= "ab") (:+ "123") (:= "c")))
      ;; Simple deletion
      ((ds "a123bc" "abc")
       '((:= "a") (:- "123") (:= "bc")))
      ;; Two insertions
      ((ds "abc" "a123b456c")
       '((:= "a") (:+ "123") (:= "b") (:+ "456") (:= "c")))
      ;; Two deletions
      ((ds "a123b456c" "abc")
       '((:= "a") (:- "123") (:= "b") (:- "456") (:= "c")))
      ;; Simple cases
      ((ds "a" "b" :timeout nil)
       '((:- "a") (:+ "b")))
      ((ds "Apples are a fruit." "Bananas are also fruit." :timeout nil)
       '((:- "Apple") (:+ "Banana") (:= "s are a") (:+ "lso") (:= " fruit.")))
      ((ds #?"ax\t" #?"\x{0680}x\x00" :timeout nil)
       '((:- "a") (:+ #?"\x{0680}") (:= "x") (:- #?"\t") (:+ #?"\x00")))
      ;; Overlaps
      ((ds "1ayb2" "abxab" :timeout nil)
       '((:- "1") (:= "a") (:- "y") (:= "b") (:- "2") (:+ "xab")))
      ((ds "abcy" "xaxcxabc" :timeout nil)
       '((:+ "xaxcx") (:= "abc") (:- "y")))
      ((ds "ABCDa=bcd=efghijklmnopqrsEFGHIJKLMNOefg" "a-bcd-efghijklmnopqrs"
           :timeout nil)
       '((:- "ABCD") (:= "a") (:- "=") (:+ "-") (:= "bcd") (:- "=") (:+ "-")
         (:= "efghijklmnopqrs") (:- "EFGHIJKLMNOefg")))
      ;; Large equality
      ((ds "a [[Pennsylvania]] and [[New" " and [[Pennsylvania]]"
           :timeout nil)
       '((:+ " ") (:="a") (:+"nd") (:=" [[Pennsylvania]]")
         (:-" and [[New"))) )))

(test diff-timeout
  (flet ((str-expn (str pow)
           (loop for n :from pow :downto 0
                 for str* := str :then (concatenate 'string str* str*)
                 finally (return str*)))
         (timed-diff (a b test)
           (let* ((t0 (dmp::get-internal-seconds))
                  (d (dmp:diff a b :test test))
                  (t1 (dmp::get-internal-seconds)))
             (values (- t1 t0) d))))
    (let ((dmp:*diff-timeout* 0.1)
          (a (str-expn #?"`Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe:\nAll mimsy were the borogoves,\nAnd the mome raths outgrabe.\n" 10))
          (b (str-expn #?"I am the very model of a modern major general,\nI've information vegetable, animal, and mineral,\nI know the kings of England, and I quote the fights historical,\nFrom Marathon to Waterloo, in order categorical.\n" 10)))
      (is (< dmp:*diff-timeout*
             (timed-diff a b #'char=)
             (* dmp:*diff-timeout* 3))))))

(test diff-ex1
  ;; A diff between two versions of King Lear's soliloquy (1st and 2nd
  ;; Quarto).  We take CHAR-EQUAL for test function to gloss over uc/lc
  ;; differences, most of which are due to changes in line breaks.

  (multiple-value-bind (q1 q2 d) (values

"Blow wind & cracke your cheekes, rage, blow
You caterickes, & Hircanios spout til you haue drencht,
The steeples drown'd the cockes, you sulpherous and
Thought executing fires, vaunt-currers to
Oke-cleauing thunderboults, singe my white head,
And thou all shaking thunder, smite flat
The thicke Rotunditie of the world, cracke natures
Mold, all Germains spill at once that make
Ingratefull man.
"

"Blow windes, & crack your cheeks; Rage, blow
You Cataracts, and Hyrricano's spout,
Till you haue drench'd our Steeples, drown the Cockes.
You Sulph'rous and Thought-executing Fires,
Vaunt-curriors of Oake-cleauing Thunder-bolts,
Sindge my white head. And thou all-shaking Thunder,
Strike flat the thicke Rotundity o'th'world,
Cracke Natures moulds, all germaines spill at once
That makes ingratefull Man.
"

'((:= "Blow wind") (:+ "es,")
  (:= " & crack") (:- "e")
  (:= " your cheek") (:- "e") (:= "s") (:- ",") (:+ ";")
  ;; ----------------
  (:= " rage, blow
You cat") (:- "e") (:+ "a") (:= "r") (:- "i") (:+ "a") (:= "c") (:- "ke") (:+ "t") (:= "s, ")
  (:- "&") (:+ "and")
  (:= " H") (:- "i") (:+ "yr") (:= "r") (:+ "i") (:= "can") (:- "i") (:= "o") (:+ "'")
  (:= "s spout") (:- " ") (:+ ",
")
  ;; ----------------
  (:= "ti") (:+ "l") (:= "l you haue drench") (:- "t,
The") (:+ "'d our")
  (:= " steeples") (:+ ",")
  (:= " drown") (:- "'d")
  (:= " the cockes") (:- ", ") (:+ ".
")
  ;; ----------------
  (:= "you sulph") (:- "e") (:+ "'") (:= "rous and") (:- "
") (:+ " ")
  (:= "Thought") (:- " ") (:+ "-")
  (:= "executing fires,") (:- " ") (:+ "
")
  ;; ----------------
  (:= "vaunt-curr") (:- "e") (:+ "io") (:= "rs ") (:- "t") (:= "o") (:- "
") (:+ "f ")
  (:= "O") (:+ "a") (:= "ke-cleauing thunder") (:+ "-")
  ;; ----------------
  (:= "bo") (:- "u") (:= "lts,") (:- " ") (:+ "
")
  (:= "sin") (:+ "d") (:= "ge my white head") (:- ",
") (:+ ". ")
  ;; ----------------
  (:= "And thou all") (:- " ") (:+ "-")
  (:= "shaking thunder,") (:- " ") (:+ "
")
  ;; ----------------
  (:= "s") (:- "mi") (:= "t") (:+ "rik") (:= "e flat") (:- "
") (:+ " ")
  (:= "The thicke Rotundit") (:- "ie") (:+ "y")
  (:= " o") (:- "f ") (:+ "'") (:= "th") (:- "e ") (:+ "'") (:= "world,") (:- " ") (:+ "
")
  ;; ----------------
  (:= "cracke natures") (:- "
") (:+ " ")
  (:= "Mo") (:+ "u") (:= "ld") (:+ "s")
  (:= ", all Germain") (:+ "e")
  (:= "s spill at once") (:- " ") (:+ "
")
  ;; ----------------
  (:= "that make") (:- "
") (:+ "s ")
  (:= "Ingratefull man.
"))

                               )
    (let ((d* (dmp:diff q1 q2 :test #'char-equal)))
      (is (equal d* d))
      (is (equalp (dmp:diff-origin d*) q1))
      (is (equalp (dmp:diff-destination d*) q2))) ))

(test diff-ex2
  ;; A diff between two DNA sequences from the NCBI GenBank, AF516753 and
  ;; AF521016. Bases are represented as symbols, so EQ is an appropriate
  ;; test function.

  (multiple-value-bind (af516753 af521016 d) (values

    '( g a g g c a g g g c g g a g c t g c g t a c t t t g t c c g c c c g c
       g c g g c c c g t c g c t c g c g c c g c g g c g g g a a a a t c c g
       a c c t g g c c g c g c a c c a c c g c c c c t t c t c g g c c c t c
       c t g c g t t t g c c c a g g g t c g g c c c g c a g t g a t g g a g
       g a g g a g g c g g a g a c c g a g g a g c a g c a g c g a t t c t c
       t t a c c a a c a g a g g c t a a a g g c a g c a g t t c a c t a t a
       c t g t g g g t t g t c t t t g c g a g g a a g t t g c a t t g g a c
       a a a g a g a t g c a g t t c a g c a a a c a g a c c a t t g c g g c
       c a t t t c g g a g c t g a c t t t c c g a c a g t g t g a a a a t t
       t t g c c a a a g a c c t t g a a a t g t t t g c a a g
       c a t a t g c a g g a a g c g g c a g g a a t a a g g a a a a g c a g
       c c t c c t g a c t t t c c t c g c t t g g t g g t t t g a g t g g a
       c c t c c c a g g c c a g t g c c g g g c c c c t c a t a g g a g a g
       g a a g c c c g g g a g g t g g c c a g g c g g c a g g a a g g c g c
       a c c c c c c c a g c a a t c t g c g c g c c g g g a c a g a a t g c
       c c t g c a g g a a c t t c t t c t g g a a g a c c t t c t c c t c c
       t g c a a a t a a a a c c t c a c c c a t g a a t g c t c a c g c a a
       g t g t a a t g a c a g a c c t g a a t a a a a t g t a t t a a g c a
       g c )

    '( g g a g c t c c t g a t a a t g t g t a c t g c g t t a c a t g a t c
       t g t a a a t c g t g g g c t a c g c a t c c t c t a c a c c c g a c
       t g g a a c a c g c t t t g c a c c t a t g c g c a c a c g t g t t t
       c g c g g a g a g t g g t g c t c a g c g t t g t g c c a g g c g c t
       g a a t c a g g c t c t g g g g t g c a g c a g t g a c c t c g g t g
       g t c a t g t c c c c g c g t c g c c c g g c c a c c t c c g c a g a
       g c a a g c t g a g c a g g g g g c t g g c t g g a g g c c g a c g g
       c g g a a t c c c c t c a a c g g a g c g c c g c c a g g g g g c g c
       g c
       g a g g c a g g g c g g a g c t g c g t a c t t t g t c c g c c c g c
       g c g g c c c g t c g c t c g c g c c g c g g c g g g a a a a t c c g
       a c c t g g c c g c g c a c c a c c g c c c c t t c t c g g c c c t c
       c t g c g t t t g c c c a g g g t c g g c c c g c a g t g a t g g a g
       g a g g a g g c g g a g a c c g a g g a g c a g c a g c g a t t c t c
       t t a c c a a c a g a g g c t a a a g g c a g c a g t t c a c t a t a
       c t g t g g g t t g t c t t t g c g a g g a a g t t g c a t t g g a c
       a a a g a g a t g c a g t t c a g c a a a c a g a c c a t t g c g g c
       c a t t t c g g a g c t g a c t t t c c g a c a g t g t g a a a a t t
       t t g c c a a a g a c c t t g a a a t g t t t g c a a g
       a c a t g c g a a a a g a a c c a c a a t t a a c a c t g a a g a t g
       t g a a g c t c t t a g c c a g g a g g a g t a a t t c a c t g
       c a t a t g c a g g a a g c g g c a g g a a t a a g g a a a a g c a g
       c c t c c t g a c t t t c c t c g c t t g g t g g t t t g a g t g g a
       c c t c c c a g g c c a g t g c c g g g c c c c t c a t a g g a g a g
       g a a g c c c g g g a g g t g g c c a g g c g g c a g g a a g g c g c
       a c c c c c c c a g c a a t c t g c g c g c c g g g a c a g a a t g c
       c c t g c a g g a a c t t c t t c t g g a a g a c c t t c t c c t c c
       t g c a a a t a a a a c c t c a c c c a t g a a t g c t c a c g c a a
       g t g t a a t g a c a g a c c t g a a t a a a a t g t a t t a a g c a
       g c
       a g t g a t c t t t c c t c t c c t c c t t c c c a a g t c a t t t g
       a a a a g t g t t t g t t a t t t a a a t t c c a a t a a t g c c c a
       a t a c t g a c g t g t c t t g a g t a a t t t g g a a c c c a a a g
       t g a a g a t c t t t g a t a a a g a t t t t t t t g t g g t t c g a
       c t g g a c t g t g c t g a g t g c g g g c a c t g g g c t t t t c t
       t c t g a t g t t c a t t a t g g t g c t g g g a a g c t c t g t c t
       t t g a t t t a a a a t a a a a t a g c t a a a g g c t a c a c a a t
       t a a g a g t t c a g a a t a a c a t c t t a t t t c a g t t t a t g
       a a t t g a t a t g a a t t g t c t a a t t t a a a a a a t a t t t c
       c c t c a c a t t a a a a g c c c a t t t t t a a c a t c a a a a a a
       a a )

    '((:+ (g g a g c t c c t g a t a a t g t g t a c t g c g t t a c a t g a
           t c t g t a a a t c g t g g g c t a c g c a t c c t c t a c a c c
           c g a c t g g a a c a c g c t t t g c a c c t a t g c g c a c a c
           g t g t t t c g c g g a g a g t g g t g c t c a g c g t t g t g c
           c a g g c g c t g a a t c a g g c t c t g g g g t g c a g c a g t
           g a c c t c g g t g g t c a t g t c c c c g c g t c g c c c g g c
           c a c c t c c g c a g a g c a a g c t g a g c a g g g g g c t g g
           c t g g a g g c c g a c g g c g g a a t c c c c t c a a c g g a g
           c g c c g c c a g g g g g c g c g c))
      (:= (g a g g c a g g g c g g a g c t g c g t a c t t t g t c c g c c c
           g c g c g g c c c g t c g c t c g c g c c g c g g c g g g a a a a
           t c c g a c c t g g c c g c g c a c c a c c g c c c c t t c t c g
           g c c c t c c t g c g t t t g c c c a g g g t c g g c c c g c a g
           t g a t g g a g g a g g a g g c g g a g a c c g a g g a g c a g c
           a g c g a t t c t c t t a c c a a c a g a g g c t a a a g g c a g
           c a g t t c a c t a t a c t g t g g g t t g t c t t t g c g a g g
           a a g t t g c a t t g g a c a a a g a g a t g c a g t t c a g c a
           a a c a g a c c a t t g c g g c c a t t t c g g a g c t g a c t t
           t c c g a c a g t g t g a a a a t t t t g c c a a a g a c c t t g
           a a a t g t t t g c a a g))
      (:+ (a c a t g c g a a a a g a a c c a c a a t t a a c a c t g a a g a
           t g t g a a g c t c t t a g c c a g g a g g a g t a a t t c a c t
           g))
      (:= (c a t a t g c a g g a a g c g g c a g g a a t a a g g a a a a g c
           a g c c t c c t g a c t t t c c t c g c t t g g t g g t t t g a g
           t g g a c c t c c c a g g c c a g t g c c g g g c c c c t c a t a
           g g a g a g g a a g c c c g g g a g g t g g c c a g g c g g c a g
           g a a g g c g c a c c c c c c c a g c a a t c t g c g c g c c g g
           g a c a g a a t g c c c t g c a g g a a c t t c t t c t g g a a g
           a c c t t c t c c t c c t g c a a a t a a a a c c t c a c c c a t
           g a a t g c t c a c g c a a g t g t a a t g a c a g a c c t g a a
           t a a a a t g t a t t a a g c a g c))
      (:+ (a g t g a t c t t t c c t c t c c t c c t t c c c a a g t c a t t
           t g a a a a g t g t t t g t t a t t t a a a t t c c a a t a a t g
           c c c a a t a c t g a c g t g t c t t g a g t a a t t t g g a a c
           c c a a a g t g a a g a t c t t t g a t a a a g a t t t t t t t g
           t g g t t c g a c t g g a c t g t g c t g a g t g c g g g c a c t
           g g g c t t t t c t t c t g a t g t t c a t t a t g g t g c t g g
           g a a g c t c t g t c t t t g a t t t a a a a t a a a a t a g c t
           a a a g g c t a c a c a a t t a a g a g t t c a g a a t a a c a t
           c t t a t t t c a g t t t a t g a a t t g a t a t g a a t t g t c
           t a a t t t a a a a a a t a t t t c c c t c a c a t t a a a a g c
           c c a t t t t t a a c a t c a a a a a a a a)))

                                             )
    (let ((d* (dmp:diff af516753 af521016 :test #'eq)))
      (is (equal d* d))
      (is (equal (dmp:diff-origin d*) af516753))
      (is (equal (dmp:diff-destination d*) af521016))) ))

(test match-alphabet
  (flet ((alphabetp (s bindings)
           (loop with tbl := (dmp::match-bitap.alphabet s #'char=)
                 for (c h) :in bindings
                 always (eql (gethash c tbl) h)
                 finally (return (= (hash-table-count tbl)
                                    (length bindings))))))
    (is (alphabetp "abc" '((#\a 4) (#\b 2) (#\c 1))))
    (is (alphabetp "abcaba" '((#\a 37) (#\b 18) (#\c 8))))))

(test match-bitap
  (flet ((mb (s p pos &key (distance 100) (threshold 0.5))
           (let ((dmp:*match-distance* distance)
                 (dmp:*match-threshold* threshold))
             (dmp::match-bitap s p pos #'char=))))
    (is-every eql
      ;; Exact matches
      ((mb "abcdefghijk" "fgh" 5) 5)
      ((mb "abcdefghijk" "fgh" 0) 5)
      ;; Fuzzy matches
      ((mb "abcdefghijk" "efxhi" 0) 4)
      ((mb "abcdefghijk" "cdefxyhijk" 5) 2)
      ((mb "abcdefghijk" "bxy" 1) nil)
      ;; Overflow
      ((mb "123456789xx0" "3456789x0" 2) 2)
      ((mb "abcdef" "xxabc" 4) 0)
      ((mb "abcdef" "defyy" 4) 3)
      ((mb "abcdef" "xabcdefy" 0) 0)
      ;; Threshold test
      ((mb "abcdefghijk" "efxyhi" 1 :threshold 0.4) 4)
      ((mb "abcdefghijk" "efxyhi" 1 :threshold 0.3) nil)
      ((mb "abcdefghijk" "bcdef" 1 :threshold 0) 1)
      ;; Multiple select
      ((mb "abcdexyzabcde" "abccde" 3) 0)
      ((mb "abcdexyzabcde" "abccde" 5) 8)
      ;; Distance test: strict location
      ((mb "abcdefghijklmnopqrstuvwxyz" "abcdefg" 24 :distance 10) nil)
      ((mb "abcdefghijklmnopqrstuvwxyz" "abcdxxefg" 1 :distance 10) 0)
      ;; Distance test: loose location
      ((mb "abcdefghijklmnopqrstuvwxyz" "abcdefg" 24 :distance 1000) 0))))

(test match-main
  (flet ((ms (s p pos &key (threshold 0.5))
           (let ((dmp:*match-threshold* threshold))
             (dmp:match s p pos :test #'char=))))
    (is-every eql
      ;; Shortcuts
      ((ms "abcdef" "abcdef" 1000) 0)
      ((ms "" "abcdef" 1) nil)
      ((ms "abcdef" "" 3) 3)
      ((ms "abcdef" "de" 3) 3)
      ((ms "abcdef" "defy" 4) 3)
      ((ms "abcdef" "abcdefy" 0) 0)
      ;; Complex match
      ((ms "I am the very model of a modern major general." " that berry " 5
         :threshold 0.7)
       4))))

(test translate-position
  ;; General case
  (is (eql (dmp:translate-position '((:- "a") (:+ "1234") (:= "xyz")) 2) 5))
  ;; Deletion
  (is (eql (dmp:translate-position '((:= "a") (:- "1234") (:= "xyz")) 3) 1)))

(test levenshtein
  ;; Trailing equality
  (is (eql (dmp:levenshtein '((:- "abc") (:+ "1234") (:= "xyz"))) 4))
  ;; Leading equality
  (is (eql (dmp:levenshtein '((:= "xyz") (:- "abc") (:+ "1234"))) 4))
  ;; Middle equality
  (is (eql (dmp:levenshtein '((:- "abc") (:= "xyz") (:+ "1234"))) 7)))

(defun hunkp (h &optional dd a0 b0 la lb)
  (and (typep h 'dmp:hunk)
       (or (null dd) (equal (dmp:hunk-diffs h) dd))
       (or (null a0) (equal (dmp:hunk-start-a h) a0))
       (or (null b0) (equal (dmp:hunk-start-b h) b0))
       (or (null la) (equal (dmp:hunk-length-a h) la))
       (or (null lb) (equal (dmp:hunk-length-b h) lb))))

(test patch-add-context
  (flet ((mkhunk (dd a0 b0 la lb ctx &key (margin 4))
           (let ((dmp:*patch-margin* margin))
             (dmp::make-hunk dd a0 b0 la lb ctx))))
    ;; Base case
    (is (hunkp (mkhunk '((:- "jump") (:+ "somersault")) 20 20 4 10
                       "The quick brown fox jumps over the lazy dog.")
               '((:= "fox ") (:- "jump") (:+ "somersault") (:= "s ov"))
               16 16 12 18))
    ;; Not enough trailing context
    (is (hunkp (mkhunk '((:- "jump") (:+ "somersault")) 20 20 4 10
                       "The quick brown fox jumps.")
               '((:= "fox ") (:- "jump") (:+ "somersault") (:= "s."))
               16 16 10 16))
    ;; Not enough leading context
    (is (hunkp (mkhunk '((:- "e") (:+ "at")) 2 2 1 2
                       "The quick brown fox jumps.")
               '((:= "Th") (:- "e") (:+ "at") (:= " qui"))
               0 0 7 8))
    ;; Ambiguity
    (is (hunkp (mkhunk '((:- "e") (:+ "at")) 2 2 1 2
                       "The quick brown fox jumps.  The quick brown fox crashes.")
               '((:= "Th") (:- "e") (:+ "at") (:= " quick brown fox jumps. "))
               0 0 27 28))))

(test print-patch
  (is (equal (with-output-to-string (s)
               (dmp:print-patch
                 (make-instance 'dmp:hunk
                   :start-a 20 :start-b 21 :length-a 18 :length-b 17
                   :diffs '((:= "jump") (:- "s") (:+ "ed") (:= " over ")
                            (:- "the") (:+ "a") (:= #?"\nlaz")))
                 s))
             #?"@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n")))

(test read-patch
  (flet ((mkpatch (str)
           (with-input-from-string (in str) (dmp:read-patch in)))
         (hunkp0 (patch &rest args)
           (apply #'hunkp (car patch) args)))
    (is (equal (mkpatch "") '()))
    (is (hunkp0 (mkpatch #?"@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n %0Alaz\n")
                '((:= "jump") (:- "s") (:+ "ed") (:= " over ") (:- "the")
                  (:+ "a") (:= #?"\nlaz"))
                20 21 18 17))
    (is (hunkp0 (mkpatch #?"@@ -1 +1 @@\n-a\n+b\n")
                '((:- "a") (:+ "b")) 0 0 1 1))
    (is (hunkp0 (mkpatch #?"@@ -1,3 +0,0 @@\n-abc\n")
                '((:- "abc")) 0 0 3 0))
    (is (hunkp0 (mkpatch #?"@@ -0,0 +1,3 @@\n+abc\n")
                '((:+ "abc")) 0 0 0 3))
    (signals simple-error (mkpatch #?"Bad\nPatch\n"))))

(test read-print-patch
  (flet ((rw (str)
           (let ((p (with-input-from-string (in str) (dmp:read-patch in))))
             (with-output-to-string (out) (dmp:print-patch p out)))))
    (is (equal #1=#?"@@ -21,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n"
               (rw #1#)))
    (is (equal #2=#?"@@ -1,9 +1,9 @@\n-f\n+F\n oo+fooba\n@@ -7,9 +7,9 @@\n obar\n-,\n+.\n tes\n"
               (rw #2#)))))

(test make-patch
  (flet ((mkp (&rest args)
           (let ((p (let ((dmp:*patch-margin* 4))
                      (apply #'dmp:make-patch args))))
             (with-output-to-string (out) (dmp:print-patch p out))))
         (str-mul (str factor)
           (loop for n :from factor :above 0
                 for str* := str :then (concatenate 'string str* str)
                 finally (return str*))))
    (signals simple-error (dmp:make-patch))
    (is (equal (dmp:make-patch "" "") '()))
    (let ((t1 "The quick brown fox jumps over the lazy dog.")
          (t2 "That quick brown fox jumped over a lazy dog."))
      (is (equal (mkp t2 t1 :test #'char=)
                 #?"@@ -1,8 +1,7 @@\n Th\n-at\n+e\n  qui\n@@ -21,17 +21,18 @@\n jump\n-ed\n+s\n  over \n-a\n+the\n  laz\n"))
      (is (equal (mkp t1 t2 :test #'char=)
                 #1=#?"@@ -1,11 +1,12 @@\n Th\n-e\n+at\n  quick b\n@@ -22,18 +22,17 @@\n jump\n-s\n+ed\n  over \n-the\n+a\n  laz\n"))
      (let ((diffs (let ((dmp:*diff-check-lines-length* nil))
                     (dmp:diff t1 t2 :test #'char=))))
        (is (equal (mkp :diffs diffs) #1#))
        (is (equal (mkp t1 :diffs diffs) #1#))
        (is (equal (mkp t1 t2 :diffs diffs) #1#))))
    ;; Character encoding / decoding
    (let ((t1 "`1234567890-=[]\\;',./")
          (t2 "~!@#$%^&*()_+{}|:\"<>?"))
      (is (equal (mkp t1 t2 :test #'char=)
                 #?"@@ -1,21 +1,21 @@\n-%601234567890-=%5B%5D%5C;',./\n+~!@#$%25%5E&*()_+%7B%7D%7C:%22%3C%3E?\n"))
      (is (hunkp (car (dmp:make-patch t1 t2 :test #'char=))
                 '((:- "`1234567890-=[]\\;',./")
                   (:+ "~!@#$%^&*()_+{}|:\"<>?")))))
    ;; Long string with repeats
    (let* ((t1 (str-mul "abcdef" 100))
           (t2 (concatenate 'string t1 #2="123"))
           (l1 (* (1- (ceiling dmp:*max-bits* dmp:*patch-margin*))
                  dmp:*patch-margin*))
           (l2 (+ l1 (length #2#)))
           (i (- (length t1) l1)))
      (is (equal (mkp t1 t2 :test #'char=)
                 #?"@@ -${(1+ i)},${l1} +${(1+ i)},${l2} @@\n ${(subseq t1 i)}\n+123\n")))))

(test add-padding
  (flet ((mkp+p (a b)
           (let ((p (let ((dmp:*patch-margin* 4))
                      (dmp:make-patch a b :test #'char=))))
             (with-output-to-string (out)
               (dmp:print-patch
                 (dmp::add-padding p (dmp::trivial-padding p 4))
                 out)))))
    ;; Both edges full
    (is (equal (mkp+p "" "test")
               #?"@@ -1,8 +1,12 @@\n %01%02%03%04\n+test\n %01%02%03%04\n"))
    ;; Both edges partial
    (is (equal (mkp+p "XY" "XtestY")
               #?"@@ -2,8 +2,12 @@\n %02%03%04X\n+test\n Y%01%02%03\n"))
    ;; Both edges none
    (is (equal (mkp+p "XXXXYYYY" "XXXXtestYYYY")
               #?"@@ -5,8 +5,12 @@\n XXXX\n+test\n YYYY\n"))))

(test split-big-hunks
  (flet ((mkp+sbh (a b)
           (let ((dmp:*max-bits* 32)
                 (dmp:*patch-margin* 4))
             (let ((p (dmp:make-patch a b :test #'char=)))
               (values
                 (with-output-to-string (out)
                   (dmp:print-patch (dmp::split-big-hunks p) out))
                 (with-output-to-string (out)
                   (dmp:print-patch p out)))))))
    (is (equal
          (mkp+sbh "abcdefghijklmnopqrstuvwxyz01234567890"
                   "XabXcdXefXghXijXklXmnXopXqrXstXuvXwxXyzX01X23X45X67X89X0")
          #?"@@ -1,32 +1,46 @@\n+X\n ab\n+X\n cd\n+X\n ef\n+X\n gh\n+X\n ij\n+X\n kl\n+X\n mn\n+X\n op\n+X\n qr\n+X\n st\n+X\n uv\n+X\n wx\n+X\n yz\n+X\n 012345\n@@ -25,13 +39,18 @@\n zX01\n+X\n 23\n+X\n 45\n+X\n 67\n+X\n 89\n+X\n 0\n"))
   (multiple-value-bind (p* p)
        (mkp+sbh "abcdef1234567890123456789012345678901234567890123456789012345678901234567890uvwxyz"
                 "abcdefuvwxyz")
      (is (equal p p*)))
    (is (equal
          (mkp+sbh "1234567890123456789012345678901234567890123456789012345678901234567890"
                   "abc")
          #?"@@ -1,32 +1,4 @@\n-1234567890123456789012345678\n 9012\n@@ -29,32 +1,4 @@\n-9012345678901234567890123456\n 7890\n@@ -57,14 +1,3 @@\n-78901234567890\n+abc\n"))
    (is (equal
          (mkp+sbh "abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1"
                   "abcdefghij , h : 1 , t : 1 abcdefghij , h : 1 , t : 1 abcdefghij , h : 0 , t : 1")
          #?"@@ -2,32 +2,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n@@ -29,32 +29,32 @@\n bcdefghij , h : \n-0\n+1\n  , t : 1 abcdef\n")) ))

(test apply-patch
  (flet ((mkp (a b)
           (let ((dmp:*max-bits* 32)
                 (dmp:*patch-margin* 4))
             (dmp:make-patch a b :test #'char=)))
         (ap (p c &key (match-d 1000) (match-th 0.5) (del-th 0.5))
           (let ((dmp:*match-distance* match-d)
                 (dmp:*match-threshold* match-th)
                 (dmp:*patch-delete-threshold* del-th)
                 (dmp:*max-bits* 32)
                 (dmp:*patch-margin* 4)) 
             (multiple-value-list (dmp:apply-patch p c :test #'char=)))))
    (is-every equalp
      ;; Null case
      ((ap (mkp "" "") "Hello world.")
       '("Hello world." ()))
      ;; Exact match
      ((ap #1=(mkp "The quick brown fox jumps over the lazy dog."
                   "That quick brown fox jumped over a lazy dog.")
           "The quick brown fox jumps over the lazy dog.")
       '("That quick brown fox jumped over a lazy dog." (t t)))
      ;; Partial match
      ((ap #1# "The quick red rabbit jumps over the tired tiger.")
       '("That quick red rabbit jumped over a tired tiger." (t t)))
      ;; Failed match
      ((ap #1# "I am the very model of a modern major general.")
       '("I am the very model of a modern major general." (nil nil)))
      ;; Big delete, small change
      ((ap (mkp "x1234567890123456789012345678901234567890123456789012345678901234567890y"
                "xabcy")
           "x123456789012345678901234567890-----++++++++++-----123456789012345678901234567890y")
       '("xabcy" (t t)))
      ;; Big delete, big change (1)
      ((ap (mkp "p1234567890123456789012345678901234567890123456789012345678901234567890q"
                "pabcq")
           "p12345678901234567890---------------++++++++++---------------12345678901234567890q")
       '("pabc12345678901234567890---------------++++++++++---------------12345678901234567890q"
         (nil t)))
      ;; Big delete, big change (2)
      ((ap (mkp "m1234567890123456789012345678901234567890123456789012345678901234567890n"
                "mabcn")
           "m12345678901234567890---------------++++++++++---------------12345678901234567890n"
           :del-th 0.6)
       '("mabcn" (t t)))
      ;; Compensate for failed patch
      ((ap (mkp "abcdefghijklmnopqrstuvwxyz--------------------1234567890"
                "abcXXXXXXXXXXdefghijklmnopqrstuvwxyz--------------------1234567YYYYYYYYYY890")
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567890"
           :match-th 0.0
           :match-d 0)
       '("ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567YYYYYYYYYY890"
         (nil t))))
    (flet ((pstr (p) (with-output-to-string (out) (dmp:print-patch p out))))
      ;; No side effects
      (let* ((p (mkp "" "test"))
             (before (pstr p))
             (d (ap p ""))
             (after (pstr p)))
        (declare (ignore d))
        (is (equal before after)))
      ;; No side effects with major delete
      (let* ((p (mkp "The quick brown fox jumps over the lazy dog." "Woof"))
             (before (pstr p))
             (d (ap p "The quick brown fox jumps over the lazy dog."))
             (after (pstr p)))
        (declare (ignore d))
        (is (equal before after))))
    (is-every equal
      ;; Edge exact match
      ((ap (mkp "" "test") "") '("test" (t)))
      ;; Near edge exact match
      ((ap (mkp "XY" "XtestY") "XY") '("XtestY" (t)))
      ;; Edge partial match
      ((ap (mkp "y" "y123") "x") '("x123" (t))))))

(cl-interpol:disable-interpol-syntax)

(defun run-all ()
  "Run all DIFF-MATCH-PATCH tests."
  (run! 'diff-match-patch))
