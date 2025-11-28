;;;; Quest07.lisp
;;;; 2025 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 25 Nov 2025 at 21:02
;;;; Finished: 27 Nov 2025 at 21:09

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :ec.2025.quest07
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)                      ; regex
   (:sr :serapeum)                      ; utilities
   (:tr :trivia)                        ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :ec.2025.quest07)
(setf 5a:*run-test-when-defined* t)  ; test as we go
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file1* "~/cl/ec/2025/quest07/input1.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2025/quest07/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2025/quest07/input3.txt"
  "Downloaded problem set")

;; ----------------------------------------------------------------------------
;;                               Quest 7: Namegraph
;;                                --- Part One ---
;;
;; LEO'S NOTES: Mostly a parsing problem to start. I'll put the names
;; in a list of strings NAMES and make a hash table of the letter
;; rules, RULES, with the keys being the initial char and the values
;; a list of acceptable following chars.
;;
;; Solving part one will be a matter of walking the names, finding the
;; char in the hash and then using MEMBER to deterimine if the rule
;; has been broken. I'll factor out the predicate ACCEPTABLE? just for
;; fun. Note that the rules are case-sensitive. e.g. in Example1 O has
;; different rules than o.
;;
;; ----------------------------------------------------------------------------

(defparameter *example1* (list "Oronris,Urakris,Oroneth,Uraketh"
                               ""
                               "r > a,i,o"
                               "i > p,w"
                               "n > e,r"
                               "o > n,m"
                               "k > f,r"
                               "a > k"
                               "U > r"
                               "e > t"
                               "O > r"
                               "t > h"))

(sr:-> parse-input (list) (values list hash-table))
(defun parse-input (input)
  "given a list of strings, return two values: a list of name strings,
and a hash-table of rules with the keys being the initial char and the
values being a list of chars that are acceptable to follow that char"
  (let ((names (sr:words (first input))) ; turn first line into list of names
        (rules (make-hash-table :test 'equal))) ; a hash of acceptable letters
    (iter (for rule in (rest (rest input)))
      (let ((chars (mapcar #'(lambda (c) (char c 0)) (sr:words rule))))
        (setf (gethash (first chars) rules) (rest chars))))
    (values names rules)))

(sr:-> acceptable? (standard-char standard-char  hash-table) boolean)
(defun acceptable? (curr nxt rules)
  "given a current char, a candidate next char, and a hash table of
rules, return t if the nxt char is acceptable (note: curr and nxt are
standard-chars not strings)"
  (consp (member nxt (gethash curr rules) :test #'char=)))

(5a:test acceptable?-test
  (multiple-value-bind (names rules) (parse-input *example1*)
    (declare (ignore names))
    (5a:is-true (acceptable? #\U #\r rules))
    (5a:is-true (acceptable? #\r #\o rules))
    (5a:is-true (acceptable? #\O #\r rules))
    (5a:is-false (acceptable? #\o #\r rules))))

(sr:-> name-checks-out? (string hash-table) boolean)
(defun name-checks-out? (name rules)
  "returns t if a name follows all the provided rules"
  (let ((len (length name)))
    (iter (for i below len)
      (always (if (>= (1+ i) len)
                  t                     ; end of string without error
                  (acceptable? (char name i) (char name (1+ i)) rules))))))

(5a:test name-checks-out?-test
  (multiple-value-bind (names rules) (parse-input *example1*)
    (5a:is-false (name-checks-out? (first names) rules))
    (5a:is-false (name-checks-out? (second names) rules))
    (5a:is-true (name-checks-out? (third names) rules))
    (5a:is-false (name-checks-out? (fourth names) rules))))

(sr:-> quest07-1 (list) string)
(defun quest07-1 (input)
  "given a list containing name strings and a list of rules, return the
name that follows all the rules"
  (multiple-value-bind (names rules) (parse-input input)
    (iter (for n in names)
      (when (name-checks-out? n rules)
        (return-from quest07-1 n)))))

(5a:test quest07-1-test
  (5a:is (string= "Oroneth" (quest07-1 *example1*))))

;; ----------------------------------------------------------------------------
;;                                --- Part Two ---
;;
;; "there is more than one name that meets the given criteria, and you
;; have to find them all."
;;
;; LEO'S NOTES: Well the effort of making the predicates above was
;; worth it. Very simple.
;;
;; ----------------------------------------------------------------------------

(defparameter *example2*
  (list "Xanverax,Khargyth,Nexzeth,Helther,Braerex,Tirgryph,Kharverax"
        ""
        "r > v,e,a,g,y"
        "a > e,v,x,r"
        "e > r,x,v,t"
        "h > a,e,v"
        "g > r,y"
        "y > p,t"
        "i > v,r"
        "K > h"
        "v > e"
        "B > r"
        "t > h"
        "N > e"
        "p > h"
        "H > e"
        "l > t"
        "z > e"
        "X > a"
        "n > v"
        "x > z"
        "T > i"))

(sr:-> quest07-2 (list) fixnum)
(defun quest07-2 (input)
  (multiple-value-bind (names rules) (parse-input input)
    (iter (for i below (length names))
      (when (name-checks-out? (nth i names) rules)
        (summing (1+ i))))))

(5a:test quest07-2-test
  (5a:is (= 23 (quest07-2 *example2*))))

;; ----------------------------------------------------------------------------
;;                              --- Part Three ---
;;
;; "Your task is to find all possible unique names that begin with
;; these prefixes and follow the rules. Each name must have at least
;; 7 and at most 11 letters."
;;
;; LEO'S NOTES: It's a tree, with each node containing a number of
;; leaves. Original parsing routine still applies.
;;
;; My base routine, NEXT-NAMES, will take a base name and the rules
;; and return in a list the names that can be created by adding a
;; single character according to the rules. Before using a base name I
;; need to make sure it's a legal name using NAMES-CHECKS-OUT?.
;;
;; The ALL-NAMES routine will take a list of names and will return a
;; list of all the names that can be created by adding a letter
;; according to the rules.
;;
;; Finally, I'll call ALL-NAMES recursively until I either run out of
;; possible extensions or all the names in the list are +max-length+
;; characters long.
;;
;; I'll collect all the names created and return the length of that
;; list.
;;
;; It's a little slow and there are some obvious ways to speed it up -
;; there some duplication in the search since "ABC" and "ABCD" will
;; overlap - I only need to search one. Memoizing the results would
;; save time too, but I'm happy with a 1.5 second solve time.
;; ----------------------------------------------------------------------------

(defparameter *example3* (list "Xaryt"
                               ""
                               "X > a,o"
                               "a > r,t"
                               "r > y,e,a"
                               "h > a,e,v"
                               "t > h"
                               "v > e"
                               "y > p,t"))

(defparameter *example4* (list "Khara,Xaryt,Noxer,Kharax"
                               ""
                               "r > v,e,a,g,y"
                               "a > e,v,x,r,g"
                               "e > r,x,v,t"
                               "h > a,e,v"
                               "g > r,y"
                               "y > p,t"
                               "i > v,r"
                               "K > h"
                               "v > e"
                               "B > r"
                               "t > h"
                               "N > e"
                               "p > h"
                               "H > e"
                               "l > t"
                               "z > e"
                               "X > a"
                               "n > v"
                               "x > z"
                               "T > i"))

(defconstant +min-length+ 7)
(defconstant +max-length+ 11)

(sr:-> next-names (string hash-table) list)
(defun next-names (name rules)
  "given a name string, return a list of all the possible names that can
be created by adding a single letter following the rules"
  (let ((key (last-elt name)))
    (iter (for c in (gethash key rules))
      (collecting (format nil "~a~a" name c)))))

(5a:test next-names-test
  (multiple-value-bind (names rules) (parse-input *example3*)
    (5a:is (equal (next-names (first names) rules) (list "Xaryth")))))

(sr:-> all-next-names (list hash-table) list)
(defun all-next-names (names rules)
  "given a list of names, return a list of all the possible names that
can be created by adding a single letter while following the rules"
  (flatten (iter (for n in names)
             (collecting (next-names n rules)))))

(5a:test all-next-names-test
  (multiple-value-bind (names rules) (parse-input *example3*)
    (5a:is (equal (all-next-names names rules) (list "Xaryth")))))

(sr:-> all-names (list hash-table list) list)
(defun all-names (names rules total-names)
  "a recursive function that given a list of names, NAMES, and a
hash-table containing the rules for the next character in the names,
RULES, and the list of names we have so far (starting with all
extensible names from the original list), TOTAL-NAMES, returns the
list of all possible names of length +max-length+ or less"
  (cond ((null names) (reverse total-names))  ; no more names to work on
        (t (setf names (all-next-names names rules))
           (setf total-names (append (reverse names) total-names)) ; save
           (setf names                  ; eliminate completed names
                 (remove-if (lambda (n) (>= (length n) +max-length+)) names))
           (all-names names rules total-names)))) ; recurse

(sr:-> quest07-3 (list) fixnum)
(defun quest07-3 (input)
  (multiple-value-bind (names rules) (parse-input input)
    (sr:~> names
           ;; eliminate names that can't be extended from the start
           (remove-if-not (lambda (n) (name-checks-out? n rules)) _)
           ;; generate all possible names of +max-length+ or less
           (all-names _ rules _)
           ;; eliminate names that are less than +min-length+
           (remove-if (lambda (n) (< (length n) +min-length+)) _)
           ;; eliminate names that are longer than +max-length+
           (remove-if (lambda (n) (> (length n) +max-length+)) _)
           ;; only unique names
           (remove-duplicates _ :test #'equal)
           ;; return the total number of names generated
           (length))))

(5a:test quest07-3-test
  (5a:is (= 25 (quest07-3 *example3*)))
  (5a:is (= 1154 (quest07-3 *example4*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 7 Part 1 is ~a"
              (quest07-1 (uiop:read-file-lines *data-file1*))))

(time (format t "The answer to EC Quest 7 Part 2 is ~a"
              (quest07-2 (uiop:read-file-lines *data-file2*))))

(time (format t "The answer to EC Quest 7 Part 3 is ~a"
              (quest07-3 (uiop:read-file-lines *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------

;; The answer to EC Quest 7 Part 1 is Aznarith
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000056 seconds of total run time (0.000050 user, 0.000006 system)
;; 100.00% CPU
;; 167,160 processor cycles
;; 32,752 bytes consed

;; The answer to EC Quest 7 Part 2 is 3026
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000059 seconds of total run time (0.000053 user, 0.000006 system)
;; 100.00% CPU
;; 175,620 processor cycles
;; 0 bytes consed

;; The answer to EC Quest 7 Part 3 is 5306681
;; Evaluation took:
;; 1.575 seconds of real time
;; 1.571635 seconds of total run time (1.495514 user, 0.076121 system)
;; [ Real times consist of 0.414 seconds GC time, and 1.161 seconds non-GC time. ]
;; [ Run times consist of 0.412 seconds GC time, and 1.160 seconds non-GC time. ]
;; 99.81% CPU
;; 4,716,771,090 processor cycles
;; 884,243,808 bytes consed
