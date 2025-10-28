;;;; Quest01.lisp
;;;; 2024 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 18 Nov 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------


;; (ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
;; (use-package :iterate) ; use iter instead of LOOP

(defpackage :quest01
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :quest01)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/ec/2024/quest01/input.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2024/quest01/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2024/quest01/input3.txt"
  "Downloaded problem set")

#| ----------------------------------------------------------------------------
--- Part One ---

Ancient Ant (A): 0
Badass Beetle (B): 1
Creepy Cockroach (C): 3

Example:
ABBAC = 5 potions

What is the exact number of potions that need to be prepared for your battle?

LEO's NOTES:

Pretty simple. Refactored to extract POTIONS-NEEDED for parts 2 & 3

---------------------------------------------------------------------------- |#

(defparameter *example* "ABBAC")

(defun potions-needed (bug)
  "given a bug return the number of potions needed to defeat it"
  (case bug
    (#\A 0)
    (#\B 1)
    (#\C 3)
    (#\D 5) ; for part 2
    (#\x 0) ; for part 2
    (otherwise (error "unknown bug"))))

(defun q01-1 (str)
  "sums the potions required by STR when char A = 0, char B = 1, char C = 3"
  (iter (for c in-string str)
    (summing (potions-needed c))))

(5a:test q01-1-test
  (5a:is (= (q01-1 *example*) 5)))

#| ----------------------------------------------------------------------------
--- Part Two ---

Round two begins! A new area awaits, bringing with it a new list of
foes, and a familiar but formidable opponent:

Diabolical Dragonfly (D): A fast and tricky enemy, hard to hit. This
creature requires 5 potions to defeat it.

This time, however, the battles become more challenging. According to
the kingdom's spies, the enemies sometimes join forces in pairs,
making them tougher to defeat.  When two monsters pair up, you will
need one more potion per creature than in a one-on-one fight.

AxBCDDCAxD = 28

What is the exact number of potions you need to order for round two?

LEO'S NOTES: 1) turn the string into a list of character pairs 2) go
through the list pairwise using the new rules (adding 2 to the potion
count when the bugs pair up, x is 0, D is 5)

---------------------------------------------------------------------------- |#

(defparameter *example2* "AxBCDDCAxD")

(defun q01-2 (str)
  "given a string of creatures, return the number of potions needed to
defeat them all"
  (let ((pairs (iter (for (a b) on (coerce str 'list) by #'cddr)
                 (collect (list a b))))) ; convert to list of char pairs

    (iter (for pair in pairs)
      (summing
        (+
         ;; the basic potion score
         (+ (potions-needed (first pair))
            (potions-needed (second pair)))
         ;; plus bonus if any
         (if (member #\x pair)
             ;; single bug - no bonus
             0
             ;; else pair - add bonus
             2))))))

(5a:test q01-2-test
  (5a:is (= 28 (q01-2 *example2*))))

#| ----------------------------------------------------------------------------
--- Part Three ---

Pretty minor change. Now there are groups of one, two, and three, and
new bonuses depending on how many bugs are teaming up. No new bugs
though.

---------------------------------------------------------------------------- |#

(defparameter *example3* "xBxAAABCDxCC")

(defun parse-bugs (str)
  "split a string of chars into a list of groups of three, assumes the string is 0 MOD 3"
  (iter (for (a b c) on (coerce str 'list) by #'cdddr)
    (collect (list a b c))))

(defun q01-3 (str)
  "given a string of creatures, return the number of potions needed to
defeat them all"

  (iter (for triplet in (parse-bugs str))
    (summing
      (+
       ;; the basic potion score
       (+ (potions-needed (first triplet))
          (potions-needed (second triplet))
          (potions-needed (third triplet)))

       ;; plus bonus if any (interesting bug here. I can't use COUNT
       ;; because the ITER macro sees it as one of its keywords, even
       ;; CL:COUNT confuses the macro expand. Hence the use of
       ;; COUNT-IF instead. This was a real head scratcher!)
       (case (count-if (lambda (c) (char= c #\x)) triplet)
         (0 6) ;; triple bugs
         (1 2) ;; double bugs
         (2 0) ;; one bug
         (3 0) ;; no bugs
         (otherwise (error "too many #\x characters")))))))

(5a:test q01-3-test
  (5a:is (= 30 (q01-3 *example3*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 01 Part 1 is ~a"
              (q01-1 (uiop:read-file-string *data-file*))))

(time (format t "The answer to EC Quest 01 Part 2 is ~a"
	      (q01-2 (uiop:read-file-string *data-file2*))))

(time (format t "The answer to EC Quest 01 Part 3 is ~a"
	      (q01-3 (uiop:read-file-string *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M4 Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------

;; The answer to EC Quest 01 Part 1 is 1322
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000105 seconds of total run time (0.000052 user, 0.000053 system)
;; 100.00% CPU
;; 0 bytes consed

;; The answer to EC Quest 01 Part 2 is 5584
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000139 seconds of total run time (0.000094 user, 0.000045 system)
;; 100.00% CPU
;; 123,872 bytes consed
