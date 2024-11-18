;;;; Quest01.lisp
;;;; 2024 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 18 Nov 2024

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
(use-package :iterate) ; use iter instead of LOOP

(defpackage :quest01
  (:use  #:cl :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :quest01)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file* "~/cl/ec/2024/quest01/input.txt"
  "Downloaded problem set")

#| ----------------------------------------------------------------------------
--- Part One ---

Provided notes:

Ancient Ant (A): Not very dangerous. Can be managed without using any
potion.

Badass Beetle (B): A big and strong bug that requires 1
potion to defeat.

Creepy Cockroach (C): Fast and aggressive! This
creature requires 3 potions to defeat it.

With this knowledge, you must order the exact number of potions that need to be made for your mission.

Example based on the following notes:

ABBAC

Each creature is shown by a single letter, leading to this sequence of battles:

No potions are needed for the first A (Ancient Ant).
1 potion is needed for the first B (Badass Beetle).
1 potion is needed for the second B (Badass Beetle).
No potions are needed for the next A (Ancient Ant).
3 potions are needed for the last monster, C (Creepy Cockroach).

In total, you need to order: 0 + 1 + 1 + 0 + 3 = 5 potions.

What is the exact number of potions that need to be prepared for your battle?

---------------------------------------------------------------------------- |#

(defparameter *example* "ABBAC")

(defun q01-1 (str)
  "sums the potions required by STR when char A = 0, char B = 1, char C = 3"
  (iter (for c in-string str)
    (summing (cond ((equal c #\B) 1)
                   ((equal c #\C) 3)
                   (t 0)))))

(5a:test q01-1-test
  (5a:is (= (q01-1 *example*) 5)))

#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

;; now solve the puzzle!
(time (format t "The answer to EC Quest 01 Part 1 is ~a"
              (q01-1 (uiop:read-file-string *data-file*))))


;; (time (format t "The answer to EC Quest 01 Part 2 is ~a"
;;	      (q01-2 (uiop:read-file-lines *data-file*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M4 Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------
