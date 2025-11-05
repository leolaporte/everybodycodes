;;;; Quest01.lisp
;;;; 2025 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 03 Nov 2025 at 15:48
;;;; Finished: 04 Nov 2025 at 21:21

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :ec.2025.quest01
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :ec.2025.quest01)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file1* "~/cl/ec/2025/quest01/input1.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2025/quest01/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2025/quest01/input3.txt"
  "Downloaded problem set")

#| ----------------------------------------------------------------------------
                           --- Part One ---

LEO'S NOTES: I'm to figure out my name by following the moves in the
third line of the input moving through the first line of the
input (ignore line 2). The only trick is that if a move would take you
off the end of the list, you stop at the end and go to the next
move.

I'll need one function MOVE that gets the current POS and the MOVE and
returns the new POS. Oh and it will need the max legal position,
RANGE-MAX. (Range-min is 0, of course.)

The parsing is so simple I'll put it in the main routine.

---------------------------------------------------------------------------- |#

(defparameter *example1* (list "Vyrdax,Drakzyph,Fyrryn,Elarzris"
                               ""
                               " R3,L2,R3,L1"))

(sr:-> move (fixnum string fixnum) fixnum)
(defun move (pos move range-max)
  "given the position in a range, POS, the next move as a string, MOVE,
and the legal range of positions from 0 to RANGE-MAX, return the next
legal POS - modified to use Alexandria CLAMP"
  (let ((dir (subseq move 0 1))                   ; direction
        (steps (parse-integer (subseq move 1))))  ; number of steps

    (setf pos
          (if (equalp dir "L")
              (- pos steps)
              (+ pos steps)))

    (clamp pos 0 range-max))) ; restrict to (< 0 pos range-max)

(5a:test move-test
  (5a:is (= 3 (move 0 "R3" 3)))
  (5a:is (= 1 (move 3 "L2" 3)))
  (5a:is (= 3 (move 1 "R3" 3)))  ; too far right
  (5a:is (= 0 (move 0 "L1" 3)))) ; too far left

(defun quest01-1 (input)
  "determing my dragon name by moving through the provided name list
using the provided moves"
  (let* ((names (sr:words (first input)))
         (moves (sr:words (third input)))
         (range-max (1- (length names)))
         (pos 0))

    (Iter (for mv in moves)
      (setf pos (move pos mv range-max)))

    (nth pos names)))

(5a:test quest01-1-test
  (5a:is (equalp "Fyrryn" (quest01-1 *example1*))))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

LEO'S NOTES:

OK now it's a circular list. Just need to use mod for the position.

---------------------------------------------------------------------------- |#

(defparameter *example2* (list "Vyrdax,Drakzyph,Fyrryn,Elarzris"
                               ""
                               "R3,L2,R3,L1"))

(sr:-> move-circular (fixnum string fixnum) fixnum)
(defun move-circular (pos move ln)
  "given the position in a range, POS, the next move as a string, MOVE,
and the length of the list, LN, return the next legal POS in a
circular list"
  (let ((dir (subseq move 0 1))                   ; direction
        (steps (parse-integer (subseq move 1))))  ; number of steps

    (mod (+ pos ; current position
            (if (equalp dir "L") (- steps) steps)) ; move (-> or <-)
         ln)))  ; mod length of list

(5a:test move-circular-test
  (5a:is (= 3 (move-circular 0 "R3" 4)))
  (5a:is (= 1 (move-circular 3 "L2" 4)))
  (5a:is (= 0 (move-circular 1 "R3" 4)))
  (5a:is (= 3 (move-circular 0 "L1" 4))))

(defun quest01-2 (input)
  "determine my dragon name by moving through the circular name list,
backward and forward, using the provided moves"
  (let* ((names (sr:words (first input)))
         (moves (sr:words (third input)))
         (ln (length names))
         (pos 0))

    (iter (for mv in moves)
      (setf pos (move-circular pos mv ln)))

    (nth pos names)))

(5a:test quest01-2-test
  (5a:is (equalp "Elarzris" (quest01-2 *example2*))))

#| ----------------------------------------------------------------------------
--- Part Three ---

LEO's NOTES: Now, instead of moving, the move list indicates which
names in the list to swap. Return the 0th name. I can reuse
MOVE-CIRCULAR from part 2 to find the index to the name to swap. Then
do the swap for each move and return the 0th name.

---------------------------------------------------------------------------- |#

(defparameter *example3* (list "Vyrdax,Drakzyph,Fyrryn,Elarzris"
                               ""
                               "R3,L2,R3,L3"))

(sr:-> swap-names (string list) list)
(defun swap-names (move names)
  "given a move string, MOVE, and a list of names, NAMES, swap the name
at index 0 with the name indicated by MOVE, returns the updated list of
names"
  (let* ((new-pos (move-circular 0 move (length names)))
         (old-name (nth 0 names))
         (new-name (nth new-pos names)))

    (setf (nth 0 names) new-name)
    (setf (nth new-pos names) old-name)

    names))

(sr:-> quest01-3 (list) string)
(defun quest01-3 (input)
  "given a list of strings containing a list of dragon names and a list
of moves, perform all the moves and return the 0th dragon names"
  (let* ((names (sr:words (first input)))
         (moves (sr:words (third input))))

    (iter (for mv in moves)
      (setf moves (swap-names mv names)))

    (nth 0 names)))

(5a:test quest01-3-test
  (5a:is (equalp "Drakzyph" (quest01-3 *example3*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 1 Part 1 is ~a"
              (quest01-1 (uiop:read-file-lines *data-file1*))))

(time (format t "The answer to EC Quest 1 Part 2 is ~a"
              (quest01-2 (uiop:read-file-lines *data-file2*))))

(time (format t "The answer to EC Quest 1 Part 3 is ~a"
	      (quest01-3 (uiop:read-file-lines *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------

;; The answer to EC Quest 1 Part 1 is Caeldax
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000132 seconds of total run time (0.000031 user, 0.000101 system)
;; 100.00% CPU
;; 0 bytes consed

;; The answer to EC Quest 1 Part 2 is Zorfal
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000081 seconds of total run time (0.000023 user, 0.000058 system)
;; 100.00% CPU
;; 0 bytes consed

;; The answer to EC Quest 1 Part 3 is Xaralquin
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000056 seconds of total run time (0.000025 user, 0.000031 system)
;; 100.00% CPU
;; 0 bytes consed
