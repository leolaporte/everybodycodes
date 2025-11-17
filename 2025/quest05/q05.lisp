;;;; Quest05.lisp
;;;; 2025 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 12 Nov 2025 at 13:29
;;;; Finished: 17 Nov 2025 at 15:39

;; ----------------------------------------------------------------------------
;;;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :ec.2025.quest05
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)                   ; regex
   (:sr :serapeum)                   ; utilities
   (:tr :trivia)                     ; pattern matching
   (:5a :fiveam)))                   ; testing framework

(in-package :ec.2025.quest05)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file1* "~/cl/ec/2025/quest05/input1.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2025/quest05/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2025/quest05/input3.txt"
  "Downloaded problem set")

;; ----------------------------------------------------------------------------
;;                            Quest 5: Fishbone Order
;;                               --- Part One ---
;; ----------------------------------------------------------------------------

;; LEO'S NOTES: We're building a binary tree. I need to return the
;; digits along the spine.

;; The first question is how to represent the tree. I could do a
;; simple list, a list of structs, or a vector of structs. The vector
;; of structs would be the fastest for adding segments. I'll try that.

;; I'll create a node structure SEGMENT with SPINE, LEFT, and RIGHT
;; leaves and a 1D array, FISHBONE, to hold a list of structs.

;; And (now that I've seen part 3) I'll refactor my code to support a
;; full SWORD record which will include the FISHBONE.

;; ASSESS-SWORD will save all the sword info provided by the sword string
;; from the problem input (including the FISHBONE for part 3) into a SWORD
;; struct.

;; ASSESS-SWORD will build the fishbone using the workhorse function,
;; INSERT-DIGIT.

;; In fact, I can put all the sword stuct work into the parse
;; function. So PARSE-SWORD takes the input string and returns a SWORD
;; struct.
;; ----------------------------------------------------------------------------

(defparameter *example1* "58:5,3,7,8,9,10,4,5,7,8,8"
  "a string of digits describing a sword")

(defstruct sword
  "info for a given sword"
  (id nil :type (or null integer))        ; the sword's ID
  (rank nil :type (or null integer))      ; strength rank (see part 3)
  (strength nil :type (or null integer))  ; strength, calculated from fishbone
  (fishbone nil :type (or null array)))   ; an array of fishbone SEGMENT

(defstruct segment
  "a node in the fishbone tree - a fishbone is a array of segments"
  (spine nil :type t)
  (left nil :type (or null integer))
  (right nil :type (or null integer)))

;; ----------------------------------------------------------------------------

(sr:-> insert-digit (fixnum array) array)
(defun insert-digit (d fishbone)
  "given a digit, insert it into the first appropriate position on the
fishbone tree, if it can't be added  to existing tree, create a new
segment with the digit as spine, add the segment to the bottom of
fishbone, return modified fishbone"

  ;; first examine all the segments we have so far and place the digit
  ;; if we can
  (iter (for i below (length fishbone))
    (let* ((current-segment (aref fishbone i))
           (spine (segment-spine current-segment))
           (left (segment-left current-segment))
           (right (segment-right current-segment)))

      (cond
        ;; d is less than spine and left is empty
        ((and (null left) (< d spine))
         (setf (segment-left current-segment) d)
         (return-from insert-digit fishbone))

        ;; d is greater than spine and right is empty
        ((and (null right) (> d spine))
         (setf (segment-right current-segment) d)
         (return-from insert-digit fishbone)))))

  ;; couldn't place it, so add a segment
  (vector-push-extend (make-segment :spine d) fishbone)

  fishbone)  ; return modified tree


(sr:-> assess-sword (list) sword)
(defun assess-sword (digits)
  "given a list of integers describing a sword, return a sword record
including id, strength, and full fishbone. (I'll need that for part
3.)"
  (let ((s (make-sword)) ; an empty sword struc
        (fb (make-array 0
                        :initial-element (make-segment)
                        :element-type 'segment
                        :adjustable t
                        :fill-pointer 0)))

    ;; save sword id
    (setf (sword-id s) (first digits))

    ;; build fishbone
    (iter (for d in (rest digits))
      (setf fb (insert-digit d fb)))

    ;; save fishbone
    (setf (sword-fishbone s) fb)

    ;; save strength
    (setf (sword-strength s)
          (parse-integer
           (format nil "~{~a~}"    ; create string from list of spines
                   (iter (for i below (length fb))
                     (collect (segment-spine (aref fb i)))))))

    ;; return sword record
    s))

(sr:-> parse-sword (string) sword)
(defun parse-sword (input)
  "given a string of digits describing a sword, return a SWORD structure"
  (sr:~> input
         (re:all-matches-as-strings "-?[0-9]+" _)  ; extract digit strings
         (mapcar #'parse-integer _)                ; convert to integers
         (assess-sword _)))                        ; build a sword struct

(sr:-> quest05-1 (string) fixnum)
(defun quest05-1 (input)
  "given a string of digits build a SWORD record, then return the
SWORD-STRENGTH"
  (sword-strength (parse-sword input)))

(5a:test quest05-1-test
  (5a:is (= 581078 (quest05-1 *example1*))))

;; ----------------------------------------------------------------------------
;;                                --- Part Two ---
;;
;; LEO'S NOTES: Same problem as above, just more swords. So calculate the
;; strengths of all the swords then subtract the weakest from the
;; strongest for the answer. Easy peasy.
;;
;; ----------------------------------------------------------------------------


(defparameter *example2* (list "1:2,4,1,1,8,2,7,9,8,6"
                               "2:7,9,9,3,8,3,8,8,6,8"
                               "3:4,7,6,9,1,8,3,7,2,2"
                               "4:6,4,2,1,7,4,5,5,5,8"
                               "5:2,9,3,8,3,9,5,2,1,4"
                               "6:2,4,9,6,7,4,1,7,6,8"
                               "7:2,3,7,6,2,2,4,1,4,2"
                               "8:5,1,5,6,8,3,1,8,3,9"
                               "9:5,7,7,3,7,2,3,8,6,7"
                               "10:4,1,9,3,8,5,4,3,5,5"))

(sr:-> quest05-2 (list) fixnum)
(defun quest05-2 (sword-strings)
  "given a list of strings representing swords, calculate the strength of
each and return the difference between the strongest and the weakest"
  (let ((strengths
          (iter (for s in sword-strings) ; make a list of all sword strengths
            (collect (sword-strength (parse-sword s))))))

    (- (apply #'max strengths) (apply #'min strengths))))

(5a:test quest05-2-test
  (5a:is (= 77053 (quest05-2 *example2*))))

;; ----------------------------------------------------------------------------
;;                                --- Part Three ---

;; LEO'S NOTES: So now I need to save sword id, strength, and the
;; fishbone itself so that I can resolve ties by checking up the
;; spine.  I can store this in a SWORD struct. I'll build a SWORD for
;; each string in the input. (I ended up refactoring the whole day to
;; build SWORD structs right into the parsing function. Which cleaned
;; up the code a bit, too. )

;; There are three parts to the puzzle:

;; 1. Parse the input into a list of SWORD structs.
;; 2. Rank the swords by strength, breaking ties by checking each level value
;; 3. Multiply each sword identifier by its rank and sum the resulting values.

;; Nothing tricky, just more steps. Well maybe the ranking part is a
;; little tricky. Ahh I've got an idea. I can use the lisp SORT
;; function with a custom predicate including the tiebreak code:
;; >SWORD?

;; ----------------------------------------------------------------------------

(defparameter *example3* (list "1:7,1,9,1,6,9,8,3,7,2"
                               "2:6,1,9,2,9,8,8,4,3,1"
                               "3:7,1,9,1,6,9,8,3,8,3"
                               "4:6,1,9,2,8,8,8,4,3,1"
                               "5:7,1,9,1,6,9,8,3,7,3"
                               "6:6,1,9,2,8,8,8,4,3,5"
                               "7:3,7,2,2,7,4,4,6,3,1"
                               "8:3,7,2,2,7,4,4,6,3,7"
                               "9:3,7,2,2,7,4,1,6,3,7"))

(defparameter *example4* (list "1:7,1,9,1,6,9,8,3,7,2"
                               "2:7,1,9,1,6,9,8,3,7,2"))

(sr:-> segment->int (segment) fixnum)
(defun segment->int (seg)
  "given a segment structure, convert it to an integer using the digits
from left, spine, and right in order, ignoring any nil values"
  (sr:~>  (list (segment-left seg) (segment-spine seg) (segment-right seg))
          (remove nil _)
          (mapcar #'write-to-string _)
          (apply #'concatenate 'string _)
          (parse-integer _)))

(5a:test segment->int-test
  (5a:is (= 531 (segment->int (make-segment :left 5 :spine 3 :right 1))))
  (5a:is (= 31 (segment->int (make-segment :left nil :spine 3 :right 1))))
  (5a:is (= 51 (segment->int (make-segment :left 5 :spine nil :right 1))))
  (5a:is (= 53 (segment->int (make-segment :left 5 :spine 3 :right nil)))))

(sr:-> >sword? (sword sword) boolean)
(defun >sword? (s1 s2)
  "a custom predicate for SORT. Given two swords, s1 and s2 return t if
s1 > s2 using problem's tie break method:

   1. If two swords have different qualities, a higher quality score
   means a better sword.

   2. If the quality of both swords is the same, the numbers resulting
   from the subsequent levels of the fishbone should be compared,
   starting from the top. A higher score on the first level, which
   differs between swords, indicates a better sword.

   3. If the above conditions are not met, the sword with the higher
   identifier is considered better."

  ;; first compare sword qualities
  (when (not (= (sword-strength s1) (sword-strength s2)))
    (return-from >sword? (> (sword-strength s1) (sword-strength s2))))

  ;; qualities are equal, so let's compare fishbone scores
  (let ((fb1 (sword-fishbone s1))
        (fb2 (sword-fishbone s2)))
    (iter (for seg1 in-vector fb1) (for seg2 in-vector fb2)
      (when (not (= (segment->int seg1) (segment->int seg2)))
        (return-from >sword? (> (segment->int seg1) (segment->int seg2))))))

  ;; it's still tied, so it comes down to which sword has the highest id
  (> (sword-id s1) (sword-id s2)))

(5a:test >sword?-test
  (let ((swords (iter (for s in *example3*) (collect (parse-sword s)))))
    (5a:is-true (>sword? (first swords) (second swords)))
    (5a:is-false (>sword? (first swords) (fifth swords)))
    (5a:is-false (>sword? (first swords) (third swords)))
    (5a:is-true (>sword? (seventh swords) (eighth swords)))))


(sr:-> quest05-3 (list) fixnum)
(defun quest05-3 (input)
  "given a list of strings representing swords, return the sum of the
product of each sword's strength rank and identifier"
  (let ((swords (iter (for s in input) ; make a list of all sword structs
                  (collect (parse-sword s)))))

    (setf swords (sort swords #'>sword?)) ; using the special sword sort

    ;; calculate the sum of the product of rank and id
    (iter (for rank below (length swords))
      (summing (* (sword-id (nth rank swords)) (1+ rank))))))

(5a:test quest05-3-test
  (5a:is (= 260 (quest05-3 *example3*)))
  (5a:is (= 4 (quest05-3 *example4*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 5 Part 1 is ~a"
              (quest05-1 (read-file-into-string *data-file1*))))

(time (format t "The answer to EC Quest 5 Part 2 is ~a"
              (quest05-2 (uiop:read-file-lines *data-file2*))))

(time (format t "The answer to EC Quest 5 Part 3 is ~a"
              (quest05-3 (uiop:read-file-lines *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------

;; The answer to EC Quest 5 Part 1 is 4852675676
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000092 seconds of total run time (0.000036 user, 0.000056 system)
;; 100.00% CPU
;; 62,576 bytes consed

;; The answer to EC Quest 5 Part 2 is 8127437040133
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000818 seconds of total run time (0.000784 user, 0.000034 system)
;; 100.00% CPU
;; 405,104 bytes consed

;; The answer to EC Quest 5 Part 3 is 32197474
;; Evaluation took:
;; 0.004 seconds of real time
;; 0.004337 seconds of total run time (0.004305 user, 0.000032 system)
;; 100.00% CPU
;; 2,358,336 bytes consed
