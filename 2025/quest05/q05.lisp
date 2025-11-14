;;;; Quest05.lisp
;;;; 2025 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 12 Nov 2025 at 13:29
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :ec.2025.quest05
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

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

#| ----------------------------------------------------------------------------
                       Quest 5: Fishbone Order
                           --- Part One ---

LEO'S NOTES: We're building something like a unary tree. I need to
return the digits along the spine.

The only question is how to represent the tree. I could do a simple
list, a list of structs, or a vector of structs. The vector seems
simplest and would be the fastest for adding segments. I'll try that.

I'll create a node structure SEGMENT with SPINE, LEFT, and RIGHT
leaves and a 1D array, FISHBONE, to hold a list of structs.

The rest should be simple.

---------------------------------------------------------------------------- |#

(defparameter *example1* "58:5,3,7,8,9,10,4,5,7,8,8")

(defstruct segment
  "a node in the fishbone tree - a fishbone is a array of segments"
  (spine nil :type t)
  (left nil :type (or null integer))
  (right nil :type (or null integer)))

(sr:-> parse-input (string) list)
(defun parse-input (input)
  "given a string of digits return a list of integers"
  (mapcar #'parse-integer
          (re:all-matches-as-strings "-?[0-9]+" input)))

(sr:-> insert-digit (fixnum array) array)
(defun insert-digit (d fishbone)
  "given a digit, insert it into the first appropriate position on the
fishbone tree, if it can't be added  to existing tree, create a new
segment with the digit as spine, add the segment to the bottom of
fishbone, return modified fishbone"

  ;; first examine all the segments we have so far
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
  fishbone) ; return modified tree


(sr:-> sword-strength (list) fixnum)
(defun sword-strength (digits)
  "given a list of digits representing a sword, return the sword's strength"
  (let ((fishbone (make-array (length digits) ; never any longer than this
                              :initial-element (make-segment)
                              :element-type 'segment
                              :adjustable t
                              :fill-pointer 0)))

    ;; insert digits into the fishbone, extending it as needed
    (iter (for d in digits)
      (setf fishbone (insert-digit d fishbone)))

    ;; collect the spine and return it as a number
    (parse-integer
     (format nil "~{~a~}" ; create string from list of spines
             (iter (for i below (length fishbone))
               (collect (segment-spine (aref fishbone i))))))))

(5a:test sword-strength-test
  (5a:is (= 21296 (sword-strength (parse-input "2,4,1,1,8,2,7,9,8,6"))))
  (5a:is (= 79388 (sword-strength (parse-input "7,9,9,3,8,3,8,8,6,8"))))
  (5a:is (= 46822 (sword-strength (parse-input "4,7,6,9,1,8,3,7,2,2"))))
  (5a:is (= 62555 (sword-strength (parse-input "6,4,2,1,7,4,5,5,5,"))))
  (5a:is (= 2335 (sword-strength (parse-input "2,9,3,8,3,9,5,2,1,4"))))
  (5a:is (= 2977 (sword-strength (parse-input "2,4,9,6,7,4,1,7,6,8")))))

  (sr:-> questo5-1 (string) fixnum)
  (defun quest05-1 (input)
    "given a string of digits, ignoring the first, sort them into a tree
then return the spine"
    (let ((sword (parse-input input)))
      (sword-strength (rest sword))))

(5a:test quest05-1-test
  (5a:is (= 581078 (quest05-1 *example1*))))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

LEO'S NOTES: Same problem as above, just more swords. So calculate the
strengths of all the swords then subtract the weakest from the
strongest for the answer. Easy peasy.

But... I will factor out the SWORD-STRENGTH code in Part 1 to
generalize it, so I can reuse it here. Otherwise it's fairly
straight-forward. (Hmm. I did have a bug. The original SWORD-STRENGTH
used >= for the right side comparison - I wanted to cover all
possibilities - apparently that was a bridge too far. Made it > only
and everything works.)

---------------------------------------------------------------------------- |#

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
(defun quest05-2 (swords)
  "given a list of strings representing swords, calculate the strength of
each and return the difference between the strongest and the weakest"
  (let ((strengths
          (iter (for s in swords)
            (collect (sword-strength (rest (parse-input s)))))))

    (- (apply #'max strengths) (apply #'min strengths))))

(5a:test quest05-2-test
  (5a:is (= 77053 (quest05-2 *example2*))))

#| ----------------------------------------------------------------------------
--- Part Three ---

LEO'S NOTES:

---------------------------------------------------------------------------- |#

(defparameter *example3* )

(defun quest05-3 (input)
  nil
  )

(5a:test quest05-3-test
  (5a:is (= (quest05-3 *example3*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 5 Part 1 is ~a"
              (quest05-1 (read-file-into-string *data-file1*))))

(time (format t "The answer to EC Quest 5 Part 2 is ~a"
              (quest05-2 (uiop:read-file-lines *data-file2*))))


;; (time (format t "The answer to EC Quest 5 Part 3 is ~a"
;;	      (quest05-3 (read-file-into-string *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------
