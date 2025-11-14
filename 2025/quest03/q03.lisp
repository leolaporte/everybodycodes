;;;; Quest03.lisp
;;;; 2025 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 07 Nov 2025 at 11:11
;;;; Finished: 8 Nov 2025 at 07:53

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :ec.2025.quest03
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :ec.2025.quest03)


(setf 5a:*run-test-when-defined* t)  ; test as we go
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file1* "~/cl/ec/2025/quest03/input1.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2025/quest03/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2025/quest03/input3.txt"
  "Downloaded problem set")

#| ----------------------------------------------------------------------------
                       Quest 3: The Deepest Fit
                           --- Part One ---

"What is the largest possible set of crates that can be formed from a
given list?"

LEO'S NOTES: This is an interesting problem! (Which I completely
misunderstood and solved before I looked at the examples. D'oh.)
Fortunately the actual problem is much simpler.

1. Parse the string into a list of numbers PARSE-INPUT (reused from
q02)

2. Sort the list from biggest to smallest crate.

3. Take the largest crate (the first crate in the sorted list) and
collect one each of the smaller crates in the list. Get the sum of all
the crates and that's your answer, right?

---------------------------------------------------------------------------- |#

(defparameter *example1* "10,5,1,10,3,8,5,2,2")

(sr:-> parse-input (string) list)
(defun parse-input (input)
  "takes a string of numbers and returns a list of integers - accomodates
negative numbers with -?"
  (mapcar #'parse-integer (re:all-matches-as-strings "-?[0-9]+" input)))

(sr:-> pack-crate (list) list)
(defun pack-crate (crates)
  "given a list of crates return the largest possible combination of
nested crates as a list of crate sizes. A crate can only be nested
inside a larger crate."
  (let* ((sorted-crates (sort crates #'>))  ; sort biggest to smallest
         (top (first sorted-crates))        ; start with largest crate
         (set (list top)))                  ; set of nested crates

    (iter (for c in (rest sorted-crates))
      (when (< c top)    ; can we nest the next one?
        (push c set)     ; yes, add it to set
        (setf top c))    ; now we're looking for the next smallest
      (finally (return (reverse set))))))

(5a:test pack-crate-test
  (5a:is (equalp (list 10 8 5 3 2 1)
                 (pack-crate (parse-input *example1*)))))

(sr:-> quest03-1 (string) fixnum)
(defun quest03-1 (input)
  "given a string that represents a list of crate sizes, find the sum of
the ctate sizes of the maximum number of crates that can be fit into
one another"
  (let ((crates (parse-input input)))
    (apply #'+ (pack-crate crates)))) ; add up crate sizes

(5a:test quest03-1-test
  (5a:is (= 29 (quest03-1 *example1*))))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

LEO's NOTES: I need to find the smallest possible set of exactly 20
crates and then return the size.

What if I do the same as above, but instead of starting from the
largest crate, I start from the smallest, and keep stacking until I
get 20? Won't that be the lowest possible set of 20 crates?

1. Sort crate list from smallest to largest
2. Start building until I get 20 crates.

What am I missing? Apparently nothing. Kinda dumb.

---------------------------------------------------------------------------- |#

(defparameter *example2*
  "4,51,13,64,57,51,82,57,16,88,89,48,32,49,49,2,84,65,49,43,9,13,2,3,75,72,63,48,61,14,40,77")

(sr:-> quest03-2 (string) fixnum)
(defun quest03-2 (input)
  "given a string representing a list of crate sizes, find the set of 20
nested crates with the lowest sum - return the sum"
  (let* ((crates (sort (parse-input input) #'<)) ; ascending sort
         (bottom (first crates))        ; start with the smallest crate
         (stack (list bottom)))         ; our stack, starting with smallest

    (iter (for c in (rest crates))      ; process remaining list
      (when (> c bottom)                ; it's a bigger crate
        (push c stack)                  ; add it
        (setf bottom c))                ; and make it the new bottom
      (when (= 20 (length stack))       ; got 20 - done
        (return-from quest03-2 (apply #'+ stack))))

    (error "Could not make a size 20 stack from input.")))

(5a:test quest03-2-test
  (5a:is (= 781 (quest03-2 *example2*))))

#| ----------------------------------------------------------------------------
                          --- Part Three ---

LEO'S NOTES: How many sets of crates can we make?

The only change from part 1 is that I keep track of unused crates and
pack them until there are none left. I'll modify my original
PACK-CRATE to just return the list of unused crates REMOVE-PACKED.

1. Sort descending
2. Pack (removing used crates from list)
3. repeat until no more crates
4. Count stacks

---------------------------------------------------------------------------- |#

(defparameter *example3* "4,51,13,64,57,51,82,57,16,88,89,48,32,49,49,2,84,65,49,43,9,13,2,3,75,72,63,48,61,14,40,77" )

(sr:-> remove-packed (list) list)
(defun remove-packed (crates)
  "given a list of crates return the list of crates leftover after
optimal packing"
  (let* ((sorted-crates (sort crates #'>))  ; sort biggest to smallest
         (top (first sorted-crates))        ; start with largest crate
         (leftovers nil))                   ; unused crates

    (iter (for c in (rest sorted-crates))
      (if (< c top)            ; can we nest the next one?
          (setf top c)         ; make it the top and continue
          (push c leftovers))  ; nope, save leftovers and cont.
      (finally (return leftovers)))))

(5a:test pack-crate-test
  (5a:is (equalp (list 2 5 10)
                 (remove-packed (parse-input *example1*)))))

(sr:-> quest03-3 (string) fixnum)
(defun quest03-3 (input)
  "given a string representing a list of crate sizes, return the minimum
number of stacks required to use all the crates"
  (let ((crates (parse-input input))
        (stacks 0))

    (iter
      (setf crates (remove-packed crates))
      (incf stacks)
      (when (null crates)
        (return-from quest03-3 stacks)))))

(5a:test quest03-3-test
  (5a:is (= 3 (quest03-3 *example3*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 3 Part 1 is ~a"
              (quest03-1 (read-file-into-string *data-file1*))))

(time (format t "The answer to EC Quest 3 Part 2 is ~a"
              (quest03-2 (read-file-into-string *data-file2*))))

(time (format t "The answer to EC Quest 3 Part 3 is ~a"
              (quest03-3 (read-file-into-string *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with M2 Macbook Air with SBCL
;; ----------------------------------------------------------------------------

;; The answer to EC Quest 3 Part 1 is 2698
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000147 seconds of total run time (0.000047 user, 0.000100 system)
;; 100.00% CPU
;; 0 bytes consed

;; The answer to EC Quest 3 Part 2 is 335
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000133 seconds of total run time (0.000090 user, 0.000043 system)
;; 100.00% CPU
;; 116,064 bytes consed

;; The answer to EC Quest 3 Part 3 is 4248
;; Evaluation took:
;; 0.240 seconds of real time
;; 0.241697 seconds of total run time (0.238883 user, 0.002814 system)
;; [ Real times consist of 0.005 seconds GC time, and 0.235 seconds non-GC time. ]
;; [ Run times consist of 0.005 seconds GC time, and 0.237 seconds non-GC time. ]
;; 100.83% CPU
;; 147,898,528 bytes consed
