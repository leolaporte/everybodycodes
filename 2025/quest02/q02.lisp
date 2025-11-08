;;;; Quest02.lisp
;;;; 2025 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 04 Nov 2025 at 21:29
;;;; Finished: 7 Nov 2025 at 10:48

;; ----------------------------------------------------------------------------
;; Prologue code for setup
;; ----------------------------------------------------------------------------

(defpackage :ec.2025.quest02
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :ec.2025.quest02)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file1* "~/cl/ec/2025/quest02/input1.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2025/quest02/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2025/quest02/input3.txt"
  "Downloaded problem set")

#| ----------------------------------------------------------------------------
                   Quest 2: From Complex to Clarity
                           --- Part One ---

LEO'S NOTES: Why this is just simple arithmetic. I'll create
overloaded arithmetic functions for each of the "complex" operations
then just do the math.

Repeat three times:
1. Start with (0 0)
2. COMPLEX-MULTIPLY it by itself
3. COMPLEX-DIVIDE the result by (10 10)
4. COMPLEX-ADD input to the result

Here's an opportunity to use Serapeum's ~> threading macro (ala
Clojure and Racket). I know it's not stock CL but I think the code is
clearer.

I guess I'll also get in the habit of creating a separate PARSE
function for the provided data.

Also, with the COMPLEX-DIVIDE use TRUNCATE not FLOOR because TRUNCATE
truncates toward zero, which is needed for negative
inputs. Fortunately I wrote tests which revealed the issue. Moral:
Even so-called trivial functions should have tests.

---------------------------------------------------------------------------- |#

(defparameter *example1* "A=[25,9]"
  "provided example data")

(sr:-> parse-input (string) list)
(defun parse-input (input)
  "takes a string in the form A=[25,9] and returns a list of two integers
- accomodates negative numbers with -?"
  (mapcar #'parse-integer (re:all-matches-as-strings "-?[0-9]+" input)))

(sr:-> complex-add (list list) list)
(defun complex-add (pair1 pair2)
  "[X1,Y1] + [X2,Y2] = [X1 + X2, Y1 + Y2]"
  (list
   (+ (first pair1) (first pair2))
   (+ (second pair1) (second pair2))))

(5a:test complex-add-test
  (5a:is (equalp '(3 3) (complex-add '(1 1) '(2 2))))
  (5a:is (equalp '(5 12) (complex-add '(2 5) '(3 7))))
  (5a:is (equalp '(8 4) (complex-add '(-2 5) '(10 -1))))
  (5a:is (equalp '(-4 -6) (complex-add '(-1 -2) '(-3 -4)))))

(sr:-> complex-multiply (list list) list)
(defun complex-multiply (pair1 pair2)
  "[X1,Y1] * [X2,Y2] = [X1 * X2 - Y1 * Y2, X1 * Y2 + Y1 * X2]"
  (list
   (- (* (first pair1) (first pair2))
      (* (second pair1) (second pair2)))
   (+ (* (first pair1) (second pair2))
      (* (second pair1) (first pair2)))))

(5a:test complex-multiply-test
  (5a:is (equalp '(0 4) (complex-multiply '(1 1) '(2 2))))
  (5a:is (equalp '(-29 29) (complex-multiply '(2 5) '(3 7))))
  (5a:is (equalp '(-15 52) (complex-multiply '(-2 5) '(10 -1)))))

(sr:-> complex-divide (list list) list)
(defun complex-divide (pair1 pair2)
  "[X1,Y1] / [X2,Y2] = [X1 / X2, Y1 / Y2] using truncate to ignore the
remainder"

  (list
   (truncate (first pair1) (first pair2))     ; use truncate to round
   (truncate (second pair1) (second pair2)))) ; toward 0 for negative values

(5a:test complex-divide-test
  (5a:is (equalp '(5 6) (complex-divide '(10 12) '(2 2))))
  (5a:is (equalp '(3 2) (complex-divide '(11 12) '(3 5))))
  (5a:is (equalp '(-5 -6) (complex-divide '(-10 -12) '(2 2))))
  (5a:is (equalp '(-3 -2) (complex-divide '(-11 -12) '(3 5)))))

(defun quest02-1 (input)
  "given a string INPUT representing two integers perform the arithmetic
tranformation prescribed in the problem set to produce a result"
  (let ((a (parse-input input))   ; reformat into list of two integers
        (res '(0 0)))             ; starting point as provided

    (dotimes (i 3) ; repeat three times
      (setf res
            (sr:~> res       ; clojure style threading (needle _ is res)
                   (complex-multiply _ _)       ; multiply res by itself
                   (complex-divide _ '(10 10))  ; divide by [10,10]
                   (complex-add _ a))))         ; add input to res

    (format nil "[~a,~a]" (first res) (second res)))) ; reformat to string

(5a:test quest02-1-test
  (5a:is (equalp "[357,862]" (quest02-1 *example1*))))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

LEO'S NOTES: I should be able to use the COMPLEX arithmetic functions
from part 1. But there are quite a few more steps in the process. In
fact, this part seems mainly to be a test to see how well I can read
instructions.

The final result is the number of points to be engraved. (I'll make a
list of the actual engravable points in case I need it for part
3. Also it might be cool to actually draw the design.)

---------------------------------------------------------------------------- |#

(defparameter *example2* "A=[35300,-64910]")

(sr:-> engravable? (list) list)
(defun engravable? (pt)
  "given a point in a 2D space, PT, return PT if it's engravable,
nil if not"
  (let ((res '(0 0)))
    (dotimes (i 100 res) ; returns result if engravable
      (setf res
            (sr:~> res
                   (complex-multiply _ _)
                   (complex-divide _ '(100000 100000))
                   (complex-add _ pt)))
      ;; make sure it's in range
      (when (or (not (< -1000000 (first res)  1000001))
                (not (< -1000000 (second res) 1000001)))
        (return-from engravable? nil))))) ; out of range so bail out

(5a:test engravable?-test
  (5a:is (equalp '(-2520 -5355) (engravable? '(35630 -64880))))
  (5a:is (equalp '(5021 6454) (engravable? '(35630 -64870))))
  (5a:is-false (engravable? '(35460 -64910)))
  (5a:is-false (engravable? '(35470 -64910))))

(sr:-> quest02-2 (string fixnum) fixnum)
(defun quest02-2 (input res)
  "given a top left corner of a grid, and the resolution required, return
the number of engravable points"
  (let* ((top-left (parse-input input)) ; provided top left corner
         (bottom-right (complex-add top-left '(1000 1000)))) ; 1000x1000 pts

    (length ;; for this part, we just need the number of points
     ;; make a list of engravable points
     (iter
       (for y from (second top-left) to (second bottom-right) by res)
       (appending
        (iter
          (for x from (first top-left) to (first bottom-right) by res)
          (when (engravable? (list x y)) ; collect engravable points
            (collect (list x y)))))))))

(5a:test quest02-2-test
  (5a:is (= 4076 (quest02-2 *example2* 10))))

#| ----------------------------------------------------------------------------
                          --- Part Three ---

Leo's NOTES: Same problem, just a finer grid. So go 1 by 1 instead of
10 by 10. I'll modify the function from part 2 to add a second
parameter: resolution. It's gonna be a bit slower, though. (Not too
bad - less than 3 seconds on my M2 Macbook Air.)

---------------------------------------------------------------------------- |#

(defparameter *example3* "A=[35300,-64910]")

(5a:test quest02-2-test
  (5a:is (= 406954 (quest02-2 *example3* 1))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 2 Part 1 is ~a"
              (quest02-1 (read-file-into-string *data-file1*))))

(time (format t "The answer to EC Quest 2 Part 2 is ~a"
	      (quest02-2 (read-file-into-string *data-file2*) 10)))

(time (format t "The answer to EC Quest 2 Part 3 is ~a"
	      (quest02-2 (read-file-into-string *data-file3*) 1)))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------

;; The answer to EC Quest 2 Part 1 is [421180,913532]
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000176 seconds of total run time (0.000168 user, 0.000008 system)
;; 100.00% CPU
;; 522,840 processor cycles
;; 0 bytes consed

;; The answer to EC Quest 2 Part 2 is 683
;; Evaluation took:
;; 0.013 seconds of real time
;; 0.012159 seconds of total run time (0.012159 user, 0.000000 system)
;; [ Real times consist of 0.002 seconds GC time, and 0.011 seconds non-GC time. ]
;; [ Run times consist of 0.001 seconds GC time, and 0.012 seconds non-GC time. ]
;; 92.31% CPU
;; 36,102,030 processor cycles
;; 47,896,960 bytes consed

;; The answer to EC Quest 2 Part 3 is 65623
;; Evaluation took:
;; 1.189 seconds of real time
;; 1.176572 seconds of total run time (1.166274 user, 0.010298 system)
;; [ Real times consist of 0.091 seconds GC time, and 1.098 seconds non-GC time. ]
;; [ Run times consist of 0.097 seconds GC time, and 1.080 seconds non-GC time. ]
;; 98.99% CPU
;; 3,561,179,340 processor cycles
;; 4,705,006,496 bytes consed
