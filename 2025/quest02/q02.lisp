;;;; Quest02.lisp
;;;; 2025 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 04 Nov 2025 at 21:29
;;;; Finished:

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

Also, with the COMPLEX-DIVIDE use TRUNCATE not FLOOR because it always
truncates toward zero, which is needed for negative inputs.

---------------------------------------------------------------------------- |#

(defparameter *example1* "A=[25,9]")

(sr:-> parse-input (string) list)
(defun parse-input (input)
  "takes a string in the form A=[25,9] and returns a list of two integers"
  (mapcar #'parse-integer (re:all-matches-as-strings "\\d+" input)))

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
  "[X1,Y1] / [X2,Y2] = [X1 / X2, Y1 / Y2] using floor to ignore the remainder"

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

---------------------------------------------------------------------------- |#

(defparameter *example2* nil)

(defun quest02-2 (input)
  nil
  )

(5a:test quest02-2-test
  (5a:is (= nil (quest02-2 (*example2*)))))

#| ----------------------------------------------------------------------------
--- Part Three ---

---------------------------------------------------------------------------- |#

(defparameter *example3* nil)

(defun quest02-3 (input)
  nil
  )

(5a:test quest02-3-test
  (5a:is (= nil (quest02-3 (*example3*)))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 2 Part 1 is ~a"
              (quest02-1 (read-file-into-string *data-file1*))))

;; (time (format t "The answer to EC Quest 2 Part 2 is ~a"
;;	      (quest02-2 (read-file-into-string *data-file2*))))

;; (time (format t "The answer to EC Quest 2 Part 3 is ~a"
;;	      (quest02-3 (read-file-into-string *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------
