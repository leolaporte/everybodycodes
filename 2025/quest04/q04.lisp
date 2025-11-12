;;;; Quest04.lisp
;;;; 2025 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 08 Nov 2025 at 07:59
;;;; Finished: 12 Nov 2025 at 13:19

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------
(defpackage :ec.2025.quest04
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :ec.2025.quest04)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(Defparameter *data-file1* "~/cl/ec/2025/quest04/input1.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2025/quest04/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2025/quest04/input3.txt"
  "Downloaded problem set")

#| ----------------------------------------------------------------------------
                      Quest 4: Teeth of the Wind
                           --- Part One ---

"How many full turns will the last gear make if the first one turns
exactly 2025 times?"

LEO'S NOTES: Input is a list of strings, first parse it into a list of
integers.

Take the gear ratios of each gear to the next (/ a b) then multiply
them together to get the number of turns the final gear will make
after one turn of the first. Works great when the gears are evenly
divisible. I guess I should ignore fractional turns for the second
example? Ah yes it says "full turns." So I'll just round down (after
taking 2025 turns) if I get a fractional result.

---------------------------------------------------------------------------- |#

(defparameter *example1* (list "128" "64" "32" "16" "8"))
(defparameter *example2* (list "102" "75" "50" "35" "13"))
(defparameter *turns* 2025)

(sr:-> parse-input (list) list)
(defun parse-input (input)
  (mapcar #'parse-integer input)) ; turn strings into integers

(sr:-> gear-ratio (list) real)
(defun gear-ratio (gears)
  "given a list of integers representing the number of teeth on each
gear, return the number of turns the final gear makes for every turn
of the first gear"
  (reduce #'*  ; multiply the ratios together
          (mapcar #'/ gears (rest gears)))) ; get the ratios

(5a:test gear-ratio-test
  (5a:is (= 16 (gear-ratio (parse-input *example1*)))))

(sr:-> quest04-1 (list) fixnum)
(defun quest04-1 (input)
  (let ((gears (parse-input input)))
    (floor (* *turns* (gear-ratio gears))))) ; round down

(5a:test quest04-1-test
  (5a:is (= 32400 (quest04-1 *example1*)))
  (5a:is (= 15888 (quest04-1 *example2*))))

#| ----------------------------------------------------------------------------
                           --- Part Two ---

LEO'S NOTES: Now I need to know how many turns the first gear has to
make to get to 10 trillion turns of the final gear. Except for the
size of that number not a problem. Fortunately Lisp doesn't
care. (Sorry Python.)

---------------------------------------------------------------------------- |#

;; examples are the same as in the previous part

(defparameter *required-turns* 10000000000000)

(sr:-> quest04-2 (list) fixnum)
(defun quest04-2 (input)
  (let ((gears (parse-input input)))
    (ceiling *required-turns* (gear-ratio gears)))) ; round up

(5a:test quest04-2-test
  (5a:is (= 625000000000 (quest04-2 *example1*)))
  (5a:is (= 1274509803922 (quest04-2 *example2*))))

#| ----------------------------------------------------------------------------
                          --- Part Three ---

LEO'S NOTES: The addition of common gears complicates a little -
mostly in my head. The rule is pretty simple.

For gears 1|2 3 4|5 6 7|8 the ratios would be

2/3 3/4 5/6 6/7

example 3 it would be

(* (/ 5 5) (/ 10 10) (/ 20 5))

example 4 is

(* (/ 5 7) (/ 21 18) (/ 36 27) (/ 27 10) (/ 50 10) (/ 50 11)) = 68.18

So the '|' character is, in effect, a break in the chain. I'll create
groups of gears separated by common gears, calculate each group's gear
ratio, then multiply the group ratios together to get the entire
chain's ratio.

---------------------------------------------------------------------------- |#

(defparameter *example3* (list "5" "5|10" "10|20" "5"))
(defparameter *example4* (list "5" "7|21" "18|36" "27|27" "10|50" "10|50" "11"))
(defparameter *turn-count* 100)

(sr:-> parse-common-gears (list) list)
(defun parse-common-gears (input)
  "given a list of strings representing a set of gears and common gears,
return a list of solo and pair integers"
  (iter (for gear in input)
    (collect (mapcar #'parse-integer
                     (re:all-matches-as-strings "\\d+" gear)))))

(sr:-> quest04-3 (list) fixnum)
(defun quest04-3 (input)
  "given a list of integers representing gears (single item lists) and
common gears (multiple item lists) return the number of times the
final gear will turn when the first gear turns *TURN-COUNT* times"
  (let ((gears (parse-common-gears input)) ; the list of gears
        (group '())   ; each group of gears between common gears
        (ratios '())) ; the final gear ratios of each group

    (iter (for g in gears)

      (cond
        ;; its a solo gear - add it to current group
        ((= (length g) 1) (push (car g) group))

        ;; common gear (multiple gears, only the first and last matter)
        ((> (length g) 1)
         (push (first g) group)      ; add first gear to current group
         ;; calculate the group's gear ratio and save it to ratios
         (push (reduce #'/ (reverse group)) ratios)

         ;; start new group with common gear and iterate
         (setf group (last g)))

        ;; should never get here
        (t (error "null gear")))

      ;; return the list of gear ratios
      (finally (return (push (reduce #'/ (reverse group)) ratios))))

    (floor (* *turn-count* (apply #'* ratios)))))

(5a:test quest04-3-test
  (5a:is (= 400 (quest04-3 *example3*)))
  (5a:is (= 6818 (quest04-3 *example4*))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 4 Part 1 is ~a"
              (quest04-1 (uiop:read-file-lines *data-file1*))))

(time (format t "The answer to EC Quest 4 Part 2 is ~a"
              (quest04-2 (uiop:read-file-lines *data-file2*))))

(time (format t "The answer to EC Quest 4 Part 3 is ~a"
	      (quest04-3 (uiop:read-file-lines *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------

;; The answer to EC Quest 4 Part 1 is 13235
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000100 seconds of total run time (0.000036 user, 0.000064 system)
;; 100.00% CPU
;; 0 bytes consed

;; The answer to EC Quest 4 Part 2 is 2918032786886
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000077 seconds of total run time (0.000029 user, 0.000048 system)
;; 100.00% CPU
;; 0 bytes consed

;; The answer to EC Quest 4 Part 3 is 577063285982
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000090 seconds of total run time (0.000049 user, 0.000041 system)
;; 100.00% CPU
;; 0 bytes consed
