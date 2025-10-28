;;;; Quest###.lisp
;;;; 2024 Everybody Codes solution
;;;; Leo Laporte
;;;; Started:
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------


;; Pre-loaded in .sbclrc:
;; (ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :serapeum :str))
;; (use-package :iterate) ; use iter instead of LOOP

(defpackage :quest###
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :quest###)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file1* "~/cl/ec/2024/quest###/input1.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2024/quest###/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2024/quest###/input3.txt"
  "Downloaded problem set")

#| ----------------------------------------------------------------------------
--- Part One ---

---------------------------------------------------------------------------- |#


#| ----------------------------------------------------------------------------
--- Part Two ---

---------------------------------------------------------------------------- |#

#| ----------------------------------------------------------------------------
--- Part Three ---

---------------------------------------------------------------------------- |#


;; now solve the puzzle!
;; (time (format t "The answer to EC Quest ### Part 1 is ~a"
;;	      (day###-1 (uiop:read-file-lines *data-file1*))))

;; (time (format t "The answer to EC Quest ### Part 2 is ~a"
;;	      (day###-2 (uiop:read-file-lines *data-file2*))))

;; (time (format t "The answer to EC Quest ### Part 3 is ~a"
;;	      (day###-3 (uiop:read-file-lines *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M4 Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------
