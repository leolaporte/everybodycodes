;;;; Quest06.lisp
;;;; 2025 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 17 Nov 2025 at 18:10
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------

(defpackage :ec.2025.quest06
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)       ; regex
   (:sr :serapeum)       ; utilities
   (:tr :trivia)         ; pattern matching
   (:5a :fiveam)))       ; testing framework

(in-package :ec.2025.quest06)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file1* "~/cl/ec/2025/quest06/input1.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2025/quest06/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2025/quest06/input3.txt"
  "Downloaded problem set")

;; ----------------------------------------------------------------------------
;;                          Quest 6: Mentorship Matrix
;;                               --- Part One ---
;;
;; LEO'S NOTES: Knights are capital letters. Novices are lower case. A
;; novice can be paired with any Knight that precedes it in the
;; string. For part one just figure out how many novice-mentor pairs
;; are possible for the letter A.
;;
;; In other words for every capital A, count how many lower case a
;; follow it. Return the total!
;;
;; Obviously it's going to get more complicated later, but no
;; premature optimization.
;;
;; ----------------------------------------------------------------------------

(defparameter *example1* "ABabACacBCbca")

(defparameter *swords* #\A)
(defparameter *archery* #\B)
(defparameter *magic* #\C)

(sr:-> match-mentors (standard-char string) fixnum)
(defun match-mentors (skill matchups)
  "given a skill to teach and a string representing available mentors and
mentees, return the number of possible matchups"
  (do   ;; loop
   ((pair-count 0) ; number of matches so far
    (ptr (position skill matchups) (position skill matchups))) ; skill ptr

   ((or (null ptr)                   ; if no more mentors...
        (< (length matchups) 1))     ; ...or no more string...
    pair-count)                      ; ...return total pairs

    (incf pair-count (count (char-downcase skill)          ; count mentees
                            (subseq matchups (incf ptr)))) ; to right of skill
    (setf matchups (subseq matchups ptr)))) ; repeat with reminder of matchups

(5a:test match-mentors-test
  (5a:is (= 5 (match-mentors *swords* *example1*))))

(sr:-> quest06-1 (string) fixnum)
(defun quest06-1 (input)
  "given a string return the number of possible match-ups of A and a"
  (match-mentors *swords* input))

(5a:test quest06-1-test
  (5a:is (= 5 (quest06-1 *example1*))))

;; ----------------------------------------------------------------------------
;;                                --- Part Two ---
;;
;; LEO'S NOTES: Now do it for all pairs. I'll refactor Part one to
;; create a function, MATCH-MENTORS, which does the same thing but
;; takes the skill as a parameter.
;;
;; ----------------------------------------------------------------------------

(sr:-> quest06-2 (string) fixnum)
(defun quest06-2 (input)
  (+
   (match-mentors *swords* input)
   (match-mentors *archery* input)
   (match-mentors *magic* input)))

(5a:test quest06-2-test
  (5a:is (= 11 (quest06-2 *example1*))))

;; ----------------------------------------------------------------------------
;;                              --- Part Three ---
;;
;; LEO'S NOTES: OK now we've got a challenge.
;;
;; "the novice can only be mentored by knights who are on the list no
;; further than 1000 tents to the left or right of the novice."
;;
;; Now we can go in both directions. So essentially we're counting the
;; mentors that are withing 1000 characters of each mentee. And
;; because the pattern repeates 1000 times we have to count overlaps.
;;
;; The input is 10,000 characters long, so the total string will be 10
;; million characters long. Better not brute force it. But I don't
;; need to. Most of it repeats. In fact, there are three distinct
;; regions. The 998 sections in the middle are identical. The two
;; outliers are the 1000 characters at the beginning of the 10 million
;; characters, and the 1000 chars at the end. The rest are just
;; wrapping around.
;;
;; So I'll make a sparse hash-table, MENTEES, with the keys being a
;; mentees position in the 10K string, and the values being the number
;; of matching MENTORS within 1000 (or *RANGE*) characters.
;;
;; The result will be the total matches in MENTEES times 998 plus the
;; total mateches for the LEFTMOST and RIGHTMOST strings.
;;
;; First the trivial function COUNT-MATCHES which simply counts
;; matches in a string within the given range.
;;
;; Handlling the overlap for all the matches in the middle is a little
;; tricky. I'm going to have to watch for one off errors. I'll write a
;; function: SET-MIDDLE-RANGE that I can test to make sure I'm on the
;; right track. (This is going to take some finger counting!)
;;
;; Once I count the middle ranges (and multiply the counts times the
;; range minus 2) I count the leftmost and rightmost ranges once, add
;; it all up and that's the total number of matches.
                                        ;
;;
;; Oh and a little teensy bug hung me up for a while. The Common Lisp
;; COUNT function treats the end as exclusive. I was thinking it was
;; inclusive. So the one off bit me after all.
;; ----------------------------------------------------------------------------

(defparameter *example2* "AABCBABCABCabcabcABCCBAACBCa")

(defparameter *repeats* 1000)
(defparameter *range* 1000)

(sr:-> count-matches (standard-char string list) fixnum)
(defun count-matches (mentee garrison-map ranges)
  "given a character representing a mentee, MENTEE, a string representing
a map of the garrison, GARRISON-MAP, and a list of start and end
points for the range to search (list can contain one or two ranges),
return the number of mentors within the range(s)"
  (loop for r in ranges    ; iter has a conflct with COUNT - use LOOP
        summing
        (count (char-upcase mentee)
               garrison-map :start (first r) :end (1+ (second r)))))

(5a:test count-matches-test
  (5a:is (= 7 (count-matches #\a *example2*
                             (list (list 0 (1- (length *example2*)))))))
  (5a:is (= 4 (count-matches #\a *example2* (list (list 0 16)))))
  (5a:is (= 0 (count-matches #\a *example2* (list (list 9 16)))))
  (5a:is (= 7 (count-matches #\a *example2* (list (list 0 10) (list 16 27))))))

(sr:-> set-middle-range (fixnum fixnum fixnum) list)
(defun set-middle-range (posn range len)
  "returns a list of starting and ending points for matching, adjusting
for underflow and overflow by wrapping around - only use this for
segments of the map that do not wrap (first and last)"
  (let ((start (- posn range))          ; unadjusted start of range
        (end (+ posn range)))           ; unadjusted end of range

    ;; now compensate for ranges that go off the string
    ;; in these cases I'll return two ranges in the list
    (cond ((< start 0)                           ; underflow
           (list (list 0 end)                    ; first chunk at left
                 (list (+ len start) (1- len)))) ; second chunk at right

          ((> end (1- len))               ; overflow
           (list (list start (1- len))    ; right chunk
                 (list 0 (mod end len)))) ; left chunk

          ;; all within range, so just return unadjusted start and end
          (t (list (list start end))))))  ; all in range

(5a:test set-middle-range-test
  (5a:is (equalp (set-middle-range 11 10 28)
                 (list (list 1 21))))
  (5a:is (equalp (set-middle-range 5 10 28)
                 (list (list 0 15) (list 23 27))))
  (5a:is (equalp (set-middle-range 25 10 28)
                 (list (list 15 27) (list 0 7))))
  (5a:is (equalp (set-middle-range 10 10 28)
                 (list (list 0 20))))
  (5a:is (equalp (set-middle-range 10 1000 10000)
                 (list (list 0 1010) (list 9010 9999))))
  (5a:is (equalp (set-middle-range 9910 1000 10000)
                 (list (list 8910 9999) (list 0 910))))
  (5a:is (equalp (set-middle-range 0 10 100)
                 (list (list 0 10) (list 90 99))))
  (5a:is (equalp (set-middle-range 99 10 100)
                 (list (list 89 99) (list 0 9)))))

(sr:-> quest06-3 (string fixnum fixnum) fixnum)
(defun quest06-3 (input range repeats)
  "given a string representing the garrison map, INPUT, the distance in
each direction from a mentee to search, RANGE, and the number of times
the input pattern repeats, REPEATS, return the total number of
possible mentor/mentee matches - this is a three step process. First we
count the number of matches for the map segments that have matching
maps on both sides (there are (- range 2) of those), then we count the
first segment which has a repeat on the right only, and the last
segment, which has a repeat on the left only. This method saves
considerable time over brute force because we only have to walk the
map three times, instead of REPEATS times"
  (let ((len (length input))            ; length of single map string
        (left-hash (make-hash-table))
        (middle-hash (make-hash-table))
        (right-hash (make-hash-table)))

    ;; count matches in the middle strings
    (iter (for i below len)
      (let ((c (char input i)))         ; walk the garrison map
        (when (lower-case-p c)          ; it's a mentee
          (setf (gethash i middle-hash) ; save matches for this mentee
                (count-matches c input
                               (set-middle-range i range len))))))

    ;; add matches for the leftmost segment
    (iter (for i below len)
      (let ((c (char input i)))
        (when (lower-case-p c)
          (setf (gethash i left-hash)
                (if (< (- i range) 0)   ;off to the left
                    (count-matches c input
                                   (list (list 0 (+ i range))))
                    ;;else already know this so just add it again
                    (gethash i middle-hash))))))

    ;; add matches for the rightmost segment
    (iter (for i below len)
      (let ((c (char input i)))
        (when (lower-case-p c)
          (setf (gethash i right-hash)
                (if (> (+ i range) (1- len)) ; off to the right?
                    (count-matches c input
                                   (list (list (- i range) (1- len))))
                    ;; else re-use existing amount
                    (gethash i middle-hash))))))

    ;; add up all the matches and return
    (+ (apply #'+ (hash-table-values left-hash))
       (apply #'+ (hash-table-values right-hash))
       (* (- repeats 2) (apply #'+ (hash-table-values middle-hash))))))

(5a:test quest06-3-test
  (5a:is (= 72 (quest06-3 *example2* 10 2)))
  (5a:is (= 34 (quest06-3 *example2* 10 1))))

;; ----------------------------------------------------------------------------

;; now solve the puzzle!
(time (format t "The answer to EC Quest 6 Part 1 is ~a"
	      (quest06-1 (read-file-into-string *data-file1*))))

(time (format t "The answer to EC Quest 6 Part 2 is ~a"
	      (quest06-2 (read-file-into-string *data-file2*))))

(time (format t "The answer to EC Quest 6 Part 3 is ~a"
              (quest06-3 (read-file-into-string *data-file3*) *range* *repeats*)))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on Framework Desktop w/ AMD AI Max+ 395, 128GB RAM
;; ----------------------------------------------------------------------------

;; The answer to EC Quest 6 Part 1 is 159
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000103 seconds of total run time (0.000043 user, 0.000060 system)
;; 100.00% CPU
;; 65,472 bytes consed

;; The answer to EC Quest 6 Part 2 is 4010
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000225 seconds of total run time (0.000195 user, 0.000030 system)
;; 100.00% CPU
;; 195,168 bytes consed

;; The answer to EC Quest 6 Part 3 is 1663540863
;; Evaluation took:
;; 0.070 seconds of real time
;; 0.070323 seconds of total run time (0.070067 user, 0.000256 system)
;; 100.00% CPU
;; 1,982,416 bytes consed
