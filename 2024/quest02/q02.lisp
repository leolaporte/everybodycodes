;;;; Quest02.lisp
;;;; 2024 Everybody Codes solution
;;;; Leo Laporte
;;;; Started: 28 Oct 2025
;;;; Finished:

;; ----------------------------------------------------------------------------
;; Prologue code for setup - same every day
;; ----------------------------------------------------------------------------
;; Pre-loaded in .sbclrc:
;; (ql:quickload '(:fiveam :iterate :cl-ppcre :trivia :alexandria :serapeum :str))

(defpackage :ec.2024.quest02
  (:use  #:cl :alexandria :iterate)
  (:local-nicknames
   (:re :cl-ppcre)      ; regex
   (:sr :serapeum)      ; utilities
   (:tr :trivia)        ; pattern matching
   (:5a :fiveam)))      ; testing framework

(in-package :ec.2024.quest02)

(setf 5a:*run-test-when-defined* t)  ; test as we go
(setf 5a:*verbose-failures* t)       ; show failing expression
(sr:toggle-pretty-print-hash-table)  ; pretty print for hashes
(declaim (optimize (debug 3)))       ; max debugging info
;; (declaim (optimize (speed 3))     ; max speed if needed

(defparameter *data-file1* "~/cl/ec/2024/quest02/input1.txt"
  "Downloaded problem set")
(defparameter *data-file2* "~/cl/ec/2024/quest02/input2.txt"
  "Downloaded problem set")
(defparameter *data-file3* "~/cl/ec/2024/quest02/input3.txt"
  "Downloaded problem set")

#| ----------------------------------------------------------------------------
--- Part One ---

"Your task is to count all the runic words within the inscription on
the helmet (your notes)."

LEO's NOTES: So the input consists of two lines, separated by an empty
line. The first line starts with WORDS: followed by a comma separated
list of runic fragments. The second line contains text. The job is to
find and count the runic fragments in the provided text.

A naive solution:
1. Create a list of runic fragments
2. Walk the list searching for each fragment in the text, count the
occurences.

So simple I don't even need to write a parser. Just for fun I'll make
COUNT-OCCURENCES recursive.

---------------------------------------------------------------------------- |#

(defparameter *example1*
  (list "WORDS:THE,OWE,MES,ROD,HER"
        ""
        "AWAKEN THE POWER ADORNED WITH THE FLAMES BRIGHT IRE"))

(defun count-occurences (rune inscription pos cnt)
  "given a rune string and an inscription string, the current position in
the inscription string, and the number of occurences so far, return
 the number of times the rune occurs in the inscription (initiate with
 0 and 0 for POS and CNT) - recursive solution"
  (let ((res (search rune inscription :start2 pos)))
    (cond ((null res) cnt)         ; not found, return count
          (t (setf pos (1+ res))   ; move to next position
             (incf cnt)            ; add to count and recurse
             (count-occurences rune inscription pos cnt)))))

(5a:test count-occurences-test
  (5a:is (= 2 (count-occurences
               "THE"
               "AWAKEN THE POWER ADORNED WITH THE FLAMES BRIGHT IRE"
               0 0)))
  (5a:is (= 1 (count-occurences
               "OWE"
               "AWAKEN THE POWER ADORNED WITH THE FLAMES BRIGHT IRE"
               0 0)))
  (5a:is (= 0 (count-occurences
               "HER"
               "AWAKEN THE POWER ADORNED WITH THE FLAMES BRIGHT IRE"
               0 0))))

(defun day02-1 (data)
  (let* ((runes (sr:words (first data) :start 6))
         (inscription (third data)))

    (iter (for rune in runes)
      (summing
        (count-occurences rune inscription 0 0)))))

(5a:test day02-1-test
  (5a:is (= 4 (day02-1 *example1*))))

#| ----------------------------------------------------------------------------
--- Part Two ---

LEO'S NOTES:

Only three changes in this part. The inscription crosses multiple
lines and instead of counting the occurences, we count the letters. So
I still have to find the rune, but I return the number of letters
found. Oh and I have to search backwards and forwards. And symbols can
overlap.

If I reverse the rune will that work? Yeah I think so. As for overlap,
my original COUNT-OCCURENCES works because I go one rune at a
time. And I merely have to multiply the count by the length to get the
total number of characters. In other words I don't need to change how
I count. All the modifications are in the main routine.

Ah one little problem. The problem example says QAQAQ is only 5 (with
the QAQ rune I counted it as 12). I should only count a matching rune
once, no matter how often it matches. In other words, I'm countine
matched runes, not the number of matches. Similarly THERE IS THE END
matches THE twice and HER once. I was counting 9 matches but there are
only 7 matching runes. (T H E R T H E).

So I guess I do need to rewrite COUNT-OCCURENCES. I could create sets
of matching characters which I can union to eliminate dupes and then
count. Or I can create a MATCHES array which has a 1 in the place of
every matched letter and a 0 if no matches. Then just add the
1s. Let's try that second method. I'll just pass a copy of the array
through each iteration.

I'll need a fundamental function MATCH-RUNE-IN-INSCRIPTION which
matches a single rune going forward with a single inscription and
returns a MATCHES array.

I'll use MATCH-RUNES-IN-INSCRIPTION to iterate all the runes (backward
and forward) for a single inscription. Each inscription will have a
dedicated MATCHES array. This will return an INTEGER which is the
total number of matches for that INSCRIPTION.

Finally the DAY02-2 function will iterate through the inscriptions
summing the returned counts for the final total.

---------------------------------------------------------------------------- |#

(defparameter *example2*
  (list "WORDS:THE,OWE,MES,ROD,HER,QAQ"
        ""
        "AWAKEN THE POWE ADORNED WITH THE FLAMES BRIGHT IRE"
        "THE FLAME SHIELDED THE HEART OF THE KINGS"
        "POWE PO WER P OWE R"
        "THERE IS THE END"
        "QAQAQ"))

(sr:-> match-rune-in-inscription (string string array) array)
(defun match-rune-in-inscription (rune inscription matches)
  "given a RUNE as a string and an INSCRIPTION as a string, and a one
dimensional array, MATCHES, of inscription length with 0 at the index
of as yet unmatched characters in the inscription, and 1 at the index
of runic chars that have been matched, return the array with any
additional matching positions marked with 1"
  (let ((pos 0)) ; start at beginning of inscription
    (iter ; loop forever
      (setf pos (search rune inscription :test #'equalp :start2 pos))
      (when (null pos) ; not found - all done
        (return-from match-rune-in-inscription matches)) ; exit loop
      (iter (for i from pos below (+ pos (length rune)))
        (setf (aref matches i) 1)) ; match! mark MATCHES array
      (incf pos))))                ; go to next char

(5a:test match-rune-in-inscription-test
  (5a:is (equalp
          (match-rune-in-inscription "THE" "THE I THE I THE"
                                     (make-array 15 :initial-element 0))
          #(1 1 1 0 0 0 1 1 1 0 0 0 1 1 1)))
  (5a:is (equalp
          (match-rune-in-inscription "QAQ" "QAQAQ"
                                     (make-array 5 :initial-element 0))
          #(1 1 1 1 1))))

(sr:-> match-runes-in-inscription (list string) integer)
(defun match-runes-in-inscription (runes inscription)
  "given a RUNE and an INSCRIPTION return the number of matched characters"
  (let ((matches (make-array (length inscription) :initial-element 0)))
    (iter (for rune in runes)
      (setf matches ; forward
            (match-rune-in-inscription rune inscription matches))
      (setf matches ; backward
            (match-rune-in-inscription (reverse rune) inscription matches)))

    (reduce #'+ matches)))

(defun day02-2 (data)
  "Given a list of strings with the first item being a list of runes and
the remainder a list of inscriptions, return the number of rune
characters (reading backward and forward) represented in the
inscriptions"
  (let ((runes (sr:words (first data) :start 6))
        (inscriptions (cddr data))) ; the lines after the first two

    (iter (for inscrip in inscriptions)
      (summing
        (match-runes-in-inscription runes inscrip)))))

(5a:test day02-2-test
  (5a:is (= 42 (day02-2 *example2*))))


#| ----------------------------------------------------------------------------
--- Part Three ---

"In this challenge, neighbouring scales can form runic words both
horizontally and vertically: left to right, right to left, top to
bottom, bottom to top. Your task remains as it was before: to find all
the runic words hidden within the armour and count the number of
scales that compose them."

LEO'S NOTES: So now I need to interpret the inscriptions as a
grid. And search each row and column for matches. As much as I'd like
to reuse my functions from the previous part I think the simplest
thing would be to create 2D array to contain the matches then go row
by row and column by column to match runes to the inscriptions.

I'll turn the inscriptions into a 2D array called GRID (keeping the
runes as a list of strings).

I can track the matched characters in a shadow 2D array called MATCHED.

The base function will be MATCH-RUNE-IN-ROW which matches a rune in a
row of the grid. This can work for rows and columns (by flipping the
col into a row before matching), but I do have to account for
wraparound (as shown in the example: THE matches HELWORLT because it
can start at the last character T and wrap around to HE). What's the
best way to handle that?

If I double the row before matching HELWORLT -> HELWORLTHELWORLT I'll
catch all the wraparound poddibilities, but then I'll have to use MOD
on the position pointer (mod ptr len) to calculate where in the
MATCHED array to put the 1.

And I'll need a function COL->ROW to turn each column into a "row".

---------------------------------------------------------------------------- |#

(defparameter *example3*
  (list "WORDS:THE,OWE,MES,ROD,RODEO"
        ""
        "HELWORLT"
        "ENIGWDXL"
        "TRODEOAL"))

(sr:-> col->row (fixnum array) string)
(defun col->row (colnum grid)
  "given a column index into a 2D array return a string representing all
the characters in that column"

  )

(sr:-> match-rune-in-row (string string) array)
(defun match-rune-in-row (rune row)
  )

(defun day02-3 (data)
  (let* ((runes (sr:words (first data) :start 6))
         (inscriptions (cddr data)) ; the lines after the first two
         (width (length (first inscriptions)))
         (height (length inscriptions)))

    ;; this is where I'll keep track of matching rune chars
    (matched (make-array '(width height)) :initial-element 0)

    ;; and this is the grid itself
    (iter (for line below height)
      (let ()))


    ))

(5a:test day02-3-test
  (5a:is (= (day02-3 *example3*) 10)))

#| ------------------------------------------------------------------------- |#

;; now solve the puzzle!
(time (format t "The answer to EC Quest 02 Part 1 is ~a"
              (day02-1 (uiop:read-file-lines *data-file1*))))

(time (format t "The answer to EC Quest 02 Part 2 is ~a"
              (day02-2 (uiop:read-file-lines *data-file2*))))

;; (time (format t "The answer to EC Quest 02 Part 3 is ~a"
;;	      (day02-3 (uiop:read-file-lines *data-file3*))))

;; ----------------------------------------------------------------------------
;; Timings with SBCL on M4 Mac mini with 64GB RAM
;; ----------------------------------------------------------------------------
