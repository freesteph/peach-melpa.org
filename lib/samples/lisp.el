;;; http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/calendar/parse-time.el
;;; Code:

(require 'cl-lib)

;; Byte-compiler warnings
(defvar parse-time-elt)
(defvar parse-time-val)

(defsubst parse-time-string-chars (char)
  (cond ((<= ?a char ?z) ?a)
        ((<= ?0 char ?9) ?0)
        ((eq char ?+) 1)
        ((eq char ?-) -1)
        ((eq char ?:) ?d)))

(defun parse-time-tokenize (string)
  "Tokenize STRING into substrings.
Each substring is a run of \"valid\" characters, i.e., lowercase
letters, digits, plus or minus signs or colons."
  (let ((start nil)
	(end (length string))
	(all-digits nil)
	(list ())
	(index 0)
	(c nil))
    (while (< index end)
      (while (and (< index end)		;Skip invalid characters.
		  (not (setq c (parse-time-string-chars (aref string index)))))
	(cl-incf index))
      (setq start index
            all-digits (eq c ?0))
      (while (and (< (cl-incf index) end)	;Scan valid characters.
		  (setq c (parse-time-string-chars (aref string index))))
	(setq all-digits (and all-digits (eq c ?0))))
      (if (<= index end)
	  (push (if all-digits (cl-parse-integer string :start start :end index)
		  (substring string start index))
		list)))
    (nreverse list)))

(defvar parse-time-months '(("jan" . 1) ("feb" . 2) ("mar" . 3)
			    ("apr" . 4) ("may" . 5) ("jun" . 6)
			    ("jul" . 7) ("aug" . 8) ("sep" . 9)
			    ("oct" . 10) ("nov" . 11) ("dec" . 12)
			    ("january" . 1) ("february" . 2)
			    ("march" . 3) ("april" . 4) ("june" . 6)
			    ("july" . 7) ("august" . 8)
			    ("september" . 9) ("october" . 10)
			    ("november" . 11) ("december" . 12)))
(defvar parse-time-weekdays '(("sun" . 0) ("mon" . 1) ("tue" . 2)
			      ("wed" . 3) ("thu" . 4) ("fri" . 5)
			      ("sat" . 6) ("sunday" . 0) ("monday" . 1)
			      ("tuesday" . 2) ("wednesday" . 3)
			      ("thursday" . 4) ("friday" . 5)
			      ("saturday" . 6)))
(defvar parse-time-zoneinfo `(("z" 0) ("ut" 0) ("gmt" 0)
			      ("pst" ,(* -8 3600)) ("pdt" ,(* -7 3600) t)
			      ("mst" ,(* -7 3600)) ("mdt" ,(* -6 3600) t)
			      ("cst" ,(* -6 3600)) ("cdt" ,(* -5 3600) t)
			      ("est" ,(* -5 3600)) ("edt" ,(* -4 3600) t))
  "(zoneinfo seconds-off daylight-savings-time-p)")
