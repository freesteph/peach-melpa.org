;;; http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/emacs-lisp/seq.el
;;; PEACH-MELPA.ORG DISCLAIMER: licensing information for the code
;;; below is available at the URL above; I do not own any of it.


(cl-defgeneric seq-intersection (sequence1 sequence2 &optional testfn)
  "Return a list of the elements that appear in both SEQUENCE1 and SEQUENCE2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (seq-contains sequence2 elt testfn)
                    (cons elt acc)
                  acc))
              (seq-reverse sequence1)
              '()))

(cl-defgeneric seq-difference (sequence1 sequence2 &optional testfn)
  "Return a list of the elements that appear in SEQUENCE1 but not in SEQUENCE2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (not (seq-contains sequence2 elt testfn))
                    (cons elt acc)
                  acc))
              (seq-reverse sequence1)
              '()))

(cl-defgeneric seq-group-by (function sequence)
  "Apply FUNCTION to each element of SEQUENCE.
Separate the elements of SEQUENCE into an alist using the results as
keys.  Keys are compared using `equal'."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (funcall function elt))
            (cell (assoc key acc)))
       (if cell
           (setcdr cell (push elt (cdr cell)))
         (push (list key elt) acc))
       acc))
   (seq-reverse sequence)
   nil))

(cl-defgeneric seq-min (sequence)
  "Return the smallest element of SEQUENCE.
SEQUENCE must be a sequence of numbers or markers."
  (apply #'min (seq-into sequence 'list)))(cl-defmethod seq-empty-p ((list list))
  "Optimized implementation of `seq-empty-p' for lists."
  (null list))


(defun seq--into-list (sequence)
  "Concatenate the elements of SEQUENCE into a list."
  (if (listp sequence)
      sequence
    (append sequence nil)))

(defun seq--into-vector (sequence)
  "Concatenate the elements of SEQUENCE into a vector."
  (if (vectorp sequence)
      sequence
    (vconcat sequence)))

(defun seq--into-string (sequence)
  "Concatenate the elements of SEQUENCE into a string."
  (if (stringp sequence)
      sequence
    (concat sequence)))

(defun seq--activate-font-lock-keywords ()
  "Activate font-lock keywords for some symbols defined in seq."
  (font-lock-add-keywords 'emacs-lisp-mode
                          '("\\<seq-doseq\\>" "\\<seq-let\\>")))

(unless (fboundp 'elisp--font-lock-flush-elisp-buffers)
  ;; In Emacs≥25, (via elisp--font-lock-flush-elisp-buffers and a few others)
  ;; we automatically highlight macros.
  (add-hook 'emacs-lisp-mode-hook #'seq--activate-font-lock-keywords))

(provide 'seq)
;;; seq.el ends here
