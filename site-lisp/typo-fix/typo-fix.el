;; 怒りのタイポ修正

;;; Custom group
(defgroup typo-fix nil
  "Automatic typo correction for common programming mistakes."
  :group 'convenience
  :prefix "typo-fix-")

(defcustom typo-fix-word-map '(("cotu" . "cout")
                               ("ocut" . "cout")
                               ("cuot" . "cout")
                               ("ctou" . "cout")
                               ("conut" . "count")
                               ("dobule" . "double")
                               ("doubel" . "double")
                               ("doule"  . "double"))
  "Association list mapping common typos to their correct spellings.
Each element is a cons cell (TYPO . CORRECTION) where TYPO is the
misspelled word and CORRECTION is the correct spelling."
  :type '(alist :key-type string :value-type string)
  :group 'typo-fix)

(defun typo-fix--delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (delete-region (point) (progn (forward-word arg) (point))))

(defun typo-fix--backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (typo-fix--delete-word (- arg)))

(defun typo-fix-correct (alt)
  "Automatically correct typos and insert the trigger character ALT.

Check if the current word at point matches any typo in `typo-fix-word-map'.
If a match is found, replace the misspelled word with its correction.
Always insert ALT (the trigger character) at the end.

ALT is typically a character like space, parentheses, or punctuation
that triggers typo checking when typed."
  (let* ((w (assoc (current-word) typo-fix-word-map)))
    (when w
      (typo-fix--backward-delete-word 1)
      (insert (cdr w))))
  (insert alt))

(provide 'typo-fix)
