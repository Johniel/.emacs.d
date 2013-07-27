(require 'multiple-cursors)

;;
;; http://qiita.com/ShingoFukuyama@github/items/3ad7e24cb2d8f55b4cc5

;; insert specific serial number
(defvar mc:insert-numbers-hist nil)
(defvar mc:insert-numbers-inc 1)
(defvar mc:insert-numbers-pad "%01d")

(defun mc:insert-numbers (start inc pad)
  "Insert increasing numbers for each cursor specifically."
  (interactive
   (list (read-number "Start from: " 0)
         (read-number "Increment by: " 1)
         (read-string "Padding (%01d): " nil mc:insert-numbers-hist "%01d")))
  (setq mc--insert-numbers-number start)
  (setq mc:insert-numbers-inc inc)
  (setq mc:insert-numbers-pad pad)
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor
    'mc:-insert-number-and-increase
    cursor)))

(defun mc:-insert-number-and-increase ()
  (interactive)
  (insert (format mc:insert-numbers-pad mc--insert-numbers-number))
  (setq mc--insert-numbers-number (+ mc--insert-numbers-number mc:insert-numbers-inc)))

(provide 'setup-multiple-cursors)
