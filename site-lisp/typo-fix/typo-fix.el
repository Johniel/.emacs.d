;; 怒りのタイポ修正

;; http://d.hatena.ne.jp/kitokitoki/20091124/p2
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; http://d.hatena.ne.jp/kitokitoki/20091124/p2
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defvar fix-word-map '(("cotu" . "cout")
                       ("ocut" . "cout")
                       ("dobule" . "double")
                       ("doubel" . "double")
                       ("doule"  . "double")))

(defun fix-typo ()
  (interactive)
  (let* ((w (assoc (current-word) fix-word-map)))
    (if w
        (progn (call-interactively 'backward-delete-word 1)
               (insert (cdr w)))))
    (insert " "))

(global-set-key (kbd "SPC") 'fix-typo)

(provide 'typo-fix)
