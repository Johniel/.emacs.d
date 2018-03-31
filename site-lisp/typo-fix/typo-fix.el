;; 怒りのタイポ修正

;; http://d.hatena.ne.jp/kitokitoki/20091124/p2
(defun typo-fix--delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; http://d.hatena.ne.jp/kitokitoki/20091124/p2
(defun typo-fix--backward-typo-fix--delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (typo-fix--delete-word (- arg)))

(defvar fix-word-map '(("cotu" . "cout")
                       ("ocut" . "cout")
                       ("cuot" . "cout")
                       ("ctou" . "cout")
                       ("conut" . "count")
                       ("dobule" . "double")
                       ("doubel" . "double")
                       ("doule"  . "double")))

(defun fix-typo (alt)
  (let* ((w (assoc (current-word) fix-word-map)))
    (when w
      (call-interactively 'typo-fix--backward-typo-fix--delete-word 1)
      (insert (cdr w))))
    (insert alt))

(global-set-key (kbd "SPC") (lambda () (interactive) (fix-typo " ")))
(global-set-key (kbd ")")   (lambda () (interactive) (fix-typo ")")))
(global-set-key (kbd "(")   (lambda () (interactive) (fix-typo "(")))

(provide 'typo-fix)
