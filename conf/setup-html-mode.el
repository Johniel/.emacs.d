(require 'sgml-mode)
(require 'zencoding-mode)

(add-hook 'html-mode-hook 'zencoding-mode)

(defun insert-angle-brackets (&optional arg)
  (interactive "P")
  (insert-pair arg ?\< ?>))

(add-hook 'html-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-=") 'insert-angle-brackets)))

(provide 'setup-html-mode)
