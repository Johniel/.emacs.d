(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdt" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mddw" . markdown-mode))

(add-hook 'markdown-mode-hook (lambda () (local-set-key (kbd "C-c C-k") nil)))

(provide 'init-markdown-mode)
