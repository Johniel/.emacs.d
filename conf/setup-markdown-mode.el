(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdt" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mddw" . markdown-mode))

(provide 'setup-markdown-mode)
