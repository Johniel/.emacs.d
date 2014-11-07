;; setup-yasnnippet.el ---

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

(yas-global-mode +1)

(setq yas/wrap-around-region t)

(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
(define-key yas-keymap (kbd "C-h") 'yas-prev-field)
(define-key yas-keymap (kbd "C-n") 'yas-next-field)

(if (not window-system)
    (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)))

(provide 'init-yasnippet)
