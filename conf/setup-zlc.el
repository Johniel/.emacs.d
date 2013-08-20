;; setup-zsh.el

;; https://github.com/mooz/emacs-zlc
(require-package 'zlc)

(let ((map minibuffer-local-map))
  ;;; like menu select
  (define-key map (kbd "<down>")  'zlc-select-next-vertical)
  (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
  (define-key map (kbd "<right>") 'zlc-select-next)
  (define-key map (kbd "<left>")  'zlc-select-previous)
  ;;; reset selection
  (define-key map (kbd "C-d") 'zlc-reset)
  ;; turn on
  (zlc-mode t))

(provide 'setup-zlc)
