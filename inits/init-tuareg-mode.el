(require 'tuareg)

(add-keywords 'tuareg-mode '("let" "rec" "in") 'font-lock-keyword-face)

(define-key tuareg-mode-map (kbd "C-M-k") nil)

(provide 'init-tuareg-mode)
