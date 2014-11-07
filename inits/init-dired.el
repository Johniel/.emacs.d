;;; sutup-dired.el

(require 'dired)
(require 'dired-x)
(require 'wdired)

(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

(provide 'init-dired)
