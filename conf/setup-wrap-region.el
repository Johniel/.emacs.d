;;; https://github.com/rejeep/wrap-region

(require 'wrap-region)

(wrap-region-global-mode t)
(add-to-list 'wrap-region-except-modes 'magit-status-mode)

(provide 'setup-wrap-region)
