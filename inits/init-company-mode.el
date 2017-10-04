(require 'company)
(require 'company-quickhelp)

(global-company-mode true)
(company-quickhelp-mode true)

(require 'helm-company)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-S-f") 'helm-company)
     (define-key company-active-map (kbd "C-S-f") 'helm-company)))

(add-to-list 'company-backends 'company-c-headers)

(provide 'init-company-mode)
