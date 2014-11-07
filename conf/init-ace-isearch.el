(require 'ace-isearch)

(global-ace-isearch-mode +1)

(setq ace-isearch-use-function-from-isearch nil)

(define-key isearch-mode-map (kbd "M-o") 'helm-multi-swoop-all-from-isearch)

(provide 'init-ace-isearch)
