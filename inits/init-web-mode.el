;;;
(require 'web-mode)

(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-indent-style 2)))

(setq web-mode-markup-indent-offset 2) ;; html indent
(setq web-mode-css-indent-offset  2)    ;; css indent
(setq web-mode-code-indent-offset 2)   ;; script indent(js,php,etc..)

(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))

(provide 'init-web-mode)
