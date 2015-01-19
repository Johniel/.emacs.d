;;;
(require 'web-mode)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'zencoding-mode)

(add-hook 'web-mode-hook
          '(lambda ()
             (zencoding-mode)
             (setq web-mode-indent-style 2)
             (setq ac-sources '(ac-source-words-in-buffer
                                ac-source-words-in-same-mode-buffers))))

(setq web-mode-markup-indent-offset 2) ;; html indent
(setq web-mode-css-indent-offset  2)    ;; css indent
(setq web-mode-code-indent-offset 2)   ;; script indent(js,php,etc..)

(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))

(provide 'init-web-mode)
