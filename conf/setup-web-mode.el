;;;
(require 'web-mode)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'highlight-indentation)
(require 'zencoding-mode)

(add-hook 'web-mode-hook
          '(lambda ()
             (zencoding-mode)
             ;; (highlight-indentation-current-column-mode)
             (setq web-mode-indent-style 2)
             (setq ac-sources '(ac-source-words-in-buffer
                                ac-source-words-in-same-mode-buffers))))

(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))

(provide 'setup-web-mode)
