(require 'whitespace)

(when (linux-p)

  (setq whitespace-style
        '(tabs tab-mark spaces space-mark))

  (setq whitespace-space-regexp "\\(\x3000+\\)")

  (setq whitespace-display-mappings
        '((space-mark ?\x3000 [?\¢¢])
          (tab-mark   ?\t   [?\xBB ?\t])
          ))

  (global-whitespace-mode 1))

(if (linux-p)
    (message "turn on global-whitespace-mode")
  (message "turn off global-whitespace-mode"))

(provide 'init-whitespace)
