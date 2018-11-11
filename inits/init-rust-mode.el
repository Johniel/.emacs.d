(require 'rust-mode)
(require 'racer)

(if (commandp "rust-format")
    (eval-after-load "rust-mode"
      '(setq-default rust-format-on-save t))
  (message "command not found: rust-format"))

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(setq company-tooltip-align-annotations t)

(add-hook 'before-save-hook 'rust-format-buffer)

(provide 'init-rust-mde)
