(require 'rust-mode)
(require 'racer)

(if (commandp "rust-format")
    (eval-after-load "rust-mode"
      '(setq-default rust-format-on-save t))
  (message "command not found: rust-format"))

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

(add-hook 'rust-mode-hook (lambda ()
                            (racer-mode)
                            (eldoc-mode)
                            (add-hook 'before-save-hook 'rust-format-buffer)))

(setq company-tooltip-align-annotations t)



(provide 'init-rust-mde)
