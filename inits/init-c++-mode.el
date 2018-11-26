(require 'util)
(require 'clang-format)

(defun clang-format-format-buffer ()
  (when (eq major-mode 'c++-mode)
    (clang-format-buffer "file")))

(if (executable-find "clang-format")
    (add-hook 'before-save-hook 'clang-format-format-buffer))

(def-face c++-macro "cyan1" "C++ macro form")
(add-keywords 'c++-mode '("each" "unless") 'c++-macro)

(add-hook 'c++-mode-hook
          '(lambda()
             (local-set-key (kbd "C-x C-e") nil)
             (local-set-key (kbd "C-x C-a") nil)
             (local-set-key (kbd "C-c C-k") nil)
             (c-set-style "ellemtel")
             (c-toggle-electric-state +1)
             (setq c-basic-offset 2)
             (setq tab-width 2)
             (c-set-offset 'inlambda 0)
             (block-open . 0)))

(provide 'init-c++-mode)
