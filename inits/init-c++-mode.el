(require 'util)
(require 'clang-format)

(defun clang-format-format-buffer ()
  (when (eq major-mode 'c++-mode)
    (clang-format-buffer "file")))

(def-face c++-macro "cyan1" "C++ macro form")
(add-keywords 'c++-mode '("each" "unless" "each_with_index" "each_pair") 'c++-macro)

(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") nil)
            (local-set-key (kbd "C-x C-a") nil)
            (local-set-key (kbd "C-c C-k") nil)
            (c-set-style "ellemtel")
            (c-toggle-electric-state +1)
            (c-set-offset 'inlambda 0)
            (setq c-basic-offset 2)
            (setq tab-width 2)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(provide 'init-c++-mode)
