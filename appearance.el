;; appearance.el ---

(require 'util)

;; Font
(set-face-attribute 'default nil :height 120)

(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")
(load-theme 'piecewise-linear t)
(enable-theme 'piecewise-linear)

(global-font-lock-mode 1)

(show-paren-mode 1)

(line-number-mode 1)

(column-number-mode 1)

(tool-bar-mode 0)

(scroll-bar-mode 0)

(transient-mark-mode 0)

(defun minor-hl-line-mode ()
  (make-variable-buffer-local 'global-hl-line-mode)
  (global-hl-line-mode +1))

(if (and window-system (not (performance-saving-p)))
    (eval-safe
     (add-hook 'prog-mode-hook 'minor-hl-line-mode)))
