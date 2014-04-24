;; appearance.el ---

(require 'util)

;; Font
(set-face-attribute 'default nil :height 110)

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

(setq-default truncate-lines t)

(global-scc-mode +1)

(defun font-exists-p (font)
  "check if font exists"
  (if (null (x-list-fonts font)) nil t))

(if (font-exists-p "Inconsolata-13")
    (set-default-font "Inconsolata-13"))

(defun minor-hl-line-mode ()
  (make-variable-buffer-local 'global-hl-line-mode)
  (global-hl-line-mode +1))

(if (and window-system (not (performance-saving-p)))
    (eval-safe
     (add-hook 'prog-mode-hook 'minor-hl-line-mode)))
