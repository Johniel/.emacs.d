;; appearance.el ---

(require 'util)

;; Font
(set-face-attribute 'default nil :height 110)

(require 'visible-mark)
(setq set-mark-command-repeat-pop t)
(setq visible-mark-max 8)

(defadvice visible-mark-move-overlays (around visible-mark-ad activate)
  (let ((mark-active t))
    ad-do-it))

(global-visible-mark-mode 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")
(load-theme 'piecewise-linear t)
(enable-theme 'piecewise-linear)

(if window-system
    (set-frame-parameter (selected-frame) 'alpha '(90 50)))

(global-font-lock-mode +1)

(show-paren-mode +1)

(line-number-mode +1)

(column-number-mode +1)

(tool-bar-mode 0)

(scroll-bar-mode 0)

(transient-mark-mode +1)

(setq-default truncate-lines +1)

(global-scc-mode +1)

(defun font-exists-p (font)
  "check if font exists"
  (if (null (x-list-fonts font)) nil t))

(if (font-exists-p "Inconsolata-13")
    (set-default-font "Inconsolata-13"))

(when (windows-p)
  (set-default-font "Consolas 10")
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    '("ＭＳ ゴシック" . "unicode-bmp"))
  (set-fontset-font (frame-parameter nil 'font)
                    'katakana-jisx0201
                    '("ＭＳ ゴシック" . "unicode-bmp")))

(defun minor-hl-line-mode ()
  (make-variable-buffer-local 'global-hl-line-mode)
  (global-hl-line-mode +1))

(if (and window-system (not (performance-saving-p)))
    (eval-safe
     (add-hook 'prog-mode-hook 'minor-hl-line-mode)))
