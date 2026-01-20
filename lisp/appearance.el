;; appearance.el ---

(require 'util)

;; Font
(set-face-attribute 'default nil :height 110)

(add-to-list 'custom-theme-load-path "~/.emacs.d/theme")
(load-theme 'piecewise-linear t)
(enable-theme 'piecewise-linear)

(if window-system
    (set-frame-parameter (selected-frame) 'alpha '(88 50)))

(global-font-lock-mode 1)

(show-paren-mode 1)

(line-number-mode 1)

(column-number-mode 1)

(menu-bar-mode 1)

(transient-mark-mode 1)

(setq-default truncate-lines t)

(if (font-exists-p "Inconsolata-13")
    (set-frame-font "Inconsolata-13" nil t))

(when (windows-p)
  (set-frame-font "Consolas 10" nil t)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    '("ＭＳ ゴシック" . "unicode-bmp"))
  (set-fontset-font (frame-parameter nil 'font)
                    'katakana-jisx0201
                    '("ＭＳ ゴシック" . "unicode-bmp")))

(if (and window-system (not (windows-p)))
    (add-hook 'prog-mode-hook 'hl-line-mode))
