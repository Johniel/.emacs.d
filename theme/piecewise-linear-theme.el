;;; piecewise-linear-theme.el --- my dark color theme  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(deftheme piecewise-linear "PiecewiseLinear Color Theme.")

(custom-theme-set-variables
 'piecewise-linear

 ;;;; lsp-mode
 '(lsp-diagnostics-attributes
   '((unnecessary :foreground "#7a7a7a")
     (deprecated  :strike-through "#e06c75"))))

(custom-theme-set-faces
 'piecewise-linear

 '(default ((t (:background "#2D0922" :foreground "ivory2"))))
 '(cursor  ((t (:background "purple2"))))

 '(region  ((t (:background "RoyalBlue4"))))
 '(hl-line ((t (:background "#2a2040"))))

 '(mode-line           ((t (:foreground "white"   :background "#2d2d30" :box nil))))
 '(mode-line-inactive  ((t (:foreground "gray11"  :background "gray30"  :box nil))))
 '(mode-line-buffer-id          ((t (:foreground "#DA8107" :background nil))))
 '(mode-line-buffer-id-inactive ((t (:foreground "gray50" :inherit mode-line-inactive))))

 '(isearch ((t (:background "red4" :foreground "ivory"))))

 '(warning ((t (:foreground "gold"))))
 '(error   ((t (:foreground "red"))))

 '(lazy-highlight ((t (:background "RoyalBlue4" :foreground "yellow"))))

 '(show-paren-match    ((t (:foreground "red"   :background nil :bold nil))))
 '(show-paren-mismatch ((t (:foreground "black" :background "yellow"))))

 '(font-lock-comment-face  ((t (:foreground "#7a7a7a"))))
 '(font-lock-string-face   ((t (:foreground "LightSalmon3"))))
 '(font-lock-keyword-face  ((t (:foreground "cyan2"))))
 '(font-lock-type-face     ((t (:foreground "LimeGreen"))))
 '(font-lock-constant-face ((t (:foreground "aquamarine"))))
 '(font-lock-builtin-face  ((t (:foreground "LightSteelBlue"))))

 '(font-lock-regexp-grouping-backslash ((t (:foreground "#666"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#f60"))))

 '(cua-rectangle ((t (:background "DodgerBlue4"))))

 '(highlight ((t (:background "dark green"))))

 '(font-lock-function-name-face ((t (:foreground "LightSkyBlue" :underline t))))

 ;;;; flymake
 '(flymake-error   ((t (:inherit nil :underline "red"))))
 '(flymake-warning ((t (:inherit nil :underline "gold"))))
 '(flymake-note    ((t (:inherit nil :underline "cyan"))))

 ;;;; flycheck
 '(flycheck-error   ((t (:inherit nil :underline "red"))))
 '(flycheck-warning ((t (:inherit nil :underline "gold"))))
 '(flycheck-info    ((t (:inherit nil :underline "cyan"))))

 ;;;; lsp-mode
 '(lsp-face-semhl-deprecated
   ((t (:strike-through "#e06c75"))))
 '(lsp-flycheck-error-deprecated-face
   ((t (:strike-through "#e06c75" :underline "red"))))
 '(lsp-flycheck-warning-deprecated-face
   ((t (:strike-through "#e06c75" :underline "gold"))))
 '(lsp-flycheck-info-deprecated-face
   ((t (:strike-through "#e06c75" :underline "cyan"))))

 ;;;; helm
 '(helm-source-header ((t (:background "#4A2040"
                                       :foreground "white"
                                       :weight normal
                                       :height 120))))
 '(helm-header        ((t (:inherit header-line :foreground "yellow2"))))
 '(helm-ff-directory  ((t (:background "DarkRed" :foreground "orange"))))
 '(helm-ff-prefix     ((t (:background "yellow" :foreground "black"))))
 '(helm-selection     ((t (:background "#006400" :foreground nil :underline nil))))

 ;;;; tabbar
 '(tabbar-default    ((t (:background "#2D0922"))))
 '(tabbar-selected   ((t (:background "#2D0922" :foreground "red"    :box nil))))
 '(tabbar-unselected ((t (:background "#2D0922" :foreground "gray75" :box nil))))

 ;;;; web-mode
 '(web-mode-doctype-face          ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face         ((t (:foreground "#7a7a7a" :weight bold))))
 '(web-mode-html-attr-name-face   ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face  ((t (:foreground "#82AE46"))))
 '(web-mode-comment-face          ((t (:foreground "#D9333F"))))
 '(web-mode-server-comment-face   ((t (:foreground "#D9333F"))))
 '(web-mode-css-rule-face         ((t (:foreground "#A0D8EF"))))
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-at-rule-face      ((t (:foreground "#FF7F00"))))

 ;;;; git-gutter-fringe
 '(git-gutter-fr:modified ((t (:foreground "green"))))
 '(git-gutter-fr:added    ((t (:foreground "deep sky blue"))))
 '(git-gutter-fr:deleted  ((t (:foreground "red"))))

 ;;;; company-mode
 '(company-tooltip                  ((t (:inherit nil :background "SteelBlue4" :foreground "white"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip :underline "magenta4" :foreground "red"))))
 '(company-tooltip-selection        ((t (:inherit company-tooltip :underline "magenta4"))))
 '(company-preview-common           ((t (:inherit nil :background "#2a2040" :foreground "red"))))
 '(company-scrollbar-bg             ((t (:inherit fringe))))
 '(company-scrollbar-fg             ((t (:inherit nil :background "#B8E0CB"))))
 '(company-tooltip-deprecated       ((t (:strike-through "#e06c75"))))

 ;;;; markdown-mode
 '(markdown-code-face ((t (:inherit fixed-pitch)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'piecewise-linear)

;;; piecewise-linear-theme.el ends here
