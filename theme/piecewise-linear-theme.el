;;; piecewise-linear-theme.el --- dark color theme

;;; Commentary:

;;

;;; Code:

(deftheme piecewise-linear "PiecewiseLinear Color Theme.")

(custom-theme-set-variables
 'piecewise-linear
 '(pl:background nil))

(custom-theme-set-faces
 'piecewise-linear

 '(default ((t (:background "#2D0922" :foreground "ivory2"))))
 '(cursor  ((t (:background "purple2"))))

 '(region  ((t (:background "RoyalBlue4"))))
 '(hl-line ((t (:background "#2a2040"))))

 '(mode-line           ((t (:foreground "white"   :background "#2d2d30" :box nil))))
 '(mode-line-inactive  ((t (:foreground "gray11"  :background "gray30"  :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#DA8107" :background nil))))

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

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; flymake
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(flymake-errline  ((t (:inherit nil :underline "red"))))
 '(flymake-warnline ((t (:inherit nil :underline "gold"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; helm
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(helm-source-header ((t (:background "#22083397778B"
                                       :foreground "white"
                                       :normal t
                                       :height 120))))
 '(helm-header        ((t (:inherit header-line :foreground "yellow2"))))
 '(helm-ff-directory  ((t (:background "DarkRed" :foreground "orange"))))
 '(helm-ff-prefix     ((t (:background "yellow" :foreground "black"))))
 '(helm-selection     ((t (:background "#006400" :foreground nil :underline nil))))

 '(helm-swoop-target-line-face       ((t (:inherit hl-line))))
 '(helm-swoop-target-line-block-face ((t (:inherit hl-line))))
 '(helm-swoop-target-word-face ((t (:background nil :foreground "red"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; tabbar
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(tabbar-default    ((t (:background "#2D0922"))))
 '(tabbar-selected   ((t (:background "#2D0922" :foreground "red"    :box nil))))
 '(tabbar-unselected ((t (:background "#2D0922" :foreground "gray75" :box nil))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; yascroll
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(yascroll:thumb-fringe    ((t (:foreground "#404070" :background "#404070"))))
 '(yascroll:thumb-text-area ((t (:foreground "#404070"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; web-mode
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(web-mode-doctype-face          ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face         ((t (:foreground "#7a7a7a" :weight bold))))
 '(web-mode-html-attr-name-face   ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face  ((t (:foreground "#82AE46"))))
 '(web-mode-comment-face          ((t (:foreground "#D9333F"))))
 '(web-mode-server-comment-face   ((t (:foreground "#D9333F"))))
 '(web-mode-css-rule-face         ((t (:foreground "#A0D8EF"))))
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-at-rule-face      ((t (:foreground "#FF7F00"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; git-gutter-fringe
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(git-gutter-fr:modified ((t (:foreground "green"))))
 '(git-gutter-fr:added    ((t (:foreground "deep sky blue"))))
 '(git-gutter-fr:deleted  ((t (:foreground "red"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; magit
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(magit-item-highlight ((t (:inherit nil :background nil))))
 '(magit-diff-add       ((t (:inherit nil :background nil :foreground "ForestGreen"))))
 '(magit-diff-del       ((t (:inherit nil :background nil :foreground "brown"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; company-mode
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(company-tooltip                  ((t (:inherit nil :background "steel" :foreground "white"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip :underline "magenta4" :foreground "red"))))
 '(company-tooltip-selection        ((t (:inherit company-tooltip :underline "magenta4"))))
 '(company-preview-common           ((t (:inherit nil :background "#2a2040" :foreground "red"))))
 '(company-scrollbar-bg             ((t (:inherit fringe))))
 '(company-scrollbar-fg             ((t (:inherit nil :background "#B8E0CB"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; markdown-mode
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(markdown-code-face ((t (:inherit fixed-pitch))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'piecewise-linear)
