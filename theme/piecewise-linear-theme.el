(deftheme piecewise-linear "PiecewiseLinear Color Theme")

(custom-theme-set-variables
 'piecewise-linear
 '(pl:background nil))

(custom-theme-set-faces
 'piecewise-linear

 '(default ((t (:background "#2D0922" :foreground "ivory"))))
 '(cursor ((t (:background "purple2"))))

 '(region ((t (:background "RoyalBlue4"))))
 '(hl-line ((t (:background "#2a2040"))))

 '(mode-line ((t (:foreground "white" :background "#2d2d30" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#DA8107" :background nil))))
 '(mode-line-inactive  ((t (:foreground "gray11" :background "gray30" :box nil))))

 '(isearch ((t (:background "red4" :foreground "ivory"))))

 '(warning ((t (:foreground "gold"))))
 '(error   ((t (:foreground "red"))))

 '(lazy-highlight ((t (:background "RoyalBlue4" :foreground "yellow"))))

 '(show-paren-match    ((t (:foreground "red" :background nil :bold t))))
 '(show-paren-mismatch ((t (:foreground "black" :background "yellow"))))

 '(font-lock-comment-face ((t (:foreground "#7a7a7a"))))
 '(font-lock-string-face ((t (:foreground "LightSalmon3"))))

 '(font-lock-regexp-grouping-backslash ((t (:foreground "#666"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#f60"))))

 '(cua-rectangle ((t (:background "DodgerBlue4"))))

 '(highlight ((t (:background "dark green"))))

 '(font-lock-function-name-face ((t (:underline t))))

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
 '(helm-source-header ((t (:normal t :family nil :height 120))))
 '(helm-header ((t (:foreground "yellow2"))))
 '(helm-ff-directory ((t (:background nil :foreground "orange"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; tabbar
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(tabbar-default ((t (:background "#2D0922"))))

 '(tabbar-unselected ((t (:background "#2D0922" :foreground "gray75" :box nil))))
 '(tabbar-selected   ((t (:background "#2D0922" :foreground "red" :box nil))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; yascroll
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(yascroll:thumb-fringe ((t (:foreground "#404070" :background "#404070"))))
 '(yascroll:thumb-text-area ((t (:foreground "#404070"))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; highlight-indentation
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 '(highlight-indentation-face ((t (:background "#1a1a1a"))))
 '(highlight-indentation-current-column-face ((t (:background "#2a2040"))))

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
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'piecewise-linear)
