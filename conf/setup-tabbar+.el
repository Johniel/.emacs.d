;;; setup-tabbar.el ---

(require 'tabbar)
(require 'tabbar+)

;; remove {home, left, right} button
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

(tabbar-mwheel-mode 0)

(setq tabbar-auto-scroll-flag 1)

(setq tabbar-separator '(1.2))

(setq tabbar-cycle-scope 'tabs)

(when window-system
  (tabbar-mode t)
  (tabbar+enable-tab-group))

(provide 'setup-tabbar+)
