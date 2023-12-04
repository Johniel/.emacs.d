;;; init-tabbar.el ---

(require 'tabbar)
(require 'util)
(require 'projectile)

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

(when (and window-system (not (windows-p)))
  (tabbar-mode t))

;; https://blog.magcho.com/2021/10/tabbar-group/
(defun my/tabbar-buffer-groups ()
  (list
   (cond
    ;; check project name by projectile.el
    ((projectile-project-name (projectile-project-root (buffer-file-name (current-buffer)))))
    ;; fallback default name
    ("default"))))
(setq tabbar-buffer-groups-function 'my/tabbar-buffer-groups)

(provide 'init-tabbar)
