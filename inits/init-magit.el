(require 'magit)

(setq magit-diff-options '("-w" "-b"))

(setq magit-last-seen-setup-instructions "1.4.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://whattheemacsd.com/setup-magit.el-01.html
;; full screen magit-status
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-magit-status-fullscreen (orig-fn &rest args)
  "Run magit-status in fullscreen."
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fn args)
  (delete-other-windows))

(advice-add 'magit-status :around #'my-magit-status-fullscreen)

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;;
(provide 'init-magit)
