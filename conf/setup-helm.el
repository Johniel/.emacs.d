;;; setup-helm.el

(require 'helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-imenu)
(require 'helm-command)

(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0)
(setq helm-candidate-number-limit 300)

(define-key helm-map (kbd "C-h")   'helm-previous-line)
(define-key helm-map (kbd "C-n")   'helm-next-line)
(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-h") 'helm-previous-source)

(require 'tabbar+)
(defun my-standard-helm ()
  (interactive)
  (helm :sources '(
                   ;; helm-c-source-buffers-list
                   ;; helm-c-source-tabbar+buffers-list
                   helm-c-source-tabbar+current-group-buffers-list
                   helm-c-source-tab-groups-list
                   ;; helm-c-source-imenu
                   helm-c-source-recentf
                   ;; helm-c-source-buffer-not-found
    )
        :buffer "*helm*"
        :keymap helm-c-buffer-map))

(require 'popwin)
(push "*helm*" popwin:special-display-config)

(provide 'setup-helm)
