(require 'php-mode)
(require 'php-eldoc)

(require 'ac-php)
(require 'ac-php-core)

(defadvice flymake-php-init (around my-php-setting activate)
  (if (not (tramp-tramp-file-p buffer-file-name))
      ad-do-it
    nil))

(defun johniel::php-mode-hook()
  ;; style
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  ;; php-eldoc
  (php-eldoc-enable)
  ;; ac-php
  (require 'company-php)
  (company-mode t)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ac-php-backend)
  (define-key php-mode-map (kbd "M-.") 'ac-php-find-symbol-at-point)
  (define-key php-mode-map (kbd "M-,") 'ac-php-location-stack-back)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (ac-php-core-eldoc-setup)
  t)

(add-hook 'php-mode-hook 'johniel::php-mode-hook)

(require 'f)
(defun ac-php-init-conf-json()
  (interactive)
  (f-touch ".ac-php-conf.json")
  (call-interactively 'ac-php-remake-tags-all))

(provide 'init-php-mode)
