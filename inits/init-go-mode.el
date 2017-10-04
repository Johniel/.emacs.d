(require 'go-mode)

(require 'company-go)
(require 'exec-path-from-shell)
(require 'go-eldoc)

;; https://github.com/nsf/gocode/tree/master/emacs-company
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

(defun init-go-mode-hook()
  (add-hook 'before-save-hook' 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (go-eldoc-setup)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  t)

(add-hook 'go-mode-hook 'init-go-mode-hook)


(defconst gopath (exec-path-from-shell-getenv "GOPATH"))

(provide 'init-go-mode)
