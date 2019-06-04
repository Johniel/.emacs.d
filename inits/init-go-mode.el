(require 'go-mode)

(require 'company-go)
(require 'exec-path-from-shell)
(require 'go-eldoc)

(require 's)
(require 'flycheck)

(defvar gopath (exec-path-from-shell-getenv "GOPATH"))
(add-to-list 'exec-path (expand-file-name (concat gopath "/bin")))

(setq company-tooltip-limit 20)
(setq company-idle-delay 0)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))

(defun init-go-mode-hook()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (go-eldoc-setup)
  (flycheck-mode)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  t)
;; (add-hook 'go-mode-hook 'init-go-mode-hook)

(add-hook 'before-save-hook 'gofmt-before-save)

;; SETUP MEMO
;;
;; mkdir ~/tmp
;; cd ~/tmp
;; git clone https://github.com/saibing/bingo.git
;; cd bingo
;; GO111MODULE=on go install
;; go get -u github.com/sourcegraph/go-langserver
;;

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(define-key go-mode-map (kbd "M-.") 'lsp-find-definition)

(add-hook 'go-mode-hook #'flycheck-mode)

(provide 'init-go-mode)
