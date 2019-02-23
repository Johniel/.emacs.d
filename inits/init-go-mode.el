(require 'go-mode)

(require 'company-go)
(require 'exec-path-from-shell)
(require 'go-eldoc)

(require 's)

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

(use-package lsp-mode
  :ensure t
  :custom ((lsp-inhibit-message t)
           (lsp-message-project-root-warning t)
           (create-lockfiles nil))
  :hook   (prog-major-mode . lsp-prog-major-mode-enable))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-go
  :ensure t
  :after (lsp-mode go-mode)
  :custom (lsp-go-language-server-flags
           '("-gocodecompletion"
             "-diagnostics"
             "-lint-tool=golint"))
  :hook (go-mode . lsp-go-enable)
  :commands lsp-go-enable)

(define-key go-mode-map (kbd "M-.") 'lsp-find-definition)

(add-hook 'go-mode-hook #'lsp)

(provide 'init-go-mode)
