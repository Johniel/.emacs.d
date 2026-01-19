(require 'go-mode)

(require 'lsp-mode)
(require 'company-go)
(require 'exec-path-from-shell)

(require 's)
(require 'flycheck)

(defvar gopath (exec-path-from-shell-getenv "GOPATH"))
(add-to-list 'exec-path (expand-file-name (concat gopath "/bin")))

(setq company-tooltip-limit 20)
(setq company-idle-delay 0)
(setq company-echo-delay 0)
(setq company-begin-commands '(self-insert-command))

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

(define-key go-mode-map (kbd "M-.") 'lsp-find-definition)

(add-hook 'go-mode-hook #'flycheck-mode)

(provide 'init-go-mode)
