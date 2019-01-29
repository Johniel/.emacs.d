;;; init.el --- ___Johniel's init file

(unless (version<= "26" emacs-version)
  (error "Use Emacs 26 or higher"))

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'load-package)

;;
(package-initialize)
(install-packages '(ac-php
                    ace-jump-helm-line
                    ace-jump-mode
                    ace-jump-zap
                    aggressive-indent
                    all-ext
                    anzu
                    auto-sudoedit
                    browse-kill-ring
                    clang-format
                    color-moccur
                    company
                    company-c-headers
                    company-go
                    company-php
                    company-quickhelp
                    company-racer
                    dash
                    dockerfile-mode
                    elisp-slime-nav
                    exec-path-from-shell
                    expand-region
                    f
                    flycheck-rust
                    fold-this
                    free-keys
                    git-gutter-fringe
                    go-eldoc
                    go-mode
                    go-rename
                    helm
                    helm-company
                    helm-ls-git
                    helm-swoop
                    highlight-indent-guides
                    highlight-symbol
                    ht
                    keyfreq
                    magit
                    markdown-mode
                    multiple-cursors
                    php-mode
                    plantuml-mode
                    popup
                    popwin
                    protobuf-mode
                    quickrun
                    racer
                    rust-mode
                    s
                    sequential-command
                    shell-pop
                    toml-mode
                    tuareg
                    web-mode
                    wgrep
                    wrap-region
                    yaml-mode
                    yasnippet
                    zlc))

;;
(require 'util)
(add-to-load-path-r "inits")
(add-to-load-path-r "elpa")
(add-to-load-path-r "lisp")
(add-to-load-path-r "site-lisp")
(add-to-load-path-r "theme")

;;
(require 'anzu)
(require 'auto-sudoedit)
(require 'color-moccur)
(require 'dash)
(require 'expand-region)
(require 'f)
(require 'fold-this)
(require 'free-keys)
(require 'git-gutter-fringe)
(require 'highlight-symbol)
(require 'ht)
(require 'keyfreq)
(require 'point-undo)
(require 'protobuf-mode)
(require 's)
(require 'tempbuf)
(require 'typo-fix)
(require 'wgrep)
(require 'yaml-mode)

;;
(require 'init-ace-jump-mode)
(require 'init-auto-save-buffers)
(require 'init-browse-kill-ring)
(require 'init-dired)
(require 'init-flymake)
(require 'init-flyspell)
(require 'init-helm)
(require 'init-hippie-expand)
(require 'init-magit)
(require 'init-multiple-cursors)
(require 'init-popwin)
(require 'init-rust-mode)
(require 'init-scratch)
(require 'init-sequential-command)
(require 'init-shell-pop)
(require 'init-tramp)
(require 'init-tuareg-mode)
(require 'init-wrap-region)
(require 'init-yasnippet)
(require 'init-zlc)

(unless (windows-p) (require 'init-company-mode))
(unless (windows-p) (require 'init-go-mode))
(unless (windows-p) (require 'init-tabbar+))
(unless (windows-p) (require 'init-whitespace))

;; Major Mode Setup
(require 'init-c++-mode)
(require 'init-c-mode)
(require 'init-emacs-lisp-mode)
(require 'init-markdown-mode)
(require 'init-php-mode)
(require 'init-python-mode)
(require 'init-web-mode)

;;
(load "mode-mappings.el")
(load "my-misc.el")
(load "global-bindings.el")
(load "appearance.el")
