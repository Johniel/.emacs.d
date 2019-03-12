;;; init.el --- ___Johniel's init file

(unless (version<= "26" emacs-version)
  (error "Use Emacs 26 or higher"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'load-package)

;;
(package-initialize)
(install-packages '(ac-php
                    aggressive-indent
                    all-ext
                    anzu
                    auto-sudoedit
                    avy
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
                    quickrun
                    racer
                    rust-mode
                    s
                    sequential-command
                    shell-pop
                    tabbar
                    toml-mode
                    tuareg
                    use-package
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
(require 's)
(require 'tempbuf)
(require 'typo-fix)
(require 'wgrep)
(require 'yaml-mode)

;;
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

;; use-package testing

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

(use-package avy
  :ensure t
  :custom (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package protobuf-mode
  :ensure t
  :config (setq c-basic-offset 2))

(use-package fish-mode :ensure t)
(use-package projectile :ensure t)
(use-package helm-projectile :ensure t)
