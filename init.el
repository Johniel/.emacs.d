;;; init.el --- ___Johniel's Emacs configuration -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:

(unless (version<= "29" emacs-version)
  (error "Use Emacs 29 or later"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'load-package)

(install-packages '(aggressive-indent
                    all-ext
                    auto-sudoedit
                    browse-kill-ring
                    clang-format
                    company
                    company-c-headers
                    company-go
                    company-quickhelp
                    cue-mode
                    dockerfile-mode
                    elisp-slime-nav
                    exec-path-from-shell
                    flycheck-rust
                    go-eldoc
                    go-mode
                    go-rename
                    helm
                    helm-company
                    helm-ls-git
                    helm-swoop
                    highlight-indent-guides
                    magit
                    multiple-cursors
                    php-mode
                    plantuml-mode
                    popup
                    popwin
                    quickrun
                    racer
                    rust-mode
                    sequential-command
                    shell-pop
                    tabbar
                    toml-mode
                    typescript-mode
                    use-package
                    web-mode
                    wrap-region
                    yasnippet
                    zlc))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(require 'use-package)

(use-package avy
  :ensure t
  :custom (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package protobuf-mode
  :ensure t
  :config (setq c-basic-offset 2))

(use-package lsp-mode
  :ensure t
  :custom ((lsp-inhibit-message t)
           (lsp-message-project-root-warning t)
           (create-lockfiles nil))
  :config (setq lsp-headerline-breadcrumb-enable nil)
  :hook   (go-mode . lsp))

(use-package lsp-ui :ensure t)

(use-package ruby-mode
  :ensure t
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package shell-pop
  :ensure t
  :config
  (setq shell-pop-window-height 30)
  (setq shell-pop-window-position "bottom"))

(use-package anzu              :ensure t)
(use-package auto-sudoedit     :ensure t)
(use-package color-moccur      :ensure t)
(use-package dash              :ensure t)
(use-package expand-region     :ensure t)
(use-package f                 :ensure t)
(use-package fish-mode         :ensure t)
(use-package free-keys         :ensure t)
(use-package git-gutter-fringe :ensure t)
(use-package helm-projectile
  :ensure t
  :bind ("C-S-w" . helm-projectile))
(use-package highlight-symbol  :ensure t)
(use-package ht                :ensure t)
(use-package keyfreq           :ensure t)
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setopt markdown-fontify-code-blocks-natively t))
(use-package projectile        :ensure t)
(use-package s                 :ensure t)
(use-package terraform-mode    :ensure t)
(use-package wgrep             :ensure t)
(use-package yaml-mode         :ensure t)
(use-package key-chord
  :ensure t
  :custom ((key-chord-two-keys-delay 0.03))
  :init (key-chord-mode 1)
  :config (key-chord-define-global "kl" 'avy-goto-word-0))

(use-package point-undo :load-path "site-lisp/point-undo")
(use-package tempbuf    :load-path "site-lisp/tempbuf")
(use-package typo-fix   :load-path "site-lisp/typo-fix")

(require 'util)
(add-to-load-path-r "inits")
(add-to-load-path-r "elpa")
(add-to-load-path-r "lisp")
(add-to-load-path-r "site-lisp")
(add-to-load-path-r "theme")

(require 'init-auto-save-buffers)
(require 'init-browse-kill-ring)
(require 'init-dired)
(require 'init-flymake)
(require 'init-flyspell)
(require 'init-helm)
(require 'init-tabbar)
(require 'init-hippie-expand)
(require 'init-magit)
(require 'init-multiple-cursors)
(require 'init-popwin)
(require 'init-rust-mode)
(require 'init-scratch)
(require 'init-sequential-command)
(require 'init-wrap-region)
(require 'init-yasnippet)
(require 'init-zlc)

(unless (windows-p) (require 'init-company-mode))
(unless (windows-p) (require 'init-go-mode))
(unless (windows-p) (require 'init-whitespace))

(require 'init-c++-mode)
(require 'init-c-mode)
(require 'init-emacs-lisp-mode)
(require 'init-web-mode)

(load "mode-mappings.el")
(load "my-misc.el")
(load "global-bindings.el")
(load "appearance.el")

;;; init.el ends here
