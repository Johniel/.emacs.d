;;; init.el --- ___Johniel's Emacs configuration -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration.

;;; Code:

(unless (version<= "30" emacs-version)
  (error "Use Emacs 30 or later"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package (Emacs 29+ built-in)
(require 'use-package)

;; Packages from install-packages (now use-package)
(use-package aggressive-indent   :ensure t)
(use-package all-ext             :ensure t)
(use-package dmacro              :ensure t)
(use-package clang-format        :ensure t)
(use-package company             :ensure t)
(use-package company-c-headers   :ensure t)
(use-package company-quickhelp   :ensure t)
(use-package cue-mode            :ensure t)
(use-package dockerfile-mode     :ensure t)
(use-package elisp-slime-nav     :ensure t)
(use-package exec-path-from-shell :ensure t)
;; Workaround for helm-regexp.el bug: helm-source-occur is referenced before definition
(defvar helm-source-occur nil)
(use-package helm                :ensure t)
(use-package helm-company        :ensure t)
(use-package helm-ls-git         :ensure t)
(use-package highlight-indent-guides :ensure t)
(use-package magit
  :ensure t
  :custom
  (magit-diff-refine-hunk t))
(use-package multiple-cursors    :ensure t)
(use-package popup               :ensure t)
(use-package popwin              :ensure t)
(use-package quickrun            :ensure t)
(use-package sequential-command
  :ensure t
  :config
  (require 'sequential-command-config)
  (sequential-command-setup-keys)
  (define-sequential-command seq-home
                             ;;  back-to-indentation
                             beginning-of-line
                             beginning-of-buffer
                             seq-return)
  (global-set-key "\C-a" 'seq-home))
(use-package tabbar              :ensure t)
(use-package typescript-mode     :ensure t)
(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode t)
  (add-to-list 'wrap-region-except-modes 'magit-status-mode))
(use-package yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-wrap-around-region t)
  :bind (:map yas-keymap
              ("<return>" . yas-exit-all-snippets)
              ("C-h" . yas-prev-field)
              ("C-n" . yas-next-field))
  :config
  (yas-global-mode +1)
  (if (not window-system)
      (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))))

(use-package zlc
  :ensure t
  :bind (:map minibuffer-local-map
              ("<down>" . zlc-select-next-vertical)
              ("<up>" . zlc-select-previous-vertical)
              ("<right>" . zlc-select-next)
              ("<left>" . zlc-select-previous)
              ("C-d" . zlc-reset))
  :config
  (zlc-mode t))

;; Packages with configuration
(use-package avy
  :ensure t
  :custom
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (avy-style 'at))

(use-package protobuf-mode
  :ensure t
  :hook (protobuf-mode . (lambda () (setq-local c-basic-offset 2))))

(use-package lsp-mode
  :ensure t
  :custom ((lsp-inhibit-message t)
           (lsp-message-project-root-warning t))
  :config (setq lsp-headerline-breadcrumb-enable nil)
  :hook   (go-mode . lsp))

(use-package lsp-ui :ensure t)

(use-package ruby-mode
  :ensure t
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package cc-mode
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (c-set-style "ellemtel")
              (c-toggle-electric-state +1)
              (setq c-basic-offset 2))))

(use-package yaml-mode      :ensure t)
(use-package terraform-mode :ensure t)
(use-package php-mode       :ensure t)
(use-package go-mode
  :ensure t
  :config
  ;; GOPATH/bin設定を統合
  (when-let ((gopath (getenv "GOPATH")))
    (add-to-list 'exec-path (expand-file-name "bin" gopath))))
(use-package go-rename      :ensure t)
(use-package plantuml-mode  :ensure t)
(use-package toml-mode      :ensure t)
(use-package web-mode
  :ensure t
  :custom
  (web-mode-markup-indent-offset 2) ; html indent
  (web-mode-css-indent-offset  2)   ; css indent
  (web-mode-code-indent-offset 2)   ; script indent(js,php,etc..)
  :config
  (add-hook 'web-mode-hook
            '(lambda ()
               (setq web-mode-indent-style 2))))
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setopt markdown-fontify-code-blocks-natively t))

(use-package shell-pop
  :ensure t
  :config
  (setq shell-pop-window-height 30)
  (setq shell-pop-window-position "bottom"))

(use-package hippie-exp
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol)))

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
(use-package projectile        :ensure t)
(use-package s                 :ensure t)
(use-package wgrep             :ensure t)
(use-package key-chord
  :ensure t
  :custom ((key-chord-two-keys-delay 0.03))
  :init (key-chord-mode 1)
  :config (key-chord-define-global "kl" 'avy-goto-word-0))

(use-package persistent-scratch
  :ensure t
  :custom
  (persistent-scratch-autosave-interval 5)
  :config
  (persistent-scratch-setup-default)
  ;; kill防止機能
  (add-hook 'kill-buffer-query-functions
            (lambda ()
              (if (string= "*scratch*" (buffer-name))
                  (progn
                    (erase-buffer)
                    (message "*scratch* cleared instead of killed")
                    nil)  ; killを拒否
                t))))

;; Load path and utilities
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'util)
(add-to-load-path-r "inits")
(add-to-load-path-r "elpa")
(add-to-load-path-r "lisp")
(add-to-load-path-r "site-lisp")
(add-to-load-path-r "theme")

;; Load path and utilities
(use-package point-undo :load-path "site-lisp/point-undo")
(use-package typo-fix   :load-path "site-lisp/typo-fix")

;; Auto-save visited files (built-in since Emacs 26.1)
(setq auto-save-visited-interval 5)
(auto-save-visited-mode 1)

;; Mode-specific init files

(require 'init-flymake)
(require 'init-helm)
(require 'init-tabbar)
(require 'init-multiple-cursors)

(unless (windows-p) (require 'init-company-mode))

(require 'init-c++-mode)
(require 'init-emacs-lisp-mode)

;; File associations
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.snip\\'" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))

;; Configuration files
(load "my-misc.el")
(load "global-bindings.el")
(load "appearance.el")

;;; init.el ends here
