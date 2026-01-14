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
(use-package browse-kill-ring    :ensure t)
(use-package clang-format        :ensure t)
(use-package company             :ensure t)
(use-package company-c-headers   :ensure t)
(use-package company-go          :ensure t)
(use-package company-quickhelp   :ensure t)
(use-package cue-mode            :ensure t)
(use-package dockerfile-mode     :ensure t)
(use-package elisp-slime-nav     :ensure t)
(use-package exec-path-from-shell :ensure t)
(use-package go-eldoc            :ensure t)
(use-package go-mode             :ensure t)
(use-package go-rename           :ensure t)
;; Workaround for helm-regexp.el bug: helm-source-occur is referenced before definition
(defvar helm-source-occur nil)
(use-package helm                :ensure t)
(use-package helm-company        :ensure t)
(use-package helm-ls-git         :ensure t)
(use-package highlight-indent-guides :ensure t)
(use-package magit               :ensure t)
(use-package multiple-cursors    :ensure t)
(use-package php-mode            :ensure t)
(use-package plantuml-mode       :ensure t)
(use-package popup               :ensure t)
(use-package popwin              :ensure t)
(use-package quickrun            :ensure t)
(use-package sequential-command  :ensure t)
(use-package tabbar              :ensure t)
(use-package toml-mode           :ensure t)
(use-package typescript-mode     :ensure t)
(use-package web-mode            :ensure t)
(use-package wrap-region         :ensure t)
(use-package yasnippet           :ensure t)
(use-package zlc                 :ensure t)

;; Packages with configuration
(use-package avy
  :ensure t
  :custom (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

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
(use-package tempbuf    :load-path "site-lisp/tempbuf")
(use-package typo-fix   :load-path "site-lisp/typo-fix")

;; Auto-save visited files (built-in since Emacs 26.1)
(setq auto-save-visited-interval 5)
(auto-save-visited-mode 1)

;; Mode-specific init files
(require 'init-browse-kill-ring)
(require 'init-flymake)
(require 'init-flyspell)
(require 'init-helm)
(require 'init-tabbar)
(require 'init-hippie-expand)
(require 'init-magit)
(require 'init-multiple-cursors)
(require 'init-scratch)
(require 'init-sequential-command)
(require 'init-yasnippet)
(require 'init-zlc)

(unless (windows-p) (require 'init-company-mode))
(unless (windows-p) (require 'init-go-mode))

(require 'init-c++-mode)
(require 'init-c-mode)
(require 'init-emacs-lisp-mode)
(require 'init-web-mode)

;; Configuration files
(load "mode-mappings.el")
(load "my-misc.el")
(load "global-bindings.el")
(load "appearance.el")

;;; init.el ends here
