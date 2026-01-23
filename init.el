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

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'util)

;; use-package (Emacs 29+ built-in)
(require 'use-package)
(setq use-package-always-ensure t)

(use-package all-ext)
(use-package auto-sudoedit)
(use-package color-moccur)
(use-package cue-mode)
(use-package dash)
(use-package dmacro)
(use-package dockerfile-mode)
(use-package exec-path-from-shell)
(use-package expand-region)
(use-package f)
(use-package fish-mode)
(use-package free-keys)
(use-package highlight-symbol)
(use-package ht)
(use-package multiple-cursors)
(use-package php-mode)
(use-package plantuml-mode)
(use-package popup)
(use-package projectile)
(use-package quickrun)
(use-package ruby-mode)
(use-package s)
(use-package tabbar)
(use-package terraform-mode)
(use-package toml-mode)
(use-package typescript-mode)
(use-package yaml-mode)

(use-package anzu
  :config
  (global-anzu-mode 1))

(use-package keyfreq
  :config
  (keyfreq-mode 1))

(use-package wgrep
  :custom
  (wgrep-enable-key "r")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package magit
  :custom
  (magit-diff-refine-hunk t))

(use-package git-gutter-fringe
  :custom
  (git-gutter-fr:side 'right-fringe)
  :config
  (global-git-gutter-mode t))

(use-package sequential-command
  :config
  (require 'sequential-command-config)
  (sequential-command-setup-keys)
  (define-sequential-command seq-home
                             ;;  back-to-indentation
                             beginning-of-line
                             beginning-of-buffer
                             seq-return)
  (global-set-key "\C-a" 'seq-home))

(use-package wrap-region
  :config
  (wrap-region-global-mode t)
  (add-to-list 'wrap-region-except-modes 'magit-status-mode))

(use-package yasnippet
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-wrap-around-region t)
  :config
  (yas-global-mode +1)
  (if (not window-system)
      (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)))
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
  (define-key yas-keymap (kbd "C-h") 'yas-prev-field)
  (define-key yas-keymap (kbd "C-n") 'yas-next-field))

(use-package zlc
  :config
  (define-key minibuffer-local-map (kbd "<down>")  'zlc-select-next-vertical)
  (define-key minibuffer-local-map (kbd "<up>")    'zlc-select-previous-vertical)
  (define-key minibuffer-local-map (kbd "<right>") 'zlc-select-next)
  (define-key minibuffer-local-map (kbd "<left>")  'zlc-select-previous)
  (define-key minibuffer-local-map (kbd "C-d") 'zlc-reset)
  (zlc-mode t))

(use-package avy
  :custom
  (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s))
  (avy-style 'at))

(use-package protobuf-mode
  :config
  (add-hook 'protobuf-mode-hook (lambda () (setq-local c-basic-offset 2))))

(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2) ; html indent
  (web-mode-css-indent-offset 2)    ; css indent
  (web-mode-code-indent-offset 2)   ; script indent(js,php,etc..)
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (setq web-mode-indent-style 2))))

(use-package markdown-mode
  :custom
  (markdown-command "multimarkdown")
  :config
  (setopt markdown-fontify-code-blocks-natively t)
  ;; Disable language prompt when inserting fenced code blocks
  (setq markdown-gfm-use-electric-backquote nil))

(use-package shell-pop
  :custom
  (shell-pop-window-height 30)
  (shell-pop-window-position "bottom"))

(use-package key-chord
  :custom
  (key-chord-two-keys-delay 0.03)
  :config
  (key-chord-mode 1)
  (key-chord-define-global "kl" 'avy-goto-word-0))

(use-package persistent-scratch
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

(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character))

;; tabbar
(use-package tabbar
  :after (util projectile)
  :custom
  (tabbar-auto-scroll-flag 1)
  (tabbar-separator '(1.2))
  (tabbar-cycle-scope 'tabs)
  :config
  ;; remove {home, left, right} button
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  (tabbar-mwheel-mode 0)

  (when (and window-system (not (windows-p)))
    (tabbar-mode t))

  (defvar my/-buffer-project-name-memo '())
  (defun my/-memoized-buffer-project-name ()
    (let ((key (buffer-file-name (current-buffer))))
      (or (cdr (assoc key my/-buffer-project-name-memo))
          (let ((val (projectile-project-name (projectile-project-root (buffer-file-name (current-buffer))))))
            (cdr (assoc key (add-to-list 'my/-buffer-project-name-memo (cons key val))))))))

  (defun my/tabbar-buffer-groups ()
    (list
     (cond
      ((my/-memoized-buffer-project-name))
      ;; fallback default name
      ("default"))))

  (setq tabbar-buffer-groups-function 'my/tabbar-buffer-groups))


;; EmacsLisp
(use-package aggressive-indent)
(use-package elisp-slime-nav)

(def-face elisp-special "khaki" "elisp special")
(add-keywords 'emacs-lisp-mode '("true" "false" "nil") 'elisp-special)
(define-key emacs-lisp-mode-map (kbd "C-x e") 'pp-macroexpand-last-sexp)
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (elisp-slime-nav-mode t)
                                  (aggressive-indent-mode)
                                  (turn-on-eldoc-mode)))


;; Go
(use-package go-mode
  :config
  ;; GOPATH/bin設定を統合
  (when-let ((gopath (getenv "GOPATH")))
    (add-to-list 'exec-path (expand-file-name "bin" gopath))))

(use-package go-rename)


;; C/C++
(use-package cc-mode
  :ensure nil
  :after util
  :config
  (def-face c++-macro "cyan1" "C++ macro form")
  (add-keywords 'c++-mode '("each" "unless" "each_with_index" "each_pair") 'c++-macro)
  (add-hook 'c-mode-hook
            (lambda ()
              (c-set-style "ellemtel")
              (c-toggle-electric-state +1)
              (setq c-basic-offset 2)))
  (add-hook 'c++-mode-hook
            (lambda()
              (local-set-key (kbd "C-x C-e") nil)
              (local-set-key (kbd "C-x C-a") nil)
              (local-set-key (kbd "C-c C-k") nil)
              (c-set-style "ellemtel")
              (c-toggle-electric-state +1)
              (c-set-offset 'inlambda 0)
              (setq c-basic-offset 2)
              (setq tab-width 2))))


;; LSP
(use-package lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  :config
  (add-hook 'go-mode-hook 'lsp))

(use-package lsp-ui)


;; Helm packages (with workaround for helm-regexp.el bug)
;; Workaround for helm-regexp.el bug: helm-source-occur is referenced before definition
(defvar helm-source-occur nil)

(use-package helm
  :custom
  (helm-input-idle-delay 0)
  (helm-candidate-number-limit 300)
  ;; 現在のウィンドウ内で分割（フレーム全体ではなく）
  (helm-split-window-inside-p t)
  ;; 下方向に分割（カーソル位置を上部に維持）
  (helm-split-window-default-side 'below)
  ;; 高さをウィンドウの50%に設定
  (helm-display-buffer-default-height 0.5)
  :config
  (define-key helm-map (kbd "C-h")   'helm-previous-line)
  (define-key helm-map (kbd "C-n")   'helm-next-line)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-h") 'helm-previous-source))

(use-package helm-ls-git
  :custom
  (helm-mini-default-sources '(helm-source-recentf
                               helm-source-ls-git-status
                               helm-source-ls-git)))

(use-package helm-projectile)

(use-package helm-c-yasnippet
  :custom
  (helm-yas-space-match-any-greedy t))

(use-package helm-company
  :after (helm company))

;; Company
(use-package company
  :config
  (global-company-mode 1)
  (add-to-list 'company-backends 'company-c-headers)
  (define-key company-mode-map   (kbd "C-S-f") 'helm-company)
  (define-key company-active-map (kbd "C-S-f") 'helm-company))

(use-package company-c-headers
  :after company)

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode 1))


;; Load path and utilities
(add-to-load-path-r "elpa")
(add-to-load-path-r "site-lisp")
(add-to-load-path-r "theme")

(use-package typo-fix :load-path "site-lisp/typo-fix")

(require 'init-flymake)

(load "my-misc.el")
(load "global-bindings.el")
(load "appearance.el")

;; File associations
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.snip\\'" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;;; init.el ends here
