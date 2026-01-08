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

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package (Emacs 29+ built-in)
(require 'use-package)

;; Load path and utilities
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'util)
(add-to-load-path-r "elpa")
(add-to-load-path-r "lisp")
(add-to-load-path-r "site-lisp")
(add-to-load-path-r "theme")

;; Site-lisp packages
(use-package point-undo :load-path "site-lisp/point-undo")
(use-package tempbuf    :load-path "site-lisp/tempbuf")
(use-package typo-fix   :load-path "site-lisp/typo-fix")
(use-package auto-save-buffers
  :load-path "site-lisp/auto-save-buffers"
  :config
  (run-with-idle-timer 5 t 'auto-save-buffers))

;;; ============================================================================
;;; Core packages
;;; ============================================================================

(use-package dash :ensure t)
(use-package s :ensure t)
(use-package f :ensure t)
(use-package ht :ensure t)

;;; ============================================================================
;;; UI / Appearance packages
;;; ============================================================================

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (setq popwin:popup-window-height 25)
  (push "*Backtrace*" popwin:special-display-config)
  (push "*quickrun*" popwin:special-display-config))

(use-package tabbar
  :ensure t
  :if (and window-system (not (windows-p)))
  :config
  (require 'projectile)
  ;; remove {home, left, right} button
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil) (cons "" nil))))
  (tabbar-mwheel-mode 0)
  (setq tabbar-auto-scroll-flag 1)
  (setq tabbar-separator '(1.2))
  (setq tabbar-cycle-scope 'tabs)
  (tabbar-mode t)

  (defvar my/-buffer-project-name-memo '())
  (defun my/-memoized-buffer-project-name ()
    (let ((key (buffer-file-name (current-buffer))))
      (or (cdr (assoc key my/-buffer-project-name-memo))
          (let ((val (projectile-project-name (projectile-project-root (buffer-file-name (current-buffer))))))
            (cdr (assoc key (add-to-list 'my/-buffer-project-name-memo (cons key val))))))))
  (defun my/tabbar-buffer-groups ()
    (list (cond ((my/-memoized-buffer-project-name)) ("default"))))
  (setq tabbar-buffer-groups-function 'my/tabbar-buffer-groups))

(use-package highlight-indent-guides :ensure t)
(use-package highlight-symbol :ensure t)

;;; ============================================================================
;;; Completion / Helm
;;; ============================================================================

;; Workaround for helm-regexp.el bug
(defvar helm-source-occur nil)

(use-package helm
  :ensure t
  :config
  (require 'cl-lib)
  (require 'helm-command)
  (require 'helm-files)
  (require 'helm-imenu)

  (setq helm-input-idle-delay 0)
  (setq helm-candidate-number-limit 300)

  (define-key helm-map (kbd "C-h")   'helm-previous-line)
  (define-key helm-map (kbd "C-n")   'helm-next-line)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-h") 'helm-previous-source)

  (setq helm-mini-default-sources
        (append '(helm-source-recentf)
                (if (windows-p) '()
                  '(helm-source-ls-git-status helm-source-ls-git))))

  ;; Window display settings
  (setq helm-split-window-inside-p t)
  (setq helm-split-window-default-side 'below)
  (setq helm-display-buffer-default-height 0.5))

(use-package helm-ls-git :ensure t)
(use-package helm-company :ensure t)
(use-package helm-projectile
  :ensure t
  :bind ("C-S-w" . helm-projectile))

(use-package company
  :ensure t
  :unless (windows-p)
  :config
  (global-company-mode t)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (with-eval-after-load 'company
    (define-key company-mode-map (kbd "C-S-f") 'helm-company)
    (define-key company-active-map (kbd "C-S-f") 'helm-company)))

(use-package company-c-headers
  :ensure t
  :unless (windows-p)
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-go :ensure t)

(use-package company-quickhelp
  :ensure t
  :unless (windows-p)
  :config
  (company-quickhelp-mode t))

(use-package zlc
  :ensure t
  :config
  (let ((map minibuffer-local-map))
    (define-key map (kbd "<down>")  'zlc-select-next-vertical)
    (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
    (define-key map (kbd "<right>") 'zlc-select-next)
    (define-key map (kbd "<left>")  'zlc-select-previous)
    (define-key map (kbd "C-d") 'zlc-reset))
  (zlc-mode t))

;;; ============================================================================
;;; Editing helpers
;;; ============================================================================

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode +1)
  (setq yas-wrap-around-region t)
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
  (define-key yas-keymap (kbd "C-h") 'yas-prev-field)
  (define-key yas-keymap (kbd "C-n") 'yas-next-field)
  (unless window-system
    (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)))

  ;; Helm integration for yasnippet
  (defun my-yas-prompt (prompt choices &optional display-fn)
    (let* ((names (cl-loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
           (selected (helm-other-buffer
                      `(((name . ,(format "%s" prompt))
                         (candidates . names)
                         (action . (("Insert snippet" . (lambda (arg) arg))))))
                      "*helm yas-prompt*")))
      (if selected
          (let ((n (cl-position selected names :test 'equal)))
            (nth n choices))
        (signal 'quit "user quit!"))))
  (setq yas-prompt-functions '(my-yas-prompt)))

(use-package multiple-cursors
  :ensure t
  :config
  (defvar mc:insert-numbers-hist nil)
  (defvar mc:insert-numbers-inc 1)
  (defvar mc:insert-numbers-pad "%01d")

  (defun mc:insert-numbers (start inc pad)
    "Insert increasing numbers for each cursor specifically."
    (interactive
     (list (read-number "Start from: " 0)
           (read-number "Increment by: " 1)
           (read-string "Padding (%01d): " nil mc:insert-numbers-hist "%01d")))
    (setq mc--insert-numbers-number start)
    (setq mc:insert-numbers-inc inc)
    (setq mc:insert-numbers-pad pad)
    (mc/for-each-cursor-ordered
     (mc/execute-command-for-fake-cursor 'mc:-insert-number-and-increase cursor)))

  (defun mc:-insert-number-and-increase ()
    (interactive)
    (insert (format mc:insert-numbers-pad mc--insert-numbers-number))
    (setq mc--insert-numbers-number (+ mc--insert-numbers-number mc:insert-numbers-inc))))

(use-package expand-region :ensure t)

(use-package wrap-region
  :ensure t
  :config
  (wrap-region-global-mode t)
  (add-to-list 'wrap-region-except-modes 'magit-status-mode))

(use-package aggressive-indent :ensure t)

(use-package browse-kill-ring
  :ensure t
  :config
  (require 'cl-lib)
  (browse-kill-ring-default-keybindings)
  (add-hook 'browse-kill-ring-hook
            (lambda ()
              (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
              (define-key browse-kill-ring-mode-map (kbd "h") 'browse-kill-ring-previous)))
  (setq browse-kill-ring-separator (concat "\n" (make-string 100 ?\\)))
  (setq browse-kill-ring-highlight-current-entry t)

  (defface my-browse-kill-ring-current-entry-face
    '((t (:background "dark green" :highlight t))) nil)

  (defun my-browse-kill-ring-forward (&optional arg)
    "Move forward by ARG `kill-ring' entries."
    (interactive "p")
    (beginning-of-line)
    (while (not (zerop arg))
      (if (< arg 0)
          (progn
            (cl-incf arg)
            (if (overlays-at (point))
                (progn
                  (goto-char (overlay-start (car (overlays-at (point)))))
                  (goto-char (previous-overlay-change (point)))
                  (goto-char (previous-overlay-change (point))))
              (progn
                (goto-char (1- (previous-overlay-change (point))))
                (unless (bobp)
                  (goto-char (overlay-start (car (overlays-at (point)))))))))
        (progn
          (cl-decf arg)
          (if (overlays-at (point))
              (progn
                (goto-char (overlay-end (car (overlays-at (point)))))
                (goto-char (next-overlay-change (point))))
            (goto-char (next-overlay-change (point)))
            (unless (eobp)
              (goto-char (overlay-start (car (overlays-at (point))))))))))
    (when browse-kill-ring-highlight-current-entry
      (let ((overs (overlay-lists))
            (current-overlay (car (overlays-at (point)))))
        (mapcar (lambda (o) (overlay-put o 'face nil))
                (nconc (car overs) (cdr overs)))
        (overlay-put current-overlay 'face 'my-browse-kill-ring-current-entry-face)))
    (when browse-kill-ring-recenter
      (recenter 1)))

  (add-hook 'browse-kill-ring-mode-hook
            (lambda ()
              (setq-local hl-line-range-function (lambda () '(0 . 0)))))

  (advice-add 'browse-kill-ring-forward :override
              (lambda (&optional arg)
                (interactive "p")
                (my-browse-kill-ring-forward arg))))

(use-package sequential-command
  :ensure t
  :config
  (require 'sequential-command-config)
  (sequential-command-setup-keys)
  (define-sequential-command seq-home
    beginning-of-line
    beginning-of-buffer
    seq-return)
  (global-set-key "\C-a" 'seq-home))

;; hippie-expand configuration
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;;; ============================================================================
;;; Version control
;;; ============================================================================

(use-package magit
  :ensure t
  :custom
  (magit-diff-options '("-w" "-b"))
  (magit-last-seen-setup-instructions "1.4.0")
  :config
  (defun my-magit-status-fullscreen (orig-fn &rest args)
    "Run magit-status in fullscreen."
    (window-configuration-to-register :magit-fullscreen)
    (apply orig-fn args)
    (delete-other-windows))
  (advice-add 'magit-status :around #'my-magit-status-fullscreen)

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

(use-package git-gutter-fringe :ensure t)

;;; ============================================================================
;;; Syntax checking
;;; ============================================================================

(use-package flymake
  :config
  (require 'flymake-proc)
  (setq flymake-start-syntax-check-on-newline nil)
  (setq flymake-log-level -1)

  (defun my-flymake-show-help ()
    (when (get-char-property (point) 'flymake-overlay)
      (let ((help (get-char-property (point) 'help-echo)))
        (if help (message "%s" help)))))
  (add-hook 'post-command-hook 'my-flymake-show-help)

  ;; C++
  (defun flymake-cpp-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "g++" (list "-Wall" "-fsyntax-only" "-std=c++20" local-file))))
  (push '("\\.cpp$" flymake-cpp-init) flymake-proc-allowed-file-name-masks)
  (push '("\\.hpp$" flymake-cpp-init) flymake-proc-allowed-file-name-masks)

  ;; C
  (defun flymake-c-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "gcc" (list "-Wall" "-fsyntax-only" local-file))))
  (push '("\\.c$" flymake-c-init) flymake-proc-allowed-file-name-masks)
  (push '("\\.h$" flymake-c-init) flymake-proc-allowed-file-name-masks)

  ;; Emacs Lisp
  (defun flymake-elisp-init ()
    (unless (string-match "^ " (buffer-name))
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list
         (expand-file-name invocation-name invocation-directory)
         (list
          "-Q" "--batch" "--eval"
          (prin1-to-string
           (quote
            (dolist (file command-line-args-left)
              (with-temp-buffer
                (insert-file-contents file)
                (emacs-lisp-mode)
                (let ((parse-sexp-ignore-comments t))
                  (condition-case data
                      (scan-sexps (point-min) (point-max))
                    (scan-error
                     (goto-char(nth 2 data))
                     (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                    file (line-number-at-pos))))))))))
          local-file)))))
  (push '("\\.el$" flymake-elisp-init) flymake-proc-allowed-file-name-masks)

  (defun my-flymake-set-no-query-on-exit (proc)
    "Set flag to allow exit without query on flymake processes."
    (when proc
      (set-process-query-on-exit-flag proc nil))
    proc)
  (advice-add 'flymake-start-syntax-check-process :filter-return #'my-flymake-set-no-query-on-exit))

(use-package flyspell
  :config
  (require 'popup)
  (defun flyspell-correct-word-popup-el ()
    "Pop up a menu of possible corrections for misspelled word before point."
    (interactive)
    (flyspell-accept-buffer-local-defs)
    (let ((cursor-location (point))
          (word (flyspell-get-word nil)))
      (if (consp word)
          (let ((start (car (cdr word)))
                (end (car (cdr (cdr word))))
                (word (car word))
                poss ispell-filter)
            (ispell-send-string "%\n")
            (ispell-send-string (concat "^" word "\n"))
            (while (progn
                     (accept-process-output ispell-process)
                     (not (string= "" (car ispell-filter)))))
            (setq ispell-filter (cdr ispell-filter))
            (or ispell-filter (setq ispell-filter '(*)))
            (if (consp ispell-filter)
                (setq poss (ispell-parse-output (car ispell-filter))))
            (cond
             ((or (eq poss t) (stringp poss)) t)
             ((null poss) (error "Ispell: error in Ispell process"))
             (t
              (flyspell-do-correct (popup-menu* (car (cddr poss)) :scroll-bar t :margin t)
                                   poss word cursor-location start end cursor-location)))
            (ispell-pdict-save t))))))

;;; ============================================================================
;;; Language modes
;;; ============================================================================

(use-package cc-mode
  :config
  ;; C mode
  (add-hook 'c-mode-hook
            (lambda ()
              (c-set-style "ellemtel")
              (c-toggle-electric-state +1)
              (setq c-basic-offset 2)
              (setq tab-width 2)
              (flymake-mode t)))

  ;; C++ mode
  (require 'clang-format)
  (defun clang-format-format-buffer ()
    (when (eq major-mode 'c++-mode)
      (clang-format-buffer "file")))
  (def-face c++-macro "cyan1" "C++ macro form")
  (add-keywords 'c++-mode '("each" "unless" "each_with_index" "each_pair") 'c++-macro)
  (add-hook 'c++-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") nil)
              (local-set-key (kbd "C-x C-a") nil)
              (local-set-key (kbd "C-c C-k") nil)
              (c-set-style "ellemtel")
              (c-toggle-electric-state +1)
              (c-set-offset 'inlambda 0)
              (setq c-basic-offset 2)
              (setq tab-width 2)
              (add-hook 'before-save-hook 'delete-trailing-whitespace)
              (flymake-mode t))))

(use-package clang-format :ensure t)

(use-package go-mode
  :ensure t
  :unless (windows-p)
  :config
  (require 'lsp-mode)
  (require 'exec-path-from-shell)
  (require 'go-eldoc)
  (require 's)

  (defvar gopath (exec-path-from-shell-getenv "GOPATH"))
  (add-to-list 'exec-path (expand-file-name (concat gopath "/bin")))

  (add-hook 'before-save-hook 'gofmt-before-save)
  (define-key go-mode-map (kbd "M-.") 'lsp-find-definition)
  :hook (go-mode . flycheck-mode))

(use-package go-eldoc :ensure t)
(use-package go-rename :ensure t)
(use-package exec-path-from-shell :ensure t)

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-inhibit-message t)
  (lsp-message-project-root-warning t)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook (go-mode . lsp))

(use-package lsp-ui :ensure t)

(use-package web-mode
  :ensure t
  :mode ("\\.twig\\'" . web-mode)
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :hook (web-mode . (lambda () (setq web-mode-indent-style 2))))

(use-package emacs-lisp-mode
  :config
  (def-face elisp-special "khaki" "elisp special")
  (add-keywords 'emacs-lisp-mode '("true" "false" "nil") 'elisp-special)
  (define-key emacs-lisp-mode-map (kbd "C-x e") 'pp-macroexpand-last-sexp)
  :hook ((emacs-lisp-mode . turn-on-eldoc-mode)
         (emacs-lisp-mode . elisp-slime-nav-mode)
         (emacs-lisp-mode . aggressive-indent-mode)
         (emacs-lisp-mode . (lambda () (when buffer-file-name (flymake-mode))))))

(use-package elisp-slime-nav :ensure t)

(use-package protobuf-mode
  :ensure t
  :hook (protobuf-mode . (lambda () (setq-local c-basic-offset 2))))

(use-package ruby-mode
  :ensure t
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package php-mode :ensure t)
(use-package typescript-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package toml-mode :ensure t)
(use-package fish-mode :ensure t)
(use-package cue-mode :ensure t)
(use-package terraform-mode :ensure t)
(use-package plantuml-mode :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :config
  (setopt markdown-fontify-code-blocks-natively t))

;;; ============================================================================
;;; Dired
;;; ============================================================================

(use-package dired
  :config
  (require 'dired-x)
  (require 'wdired)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;;; ============================================================================
;;; Whitespace
;;; ============================================================================

(use-package whitespace
  :unless (windows-p)
  :config
  (setq whitespace-style '(tabs tab-mark spaces space-mark))
  (setq whitespace-space-regexp "\\(\x3000+\\)")
  (setq whitespace-display-mappings
        '((space-mark ?\x3000 [?\　])
          (tab-mark   ?\t   [?\xBB ?\t]))))

;;; ============================================================================
;;; Scratch buffer
;;; ============================================================================

(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(defun my-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))

(add-hook 'kill-buffer-query-functions
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my-make-scratch 0) nil)
                        t))))

(add-hook 'after-save-hook
          (function (lambda ()
                      (unless (member "*scratch*" (my-buffer-name-list))
                        (my-make-scratch 1)))))

;;; ============================================================================
;;; Other packages
;;; ============================================================================

(use-package avy
  :ensure t
  :custom (avy-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)))

(use-package key-chord
  :ensure t
  :custom (key-chord-two-keys-delay 0.03)
  :init (key-chord-mode 1)
  :config (key-chord-define-global "kl" 'avy-goto-word-0))

(use-package shell-pop
  :ensure t
  :config
  (setq shell-pop-window-height 30)
  (setq shell-pop-window-position "bottom"))

(use-package anzu :ensure t)
(use-package auto-sudoedit :ensure t)
(use-package color-moccur :ensure t)
(use-package free-keys :ensure t)
(use-package keyfreq :ensure t)
(use-package popup :ensure t)
(use-package projectile :ensure t)
(use-package quickrun :ensure t)
(use-package wgrep :ensure t)
(use-package all-ext :ensure t)

;;; ============================================================================
;;; Configuration files
;;; ============================================================================

(load "mode-mappings.el")
(load "my-misc.el")
(load "global-bindings.el")
(load "appearance.el")

;;; init.el ends here
