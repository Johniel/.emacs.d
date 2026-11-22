;;; my-misc.el ---  -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'util)
(require 'commands)

;;
(setopt use-short-answers t)
(defalias 'quit 'save-buffers-kill-terminal)

;;
(savehist-mode 1)

;;
(auto-sudoedit-mode 1)

;;
(global-auto-revert-mode -1)

;;
(setq inhibit-startup-screen t)

;;
(set-default 'indicate-empty-lines t)

;;
(setq scroll-conservatively 1)
(pixel-scroll-precision-mode 1)

;;
(add-to-list 'free-keys-modifiers "C-S")

;;
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;
(setq next-line-add-newlines nil)

;;
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;
(prefer-coding-system 'utf-8)

;; *.~
(setq make-backup-files nil)
;; .#*
(setq auto-save-default nil)

;; Auto-save visited files (built-in since Emacs 26.1)
(setq auto-save-visited-interval 5)
(auto-save-visited-mode 1)

;; ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)

;; display full-path in title-bar
;; http://wiki.s17.xrea.com/x/wiki/wiki.cgi?.emacs
(defvar dired-mode-p nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (setq-local dired-mode-p t)))
(setq frame-title-format '((buffer-file-name "%f"
                                             (dired-mode-p default-directory
                                                           mode-line-buffer-identification))))

;;
(add-hook 'window-setup-hook
          (lambda ()
            (set-frame-parameter nil 'fullscreen 'maximized)))

;;
;; http://d.hatena.ne.jp/sugyan/20120120/132703749
(add-hook 'after-init-hook
          (lambda ()
            (message "init time: %.3f sec"
                     (float-time (time-subtract after-init-time before-init-time)))))

(require 'dired-x)

;;
(setq save-place-file (expand-file-name "saved-places" user-emacs-directory))
(save-place-mode 1)

;; http://emacsredux.com/blog/2013/05/04/erase-buffer/
(put 'erase-buffer 'disabled nil)

;; https://lists.gnu.org/archive/html/help-gnu-emacs/2021-05/msg00558.html
(setq elisp-flymake-byte-compile-load-path load-path)

(setopt comint-process-echoes true)

(global-so-long-mode)

;; recentf
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 500)
(setq recentf-exclude
      '("/elpa/"
        "/recentf\\'"
        "/saved-places\\'"
        "/bookmarks\\'"
        "/transient/"))
(recentf-mode 1)


;; hippie-expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;; cua
;; Otherwise cua-mode sets delete-selection-save-to-register to ?0,
;; which highlights text typed over a region.
(setq cua-delete-copy-to-register-0 nil)
(cua-mode 1)
(setq cua-enable-cua-keys nil)
(define-key cua-global-keymap (kbd "C-<return>") nil)
(define-key cua-global-keymap (kbd "C-S-<return>") 'cua-set-rectangle-mark)

;; auto-delete-trailing-whitespace
(defcustom auto-delete-trailing-whitespace true
  "If non-nil, automatically delete trailing whitespace on save in `prog-mode' buffers."
  :type 'boolean
  :group 'editing)

(defun toggle-auto-delete-trailing-whitespace ()
  "Toggle automatic deletion of trailing whitespace on save."
  (interactive)
  (setq auto-delete-trailing-whitespace (not auto-delete-trailing-whitespace))
  (message "Auto delete trailing whitespace: %s"
           (if auto-delete-trailing-whitespace "ON" "OFF")))

(add-hook 'before-save-hook (lambda ()
                              (if (and auto-delete-trailing-whitespace (derived-mode-p 'prog-mode))
                                  (delete-trailing-whitespace))))

;;; my-misc.el ends here
