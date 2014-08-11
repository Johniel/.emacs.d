;;; my-misc.el ---

(require 'util)
(require 'commands)

;;
(when (eq system-type 'gnu/linux)
  (require 'notifications))

;;
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'quit 'save-buffers-kill-terminal)

;;
(setq initial-scratch-message (concat initial-scratch-message
                                      "(require 'f)\n"
                                      "(require 's)\n"
                                      "(require 'ht)\n"
                                      "(require 'dash)\n"))
;;
(savehist-mode 1)

;;
(global-auto-revert-mode -1)
;; http://d.hatena.ne.jp/syohex/20130206/1360157000
(require 'notifications)
(defun my/after-revert-hook ()
  (notifications-notify :title (format "Revert %s" (buffer-file-name))
                        :body "Check it out" :urgency 'critical))
(add-hook 'after-revert-hook 'my/after-revert-hook)

;;
(global-git-gutter-mode t)
(setq git-gutter-fr:side 'right-fringe)

;;
(push '("*quickrun*") popwin:special-display-config)

;;
(recentf-mode 1)
(setq recentf-max-menu-items 200)
(setq recentf-max-saved-items 500)

;;
(iswitchb-mode 1)
(setq read-buffer-function 'iswitchb-read-buffer)
(setq iswitchb-regexp nil)
(setq iswitchb-prompt-newbuffer nil)

;;
(setq git-gutter-fr:side 'right-fringe)

;;
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;;
(setq inhibit-startup-screen t)

;;
(set-default 'indicate-empty-lines t)

;;
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;
(setq scroll-conservatively 1)

;;
(global-anzu-mode t)

;;
(add-to-list 'free-keys-modifiers "C-S")

;;
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;;
(setq next-line-add-newlines nil)
;; (setq require-final-newline t)

;;
(setq c-auto-newline nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
;; revert-buffer-with-coding-system

;;
;; (require 'wgrep)
;; (setq wgrep-enable-key "r")

;;
(setq make-backup-files (if (windows-p) -1 +1)) ; ~
(setq auto-save-default -1) ; #

;; ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)

;; display full-path in title-bar
;; http://wiki.s17.xrea.com/x/wiki/wiki.cgi?.emacs
(defvar dired-mode-p nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (make-local-variable 'dired-mode-p)
            (setq dired-mode-p t)))
(setq frame-title-format-orig frame-title-format)
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

;;
(add-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line)

;;
(add-hook 'dired-load-hook '(lambda () (load "dired-x")))

;;
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")
