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
(savehist-mode 1)

;;
(auto-sudoedit-mode 1)

;;
(global-auto-revert-mode -1)
;; http://d.hatena.ne.jp/syohex/20130206/1360157000
(require 'notifications)
(when (linux-p)
  (defun my/after-revert-hook ()
    (notifications-notify :title (format "Revert %s" (buffer-file-name))
                          :body "Check it out" :urgency 'critical))
  (add-hook 'after-revert-hook 'my/after-revert-hook))

(when (mac-p)
  (defvar notification-center-title "Emacs")
  (defun notification-center (title body)
    (let ((tmpfile (make-temp-file "notification-center")))
      (with-temp-file tmpfile
        (insert (format "display notification \"%s\" with title \"%s\"" body title)))
      (unless (zerop (call-process "osascript" tmpfile))
        (message "Failed: Call AppleScript"))
      (delete-file tmpfile)))

  (defun notifications-notify:override (&rest params)
    (let ((title (plist-get params :title))
          (body (plist-get params :body)))
      (notification-center title body)))

  (advice-add 'notifications-notify :override 'notifications-notify:override))


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
(cua-mode 1)
(setq cua-enable-cua-keys nil)

;;
(setq inhibit-startup-screen t)

;;
(set-default 'indicate-empty-lines t)

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

;;
(require 'wgrep)
(setf wgrep-enable-key "r")
(setq wgrep-auto-save-buffer t)
(setq wgrep-change-readonly-file t)

;; ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)

;; display full-path in title-bar
;; http://wiki.s17.xrea.com/x/wiki/wiki.cgi?.emacs
(defvar dired-mode-p nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (setq-local dired-mode-p t)))
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
(define-minor-mode dtw-mode
  ""
  :group 'dtw
  :init-value nil
  :global t
  :lighter dtw-mode
  (progn (if dtw-mode
             (add-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line)
           (remove-hook 'before-save-hook 'delete-trailing-whitespace-except-current-line))))

(defun dtw--turn-on ()
  (dtw-mode +1))

(define-globalized-minor-mode global-dtw-mode dtw-mode dtw--turn-on
  :group 'dtw)

;;
(add-hook 'dired-load-hook '(lambda () (load "dired-x")))

;;
(setq save-place-file "~/.emacs.d/saved-places")
(save-place-mode 1)

;;
(unless (windows-p) (keyfreq-mode +1))

;; http://emacsredux.com/blog/2013/05/04/erase-buffer/
(put 'erase-buffer 'disabled nil)

;; *.~
(setq make-backup-files nil)
;; .#*
(setq auto-save-default nil)

;; (fido-vertical-mode +1)

;; https://lists.gnu.org/archive/html/help-gnu-emacs/2021-05/msg00558.html
(setq elisp-flymake-byte-compile-load-path load-path)
