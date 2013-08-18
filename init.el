;;; init.el --- ___Johniel's init file

;;
;;
;;

(unless (>= 24 emacs-major-version)
  (error "Use Emacs 24 or higher"))

(add-to-list 'load-path "~/.emacs.d")

(defun performance-saving-p ()
  (member (system-name) '()))

(defun linux-p    () (eq 'gnu/linux  system-type))
(defun windowns-p () (eq 'windows-nt system-type))

;;
;;
;;

(require 'util)
(add-to-load-path-r "elpa")
(add-to-load-path-r "site-lisp")
(add-to-load-path-r "conf")
(add-to-load-path-r "theme")

;;
;;
;;

(require 'all-ext)
(require 'color-moccur)
(require 'dash)
(require 'expand-region)
(require 'fold-this)
(require 'mark-multiple)
(require 'litable)
(require 'point-undo)
(require 'redo+)
(require 's)
(require 'smooth-scrolling)
(require 'tempbuf)
(require 'yascroll)

;;
;;
;;

(require 'setup-ace-jump-mode)
(require 'setup-auto-complete)
(require 'setup-auto-save-buffers)
(require 'setup-browse-kill-ring)
(require 'setup-eshell)
(require 'setup-dired)
(require 'setup-flymake)
(require 'setup-flyspell)
(require 'setup-golden-ratio)
(require 'setup-gtags)
(require 'setup-helm)
(require 'setup-highlight-indentation)
(require 'setup-hippie-expand)
(require 'setup-howm)
(require 'setup-linum)
(require 'setup-magit)
(require 'setup-multiple-cursors)
(require 'setup-package)
(require 'setup-popwin)
(require 'setup-scratch)
(require 'setup-screen-lines)
(require 'setup-sequential-command)
(require 'setup-skk)
(require 'setup-smartchr)
(require 'setup-tabbar)
(require 'setup-web-mode)
(require 'setup-whitespace)
(require 'setup-wrap-region)
(require 'setup-yasnippet)
(require 'setup-zlc)
;; (require 'setup-cedet)
;; (require 'setup-tramp)
;; (require 'setup-smex)
;; (require 'setup-guide-key)
(require 'integrate-yas-ac)

;;
;; Major Mode Setup
;;

(require 'setup-auctex) ;; latex
(require 'setup-c-mode)
(require 'setup-c++-mode)
(require 'setup-clojure-mode)
(require 'setup-emacs-lisp-mode)
(require 'setup-html-mode)
(require 'setup-java-mode)
(require 'setup-markdown-mode)

;;
;;
;;
(load "mode-mappings.el")
(load "my-misc.el")
(load "key-bindings.el")
(load "appearance.el")

;;
;;
;;

;; (require 'k7)
(put 'erase-buffer 'disabled nil)

;; latest ddskk
(put  'modeline 'face-alias 'mode-line)
