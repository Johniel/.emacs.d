;;; init.el --- ___Johniel's init file

;;
;;
;;

(unless (>= 24 emacs-major-version)
  (error "Use Emacs 24 or higher"))

(add-to-list 'load-path "~/.emacs.d")

;;
;;
;;
(require 'load-package)
(require-packages '(ac-helm
                    ac-nrepl
                    ace-jump-mode
                    align-cljlet
                    all-ext
                    anzu
                    auctex
                    auto-complete
                    browse-kill-ring
                    cl-lib
                    clojure-mode
                    clojurescript-mode
                    color-moccur
                    csv-mode
                    dash
                    elisp-slime-nav
                    expand-region
                    fold-this
                    golden-ratio
                    gtags
                    helm
                    helm-gtags
                    highlight-indentation
                    ht
                    keyfreq
                    litable
                    magit
                    markdown-mode
                    multiple-cursors
                    nrepl
                    php-eldoc
                    php-mode
                    popup
                    popwin
                    redo+
                    s
                    screen-lines
                    smooth-scrolling
                    wrap-region
                    web-mode
                    yascroll
                    yasnippet
                    zencoding-mode
                    zlc))

;;
;;
;;
(require 'util)
(add-to-load-path-r "elpa")
(add-to-load-path-r "site-lisp")
(add-to-load-path-r "conf")
(add-to-load-path-r "theme")

(defun performance-saving-p ()
  (or  (windows-p)
       (member (system-name) '())))

;;
;;
;;
(require 'all-ext)
(require 'anzu)
(require 'color-moccur)
(require 'dash)
(require 'expand-region)
(require 'fold-this)
(require 'ht)
(require 'litable)
(require 'point-undo)
(require 'redo+)
(require 's)
(require 'smooth-scrolling)
(require 'tempbuf)
(unless (performance-saving-p) (require 'yascroll))

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
(if (linux-p) (require 'setup-gtags))
(require 'setup-helm)
(require 'setup-highlight-indentation)
(require 'setup-hippie-expand)
(require 'setup-howm)
(require 'setup-keyfreq)
(require 'setup-linum)
(require 'setup-magit)
(require 'setup-multiple-cursors)
(require 'setup-popwin)
(require 'setup-scratch)
(require 'setup-screen-lines)
(require 'setup-sequential-command)
(if (linux-p) (require 'setup-skk))
(require 'setup-smartchr)
(unless (performance-saving-p) (require 'setup-tabbar+))
(require 'setup-whitespace)
(require 'setup-wrap-region)
(require 'setup-yasnippet)
(require 'setup-zlc)
;; (require 'setup-cedet)
(require 'setup-tramp)
(require 'integrate-yas-ac)

;;
;; Major Mode Setup
;;
(require 'setup-auctex) ;; latex
(require 'setup-c++-mode)
(require 'setup-c-mode)
(require 'setup-clojure-mode)
(require 'setup-emacs-lisp-mode)
(require 'setup-java-mode)
(require 'setup-markdown-mode)
(require 'setup-php-mode)
(require 'setup-web-mode)

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
(put 'modeline 'face-alias 'mode-line)
