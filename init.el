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
                    anzu
                    auto-complete
                    browse-kill-ring
                    cider
                    cl-lib
                    clojure-mode
                    clojure-cheatsheet
                    clojurescript-mode
                    color-moccur
                    dash
                    elisp-slime-nav
                    elpy
                    expand-region
                    f
                    fold-this
                    free-keys
                    git-gutter-fringe
                    gtags
                    haskell-mode
                    helm
                    helm-gtags
                    helm-ls-git
                    highlight-indentation
                    ht
                    magit
                    markdown-mode
                    multiple-cursors
                    ov
                    php-eldoc
                    php-mode
                    popup
                    popwin
                    quickrun
                    redo+
                    s
                    screen-lines
                    web-mode
                    wrap-region
                    yaml-mode
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
(require 'anzu)
(require 'color-moccur)
(require 'dash)
(require 'expand-region)
(require 'f)
(require 'fold-this)
(require 'free-keys)
(require 'git-gutter-fringe)
(require 'ht)
(require 'ov)
(require 'point-undo)
(require 'redo+)
(require 's)
(require 'scc)
(require 'tempbuf)
(require 'yaml-mode)
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
(if (linux-p) (require 'setup-gtags))
(require 'setup-helm)
(require 'setup-highlight-indentation)
(require 'setup-hippie-expand)
(require 'setup-howm)
(require 'setup-magit)
(require 'setup-multiple-cursors)
(require 'setup-popwin)
(require 'setup-scratch)
(require 'setup-screen-lines)
(require 'setup-sequential-command)
(if (linux-p) (require 'setup-skk))
(unless (performance-saving-p) (require 'setup-tabbar+))
(require 'setup-whitespace)
(require 'setup-wrap-region)
(require 'setup-yasnippet)
(require 'setup-zlc)
(require 'setup-tramp)
(require 'integrate-yas-ac)

;;
;; Major Mode Setup
;;
(require 'setup-c++-mode)
(require 'setup-c-mode)
(require 'setup-clojure-mode)
(require 'setup-emacs-lisp-mode)
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
