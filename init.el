;;; init.el --- ___Johniel's init file

;;
;;
;;

(unless (>= emacs-major-version 24)
  (error "Use Emacs 24 or higher"))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'load-package)

;;
;;
;;
(install-packages '(ac-helm
                    ac-nrepl
                    ace-isearch
                    ace-jump-mode
                    ace-jump-zap
                    align-cljlet
                    all-ext
                    anzu
                    auto-complete
                    browse-kill-ring
                    cider
                    cl-lib
                    clojure-cheatsheet
                    clojure-mode
                    clojurescript-mode
                    color-moccur
                    dash
                    dirtree
                    elisp-slime-nav
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
                    ht
                    jedi
                    keyfreq
                    lua-mode
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
                    shell-pop
                    web-mode
                    wgrep
                    wrap-region
                    yaml-mode
                    yasnippet
                    zencoding-mode
                    zlc))

;;
;;
;;
(require 'util)
(add-to-load-path-r "inits")
(add-to-load-path-r "elpa")
(add-to-load-path-r "lisp")
(add-to-load-path-r "site-lisp")
(add-to-load-path-r "theme")

;;
;;
;;
(defun performance-saving-p ()
  (or  (windows-p)
       (member (system-name) '())))

;;
;;
;;
(require 'anzu)
(require 'color-moccur)
(require 'dash)
(require 'dirtree)
(require 'expand-region)
(require 'f)
(require 'fold-this)
(require 'free-keys)
(require 'git-gutter-fringe)
(require 'ht)
(require 'keyfreq)
(require 'lua-mode)
(require 'ov)
(require 'point-undo)
(require 'redo+)
(require 's)
(require 'scc)
(require 'tempbuf)
(require 'typo-fix)
(require 'wgrep)
(require 'yaml-mode)

;;
;;
;;
(require 'init-ace-isearch)
(require 'init-ace-jump-mode)
(unless (performance-saving-p) (require 'init-auto-complete))
(require 'init-auto-save-buffers)
(require 'init-browse-kill-ring)
(require 'init-eshell)
(require 'init-dired)
(require 'init-flymake)
(require 'init-flyspell)
(unless (windows-p) (require 'init-gtags))
(require 'init-helm)
(require 'init-hippie-expand)
(require 'init-howm)
(require 'init-magit)
(require 'init-multiple-cursors)
(require 'init-popwin)
(require 'init-scratch)
(require 'init-screen-lines)
(require 'init-sequential-command)
(require 'init-shell-pop)
(if (linux-p) (require 'init-skk))
(unless (performance-saving-p) (require 'init-tabbar+))
(require 'init-whitespace)
(require 'init-wrap-region)
(require 'init-yasnippet)
(require 'init-zlc)
(require 'init-tramp)
(require 'integrate-yas-ac)

;;
;; Major Mode Setup
;;
(require 'init-c++-mode)
(require 'init-c-mode)
(require 'init-clojure-mode)
(require 'init-emacs-lisp-mode)
(require 'init-lua-mode)
(require 'init-markdown-mode)
(require 'init-php-mode)
(require 'init-python-mode)
(require 'init-web-mode)

;;
;;
;;
(load "mode-mappings.el")
(load "my-misc.el")
(load "global-bindings.el")
(load "appearance.el")

;;
;;
;;

;; (require 'k7)
(put 'erase-buffer 'disabled nil)

;; latest ddskk
(put 'modeline 'face-alias 'mode-line)
