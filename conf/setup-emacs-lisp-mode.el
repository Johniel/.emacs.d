(require 'util)

(def-face elisp-special "khaki" "elisp special")
(def-face elisp-keyword "cyan1" "additional elisp keyword")

;; (add-keywords 'emacs-lisp-mode '("nil") 'elisp-special)
;; (add-keywords 'emacs-lisp-mode '("require-package") 'elisp-keyword)

(define-key emacs-lisp-mode-map (kbd "C-x e") 'pp-macroexpand-last-sexp)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(require-package 'elisp-slime-nav)
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
;; (eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

(require 'auto-complete)
(require 'auto-complete-config)

(defun my-ac-emacs-lisp-mode-setup ()
  (setq ac-sources '(ac-source-features
                     ac-source-functions
                     ac-source-variables
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers)))
(add-hook 'emacs-lisp-mode-hook 'my-ac-emacs-lisp-mode-setup)

(provide 'setup-emacs-lisp-mode)
