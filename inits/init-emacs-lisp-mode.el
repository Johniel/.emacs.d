(require 'util)

(def-face elisp-special "khaki" "elisp special")
(def-face elisp-keyword "cyan1" "additional elisp keyword")

;; (add-keywords 'emacs-lisp-mode '("nil") 'elisp-special)
;; (add-keywords 'emacs-lisp-mode '("require-package") 'elisp-keyword)

(define-key emacs-lisp-mode-map (kbd "C-x e") 'pp-macroexpand-last-sexp)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(require 'elisp-slime-nav)
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
;; (eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (my-ac-emacs-lisp-mode-setup)))


(provide 'init-emacs-lisp-mode)
