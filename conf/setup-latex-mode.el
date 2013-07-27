(require 'tex-mode)

;; http://d.hatena.ne.jp/ryoma_robo/20120503/1336012148

(setq tex-default-mode 'latex-mode)

;; (setq-default ispell-program-name "/usr/bin/aspell") ;; path to aspell
(setq flyspell-issu-welcome-flag nil)

(add-hook 'tex-mode-hook 'flyspell-mode)
(add-hook 'slitex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'bibtex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(add-to-list 'tex-compile-commands
             '("platex %f"))

(add-hook 'flyspell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c n") 'flyspell-correct-word-before-point)))

;; DocView auto-revert-mode
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq doc-view-continuous t)


(provide 'setup-latex-mode)
