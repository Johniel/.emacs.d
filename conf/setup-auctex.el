;;; setup-auctex.el ---

(require 'latex)
(require 'tex-site)
(require 'tex-mode)

(defvar tex-mode-list
  '(tex-mode-hook
    slitex-mode-hook
    latex-mode-hook
    bibtex-mode-hook
    LaTeX-mode-hook))

;; http://d.hatena.ne.jp/ryoma_robo/20120503/1336012148

;; (setq tex-default-mode 'latex-mode)

;; (setq-default ispell-program-name "/usr/bin/aspell") ;; path to aspell
(setq flyspell-issu-welcome-flag nil)


;; DocView auto-revert-mode
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq doc-view-continuous t)

(dolist (mode tex-mode-list)
  (add-hook mode 'flyspell-mode))

(add-to-list 'tex-compile-commands
             '("platex %f"))

(require 'setup-flyspell)
(add-hook 'flyspell-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-c n") 'flyspell-correct-word-popup-el)))

;; http://wikemacs.org/wiki/AUCTeX
(require 'font-latex)
(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)
; modify Beamer as well
(custom-set-faces
 '(font-latex-slide-title-face ((t (:inherit font-lock-type-face)))))
(font-latex-update-sectioning-faces)

(provide 'setup-auctex)
