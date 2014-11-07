;;; http://code.google.com/p/my-dot-emacs-files/source/browse/trunk/.emacs23.3/preferences/library.config/cedet.el?spec=svn129&r=129

(require 'cedet nil t)

;; Configuration variables here:
(setq semantic-load-turn-useful-things-on t)
(setq semantic-load-turn-everything-on t)

;;
(setq semantic-default-submodes
      '(
        ;;
        ;;
        global-semantic-idle-scheduler-mode

        ;;
        global-semanticdb-minor-mode

        ;;
        ;; global-semantic-idle-summary-mode

        ;;
        ;; global-semantic-idle-completions-mode

        ;;
        ;; global-semantic-decoration-mode

        ;;
        ;; global-semantic-highlight-func-mode

        ;;
        ;; global-semantic-mru-bookmark-mode
        ))

;; do not swap this lines
(global-ede-mode 1)
(semantic-mode 1)

(semantic-add-system-include "/usr/include" 'c++-mode)
(semantic-add-system-include "/usr/local/include" 'c++-mode)
(semantic-add-system-include "/usr/include/c++/4.6" 'c++-mode)

;;
;; *_flymake.cpp file is flooded!
;; (setq semantic-idle-work-parse-neighboring-files-flag t)

;; (define-key semantic-mode-map (kbd "<backtab>") 'semantic-complete-analyze-inline-idle)
;; (define-key semantic-mode-map (kbd "C-<tab>") 'semantic-complete-analyze-inline-idle)
;; (define-key semantic-mode-map (kbd "M-<tab>") 'semantic-complete-analyze-inline-idle)
;; (define-key semantic-mode-map (kbd "M-SPC") 'semantic-complete-analyze-inline-idle)

(require 'semantic/complete)
(setq semantic-complete-inline-map
      (let ((km semantic-complete-inline-map))
        (define-key km " " 'semantic-complete-inline-TAB)
        km))

(provide 'init-cedet)
