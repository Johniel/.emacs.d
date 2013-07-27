(require 'util)

(require 'clojure-mode)
(require 'clojurescript-mode)
(require 'nrepl)
(require 'align-cljlet)
;; (require 'ac-clj)
(require 'highlight-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-face clojure-special "khaki" "Clojure special")
(add-keywords 'clojure-mode '("true" "false" "nil") 'clojure-special)
(add-keywords 'clojure-mode '("%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9") 'clojure-special)

(defun my-ac-clojure-mode-setup ()
  (setq ac-sources '(ac-source-dictionary
                     ac-source-words-in-buffer
                     ac-source-words-in-same-mode-buffers)))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (highlight-indentation-current-column-mode)
             (my-ac-clojure-mode-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nREPL
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq nrepl-popup-stacktraces nil)

(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)

(add-hook 'nrepl-mode-hook
          'clojure-mode-font-lock-setup)

(defalias 'clojure-repl 'nrepl-jack-in)
(defalias 'clj-repl 'nrepl-jack-in)

(defadvice nrepl-jack-in (after connect-to-clojure-repl activate)
  (nrepl-enable-on-existing-clojure-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ac-nrepl
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ac-nrepl)

(add-hook 'nrepl-mode-hook
          '(lambda ()
             (ac-nrepl-setup)
             (set-auto-complete-as-completion-at-point-function)
             (setq ac-auto-start nil)))

(add-hook 'nrepl-interaction-mode-hook
          '(lambda ()
             (ac-nrepl-setup)
             (set-auto-complete-as-completion-at-point-function)))

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

;;;
(provide 'setup-clojure-mode)
