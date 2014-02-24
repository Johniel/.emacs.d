(require 'util)

(require 'clojure-mode)
(require 'clojurescript-mode)
(require 'align-cljlet)
(require 'highlight-indentation)
(require 'cider)

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
;; cider
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'cider-mode-hook 'cider-turn-on-eldec-mode)

;;;
(provide 'setup-clojure-mode)
