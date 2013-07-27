;;;
(require 'foreign-regexp)

(custom-set-variables
 '(foreign-regexp/regexp-type 'perl) ;; Choose by your preference.
 '(reb-re-syntax 'foreign-regexp)) ;; Tell re-builder to use foreign regexp.

(provide 'setup-foreign-regexp)
