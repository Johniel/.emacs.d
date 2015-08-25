(require 'util)

(require 'clojure-mode)
(require 'align-cljlet)
(require 'cider)

(def-face clojure-special "khaki" "Clojure special")
(add-keywords 'clojure-mode '("true" "false" "nil") 'clojure-special)
(add-keywords 'clojure-mode '("%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9") 'clojure-special)

;;;
(provide 'init-clojure-mode)
