(require 'package)

;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;
(defun install-package (p)
  (when (not (package-installed-p p))
    (message "Installing: %s" p)
    (package-install p)
    (delete-other-windows)))

;;
(defun install-packages (packages)
  (if (member nil (mapcar 'package-installed-p packages))
      (progn (package-refresh-contents)
             (mapcar 'install-package packages))))

(provide 'load-package)
