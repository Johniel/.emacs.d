(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;
(defun require-package (p)
  (when (not (package-installed-p p))
    (message "Installing: %s" p)
    (package-install p)
    (delete-other-windows)))

;;
(defun packages-install (packages)
  (package-refresh-contents)
  (mapcar 'require-package packages))


(provide 'init-package)
