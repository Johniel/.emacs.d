(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;;
(defun install-package (p)
  (when (not (package-installed-p p))
    (message "Installing: %s" p)
    (package-install p)
    (delete-other-windows)))

;;
(defun require-packages (packages)
  (if (member nil (mapcar 'package-installed-p packages))
      (progn (package-refresh-contents)
             (mapcar 'install-package packages))))

(provide 'load-package)
