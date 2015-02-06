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
(defun install-packages (packages)
  (if (member nil (mapcar 'package-installed-p packages))
      (progn (package-refresh-contents)
             (mapcar 'install-package packages))))

(defmacro require-package (p)
  `(progn (if (not (locate-library (symbol-name ,p)))
              (install-package ,p)
            (require ,p))))

(provide 'load-package)
