(require 'package)
(require 'dash)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defun packages-install (packages)
  "install package if they are missing"
  (--each packages
    (when (not (package-installed-p it))
      (message "install %s" it)
      (package-install it)))
  (delete-other-windows))

(provide 'setup-package)
