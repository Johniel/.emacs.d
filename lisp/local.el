;;; local.el --- Local machine-specific configuration -*- lexical-binding: t -*-

;; Copyright (C) 2026 johniel

;; Author: johniel
;; Created: 2026-01-29
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file holds the defaults of machine-specific configurations and is
;; version controlled.  To change them on a machine, create
;; local-override.el next to this file and set only the values to change.
;; local-override.el is loaded at the end of this file if it exists and is
;; not version controlled.
;;
;; Typical contents:
;; - Local file paths and directories
;; - Machine-specific compiler settings
;; - Personal credentials and API keys
;; - Environment-specific development settings
;; - Host-specific network configurations
;;
;; Write sensitive information only in local-override.el.

;;; Code:

;; ============================================================================
;; Machine Information
;; ============================================================================

;; Uncomment and customize as needed:
;; (defconst my/machine-name (system-name)
;;   "Current machine name for conditional configuration.")

;; (defconst my/is-work-machine
;;   (string-match-p "work\\|company" (system-name))
;;   "Non-nil if this is a work machine.")

;; ============================================================================
;; Local Development Environment
;; ============================================================================

;; Flycheck compiler settings
(defvar local-flycheck-c/c++-gcc-executable nil
  "Path to the local g++ compiler executable for Flycheck C/C++ syntax checking.
Should be an absolute path to the g++ binary, e.g., \"/opt/homebrew/bin/g++-14\".")

(defvar local-flycheck-gcc-args nil
  "List of compiler arguments for Flycheck g++ checker.
Should include include paths, standards, and warning flags as strings.
Example: '(\"-std=c++23\" \"-I/usr/local/include\" \"-Wall\" \"-Wextra\")")

;; Usage examples (in local-override.el):
;; (setq local-flycheck-c/c++-gcc-executable "/opt/homebrew/bin/g++-14")
;; (setq local-flycheck-gcc-args
;;   '("-std=c++23"
;;     "-Wall"
;;     "-Wextra"
;;     "-I/opt/homebrew/include"
;;     "-I/opt/homebrew/Cellar/gcc/14.2.0_1/include/c++/14"))

;; Example: Local project directories
;; (setq my/projects-directory "~/Projects")
;; (setq my/competitive-programming-dir "~/competitive")

;; ============================================================================
;; Personal Information
;; ============================================================================

;; Example: Personal details (uncomment and customize)
;; (setq user-full-name "Your Full Name")
;; (setq user-mail-address "your.email@example.com")

;; ============================================================================
;; Network and Proxy Settings
;; ============================================================================

;; Example: Corporate proxy settings
;; (when my/is-work-machine
;;   (setq url-proxy-services
;;         '(("http"  . "proxy.company.com:8080")
;;           ("https" . "proxy.company.com:8080"))))

;; ============================================================================
;; Security and Credentials
;; ============================================================================

;; Example: API keys and secrets (be careful with these!)
;; (setq my/github-token "your-github-token-here")
;; (setq my/openai-api-key "your-openai-key-here")

;; WARNING: Consider using auth-sources or external tools like pass/keychain
;; for sensitive credentials instead of hardcoding them here.

;; ============================================================================
;; Local Function Definitions
;; ============================================================================

;; Add any machine-specific utility functions here

;; Example: Quick access to local directories
;; (defun my/open-projects ()
;;   "Open projects directory in dired."
;;   (interactive)
;;   (dired my/projects-directory))

;; ============================================================================
;; Load Additional Local Files
;; ============================================================================

;; Example: Load additional local configuration files
;; (let ((local-configs '("local-projects.el" "local-secrets.el")))
;;   (dolist (config local-configs)
;;     (let ((file (expand-file-name config
;;                                   (file-name-directory
;;                                    (or load-file-name buffer-file-name)))))
;;       (when (file-exists-p file)
;;         (load file)))))

;; ============================================================================
;; Machine-specific Overrides
;; ============================================================================

(load (expand-file-name "local-override"
                        (file-name-directory (or load-file-name buffer-file-name)))
      t)

(provide 'local)

;;; local.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
