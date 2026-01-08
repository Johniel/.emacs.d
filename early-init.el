;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; This file is loaded before init.el, before package.el and the GUI
;; is initialized. Use it for settings that must be set early.

;;; Code:

;; Increase GC threshold during startup (restore in init.el or after-init-hook)
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent package.el from auto-initializing (we call package-initialize in init.el)
(setq package-enable-at-startup nil)

;; Disable UI elements before frame creation to prevent flicker
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; menu-bar is kept enabled (set in appearance.el)

;; Suppress native-comp warnings (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil))

;; Prevent resizing frame when font changes
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
