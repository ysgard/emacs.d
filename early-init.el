;;; early-init.el
;;;
;;; Commentary:
;;; Stuff that happens before the stuff that happens
;;;
;;; Code:
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Stop Emacs from trying to resize on launch
(setq frame-inhibit-implied-resize t)
;; Disable the built-in packager as we're using straight
(setq package-enable-at-startup nil)

;;; early-init.el ends here
