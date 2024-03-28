;;; ys-misc.el --- miscellanea
;;;
;;; Commentary:
;;;
;;; Miscellaneous packages whose definitions are too short to warrant
;;; separate files.
;;;
;;; Code:

;; Magit
(use-package magit)

;; Git gutter
(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

;; Terraform
(use-package terraform-mode
  :custom (terraform-indent-level 2)
  :config
  (defun my-terraform-mode-init ()
    (outline-minor-mode 1)
    (setq terraform-format-on-save t))
  :hook (terraform-mode-hook . my-terraform-mode-init))


;; posframe for pop-up frames (used by a couple other packages)
(use-package posframe)


(provide 'ys-misc)

;;; ys-misc.el ends here
