;;; ys-dashboard.el
;;;
;;; Commentary:
;;;
;;; Dashboard is a neat package that lets up customize our Emacs opening screen.
;;;
;;; Code:
(use-package dashboard
  :ensure t
  :demand t ; We want this loaded immediately, not lazy-loaded
  :custom
  (dashboard-startup-banner (expand-file-name "images/sun.png" *ys/asset-dir*))
  (dashboard-center-content t)
  (dashboard-items '((projects . 5)
                     (recents . 5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-set-footer t)
  :config
  (dashboard-setup-startup-hook))

(provide 'ys-dashboard)

;;; ys-dashboard.el ends here
