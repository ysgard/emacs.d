;;; ys-navigation.el --- File navigation and sidebars
;;;
;;; Commentary:
;;;
;;; Dired, treemacs, fuzzy-finding, etc...
;;;
;;; Code:

;; Dired
(require 'dired)
;; Reuse dired buffers
(use-package dired-single
  :config
  (define-key dired-mode-map (kbd "RET") 'dired-single-buffer))

;; Dired project drawer
(use-package dired-sidebar
  :after dired-subtree
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :general
  (leader-keys
    :keymap 'dired-mode-map
    "w o" '(other-window :which-key "other"))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__"
        dired-sidebar-theme 'nerd
        dired-sidebar-use-term-integration t
        dired-sidebar-use-custom-font nil
        dired-sidebar-should-follow-file nil
        dired-sidebar-use-one-instance t))

;; Have a nice sidebar for buffers too
(use-package ibuffer-sidebar
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font nil))

;; Treemacs
(use-package treemacs
  :general
  (leader-keys
    "t" '(treemacs :which-key "treemacs")))
(use-package treemacs-evil
  :bind (:map evil-treemacs-state-map
              ("?" . treemacs-helpful-hydra)
              ("M-?" . treemacs-helpful-hydra))
  :after (treemacs evil))
(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
(use-package treemacs-magit
  :after (treemacs magit))


(provide 'ys-navigation)

;;; ys-navigation.el ends here
