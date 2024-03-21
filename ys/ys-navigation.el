;;; ys-navigation.el --- File navigation and sidebars
;;;
;;; Commentary:
;;;
;;; Dired, treemacs, fuzzy-finding, etc...
;;;
;;; Code:

;; Ido config
(use-package emacs
  :init
  ;; We use ido for fuzzy finding
  (setq ido-enable-flex-matching t ; Match characters if we can't match substring
        ido-enable-prefix nil      ; Finds matches that aren't prefixes
        ido-use-filename-at-point 'guess
        ido-use-url-at-point t
        ido-max-prospects 10
        ido-create-new-buffer 'always
        ido-use-virtual-buffers t)
  ;; Show more commonly used file extensions first
  (setq ido-file-extensions-order '(".tf" ".rb" ".md" ".txt"))
  (ido-mode t)
  ;; Add a hook to `~' sends us to homedir with ido-find-file
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-file-completion-map
                          (kbd "~")
                          (lambda ()
                            (interactive)
                            (if (looking-back "/")
                                (insert "~/")
                              (call-interactively 'self-insert-command)))))))

;; Fuzzy finding
;; Ido everywhere!
(use-package ido-completing-read+
  :config (ido-ubiquitous-mode t))

;; Smex provides an ido-like interface to M-x
(use-package smex
  :config (smex-initialize)
  :general
  (leader-keys
    "X" '(smex :which-key "execute")))

(use-package ido-vertical-mode :config (ido-vertical-mode))

(use-package flx-ido
  :config
  (flx-ido-mode t)
  (setq ido-enable-flex-matching t
        ido-use-faces nil
        gc-cons-threshold 20000000))

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
  :after (treemacs evil))
(use-package treemacs-projectile
  :after (treemacs projectile))
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))
(use-package treemacs-magit
  :after (treemacs magit))


(provide 'ys-navigation)

;;; ys-navigation.el ends here
