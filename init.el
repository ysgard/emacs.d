;;; init.el
;;;
;;; Commentary: Entry point for our Emacs config
;;; Code:

;; Import some utility functions
(add-to-list 'load-path "~/.emacs.d/ys")
(require 'ys-lib)

;;; Package management
(require 'ys-package)

;;; Garbage collection

(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))


;;; Base
(require 'ys-base)

;;; Editing
(require 'ys-editor)

;; Set sane defaults
(use-package emacs ; Wrap emacs-specific settings in a use-package block
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


;; Keybinds
;; We load this a bit early so other packages can take advantage
;; of :general in their package loads.
(require 'ys-keybinds)
;; Become EVIL
(require 'ys-evil)
;;; Terminal configs
(require 'ys-terminal)
;;; Display and Themes
(require 'ys-display)
;;; File navigation and sidebars
(require 'ys-navigation)

;;; Tree-sitter
(use-package emacs
  :init
  (setq treesit-language-source-alist
        '((bash "git@github.com:tree-sitter/tree-sitter-bash")
          (cmake "git@github.com:tree-sitter/tree-sitter-cmake")
          (css "git@github.com:tree-sitter/tree-sitter-css")
          (elisp "git@github.com:tree-sitter/tree-sitter-elisp")
          (go "git@github.com:tree-sitter/tree-sitter-go")
          (html "git@github.com:tree-sitter/tree-sitter-html")
          (javascript "git@github.com:tree-sitter/tree-sitter-javascript" "master" "src")
          (json "git@github.com:tree-sitter/tree-sitter-json")
          (make "git@github.com:tree-sitter/tree-sitter-make")
          (markdown "git@github.com:tree-sitter/tree-sitter-markdown")
          (python "git@github.com:tree-sitter/tree-sitter-python")
          (regex "git@github.com:tree-sitter/tree-sitter-regex")
          (ruby "git@github.com:tree-sitter/tree-sitter-ruby")
          (toml "git@github.com:tree-sitter/tree-sitter-toml")
          (tsx "git@github.com:tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "git@github.com:tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "git@github.com:tree-sitter/tree-sitter-yaml"))))


;;; Packages


;; Projects
(use-package projectile
  :demand
  :general
  (leader-keys
    :states 'normal
    "SPC" '(projectile-find-file :which-key "find file")

    ;; Buffers
    "p b" '(projectile-switch-to-buffer :which-key "switch buffer")

    ;; Projects
    "p" '(:ignore t :which-key "projects")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "p p" '(projectile-switch-project :which-key "switch project")
    "p a" '(projectile-add-known-project :which-key "add project")
    "p r" '(projectile-remove-known-project :which-key "remove project"))
  :init
  (projectile-mode +1))

;; Make sure our path matches the system's
;; We defer initialization until after init
;; As most of our use-package use defer.
(defun ys/exec-path-from-shell-init ()
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package exec-path-from-shell
  :init
  (add-hook 'after-init-hook 'ys/exec-path-from-shell-init))

;; Fuzzy finding
;; Ido everywhere!
(use-package ido-completing-read+
  :config (ido-ubiquitous-mode t))

;; Smex provides an ido-like interface to M-x
(use-package smex
  :config (smex-initialize)
  :general
  (leader-keys
    "x" '(smex :which-key "execute")))

(use-package ido-vertical-mode :config (ido-vertical-mode))

(use-package flx-ido
  :config
  (flx-ido-mode t)
  (setq ido-enable-flex-matching t
        ido-use-faces nil
        gc-cons-threshold 20000000))


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
(use-package terraform-mode)

;; Profiler for startup times
(use-package esup
  :ensure t)

;;; Dashboard
(require 'ys-dashboard)
(dashboard-setup-startup-hook)

;;; init.el ends here
